#' Ingest Legacy Zooplankton Data (1984-2015)
#'
#' Downloads, validates, and harmonizes the 1984-2015 legacy zooplankton dataset
#' from SharePoint. Taxa are matched against the WoRMS database and the output
#' is written in both Parquet and CSV formats to the hot storage bucket.
#'
#' @return Invisible NULL. Outputs are uploaded to SharePoint as
#'   `McZoo_84-15.parquet` and `McZoo_84-15.csv`.
#'
#' @details
#' The function performs the following steps:
#' 1. Downloads sample ID metadata (`ids_84_15.csv`) and biological data
#'    (`zoo_84_15.csv`) from the `legacy_data` SharePoint bucket.
#' 2. Integrates manually curated unmatched taxa from
#'    `unmatched_worms_84_15.xlsx` to correct known synonyms before WoRMS
#'    validation.
#' 3. Queries WoRMS via `worrms::wm_records_taxamatch()` for every unique
#'    taxon; unmatched taxa are flagged with `match_type = "no_match"`.
#' 4. Selects a single AphiaID per taxon (lowest, i.e. oldest classification).
#' 5. Joins biological observations with WoRMS-validated taxonomy and sample
#'    metadata, pivoting date columns to long format.
#' 6. Standardises life-stage codes.
#' 7. Aggregates duplicate records by summing `Abundance`.
#' 8. Uploads tidy data to the hot storage bucket in Parquet and CSV formats.
#'
#' @note Unmatched taxa (no WoRMS hit) are reported to the console and excluded
#'   from the final export. Curators should review them and add corrections to
#'   `unmatched_worms_84_15.xlsx` before re-running.
#'
#' @keywords ingestion legacy
#' @export
#'
#' @examples
#' \dontrun{
#' ingest_legacy_84_15()
#' }
ingest_legacy_84_15 <- function() {
  conf <- read_config()

  logger::log_info(
    "Downloading sample ID metadata (1984-2015)...",
    namespace = "ZooGoN"
  )
  ids <-
    download_sharepoint_file(
      prefix = "ids_84_15.csv",
      options = conf$storage$sharepoint$credentials,
      bucket = "legacy_data",
      filename = TRUE
    ) |>
    dplyr::mutate(
      date = lubridate::as_date(as.numeric(.data$date), origin = "1899-12-30"),
      sample_id = janitor::make_clean_names(.data$sample_id),
      sample_id = stringr::str_replace_all(.data$sample_id, "_", "")
    )

  logger::log_info(
    "Downloading biological data (1984-2015)...",
    namespace = "ZooGoN"
  )
  bio <-
    download_sharepoint_file(
      prefix = "zoo_84_15.csv",
      options = conf$storage$sharepoint$credentials,
      bucket = "legacy_data",
      filename = TRUE
    ) |>
    dplyr::mutate(
      dat_id = seq_len(dplyr::n()),
      TAXA = stringr::str_trim(.data$TAXA)
    )

  logger::log_debug(
    "Biological records loaded: {nrow(bio)}",
    namespace = "ZooGoN"
  )

  dates <-
    bio |>
    dplyr::select(-c(1:10), "dat_id")

  logger::log_info(
    "Integrating manually curated unmatched taxa...",
    namespace = "ZooGoN"
  )
  unmatched_fixed <-
    download_sharepoint_file(
      prefix = "unmatched_worms_84_15.xlsx",
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$unmatched_bucket,
      filename = TRUE
    ) |>
    janitor::clean_names()

  bio <-
    bio |>
    dplyr::left_join(unmatched_fixed, by = c("TAXA" = "reported_taxa")) |>
    dplyr::mutate(
      TAXA = dplyr::case_when(
        !is.na(.data$accepted_scientific_name) ~ .data$accepted_scientific_name,
        TRUE ~ .data$TAXA
      ),
      stage = dplyr::case_when(
        !is.na(.data$lifestage) ~ .data$lifestage,
        TRUE ~ .data$stage
      )
    ) |>
    dplyr::select(-c("accepted_scientific_name", "aphia_id", "lifestage"))

  reported_taxa <-
    as.character(unique(bio$TAXA))

  logger::log_info(
    "Querying WoRMS for {length(reported_taxa)} unique taxa...",
    namespace = "ZooGoN"
  )

  worms_matched <- purrr::map2_dfr(
    .x = seq_along(reported_taxa),
    .y = reported_taxa,
    .f = function(i, taxon) {
      res <- tryCatch(
        worrms::wm_records_taxamatch(taxon),
        error = function(e) NULL
      )
      if (
        is.null(res) ||
          length(res) == 0 ||
          is.null(res[[1]]) ||
          nrow(res[[1]]) == 0
      ) {
        return(dplyr::tibble(
          original = taxon,
          AphiaID = NA_integer_,
          scientificname = NA_character_,
          status = NA_character_,
          match_type = "no_match"
        ))
      }

      res[[1]] %>% dplyr::mutate(original = taxon, .before = 1)
    }
  )

  verification_unmatched <-
    worms_matched |>
    dplyr::filter(
      is.na(.data$valid_AphiaID) | .data$match_type == "no_match"
    ) |>
    dplyr::distinct(.data$original)

  if (nrow(verification_unmatched) > 0) {
    logger::log_warn(
      "{nrow(verification_unmatched)} taxa did not match on WoRMS and will be excluded.",
      namespace = "ZooGoN"
    )
    print(verification_unmatched)
  } else {
    logger::log_info("All taxa matched on WoRMS.", namespace = "ZooGoN")
  }

  worms_matched_clean <-
    worms_matched |>
    dplyr::select(
      "original",
      "AphiaID",
      "lsid",
      "scientificname",
      "status",
      "valid_AphiaID",
      "valid_name",
      "match_type"
    ) |>
    dplyr::distinct()

  logger::log_info(
    "Joining taxonomy with sample metadata...",
    namespace = "ZooGoN"
  )

  taxa_df <-
    bio |>
    dplyr::select(
      "dat_id",
      reported_taxa = "TAXA",
      "stage",
    ) |>
    dplyr::full_join(
      worms_matched_clean,
      by = c("reported_taxa" = "original")
    ) |>
    dplyr::mutate(
      AphiaID = dplyr::if_else(
        .data$status == "unaccepted",
        .data$valid_AphiaID,
        .data$AphiaID
      ),
      scientificname = dplyr::if_else(
        .data$status == "unaccepted",
        .data$valid_name,
        .data$scientificname
      ),
      lsid = dplyr::if_else(
        .data$status == "unaccepted",
        paste0("urn:lsid:marinespecies.org:taxname:", .data$AphiaID),
        .data$lsid
      )
    ) |>
    dplyr::select(-c("valid_AphiaID", "valid_name")) |>
    dplyr::group_by(.data$dat_id, .data$reported_taxa, .data$stage) |>
    dplyr::slice_min(order_by = .data$AphiaID, n = 1, with_ties = FALSE) |>
    dplyr::select(-"AphiaID") |>
    dplyr::ungroup() |>
    dplyr::full_join(dates, by = "dat_id") |>
    tidyr::pivot_longer(
      -c(
        "dat_id",
        "reported_taxa",
        "lsid",
        "scientificname",
        "status",
        "match_type",
        "stage"
      ),
      names_to = "date",
      values_to = "ind_m3"
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = lubridate::as_date(as.numeric(.data$date), origin = "1899-12-30")
    ) |>
    dplyr::select(-"dat_id") |>
    dplyr::left_join(ids, by = "date") |>
    dplyr::relocate("sample_id", .before = "reported_taxa") |>
    dplyr::relocate("stage", .after = "ind_m3") |>
    dplyr::relocate("date", .after = "sample_id") |>
    dplyr::arrange(.data$sample_id) |>
    dplyr::arrange(.data$date, .data$sample_id) |>
    dplyr::rename(
      eventID = "sample_id",
      eventDate = "date",
      Abundance = "ind_m3",
      lifeStage = "stage"
    ) |>
    dplyr::distinct()

  # Check for unmatched taxa (to be checked by curators)
  unmatched <-
    taxa_df |>
    dplyr::select(
      "reported_taxa",
      "scientificname",
      "lsid",
      "status",
      "match_type"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(is.na(.data$lsid)) |>
    dplyr::select("reported_taxa") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "accepted scientific name" = NA_character_,
      "lifestage" = NA_character_
    )

  logger::log_info("Preparing tidy export data...", namespace = "ZooGoN")
  tidy_data <-
    taxa_df |>
    dplyr::select(
      "eventID",
      "eventDate",
      scientificName = "scientificname",
      "lsid",
      "Abundance",
      "lifeStage"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$lsid)) |>
    # standardize lifeStage format
    dplyr::mutate(
      lifeStage = dplyr::case_when(
        .data$lifeStage == "f/m" ~ "fm",
        .data$lifeStage == "f+m+j" ~ "fmj",
        .data$lifeStage == "larvae" ~ "lar",
        .data$lifeStage == "eggs" ~ "egg",
        TRUE ~ .data$lifeStage
      )
    ) |>
    # remove NA counts & IDs
    dplyr::filter(
      !is.na(.data$Abundance),
      !is.na(.data$eventDate),
      !is.na(.data$eventID)
    ) |>
    # remove duplicates
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # fix appendicularia
    dplyr::mutate(
      lsid = dplyr::if_else(
        .data$lsid == "urn:lsid:marinespecies.org:taxname:103357",
        "urn:lsid:marinespecies.org:taxname:146421",
        .data$lsid
      )
    ) |>
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # fix wrong date for mc994
    dplyr::mutate(
      eventDate = dplyr::if_else(
        .data$eventID == "mc994",
        as.Date("2012-01-31"),
        .data$eventDate
      )
    ) |>
    # fix appendicularia
    dplyr::mutate(
      lsid = dplyr::if_else(
        .data$lsid == "urn:lsid:marinespecies.org:taxname:103357",
        "urn:lsid:marinespecies.org:taxname:146421",
        .data$lsid
      )
    ) |>
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::relocate("Abundance", .before = "lifeStage")

  logger::log_debug(
    "Tidy data: {nrow(tidy_data)} rows, {length(unique(tidy_data$scientificName))} unique taxa",
    namespace = "ZooGoN"
  )

  logger::log_info(
    "Uploading legacy 1984-2015 data (CSV + Parquet)...",
    namespace = "ZooGoN"
  )
  formats <- c("parquet", "csv")

  purrr::walk(formats, function(fmt) {
    filename <- paste0("McZoo_84-15.", fmt)

    # Write locally
    if (fmt == "parquet") {
      arrow::write_parquet(tidy_data, filename)
    } else {
      readr::write_csv2(tidy_data, filename)
    }

    # Upload to SharePoint
    upload_sharepoint_df(
      data = tidy_data,
      prefix = filename,
      bucket = conf$storage$sharepoint$buckets$legacy_bucket_processed,
      options = conf$storage$sharepoint$credentials,
      format = fmt,
      filename = TRUE
    )
  })

  logger::log_success("ingest_legacy_84_15 complete", namespace = "ZooGoN")
}


#' Ingest Legacy Zooplankton Data (2016-2020)
#'
#' Downloads, validates, and harmonizes the 2016-2020 legacy zooplankton dataset
#' from SharePoint. Taxa are matched against the WoRMS database and the output
#' is written in both Parquet and CSV formats to the hot storage bucket.
#'
#' @return Invisible NULL. Outputs are uploaded to SharePoint as
#'   `McZoo_16-20.parquet` and `McZoo_16-20.csv`.
#'
#' @details
#' The function performs the following steps:
#' 1. Downloads sample ID metadata (`ids_16_20.csv`) and biological data
#'    (`zoo_16_20.csv`) from the `legacy_data` SharePoint bucket.
#' 2. Integrates manually curated unmatched taxa from
#'    `unmatched_worms_16_20.xlsx` to correct known synonyms before WoRMS
#'    validation.
#' 3. Queries WoRMS via `worrms::wm_records_taxamatch()` for every unique
#'    taxon; unmatched taxa are flagged with `match_type = "no_match"`.
#' 4. Selects a single AphiaID per taxon (lowest, i.e. oldest classification).
#' 5. Joins biological observations with WoRMS-validated taxonomy and sample
#'    metadata.
#' 6. Standardises life-stage codes and converts abundance to numeric.
#' 7. Aggregates duplicate records by summing `Abundance`.
#' 8. Uploads tidy data to the hot storage bucket in Parquet and CSV formats.
#'
#' @note Unmatched taxa (no WoRMS hit) are reported to the console and excluded
#'   from the final export. Curators should review them and add corrections to
#'   `unmatched_worms_16_20.xlsx` before re-running.
#'
#' @keywords ingestion legacy
#' @export
#'
#' @examples
#' \dontrun{
#' ingest_legacy_16_20()
#' }
ingest_legacy_16_20 <- function() {
  conf <- read_config()

  logger::log_info(
    "Downloading sample ID metadata (2016-2020)...",
    namespace = "ZooGoN"
  )
  ids <-
    download_sharepoint_file(
      prefix = "ids_16_20.csv",
      options = conf$storage$sharepoint$credentials,
      bucket = "legacy_data",
      filename = TRUE
    ) |>
    dplyr::rename("sample_id" = "id") |>
    dplyr::mutate(
      date = lubridate::mdy(.data$date),
      sample_id = janitor::make_clean_names(.data$sample_id),
      sample_id = stringr::str_replace_all(.data$sample_id, "_", "")
    )

  logger::log_info(
    "Downloading biological data (2016-2020)...",
    namespace = "ZooGoN"
  )
  bio <- download_sharepoint_file(
    prefix = "zoo_16_20.csv",
    options = conf$storage$sharepoint$credentials,
    bucket = "legacy_data",
    filename = TRUE
  ) |>
    tidyr::separate(
      col = 1,
      into = c("sample_id", "date", "taxa", "stage", "ind_m3"),
      sep = ";"
    ) |>
    dplyr::mutate(
      date = lubridate::mdy(.data$date),
      dat_id = seq_len(dplyr::n()),
      taxa = stringr::str_trim(.data$taxa),
      ind_m3 = stringr::str_replace_all(.data$ind_m3, ",", ".")
    )

  logger::log_debug(
    "Biological records loaded: {nrow(bio)}",
    namespace = "ZooGoN"
  )

  logger::log_info(
    "Integrating manually curated unmatched taxa...",
    namespace = "ZooGoN"
  )
  unmatched_fixed <-
    download_sharepoint_file(
      prefix = "unmatched_worms_16_20.xlsx",
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$unmatched_bucket,
      filename = TRUE
    ) |>
    janitor::clean_names()

  update_bio <-
    bio |>
    dplyr::left_join(unmatched_fixed, by = c("taxa" = "reported_taxa")) |>
    dplyr::mutate(
      taxa = dplyr::case_when(
        !is.na(.data$accepted_scientific_name) ~ .data$accepted_scientific_name,
        TRUE ~ .data$taxa
      ),
      stage = dplyr::case_when(
        !is.na(.data$lifestage) ~ .data$lifestage,
        TRUE ~ .data$stage
      )
    ) |>
    dplyr::select(-c("accepted_scientific_name", "aphia_id", "lifestage"))

  reported_taxa <-
    as.character(unique(update_bio$taxa))

  logger::log_info(
    "Querying WoRMS for {length(reported_taxa)} unique taxa...",
    namespace = "ZooGoN"
  )
  worms_matched <- purrr::map2_dfr(
    .x = seq_along(reported_taxa),
    .y = reported_taxa,
    .f = function(i, taxon) {
      res <- tryCatch(
        worrms::wm_records_taxamatch(taxon),
        error = function(e) NULL
      )
      if (
        is.null(res) ||
          length(res) == 0 ||
          is.null(res[[1]]) ||
          nrow(res[[1]]) == 0
      ) {
        return(dplyr::tibble(
          original = taxon,
          AphiaID = NA_integer_,
          scientificname = NA_character_,
          status = NA_character_,
          match_type = "no_match"
        ))
      }
      res[[1]] %>% dplyr::mutate(original = taxon, .before = 1)
    }
  )

  verification_unmatched <-
    worms_matched |>
    dplyr::filter(
      is.na(.data$valid_AphiaID) | .data$match_type == "no_match"
    ) |>
    dplyr::distinct(.data$original)

  if (nrow(verification_unmatched) > 0) {
    logger::log_warn(
      "{nrow(verification_unmatched)} taxa did not match on WoRMS and will be excluded.",
      namespace = "ZooGoN"
    )
    print(verification_unmatched)
  } else {
    logger::log_info("All taxa matched on WoRMS.", namespace = "ZooGoN")
  }

  worms_matched_clean <-
    worms_matched |>
    dplyr::select(
      "original",
      "AphiaID",
      "lsid",
      "scientificname",
      "status",
      "valid_AphiaID",
      "valid_name",
      "match_type"
    ) |>
    dplyr::distinct()

  logger::log_info(
    "Joining taxonomy with sample metadata...",
    namespace = "ZooGoN"
  )
  taxa_df <-
    update_bio |>
    dplyr::select(
      "dat_id",
      "sample_id",
      reported_taxa = "taxa",
      "stage",
      "date",
      "ind_m3",
    ) |>
    dplyr::full_join(
      worms_matched_clean,
      by = c("reported_taxa" = "original")
    ) |>
    dplyr::mutate(
      AphiaID = dplyr::if_else(
        .data$status == "unaccepted",
        .data$valid_AphiaID,
        .data$AphiaID
      ),
      scientificname = dplyr::if_else(
        .data$status == "unaccepted",
        .data$valid_name,
        .data$scientificname
      ),
      lsid = dplyr::if_else(
        .data$status == "unaccepted",
        paste0("urn:lsid:marinespecies.org:taxname:", .data$AphiaID),
        .data$lsid
      )
    ) |>
    dplyr::select(-c("valid_AphiaID", "valid_name")) |>
    dplyr::group_by(.data$dat_id, .data$reported_taxa, .data$stage) |>
    dplyr::slice_min(order_by = .data$AphiaID, n = 1, with_ties = FALSE) |>
    dplyr::select(-"AphiaID") |>
    dplyr::ungroup() |>
    janitor::clean_names() |>
    dplyr::left_join(ids, by = c("date", "sample_id")) |>
    dplyr::relocate("stage", .after = "ind_m3") |>
    dplyr::relocate("date", .after = "sample_id") |>
    dplyr::arrange(.data$sample_id) |>
    dplyr::rename(
      eventID = "sample_id",
      eventDate = "date",
      Abundance = "ind_m3",
      lifeStage = "stage"
    ) |>
    dplyr::distinct()

  unmatched <-
    taxa_df |>
    dplyr::select(
      "reported_taxa",
      "scientificname",
      "lsid",
      "status",
      "match_type"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(is.na(.data$lsid)) |>
    dplyr::select("reported_taxa") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "accepted scientific name" = NA_character_,
      "lifestage" = NA_character_
    )

  logger::log_info("Preparing tidy export data...", namespace = "ZooGoN")
  tidy_data <-
    taxa_df |>
    dplyr::select(
      "eventID",
      "eventDate",
      scientificName = "scientificname",
      "lsid",
      "Abundance",
      "lifeStage"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$lsid)) |>
    dplyr::mutate(
      lifeStage = dplyr::case_when(
        .data$lifeStage == "f+m" ~ "fm",
        .data$lifeStage == "f+m+j" ~ "fmj",
        .data$lifeStage == "larvae" ~ "lar",
        .data$lifeStage == "eggs" ~ "egg",
        TRUE ~ .data$lifeStage
      )
    ) |>
    dplyr::mutate(
      Abundance = dplyr::if_else(
        .data$Abundance %in% c("#N/D", "#N/A"),
        NA_character_,
        .data$Abundance
      ),
      lifeStage = dplyr::if_else(
        .data$lifeStage %in% c("#N/D", "#N/A"),
        NA_character_,
        .data$lifeStage
      ),
      Abundance = as.numeric(.data$Abundance),
    ) |>
    dplyr::filter(
      !is.na(.data$Abundance),
      !is.na(.data$eventDate),
      !is.na(.data$eventID)
    ) |>
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # fix appendicularia
    dplyr::mutate(
      lsid = dplyr::if_else(
        .data$lsid == "urn:lsid:marinespecies.org:taxname:103357",
        "urn:lsid:marinespecies.org:taxname:146421",
        .data$lsid
      )
    ) |>
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::relocate("Abundance", .before = "lifeStage")

  logger::log_debug(
    "Tidy data: {nrow(tidy_data)} rows, {length(unique(tidy_data$scientificName))} unique taxa",
    namespace = "ZooGoN"
  )

  logger::log_info(
    "Uploading legacy 2016-2020 data (CSV + Parquet)...",
    namespace = "ZooGoN"
  )
  formats <- c("parquet", "csv")

  purrr::walk(formats, function(fmt) {
    filename <- paste0("McZoo_16-20.", fmt)

    if (fmt == "parquet") {
      arrow::write_parquet(tidy_data, filename)
    } else {
      readr::write_csv2(tidy_data, filename)
    }

    upload_sharepoint_df(
      data = tidy_data,
      prefix = filename,
      bucket = conf$storage$sharepoint$buckets$legacy_bucket_processed,
      options = conf$storage$sharepoint$credentials,
      format = fmt,
      filename = TRUE
    )
  })

  logger::log_success("ingest_legacy_16_20 complete", namespace = "ZooGoN")
}
