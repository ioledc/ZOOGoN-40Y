#' Extract standardized genus-species names from taxonomic strings
#'
#' This function parses a vector of taxonomic names and attempts to extract or
#' standardize genus-species level information. It handles various cases such as
#' species complexes (e.g., with `+`), family-level entries with `n.i.`, higher
#' groups with dashes, and standard binomial names (with or without subgenus and
#' author information). If no rule applies, the original name is retained.
#'
#' @param taxa_vector A character vector of taxonomic names.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{original_name}{The original taxonomic string.}
#'   \item{genus_species}{The extracted or standardized genus-species name.}
#' }
#'
#' @examples
#' extract_genus_species(c(
#'   "Sardinella+Sardinops",
#'   "Clupeidae n.i.",
#'   "Engraulis - group",
#'   "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)"
#' ))
#'
#' @export
extract_genus_species <- function(taxa_vector) {
  tibble::tibble(
    original_name = taxa_vector,
    genus_species = dplyr::case_when(
      is.na(taxa_vector) ~ NA_character_,

      # Handle species complexes with + (convert to Genus spp.)
      stringr::str_detect(taxa_vector, "\\+") ~
        {
          genus <- stringr::str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(genus, "spp")
        },

      # Handle family level with n.i. (create genus name from family)
      stringr::str_detect(taxa_vector, "[A-Z][a-z]*idae\\s+n\\.i\\.") ~
        {
          family <- stringr::str_extract(taxa_vector, "[A-Z][a-z]*idae")
          genus <- stringr::str_replace(family, "idae$", "genus")
          paste(genus, "sp")
        },

      # Handle higher groups with dashes (take main group)
      stringr::str_detect(taxa_vector, "^[A-Z][a-z]+\\s*-") ~
        {
          main_group <- stringr::str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(main_group, "indet")
        },

      # Extract Genus species from standard binomial patterns
      # Handles: "Genus species (Author, Year)" or "Genus (Subgenus) species Author"
      stringr::str_detect(
        taxa_vector,
        "^[A-Z][a-z]+\\s+(?:\\([A-Z][a-z]+\\)\\s+)?[a-z]+"
      ) ~
        stringr::str_extract(
          taxa_vector,
          "^[A-Z][a-z]+(?:\\s+\\([A-Z][a-z]+\\))?\\s+[a-z]+"
        ) %>%
          stringr::str_remove("\\s+\\([A-Z][a-z]+\\)") |> # Remove subgenus if present
          stringr::str_trim(),

      # For everything else, keep original
      TRUE ~ taxa_vector
    )
  )
}
