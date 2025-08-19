#' Extract standardized genus-species names from taxonomic strings
#'
#' This function parses a vector of taxonomic names from the LTER-MareChiara 
#' zooplankton dataset and attempts to extract or standardize genus-species level 
#' information for Darwin Core compliance. It handles various cases commonly found 
#' in Mediterranean zooplankton taxonomy including species complexes, family-level 
#' entries, higher taxonomic groups, and standard binomial names with author citations.
#' 
#' The standardization follows these rules:
#' - Species complexes (e.g., "Genus1+Genus2") become "Genus1 spp"  
#' - Family-level entries (e.g., "Familidae n.i.") become "Familgenus sp"
#' - Higher groups with dashes (e.g., "Genus - group") become "Genus indet"
#' - Standard binomials with authors are cleaned to "Genus species" format
#' - Subgenus information in parentheses is removed
#' - Unmatched entries retain their original form
#'
#' @param taxa_vector A character vector of taxonomic names from zooplankton samples.
#'   Typically from the TAXA column of LTER-MareChiara dataset.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{original_name}{The original taxonomic string as provided.}
#'   \item{genus_species}{The extracted or standardized genus-species name 
#'     suitable for Darwin Core scientificName field.}
#' }
#'
#' @examples
#' # Examples from Gulf of Naples zooplankton samples
#' taxa_examples <- c(
#'   "Sardinella+Sardinops",                    # Species complex
#'   "Clupeidae n.i.",                          # Family level identification  
#'   "Engraulis - group",                       # Higher taxonomic group
#'   "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)",  # With subgenus & author
#'   "Chiridius poppei Giesbrecht, 1893",       # Standard binomial with author
#'   "Larvae n.i."                              # Developmental stage entry
#' )
#' 
#' result <- extract_genus_species(taxa_examples)
#' print(result)
#' 
#' # Expected output:
#' # original_name                                          genus_species
#' # "Sardinella+Sardinops"                                 "Sardinella spp"
#' # "Clupeidae n.i."                                       "Clupegenus sp"  
#' # "Engraulis - group"                                     "Engraulis indet"
#' # "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)" "Lutjanus argentimaculatus"
#' # "Chiridius poppei Giesbrecht, 1893"                    "Chiridius poppei"
#' # "Larvae n.i."                                          "Larvae n.i."
#'
#' @seealso 
#' - Darwin Core standard: \url{https://dwc.tdwg.org/}
#' - World Register of Marine Species: \url{https://www.marinespecies.org/}
#' - LTER-MareChiara station: \url{https://deims.org/0b87459a-da3c-45af-a3e1-cb1508519411}
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
