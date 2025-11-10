#' Load environment variables from .env file
#'
#' Loads environment variables from .env file if it exists. This function should
#' be called before reading configuration to ensure dotenv variables are available.
#'
#' @param file Path to .env file, defaults to ".env" in current working directory
#' @return NULL (called for side effects)
#'
#' @keywords helper
#' @export
#'
load_dotenv <- function(file = ".env") {
  if (file.exists(file)) {
    logger::log_info("Loading environment variables from {file}")
    dotenv::load_dot_env(file = file)
  } else {
    logger::log_debug("No .env file found at {file}, skipping dotenv loading")
  }
  invisible(NULL)
}


#' Read configuration file
#'
#' Reads configuration file in `config.yml` and adds some logging lines. Also
#' loads environment variables from .env file if present. Wrapped for convenience
#'
#' @return the environment parameters
#'
#' @keywords helper
#' @export
#'
read_config <- function() {
  # Load .env file first to make variables available to config.yml
  load_dotenv()

  logger::log_info("Loading configuration file...")

  conf <- config::get(
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file = system.file(
      "config.yml",
      package = "ZooGoN"
    )
  )
  logger::log_info("Using configutation: {attr(conf, 'config')}")
  logger::log_debug("Running with parameters {conf}")

  conf
}
