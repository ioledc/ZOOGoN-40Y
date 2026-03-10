#' Render ZooGoN MC Survey Report
#'
#' This function renders the Quarto report using the preprocessed survey data.
#' It downloads the latest preprocessed data from SharePoint and renders the
#' report to HTML format, saving it in the working directory.
#'
#' @param output_dir Directory where the rendered report will be saved.
#'   Defaults to the current working directory.
#'
#' @return Invisible NULL. Renders the report to the output directory.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads configuration settings from config.yml
#' 2. Locates Report.qmd inside the installed package (inst/report/)
#' 3. Renders the Quarto report to HTML in the specified output directory
#'
#' @keywords workflow report
#' @export
#'
#' @examples
#' \dontrun{
#' render_report()
#' }
render_report <- function(output_dir = "/home") {
  conf <- read_config()

  report_path <- system.file("report/REPORT.qmd", package = "ZooGoN")

  if (report_path == "") {
    stop("Report.qmd not found in inst/report/. Make sure the package is installed.")
  }

  output_file <- paste0("ZooGoN-report-", Sys.Date(), ".html")

  logger::log_info("Rendering Quarto report...", namespace = "ZooGoN")
  quarto::quarto_render(
    input         = report_path,
    output_format = "html",
    output_file   = output_file,
    execute_params = list(
      sharepoint_site_url = conf$storage$sharepoint$credentials$site_url,
      data_prefix         = conf$ingestion$surveys$preprocessed$file_prefix,
      automation_bucket   = conf$storage$sharepoint$buckets$automation_bucket
    )
  )

  # Quarto saves the file next to the .qmd — move it to output_dir
  rendered_path <- file.path(dirname(report_path), output_file)
  dest_path <- file.path(output_dir, output_file)
  file.copy(rendered_path, dest_path, overwrite = TRUE)

  logger::log_success(
    "render_report complete. Report saved as: {dest_path}",
    namespace = "ZooGoN"
  )
  invisible(NULL)
}


#' Run the full ZooGoN pipeline
#'
#' Runs the full ZooGoN data pipeline in sequence:
#' ingestion → preprocessing → report rendering.
#'
#' @return Invisible NULL.
#' @keywords workflow
#' @export
#'
#' @examples
#' \dontrun{
#' run_pipeline()
#' }
run_pipeline <- function() {
  logger::log_info("Starting ZooGoN pipeline...", namespace = "ZooGoN")

  ingest_surveys()
  preprocess_surveys()
  render_report()

  logger::log_success("ZooGoN pipeline complete", namespace = "ZooGoN")
  invisible(NULL)
}

