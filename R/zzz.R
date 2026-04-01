.onLoad <- function(libname, pkgname) {
  # Set up the "ZooGoN" logger namespace so users can control our logs
 # independently from global: log_threshold(DEBUG, namespace = "ZooGoN")
  logger::log_formatter(logger::formatter_glue, namespace = pkgname)
  logger::log_layout(logger::layout_glue_generator(
    format = "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"
  ), namespace = pkgname)

}
