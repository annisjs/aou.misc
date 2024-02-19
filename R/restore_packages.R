#' Restore packages to workspace
#' @examples
#' \dontrun{
#' backup_packages() # must be run at least once before
#' restore_packages()
#' }
#' @export
restore_packages <- function()
{
  package_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"),"packages")
  system(str_glue("gsutil -m cp -r {package_path}/* /home/jupyter/packages 2>&1"), intern = TRUE)
}