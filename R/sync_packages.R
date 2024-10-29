#' Sync packages between workspace and bucket
#' @examples 
#' \dontrun{
#' sync_packages()
#' }
#' @details Syncing is done by first syncing local packages to the bucket. Packages are then synced from the bucket to local storage.
#' The sync_packages function can be left at the top of the script so that the workspace and bucket packages remain synchronized. 
#' The sync_packages function is especially useful when the persistent disk has been deleted after downloading R packages. In this case,
#' packages will remain on the bucket but not on local storage. When respawning the environment, the sync_packages function can be run and
#' all packages from the bucket will sync to the local storage. This should save the time it takes to download and re-install R packages.
#' @export
sync_packages <- function()
{
  my_bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(stringr::str_glue("gcloud storage rsync /home/jupyter/packages {my_bucket}/packages --recursive"),intern=T)
  system(stringr::str_glue("gcloud storage rsync {my_bucket}/packages /home/jupyter/packages --recursive"),intern=T)
}