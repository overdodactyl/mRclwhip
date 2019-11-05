#' @title Create a new R project and accompanying folders
#' @description Create a new project directory and accompanying folders
#' @param project_dir Name of directory to put project folder in
#' @param project_name Name of project folder
#' @param skeleton Folders to create inside project directory
#' @param launch Launch new project
#' @param new_session If launching project, launch in a new session?
#' @param rproj Create a .Rproj file?
#' @examples
#' # new_project(s123456.PI.Project)
#' # The above command would create a the following folder structure:
#' #  |--- /projects/bsi/fl/studies
#' #      |--- s123456.PI.Project
#' #          |--- rpgm
#' #              |--- rpgm.Rproj
#' #              |--- rawdata/
#' #              |--- documents/
#' #              |--- figures/
#' #              |--- data/
#' @export


new_project <- function(project_dir = "/projects/bsi/fl/studies", project_name = "sXXXXXX.new.study",
                        launch = TRUE, new_session = TRUE, rproj = T,
                        skeleton = c("rawdata", "documents", "figures", "data")) {
  project_folder <- fs::path(project_dir, project_name)

  if (fs::dir_exists(project_folder)) {
    stop(paste(
      project_folder,
      "already exists. Please choose a different project name or directory location"
    ))
  }

  dir.create(project_folder)

  r_folder <- fs::path(project_folder, "rpgm")

  fs::dir_create(r_folder)

  if (rproj) rstudioapi::initializeProject(path = r_folder)

  fs::dir_create(fs::path(r_folder, skeleton))

  if (launch) {
    proj_file <- fs::path(r_folder, "rpgm.Rproj")
    rstudioapi::openProject(path = proj_file, newSession = new_session)
  }
}
