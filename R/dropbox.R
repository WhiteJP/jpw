
# Functions for integrating with dropbox ----------------------------------

#' Find users Dropbox base directory
#'
#' This function tries to find the users Dropbox directory by looking for the
#' info.json file in the Dropbox directory of the users appdata.
#'
#' @param version A string specifying the version of Dropbox to use, if 2 are
#'  found on he machine. Can be either "personal" or "business" (default).
#' @return A string with the path to the users Dropbox directory
#'
#' @export
#' @rdname dropbox_funs
find_dropbox_dir <- function(version = c("business", "personal")) {

  version <- match.arg(version)

  # Find info.json file in dropbox directory of appdata, which contains the path
  # to the users dropbox directory
  possible_infojson_paths <- c(
    fs::path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json"),  # Windows
    fs::path(Sys.getenv("APPDATA"), "Dropbox", "info.json"),       # Windows (alternative)
    fs::path(Sys.getenv("HOME"), ".dropbox", "info.json"),         # macOS/Linux
    "/var/lib/dropbox/.dropbox/info.json"                         # Some Linux distributions
  )

  info <- list()
  for (path in possible_infojson_paths) {
    if (fs::file_exists(path)) {
      info <- c(info, jsonlite::read_json(path))
    }
  }

  if (length(info) == 0) {
    stop("Could not find Dropbox directory. Please specify manually")
  } else if (length(info) == 1) {
    out_path <- info[[1]]$path
  } else {
    warning(
      paste0(
        "Found multiple Dropbox directories. Using ", version, ". ",
        "To specify a different version, use the `version` argument."
      )
    )
    out_path <- info[[version]]$path
  }

  out_path
}


#'@param create_dir logical. Whether to create the directory if it does not exist.
#' Defaults to `FALSE`.
#'
#'@export
#'@rdname dropbox_funs
get_dropbox_repo_dir <- function(
    repo_name = get_git_repo_name(),
    repos_subdir = "projects",
    dropbox_base_dir = find_dropbox_dir(),
    create_dir = FALSE
  ){
  path <- fs::path(dropbox_base_dir, repos_subdir, repo_name)
  if(!fs::dir_exists(path)){
    if (create_dir) fs::dir_create(path) else stop(paste0(path, " does not exist"))
  }
  path
}

#'@export
#'@rdname dropbox_funs
get_dropbox_data_path <-  function(
    repos_subdir = "projects",
    repo_name = get_git_repo_name(),
    dropbox_base_dir = find_dropbox_dir(),
    create_dir = FALSE
){
  path <- fs::path(
    get_dropbox_repo_dir(repo_name, repos_subdir, dropbox_base_dir), "data"
  )
  if(!fs::dir_exists(path)){
    if (create_dir) fs::dir_create(path) else stop(paste0(path, " does not exist"))
  }
  path
}

#'@export
#'@rdname dropbox_funs
get_dropbox_output_path <-  function(
    repos_subdir = "projects",
    repo_name = get_git_repo_name(),
    dropbox_base_dir = find_dropbox_dir(),
    create_dir = FALSE
){
  fs::path(
    get_dropbox_repo_dir(repo_name, repos_subdir, dropbox_base_dir), "output"
  )
  if(!fs::dir_exists(path)){
    if (create_dir) fs::dir_create(path) else stop(paste0(path, " does not exist"))
  }
  path
}

#'@export
#'@rdname dropbox_funs
get_data_from_dropbox <- function(file_name){
  dropbox_data_path <- get_dropbox_data_path()
  file_path <- fs::path(dropbox_data_path, file_name)
  if(fs::file_exists(file_path)){
    return(file_path)
  } else {
    warning(paste0("File ", file_name, " not found in Dropbox"))
    return(NULL)
  }
}


get_git_repo_name <- function() {
  config_file <- file.path(".git", "config")
  if (file.exists(config_file)) {
    config <- readLines(config_file)
    url_line <- grep("url = ", config, value = TRUE)
    if (length(url_line) > 0) {
      repo_name <- sub(".*/(.*?)\\.git", "\\1", url_line)
      return(repo_name)
    }
  }
  warning("Could not find Git repository name")
  return(NULL)
}

