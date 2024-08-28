
#' Functions for integrating with dropbox
#'
#' A set of functions for easy integration with dropbox. This envisages a sister
#' directory in the users dropbox (defaulrs to "projects", with a subdir for
#' each repository) where are all non-code files (data, output, papers, etc.)
#' can be stored and easily accessed, yet keeping the github repository clean.
#'
#' `find_dropbox_dir()`tries to find the users Dropbox directory by looking for the
#'  info.json file in the Dropbox directory of the users appdata.
#'
#' `get_dropbox_repo_dir()` returns the path to the directory where the non-code
#'  files (data, output, papers, etc.) for a repository are stored in Dropbox.
#'
#' `dropbox_path()` extends `get_dropbox_repo_dir()` by allowing additional path
#'  components to be joined to the end of the path with `fs::path()`.
#'
#' @param version A string specifying the version of Dropbox to use, if 2 are
#'  found on the machine. Can be either "personal" or "business" (default).
#' @return A path string returned by fs::path() to the relevant directory/file.
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

  fs::path(out_path)
}


#'@param repo_name character. Name of the git repository. Defaults to the name of the
#' current git repository found via searching the `git.config` of the current R
#' project, via `get_git_repo_name()`.
#'@param repos_subdir character. Subdirectory within the Dropbox directory where
#' each repository subdirectory is found. Defaults to "projects".
#'@param dropbox_base_dir character. Path to the base Dropbox directory. Defaults to
#' the Dropbox directory found via `find_dropbox_dir()`.
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

#'#'@param ... character arguments. Additional path components to be joined to the
#' end of the path with `fs::path()`.
#'
#'@export
#'@rdname dropbox_funs
dropbox_path <-  function(
    ...,
    repos_subdir = "projects",
    repo_name = get_git_repo_name(),
    dropbox_base_dir = find_dropbox_dir(),
    create_dir = FALSE
){
  repo_dir <- get_dropbox_repo_dir(
    repo_name = repo_name,
    repos_subdir = repos_subdir,
    dropbox_base_dir = dropbox_base_dir,
    create_dir = FALSE # create the dir in later step
  )

  path <- fs::path(repo_dir, ...)

  if (is_file_path(path)) {
    # If it's a file, check if its parent directory exists
    parent_dir <- fs::path_dir(path)
    if (!fs::dir_exists(parent_dir)) {
      if (create_dir) {
        fs::dir_create(parent_dir)
      } else {
        stop(paste0("Parent directory ", parent_dir, " does not exist"))
      }
    }
  } else if (!fs::dir_exists(path)) {
    # If it's not a file and the directory doesn't exist
    if (create_dir) {
      fs::dir_create(path)
    } else {
      stop(paste0("Directory ", path, " does not exist"))
    }
  }

  path
}

# Helpers -----------------------------------------------------------------

get_git_repo_name <- function() {
  config_file <- here::here(".git", "config")
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

is_file_path <- function(path) {
  ext <- fs::path_ext(path)
  nchar(ext) != 0
}
