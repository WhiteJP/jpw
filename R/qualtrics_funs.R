#' Funcion to read csv files downloaded from Qualtrics
#'
#' Wrapper around [readr::read_csv()] to help with reading files from qualtrics.
#' Automatically moves the second and third rows, keeping column names,
#' before passing to [readr::read_csv()].
#'
#' @param path character. The path to the file to read.
#' @param remove_extra_cols logical. If TRUE, removes columns from qualtrics that
#'  are not often needed for analysis. See source code for list. Default is TRUE.
#' @param ... additional arguments to pass to [readr::read_csv()]
#'
#' @export
read_csv_qualtrics <- function(path, remove_extra_cols = TRUE, ...){

  if(!file.exists(path)){
    stop("Error: The specified file cannot be found.")
  }

  cols_to_remove <- if(remove_extra_cols) {
    c("Status", "IPAddress", "Progress", "RecipientLastName", "RecipientFirstName",
      "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude",
      "DistributionChannel")
  } else {
    character(0)
  }

  readr::read_csv(
    path,
    skip = 3,
    col_names = strsplit(readLines(path, n = 1), ",")[[1]],
    col_select = !tidyr::all_of(cols_to_remove),
    ...
  )
}
