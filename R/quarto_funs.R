#' Render Quarto docment to another directory
#'
#' Currently `quarto::quarto_render()` does not allow to render quarto to another
#' directory. This function creates a temporary directory within which to render
#' the .qmd doc, then copies files to the output directory.
#'
#' @param qmd_file string. file path for input `.qmd` file
#' @param out_dir string. path to output directory in which to save .qmd output
#' @param render_in_temp logical. whether to render in a temporary directory or in
#'  the same directory as the input file. Defaults to `FALSE`.
#' @param overwrite logical. whether to overwrite existing files in the output
#'  directory.
#' @param ... passed to quarto_render
#' @returns Invisibly returns character vector with output file path(s). Mainly
#' called for side effects -- rendering quarto in new directory.
#'
#' @export
quarto_render_to_dir <- function(
    qmd_file,
    out_dir,
    render_in_temp = FALSE,
    overwrite = TRUE,
    ...) {

  # set directory to execute
  if(render_in_temp){
    render_dir <- tempfile()
    fs::dir_create(render_dir)
    render_file <- fs::path(render_dir, fs::path_file(qmd_file))
    fs::file_copy(qmd_file, render_file)

  } else {
    render_dir <- fs::path_dir(qmd_file)
    render_file <- qmd_file
  }

  # Render
  quarto::quarto_render(input = render_file, ...)

  # Move output files to output directory
  # (match all files with same name except .qmd)
  output_files <- fs::dir_ls(
    render_dir,
    regexp = exclude_r_files_regex(fs::path_file(render_file)),
    perl = TRUE
  )

  # copy to output dir
  fs::file_copy(
    output_files,
    fs::path(out_dir, fs::path_file(output_files)),
    overwrite = overwrite
  )

  if(render_in_temp) {
    fs::dir_delete(render_dir)
  } else {
    fs::file_delete(output_files)
  }
  final_path <- fs::path(out_dir, fs::path_file(output_files))
  invisible(final_path)
}


# Helpers -----------------------------------------------------------------

exclude_r_files_regex <- function(out_name) {
  base_name <- fs::path_ext_remove(out_name)
  paste0(
    "(?i)",  # Case insensitive
    "(?!",   # Negative lookahead start
    ".*\\.",  # Any characters followed by a dot
    "(qmd|rmd|rmarkdown|r|rdata|rds)$",  # R-related extensions
    ")",     # Negative lookahead end
    ".*",    # Any characters
    base_name,  # The base name of the output file
    "\\."    # Followed by a dot
  )
}
