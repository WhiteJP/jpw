#' Render Quarto docment to another directory
#'
#' Currently `quarto::quarto_render()` does not allow to render quarto to another
#' directory. This function creates a temporary directory within which to render
#' the .qmd doc, then copies files to the output directory.
#'
#' @param qmd_file string. file path for input `.qmd` file
#' @param out_dir string. path to output directory in which to save .qmd output
#' @param out_name string. corresponding to file name of output file.
#' @param render_in_temp logical. whether to render in a temporary directory or in
#'  the same directory as the input file. Defaults to `FALSE`.
#' @param ... passed to quarto_render
#' @returns Invisibly returns character vector with output file path(s). Mainly
#' called for side effects -- rendering quarto in new directory.
#'
#' @export
quarto_render_to_dir <- function(
    qmd_file,
    out_dir,
    out_name = basename(qmd_file),
    render_in_temp = FALSE,
    ...) {

  # set directory to execute
  if(render_in_temp){
    render_dir <- tempfile()
    dir.create(render_dir)
    render_file <- fs::path(temp, basename(qmd_file))
    fs::file_copy(qmd_file, render_file)

  } else {
    render_dir <- dirname(qmd_file)
    render_file <- qmd_file
  }

  # Render
  quarto::quarto_render(input = render_file, output_file = out_name, ...)

  # Move output files to output directory
  # (match all files with same name except .qmd)
  output_files <- fs::dir_ls(
    render_dir,
    regexp = paste0("(?!.*\\.qmd$).*", fs::path_ext_remove(out_name), ".x*"),
    perl = TRUE
  )

  # copy to output dir
  fs::file_copy(output_files, fs::path(out_dir, basename(output_files)))

  if(render_in_tempt) fs::dir_delete(temp) #delete temp dir if in temp

  invisible(fs::path(out_dir, basename(output_files)))
}
