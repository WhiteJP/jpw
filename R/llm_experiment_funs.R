#' Extract data from Hause's chatbot
#'
#' This function takes a set of vectors, containing the json output from Hause's
#' chatbot, parses them and returns a tibble with the data.
#'
#' @param ... Vectors containing the json output from Hause's chatbot
#' @return A nested tibble with the data extracted from the chatbot
#'
#' @export
parse_chatbot_info <- function(...) {
  requireNamespace('jsonlite')
  requireNamespace('tibble')
  all_vecs <- list(...)

  # Ensure all vectors have the same length
  if (length(unique(sapply(all_vecs, length))) != 1) {
    stop("All input vectors must have the same length")
  }

  # get NA matrix
  na_matrix <- sapply(all_vecs, is.na)
  nrows <- nrow(na_matrix)
  ncols <- ncol(na_matrix)

  # convert to vector of json strings
  json_strings <- character(nrows)

  for(i in 1:nrows) {
    nas <- na_matrix[i, ]

    # If all NA, skip
    if(all(nas)) next

    # if whole string is in one column, no need to paste etc.
    if(!nas[1] && all(nas[2:ncols])) {
      json_strings[i] <- all_vecs[[1]][i]

      # Otherwise, paste all non-na columns together removing
    } else {
      last_col <- ifelse(all(!nas), ncols, which(nas)[1]-1) ## get last non-NA column, or last col if all are full
      for (j in 1:last_col) {
        s <- all_vecs[[j]][i]
        nchars <- nchar(s)

        if (j == 1) {
          json_strings[i] <- substr(s, 1, nchars - 1)
        } else if (j < last_col) {
          json_strings[i] <- paste0(json_strings[i], substr(s, 2, nchars - 1))
        } else (
          json_strings[i] <- paste0(json_strings[i], substr(s, 2, nchars))
        )
      }
    }
  }

  json_parsed <- lapply(json_strings, function(s) {
    if(!nzchar(s)) NULL else jsonlite::fromJSON(s)
  })

  #return tibble
  out <- tibble::tibble(json_parsed)
  tidyr::unnest_wider(out, 1)
}
