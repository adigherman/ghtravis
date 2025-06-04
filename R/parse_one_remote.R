#' @title Parse a Remote file
#' @description Parse Remote from DESCRIPTION
#'
#' @param x Parses a remote field
#' @return A repo object, from \code{devtools}
#' @export
#' @import remotes
#'
#' @examples
#' parse_one_remote("stnava/ANTsR")
# @return A repo object, from \code{devtools}
parse_one_remote <- function(x) {

  pieces <- strsplit(x, "::", fixed = TRUE)[[1]]

  if (length(pieces) == 1) {
    type <- "github"
    repo <- pieces
  } else if (length(pieces) == 2) {
    type <- pieces[1]
    if(grepl( "=url", pieces[1], fixed = TRUE)) { type <- "url"}
    repo <- pieces[2]
  } else {
    stop("Malformed remote specification '",
         x, "'", call. = FALSE)
  }
  fun <- tryCatch(get(
    paste0(tolower(type), "_remote"),
    # envir = asNamespace("devtools"),
    envir = asNamespace("remotes"),
    mode = "function", inherits = FALSE),
    error = function(e) stop("Unknown remote type: ",
                             type, call. = FALSE)
  )
  args = list(repo)
  if (tolower(type) == "github") {
    args$auth_token = github_pat(quiet = TRUE)
  }
  do.call(fun, args = args)
}

#' @rdname parse_one_remote
#' @export
parse_remotes = function(x) {
  lapply(x, parse_one_remote)
}