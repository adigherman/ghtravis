#' Print Description Elements out
#'
#' @param path path to DESCRIPTION file
#' @param repo remote repository on GitHub
#' @param dep_types Types of dependencies to show
#' @param ... Additional arguments passed to \code{\link{get_remote_package_dcf}}
#'
#' @return Nothing
#' @export
#' @import desc
#' @importFrom utils as.person
cat_desc_elements = function(path = "DESCRIPTION",
                             dep_types = desc::dep_types){
  desc = description$new(file = path)
  desc$coerce_authors_at_r()
  desc$del("Author")
  pack = unname(desc$get("Package"))
  man = desc$get_maintainer()
  man = as.person(man)

  ver = as.character(desc$get_version())

  deps = desc$get_deps()
  # dep_types = c("Depends",
  #               "Imports",
  #               "Suggests")
  deps = deps[ deps$type %in% dep_types,, drop = FALSE]

  collapser = function(get_type) {
    x = deps[ deps$type %in% get_type,, drop = FALSE]
    if (nrow(x) > 0) {
      pack = deparse_deps(x)
      pack = paste0(get_type, ": ", pack)
    } else {
      pack = NULL
    }
    return(pack)
  }
  col_deps = sapply(dep_types,
                    collapser)
  names(col_deps) = dep_types
  col_deps = unlist(col_deps)

  paster = function(x, y) {
    if (length(y) == 0) {
      return(NULL)
    }
    paste(x, y)
  }

  res = c(
    paste("Name:", pack),
    paste("Version:", ver),
    paste("Maintainer:", format(man,
                                include = c("given", "family"))),
    paste("Email:", man$email)
  )
  if ( length(col_deps) > 0) {
    res = c(res, col_deps)
  }

  cat(res, sep = "\n")
  return(invisible(NULL))
}

#' @export
#' @rdname cat_desc_elements
cat_desc_elements_remote = function(repo, ...) {
  path = get_remote_package_dcf(remotes = repo, ...)
  cat_desc_elements(path = path)
}

# taken from desc
deparse_deps = function (deps) {
  tapply(seq_len(nrow(deps)), deps$type, function(x) {
    pkgs <- paste0(deps$package[x],
                   ifelse(deps$version[x] ==
                            "*", "",
                          paste0(" (", deps$version[x], ")")), collapse = ", ")
  })
}