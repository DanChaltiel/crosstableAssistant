

#' Search for object with specific class in an environment
#' source: https://github.com/dreamRs/esquisse/blob/master/R/utils.R
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}


search_dataframes = function(){
  all = search_obj(what = c("data.frame", "tbl", "tbl_df"))
  dims = all %>% map(get) %>% map(dim) %>% map(paste, collapse=" x ")
  glue("{all} ({dims})")
}

#' Retrieve a data.frame by name from an environment
#'
#' @param df character, name of the object
#' @param env an environment
#'
#' @return the object
#' @noRd
#'
#' @importFrom utils data
#'
get_df <- function(df, env = globalenv()) {
  if (df %in% ls(name = env)) {
    get(x = df, envir = env)
  } else if (df %in% data(package = "ggplot2", envir = environment())$results[, "Item"]) {
    get(utils::data(list = df, package = "ggplot2", envir = environment()))
  } else {
    NULL
  }
}



#' Create badge according to data type
#'
#' It uses conventions defined in the package, variable type are retrieve with \code{\link{col_type}}.
#'
#' @param col_name Variable's name
#' @param col_type Variable's type : 'discrete', 'time', 'continuous', 'id'
#'
#' @noRd
badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class='label label-discrete badge-dad', col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class='label label-datetime badge-dad', col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class='label label-continue badge-dad', col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class='label label-default badge-dad', col_name_i)
      }
    }
  )
  res
}