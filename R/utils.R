init_scriptr_env <- function(env) {
  if (is.null(env$scriptr_env)) {
    env$scriptr_env <- rlang::new_environment(list(calls = list()))
  }
}

#' @export
is_bangbang <- function(x) {
  if (length(x) == 2 && x[[1]] == sym("!") && length(x[[2]]) == 2 && x[[2]][[1]] == sym("!") && typeof(x[[2]][[2]]) == "symbol") {
    return(x[[2]][[2]])
  }
  return(NULL)
}

#' @export
replace_bangbang <- function(expr, values) {
  for (i in seq_along(expr)) {
    var <- is_bangbang(expr[[i]])
    if (!is.null(var)) {
      if (as_name(var) %in% names(values)) {
        expr[[i]] = values[[as_name(var)]]
      }
    }
    else if (typeof(expr[[i]]) == "language") {
      expr[[i]] <-  replace_bangbang(expr[[i]], values = values)
    }
  }
  expr
}

#' @export
replace_glue_char <- function(expr, values) {
  for (i in seq_along(expr)) {
    if (length(expr[[i]]) ==2 && expr[[i]][[1]] == sym("glue_char")) {
      expr[[i]] <- glue(expr[[i]][[2]], .envir = new_environment(data = values))
    }
    else if (typeof(expr[[i]]) == "language") {
      expr[[i]] <-  replace_glue_char(expr[[i]], values = values)
    }
  }
  expr
}

#' @export
replace_glue_sym <- function(expr, values) {
  for (i in seq_along(expr)) {
    if (length(expr[[i]]) ==2 && expr[[i]][[1]] == sym("glue_sym")) {
      expr[[i]] <- sym(glue(expr[[i]][[2]], .envir = new_environment(data = values)))
    }
    else if (typeof(expr[[i]]) == "language") {
      expr[[i]] <-  replace_glue_char(expr[[i]], values = values)
    }
  }
  expr
}

#' @export
consolidate_lists <- function(...) {
  list_all <- rlang::list2(...) %>%
    discard(is.null) %>%
    purrr::list_flatten()

  assertthat::assert_that(length(list_all) >= 1,
                          msg = "Please provide at least one list"
  )

  assertthat::assert_that(!"" %in% names(list_all),
                          msg = "All list elements must have a non-empty name"
  )

  indices_last <- data.frame(value = names(list_all)) %>%
    mutate(n = row_number()) %>%
    group_by(value) %>%
    filter(n == max(n)) %>%
    pull(n)

  list_all[indices_last]
}

#' @export
set_scriptr_sources <- function(packages) {
  scriptr_env$sources <- packages
}
