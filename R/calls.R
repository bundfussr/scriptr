#' Adds a Call
#'
#' @param id Identifier
#' @param new_call Call
#' @param description Description
#'   The description is added as comment before the call.
#' @param env Environment where to store the call
#'
#' @export
#'
add_call <- function(id, new_call, description = NULL, env = rlang::caller_env()) {
  init_scriptr_env(env)
  env$scriptr_env$calls[[id]] <- call(enexpr(new_call), description = description)
}

call <- function(call, description) {
  out <- list(
    call = call,
    description = description
  )

  class(out) <- c("call", class(call))
  out
}

# add_call(id = "chg", call = derive_var_chg())

#' Adds a Call Template
#'
#' @param id Identifier
#' @param call Call
#' @param description Description
#'   The description is added as comment before the call.
#' @param defaults List of default values
#' @param env Environment where to store the call template
#'
#' @export
add_call_template <- function(id,
                              call,
                              description = NULL,
                              defaults = NULL,
                              env = rlang::caller_env()) {
  init_scriptr_env(env)

  env$scriptr_env$calls[[id]] <- call_template(
    enquo0(call),
    description = description,
    defaults = defaults
  )
}

call_template <- function(call, description = NULL, defaults) {
  out <- list(
    call = call,
    description = description,
    defaults = defaults
  )

  class(out) <- c("call_template", "list")
  out
}

#' Inserts Calls to a Script
#'
#' @param input Input object
#'
#'   The specified object is passed via a pipe to the first call.
#'
#' @param ids Calls
#'
#'   A list of call ids is expected.
#'
#' @export
insert_calls <- function(input, ids, output = NULL) {
  output <- enexpr(output)
  if (is.null(output)) {
    output_name <- expr_deparse(enexpr(input))
  } else {
    output_name <- expr_deparse(output)
  }
  paste(
    output_name,
    "<-",
    expr_deparse(enexpr(input)),
    "%>%",
    paste(
      get_calls(ids),
      collapse = " %>% "
    )
  )
}


#' @export
get_calls <- function(ids) {
  calls <- vector("character", length(ids))
  for (i in seq_along(ids)) {
    if (typeof(ids[[i]]) == "symbol") {
      calls[[i]] <- get_call_code(ids[[i]])
    }
    else {
      calls[[i]] <- get_call_code(call_name(ids[[i]]), args = call_args(ids[[i]]))
    }
  }
  calls
}

#' @export
get_call_code <- function(id, args = NULL) {
  id_char <- as_name(id)
  call <- get_call(id_char)
  if (inherits(call, "call")) {
    code <- paste(expr_deparse(call$call), collapse = " ")
  }
  else if (inherits(call, "call_template")) {
    expr <- use_template(
      quo_get_expr(call$call),
      values = consolidate_lists(call$defaults, args)
    )
    code <- paste(expr_deparse(expr), collapse = " ")
  }
  else {
    cli_abort("{.val {id_char}} has unsupported class {.val {class(call)}}")
  }
  if (!is.null(call$description)) {
    paste(paste("\n#", call$description), code, sep = "\n")
  } else {
    code
  }
}

#' @export
get_call <- function(id) {
  if (!is.null(global_env()$scriptr_env)) {
    if (id %in% names(global_env()$scriptr_env$calls)) {
      return(global_env()$scriptr_env$calls[[id]])
    }
  }
  for (s in scriptr_env$sources) {
    source_env <- get_source_env(s)
    if (!is.null(source_env)) {
      if (id %in% names(source_env$calls)) {
      return(source_env$calls[[id]])
    }
  }
  }
  cli_abort("{.val {id}} not found!")
}

#' @export
get_source_env <- function(source) {
  eval(parse_expr(paste0(source, ":::scriptr_env")))
}

#' @export
use_template <- function(expr, values) {
  expr %>%
    replace_bangbang(values = values) %>%
    replace_glue_sym(values = values) %>%
    replace_glue_char(values = values)
}
