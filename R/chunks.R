#' Adds a Chunk
#'
#' @param id Identifier
#' @param new_chunk Chunk
#' @param description Description
#'   The description is added as comment before the chunk.
#' @param env Environment where to store the chunk
#'
#' @export
#'
add_chunk <- function(id, new_chunk, description = NULL, env = rlang::caller_env()) {
  init_scriptr_env(env)
  env$scriptr_env$chunks[[id]] <- chunk(enexpr(new_chunk), description = description)
}

#' Adds a Chunk Template
#'
#' @param id Identifier
#' @param chunk Chunk
#' @param description Description
#'   The description is added as comment before the chunk.
#' @param defaults List of default values
#' @param env Environment where to store the chunk template
#'
#' @export
add_chunk_template <- function(id,
                               chunk,
                               description = NULL,
                               defaults = NULL,
                               env = rlang::caller_env()) {
  init_scriptr_env(env)

  env$scriptr_env$chunks[[id]] <- chunk_template(
    quo_get_expr(enquo0(chunk)),
    description = description,
    defaults = defaults)
}

chunk <- function(chunk, description) {
  out <- list(
    chunk = chunk,
    description = description
  )

  class(out) <- c("chunk", class(chunk))
  out
}

chunk_template <- function(chunk, description, defaults) {
  out <- list(
    chunk = chunk,
    description = description,
    defaults = defaults
  )

  class(out) <- c("chunk_template", "list")
  out
}

#' Inserts Chunks into a Script
#'
#' @param ids Chunk ids
#'
#'   A list of chunk ids is expected.
#'
#' @export
insert_chunks <- function(ids) {
  paste(
    paste(
      get_chunks(ids),
      collapse = "\n"
    )
  )
}


#' @export
get_chunks <- function(ids) {
  chunks <- vector("character", length(ids))
  for (i in seq_along(ids)) {
    if (typeof(ids[[i]]) == "symbol") {
      chunks[[i]] <- get_chunk_code(ids[[i]])
    }
    else {
      chunks[[i]] <- get_chunk_code(call_name(ids[[i]]), args = call_args(ids[[i]]))
    }
  }
  chunks
}

#' @export
get_chunk_code <- function(id, args = NULL) {
  id_char <- as_name(id)
  chunk <- get_chunk(id_char)
  if (inherits(chunk, "chunk")) {
    code <- paste(chunk_deparse(chunk$chunk), collapse = "\n")
  }
  else if (inherits(chunk, "chunk_template")) {
    expr <- use_template(
      chunk$chunk,
      values = consolidate_lists(chunk$defaults, args)
    )
    code <- paste(chunk_deparse(expr), collapse = "\n")
  }
  else {
    cli_abort("{.val {id_char}} has unsupported class {.val {class(chunk)}}")
  }
  code <- str_remove_all(code, "(^`?\\{`?\\n|\\n\\}$)") %>% paste0("\n")
  if (!is.null(chunk$description)) {
    paste(format_description(chunk$description), code, sep = "\n")
  } else {
    code
  }
}

#' @export
chunk_deparse <- function(chunk) {
  map_chr(as.vector(unclass(chunk)), function(x) {
    if (typeof(x) == "character" && (str_starts(x, "#") || x == "")) {
      x
    } else if (length(x) > 1) {
      paste(expr_deparse(x), collapse = " ")
    } else {
      expr_deparse(x)
    }
  })
}

#' @export
get_chunk <- function(id) {
  if (!is.null(global_env()$scriptr_env)) {
    if (id %in% names(global_env()$scriptr_env$chunks)) {
      return(global_env()$scriptr_env$chunks[[id]])
    }
  }
  for (s in scriptr_env$sources) {
    source_env <- get_source_env(s)
    if (!is.null(source_env)) {
      if (id %in% names(source_env$chunks)) {
        return(source_env$chunks[[id]])
      }
    }
  }
  cli_abort("{.val {id}} not found!")
}
