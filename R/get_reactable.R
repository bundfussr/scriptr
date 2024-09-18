#' @export
get_reactable <- function(env = scriptr_env) {
  calls <- env$calls
  call_data <- tibble(
    Id = names(calls),
    Description = map(calls, function(x) x$description),
    Code = map(calls, function(x) {
      if (inherits(x, "call_template")) {
        style_code(paste(expr_deparse(quo_get_expr(x$call)), collapse = " "))
      } else {
        style_code(paste(expr_deparse(x$call), collapse = " "))
      }
    }
    )) %>% mutate(
      Type = map(calls, function(x) class(x)[[1]])
    )
  chunks <- env$chunks
  chunk_data <- tibble(
    Id = names(chunks),
    Description = map(chunks, function(x) x$description),
    Code = map(chunks, function(x) {
      style_code(paste(chunk_deparse(x$chunk), collapse = "\n")%>% str_remove_all("(^`?\\{`?\\n|\\n\\}$)"))
    }
    )) %>% mutate(
      Type = map(chunks, function(x) class(x)[[1]])
    )

  all_data <- bind_rows(call_data, chunk_data)

  reactable(
    all_data,
    columns = list(
      # Id = colDef(width = 150),
      # Description = colDef(width = 300),
      Code = colDef(
        minWidth = 200,
        cell = function(value, index) {
          pre(paste(value, collapse = "\n"))
          #       # Render as a link
          #       url <- paste0(
          #         "../reference/",
          #         substr(value, 1, nchar(value) - 2),
          #         ".html"
          #       )
          #       htmltools::tags$a(href = url, as.character(value))
        }
      )
      # Type = colDef(
      #   width = 150
      # )
    ),
    filterable = TRUE,
    resizable = TRUE,
    defaultPageSize = 20
  )
}
