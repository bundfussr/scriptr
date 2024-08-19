#' Generate a Script
#'
#' @param code Code for creating the script
#' @param file File to write the script to
#'
#' @export
execute <- function(code, file) {
  code <- enexpr(code)
  on.exit(close(file))

  print(code)
  print(length(code))
  print(code[[length(code)]])
  if (code[[1]] == "{") {
    code[[1]] <- NULL
  }
  for (i in seq_along(code)) {
    print(paste("Statement", i))
    print(typeof(code[[i]]))
    print(code[[i]])
    if (typeof(code[[i]]) == "character" && str_starts(code[[i]], "#")) {
      write(code[[i]], file)
    } else if (str_starts(paste(expr_deparse(code[[i]]), collapse = " ")
                          , "insert_calls\\(")) {
      print("insert found")
      write(style_text(eval(code[[i]])), file)
    } else {
      write(style_text(expr_deparse(code[[i]])), file)
    }
  }
}
