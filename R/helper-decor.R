print_Param <- function(x, df.param = list()) {
  stopifnot(is.list(x))
  stopifnot(collapse::ldepth(x) <= 2)

  .item <- x
  .nmlist <- names(x)

  for (i in seq_along(.item)) {
    .nm <- .nmlist[i]
    .val <- .item[[i]]
    cat("\n*", .nm, ":", sep = "")
    if (is.vector(.val)) cat(" ", paste(.val, collapse = ", "), "\n", sep = "")
    if (is.data.frame(.val)) {
      cat("\n")
      do.call(data.table:::print.data.table, c(list(.val, class = FALSE, row.names = TRUE), df.param))
    }
  }
}
