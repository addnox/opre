## Below base generics are used
## mean, sd, scale

## Below generics are created by this package
# fVaR
# fTVaR

double_dispatch <- function(single_dispatch, second_obj) {
  .f <- get(paste0(single_dispatch, ".", second_obj), envir = parent.frame())
  .f
}

#' @export
calc <- function(x, ...) {
  UseMethod("calc")
}

#' @export
mod <- function(x, ...) {
  UseMethod("mod")
}
