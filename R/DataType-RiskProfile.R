#' Create a RiskProfile object
#' @examples
#' x <- data.frame(N = c(100, 50, 10), SumInsured = c(100 * 10, 50 * 20, 10 * 30), Prem = c(10, 10, 10))
#' rp <- RiskProfile(x, TSI = "SumInsured", Premium = "Prem")
#' rp
#'
#' ## Scale a Risk Profile
#' mod(data.table::copy(rp), 3)
#' mod(data.table::copy(rp), c(3, 2, 0))
#' @export
RiskProfile <- function(x, ...) {
  RP <- field_map(x, ...) |>
     field_check(required_field = c("N", "TSI", "Premium"))

   structure(RP, class = c("RiskProfile", class(RP)))
}

#' @export
print.RiskProfile <- function(x, ...) {
  cat("<RiskProfile>\n")
  NextMethod()
  invisible(x)
}

#' Modify a RiskProfile object
#' @export
#' @rdname RiskProfile
#' @usage NULL
mod.RiskProfile <- function(x, method = c("Scale"), ...) {
  method <- match.arg(method)
  .f <- double_dispatch("mod.RiskProfile", method)
  .f(x, ...)
}

#' @export
#' @rdname RiskProfile
#' @usage
#' ## Scale a Risk Profile
#' mod(RiskProfile, "Scale", scale)
mod.RiskProfile.Scale <- function(x, scale = 1) {
  if (!length(scale) == 1 & !length(scale) == nrow(x)) stop("`scale` must be atomic, or with the same length as the number of rows of `x`.", call. = FALSE)

  x[, `:=`(N = scale * N, TSI = scale * TSI, Premium = scale * Premium)]
  x[]
}

