#' Create a Triangle object
#' @export
#' @examples
#' prem <- Triangle(TRI$Premium)
#' loss <- Triangle(TRI$Incurred)
#' loss / prem
#'
#' ## Chain-Ladder Method
#' calc(prem, "ChainLadder", "weighted")
#' calc(prem, "ChainLadder", c(1.2, 1.1, 1.05))
#' calc(prem, "ChainLadder", "weighted", floor = 1)
#'
#' ## BF with CapeCod ELR
#' UltPrem <- calc(prem, "ChainLadder", floor = 1)$ultimate
#' ELR <- CapeCod(loss, premium = UltPrem, floor = 1)
#' calc(loss, "BF", expected_loss = ELR * UltPrem, floor = 1)

Triangle <- function(x, origin = 1L) {
  xdata <- x

  if (origin == 1L) {
    origin <- x[[1L]]
    xdata <- x[, -1L]
  } else if (is.null(origin)) {
    origin <- 1:nrow(xdata)
  } else if (!length(origin) == nrow(x)) {
    stop("origin must be 1L, NULL or a vector with the same length as nrow of x", call. = FALSE)
  }

  dev <- 1:ncol(xdata)

  mat <- as.matrix(xdata)
  dimnames(mat) <- list("origin" = origin, "dev" = dev)

  structure(mat, class = c("Triangle", "matrix"))
}

#' @export
print.Triangle <- function(x, ...) {
  cat("<Triangle>\n")
  print.default(unclass(x))
  invisible(x)
}

#' @export
#' @rdname Triangle
#' @usage NULL
calc.Triangle <- function(x, method = c("ChainLadder", "BF"), ...) {
  method <- match.arg(method)
  .f <- double_dispatch("calc.Triangle", method)
  .f(x, ...)
}

#' @export
#' @rdname Triangle
#' @usage
#' ## Develope a Triangle using Chain Ladder method
#' calc(Triangle, "ChainLadder", link_ratio = "weighted")
calc.Triangle.ChainLadder <- function(x, link_ratio = "weighted", ...) {
  # determine age-to-age factor (ata)
  inc_ata <- ata(x, link_ratio, ...)

  # loop through columns (i.e. dev)
  res <- tri_fill_lower(x, inc_ata)

  res
}

#' @export
#' @rdname Triangle
#' @usage
#' ## Develope a Triangle using B-F method
#' calc(Triangle, "BF", expected_loss, link_ratio = "weighted")
calc.Triangle.BF <- function(x, expected_loss, link_ratio = "weighted", ...) {
  inc_ata <- ata(x, link_ratio, ...)
  report_pct <- rev(1 /ata_to_cum(inc_ata)) ## rev so that earliest year comes first

  ult_loss <- tri_latest(x) + (1 - report_pct) * expected_loss

  tri_fill_lower(x, report_pct = report_pct, ultimate_loss = ult_loss)
}

#' @rdname Triangle
#' @usage ## Helper Functions
.Helper <- NULL

#' @export
#' @rdname Triangle
CapeCod <- function(x, premium, link_ratio = "weighted", ...) {
  inc_ata <- ata(x, link_ratio, ...)
  report_pct <- rev(1 / ata_to_cum(inc_ata)) ## rev so that earliest year comes first
  latest_loss <- tri_latest(x)
  sum(latest_loss) / sum(premium * report_pct)
}

tri_fill_lower <- function(tri, inc_ata = NULL, report_pct = NULL, ultimate_loss = NULL) {
  if (!is.null(inc_ata)) {
    # loop through columns (i.e. dev)
    res <- cbind(tri, NA_real_)
    for (j in seq_len(ncol(tri))) {
      col_from <- res[, j]
      col_to <- res[, j + 1]
      res[is.na(col_to), j + 1] <- (col_from * inc_ata[j])[is.na(col_to)]
    }
  } else if (!is.null(report_pct) & !is.null(ultimate_loss)) {
    res <- cbind(tri, ultimate_loss)
    for (j in rev(seq_len(ncol(tri)))) {
      col_from <- res[, j + 1]
      col_to <- res[, j] ## backward fill
      res[is.na(col_to), j] <- (col_from * rev(report_pct)[j])[is.na(col_to)]
    }
  }

  res <- data.table::as.data.table(res, keep.rownames = "origin")
  data.table::setnames(res, ncol(res), "ultimate")
  res[]
}

#' @export
#' @rdname Triangle
tri_latest <- function(tri) {
  res <- unlist(apply(tri, 1L, function(x) data.table::last(na.omit(x)), simplify = FALSE))
  res
}

#' @export
#' @rdname Triangle
tri_link_ratio <- function(x, average = c(NA, "weighted", "simple")) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  average <- match.arg(average)
  nmx <- colnames(x)
  tri_from <- x[, -ncol(x)]
  tri_to <- x[, -1L]

  if (is.na(average)) {
    res <- tri_to / tri_from
    colnames(res) <- paste0(nmx[-length(nmx)], "-", nmx[-1L])
  } else if (average == "simple") {
    yty <- tri_to / tri_from
    yty[is.infinite(yty)] <- NA_real_
    res <- colMeans(yty, na.rm = TRUE)
    names(res) <- paste0(nmx[-length(nmx)], "-", nmx[-1L])
  } else if (average == "weighted") {
    idx <- is.na(tri_to) | is.na(tri_from)
    tri_to[idx] <- NA_real_
    tri_from[idx] <- NA_real_
    res <- colSums(tri_to, na.rm = TRUE) / colSums(tri_from, na.rm = TRUE)
    names(res) <- paste0(nmx[-length(nmx)], ":", nmx[-1L])
  }

  res
}

#' @export
#' @rdname Triangle
ata <- function(tri, link_ratio, tail_factor = 1, floor = 0, interp = NULL) {
  # determine age-to-age factor (ata)
  if (is.numeric(link_ratio)) {
    ata <- link_ratio
    length(ata) <- ncol(tri) - 1
  } else if (link_ratio == "weighted") {
    ata <- tri_link_ratio(tri, "weighted")
  } else if (link_ratio == "simple") {
    ata <- tri_link_ratio(tri, "simple")
  }

  ata[is.na(ata)] <- 1
  ata <- pmax(floor, ata)
  c(ata, tail_factor)
}

ata_to_cum <- function(x) {
  rev(cumprod(rev(x)))
}
