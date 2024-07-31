#' Parse a character vector to numeric
#' @examples
#' parse_numeric(c("$123,456.789", "N/A", "3.1415926525K", "99.95%", "Inf", "inf"))
#' @export
parse_numeric <- function(x, na = c("", "NA", "N/A")) {
  res <- vapply(
      x,
      function(x) {
        if (tolower(x) %in% tolower(na)) return(NA_character_)
        if (tolower(x) == "inf") return("Inf")


      stringi::stri_replace_first_regex(x, "^\\D*", "") |>
        stringi::stri_replace_all_fixed(c("%", "K", "M", "B", "T", ","), c("e-2", "e3", "e6", "e9", "e12", ""), vectorize_all = FALSE, case_insensitive = TRUE)
      },
      character(1L)
    )

  as.numeric(res)
}

#' Parse a character vector to Date
#' @examples
#' x <- c("2021-01-03", "12-31-2019", "Jan 2 2022", "2023/1/1")
#' parse_date(x)
parse_date <- function(x) {
  x <- trimws(x)
  res <- data.table::fifelse(
    grepl("\\d{5}", x),
    as.Date(x, origin = "1899-12-30"),
    anytime::anydate(x)
  )

  res
}
