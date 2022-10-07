#' A standardization function
#'
#' @param data
#' @param col
#'
#' @return
#' @export
#'
#' @examples
#' pk_variables()
pk_variables <- function(data, col) {
  col <- as.name(col)

  values <- data %>%
    mutate(
      text = as.character({{ col }}),
      number = as.numeric({{ col }}), #NA for BLQ values with special characters in the string
      number=case_when(
        grepl("<[A-Za-z]|[A-Za-z]<|<+[[:space:]]+[A-Za-z]|[A-Za-z]+[[:space:]]+<|<|<+[[:space:]]+[[:space:]]+<",text) ~ 0,#converting NA's into 0
        TRUE ~ number
      ),
      llq = case_when(
        grepl("BLQ|LLQ|BQL|LLOQ|<", {{ col }}, ignore.case = TRUE) ~ str_extract({{ col }}, "\\d+\\.*\\d*"),
        TRUE ~ ""
      ),
      ulq = case_when(
        grepl("ulq|uql", {{ col }}, ignore.case = TRUE) ~ str_extract({{ col }}, "\\d+\\.*\\d*"),
        TRUE ~ ""
      )
    )

  ret <-
    data %>%
    mutate(
      AVALN = values$number,
      AVALC = values$text,
      ALLOQ = values$llq,
      AULOQ = values$ulq
    )
}


