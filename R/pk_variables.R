#' Numeric recoding of AVALC to generate AVALN, ALLOQ, AULOQ
#'
#' @param data is a data frame containing AVALC variable for recoding
#' @param col is the equivalent for AVALC variable
#'
#' @return AVALN, ALLOQ and AULOQ
#' @export
#'
#' @examples pk_variables(data.frame(AVALC=c("<2.30","3.45","BLQ","BQL<2.1")),"AVALC")
pk_variables <- function(data, col) {
  col <- as.name(col)

  values <- data %>%
    mutate(
      text = as.character({{ col }}),
      number = as.numeric({{ col }}),
      number=case_when(
        grepl("<[A-Za-z]|[A-Za-z]<|<+[[:space:]]+[A-Za-z]|[A-Za-z]+[[:space:]]+<|<|<+[[:space:]]+[[:space:]]+<",text) ~ 0,
        TRUE ~ number
      ),
      llq = case_when(
        grepl("BLQ|LLQ|BQL|LLOQ|<", {{ col }}, ignore.case = TRUE) ~ str_extract({{ col }}, "\\d+\\.*\\d*"),
        TRUE ~ ""
      ),
      ulq = case_when(
        grepl("ulq|uql|>", {{ col }}, ignore.case = TRUE) ~ str_extract({{ col }}, "\\d+\\.*\\d*"),
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


