#' A standardization function
#'
#' @name check_merge_output
#' @param data is a data frame
#' @param study is a character STUDYID pattern to check data frame for duplicate rows for each study
#' @param ... are grouping variables
#' @description checks for duplicate rows
#'
#' @return throws an error and prints duplicate rows
#' @export
#'
#' @examples
check_merge_output <- function(data, study, ...) {
  group_var <- dplyr::enquos(...)
  for (i in 1:length(study)) {
    print(paste0("Checking study: ", study[i]))
    check_dupl <-
      data %>%
      ungroup() %>%
      filter(STUDYID %in% study[i]) %>%
      group_by(!!!group_var) %>%
      mutate(row = row_number()) %>%
      verify(row %in% 1)
  }
}

utils::globalVariables(c("STUDYID","row_number"))
