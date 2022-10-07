#' A standardization function
#'
#' @name check_merge_output
#' @param data
#' @param study
#' @param ...
#' @description checks for dupplicate rows
#'
#' @return returns duplicate rows
#' @export
#'
#' @examples
#' check_merge_output()
check_merge_output <- function(data, study, ...) {
  group_var <- dplyr::enquos(...)
  for (i in 1:length(study)) {
    print(paste0("Checking study: ", study[i]))
    check_dupl <-
      data %>%
      ungroup() %>%
      filter(STUDYID %in% study[i]) %>%
      group_by(!!!group_var) %>%
      dplyr::mutate(row = row_number()) %>%
      verify(row %in% 1)
  }
}
