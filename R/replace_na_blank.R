#' Replaces NA and blanks with replacement pattern in multiple columns.
#'
#' @param data is a data.frame.
#' @param columns specifies columns.
#' @param replacement Specifies columns where replacement pattern is to be applied.
#' @return Data.frame where NA and blanks are replaced with specified replacement pattern.
#' @export
#' @examples replace_na_blank(data.frame(col1=c(1,"",NA_character_)),c("col1"),"U")
replace_na_blank<-function(data,columns,replacement){
  for (col in columns) {
    col <- paste0(col)

    data<-data %>%
      mutate(!!sym(col):= case_when(
        is.na(!!sym(col))|!!sym(col) %in% "" ~ replacement,
        TRUE ~ !!sym(col)))
  }
  data
}
