#' Replaces NA and blanks with replacement pattern
#'
#' @param data is a data frame
#' @param columns to replace
#' @param replacement pattern to replace NA and blanks in columns
#' @return data frame with missing rows ("", NA) replaced with specified replacement pattern
#' @export
#'
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
