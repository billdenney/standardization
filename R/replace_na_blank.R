#' A standardization function
#'
#' @param data 
#' @param columns 
#' @param replacement 
#'
#' @return
#' @export
#'
#' @examples
#' replace_na_blank()
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