#' Verify that no unexpected duplicates are in the data
#' 
#' @param data The data to check
#' @param id_col,time_col,param_col Character scalars for the ID, TIME, and parameter name
#' @param expected A list of expected duplicated values
#' @return The \code{data} if there is unexpected duplication, raising an error, otherwise returns data
#' @export
#' @examples 
#' df_wo_dup<-data.frame(
#' USUBJID=c(rep("CDISC01.10001",6)),
#' PARAM=c("pk","pk","dosing","dosing","dosing","dosing"),
#' TSFD=c(2542.9,25421.9,3213,2543,2456,3241),
#' VISIT=c("WEEK 16","WEEK 16","Week 3","Week 4","Week 5","Week 6"),
#' ATPT=c("","","0 hours","0 hours","0 hours","0 hours"),
#' ADTC=c("2020-03-27T10:01:UN","","","","","")
#' ) 
#'  
#' verify_no_duplicates(data=df_wo_dup)
verify_no_duplicates <- function(data, id_col="USUBJID", time_col="TSFD", param_col="PARAM", expected=list()) {
  
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data[[id_col]]) & length(data[[id_col]]) > 1 & id_col %in% names(data))
  stopifnot(is.numeric(data[[time_col]]) & length(data[[time_col]]) > 1 & time_col %in% names(data))
  stopifnot(is.character(data[[param_col]]) & length(data[[param_col]]) > 1 & param_col %in% names(data))
  
  dup_count <-
    data %>%
    dplyr::group_by(.data[[id_col]], .data[[time_col]],.data[[param_col]]) %>%
    dplyr::summarize(count=dplyr::n()) %>%
    dplyr::ungroup() 
  
  duplicated <-
    dup_count %>%
    filter(count > 1)  
  
  
  for (current_expected in expected) {
    duplicated <- dplyr::anti_join(duplicated, expected)
  }
  
  if(nrow(duplicated) > 0){
    print(duplicated)
    stop("The above rows are not expected to be duplicated, please check them")
  } 
  data
}

utils::globalVariables(c(".data","count"))