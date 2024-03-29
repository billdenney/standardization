#' checks if missing values in AVALN column correspond to missing values in AVALC column.
#'
#' @param data Data.frame to be checked.
#' @param id_col STUDYID variable.
#' @param AVALC_col AVALC variable.
#' @param AVALN_col AVALN variable.
#' @param param_col PARAM variable.
#' @param expected_missing Character vector of AVALC data that are expected to have missing values in AVALN.
#' @description Verifies there are no missing values in AVALN when AVALC data is present unless specified in expected_missing.
#' @return Error message for AVALN rows with missing values.
#' @export
#'
#' @examples
#' df2<-data.frame(
#' STUDYID=paste("CDISK_00",1:7),
#' PARAM=c(rep("Hb",3),rep("Glucose",4)),
#' AVALC=c(20.3,21.2,22.5,60.5,55.7,"BLQ","Sample collected"),
#' AVALN=c(20.3,21.2,22.5,60.5,55.7,NA_integer_,NA_integer_)
#' )
#'
#' verify_avaln(df2,expected_missing = c("BLQ","Sample collected"))
verify_avaln<-function(data,id_col="STUDYID",AVALC_col="AVALC",AVALN_col="AVALN",param_col="PARAM",expected_missing){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data[[id_col]]) & length(data[[id_col]]) > 1 & id_col %in% names(data))
  stopifnot(is.character(data[[AVALC_col]]) & length(data[[AVALC_col]]) > 1 & AVALC_col %in% names(data))
  stopifnot(is.numeric(data[[AVALN_col]]) & length(data[[AVALN_col]]) > 1 & AVALN_col %in% names(data))
  stopifnot(is.character(data[[param_col]]) & length(data[[param_col]]) > 1 & param_col %in% names(data))


  columns<-c(id_col,param_col,AVALC_col,AVALN_col)

  check_AVALN <-
    data %>%
    select(all_of(columns)) %>%
    filter(is.na(.data[[AVALN_col]]))%>%
    group_by(.data[[id_col]],.data[[param_col]],.data[[AVALC_col]],.data[[AVALN_col]]) %>%
    distinct() %>%
    mutate(check=case_when(
      .data[[AVALC_col]] ==.data[[AVALN_col]] ~"TRUE",
      is.na(.data[[AVALC_col]]) == is.na(.data[[AVALN_col]])~ "TRUE",
      TRUE~"FALSE"
    )) %>%
    ungroup()%>%
    filter(check %in% FALSE) %>%
    filter(!.data[[AVALC_col]] %in% expected_missing)

  if(nrow(check_AVALN) > 0){
    print(check_AVALN)
    stop("The above AVALN rows are not expected to be missing, please check them")

  }

}

utils::globalVariables(c('data', 'id_col', 'AVALC_col', 'AVALN_col', 'param_col',
                         'expected_missing','check'))
