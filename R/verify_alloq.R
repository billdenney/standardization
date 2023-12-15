#' Verifies if ALLOQ values are missing when AVALC data is present
#'
#' @param data Data.frame to be checked
#' @param id_col STUDYID variable
#' @param AVALC_col AVALC variable
#' @param ALLOQ_col ALLOQ variable
#' @param param_col PARAM variable
#' @param expected_missing Character vector of AVALC data that are expected to have missing values in ALLOQ.
#' @description Checks if ALLOQ values are missing when AVALC data is present.
#' @return Error message for data rows with missing values in ALLOQ.
#' @export
#'
#' @examples
#' df1<-data.frame(
#' STUDYID=paste("CDISK_00",1:3),
#' PARAM=c(rep("Hb",3)),
#' AVALC=c("BLQ","BLOQ","BLQ"),
#' ALLOQ=c(20.3,21.2,NA_integer_)
#' )
#'
#' verify_alloq(df1,expected_missing = "BLQ")
verify_alloq<-function(data,id_col="STUDYID",AVALC_col="AVALC",ALLOQ_col="ALLOQ",param_col="PARAM",expected_missing){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data[[id_col]]) & length(data[[id_col]]) > 1 & id_col %in% names(data))
  stopifnot(is.character(data[[AVALC_col]]) & length(data[[AVALC_col]]) > 1 & AVALC_col %in% names(data))
  stopifnot(is.numeric(data[[ALLOQ_col]]) & length(data[[ALLOQ_col]]) > 1 & ALLOQ_col %in% names(data))
  stopifnot(is.character(data[[param_col]]) & length(data[[param_col]]) > 1 & param_col %in% names(data))

  cols<-c("STUDYID","PARAM","AVALC","ALLOQ")

  check_ALLOQ <-
    data %>%
    select(all_of(cols)) %>%
    group_by(STUDYID,PARAM,AVALC,ALLOQ) %>%
    distinct() %>%
    mutate(check=case_when(
      .data[[AVALC_col]] ==.data[[ALLOQ_col]] ~"TRUE",
      is.na(.data[[AVALC_col]]) == is.na(.data[[ALLOQ_col]])~ "TRUE",
      TRUE~"FALSE"
    )) %>%
    filter(check %in% FALSE) %>%
    filter(!AVALC %in% expected_missing)

  if(nrow(check_ALLOQ) > 0){
    print(check_ALLOQ)
    stop("The above ALLOQ rows are not expected to be missing, please check them")
  }

}


utils::globalVariables(c('data', 'id_col', 'AVALC_col', 'ALLOQ_col', 'param_col',
                         'expected_missing','check','ALLOQ', 'AVALC'))
