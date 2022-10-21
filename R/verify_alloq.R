#' verifies for unexpected missing values in ALLOQ
#'
#' @param data data.frame to be checked
#' @param id_col STUDYID variable
#' @param AVALC_col AVALC variable
#' @param ALLOQ_col ALLOQ variable
#' @param param_col PARAM variable
#' @param expected_missing character vector of exclusions from AVALC that are expected to have missing values in AVALN
#' @description Verifies missing AVALC matches missing AVALN values unless AVALC is in a vector that is pre-specified
#' @return error with missing AVALN rows and otherwise NULL
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
  
  
  check_ALLOQ <-
    data %>%
    select(STUDYID,PARAM,AVALC,ALLOQ) %>% 
    group_by(STUDYID,PARAM,AVALC,ALLOQ) %>%
    distinct() %>%
    ungroup() %>% 
    filter(grepl("<[0-9]|< [0-9]|BLOQ|BLLOQ|BLQ|BQL",AVALC)) %>% 
    rowwise() %>% 
    mutate(check=case_when(
      is.na(ALLOQ) %in% !is.na(AVALC) ~"FALSE",
      TRUE~"TRUE"
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
