#' Compares and checks for uniformity of units within a parameter name across two studies
#'
#' @param data is a data frame
#' @param col1 is for study1
#' @param col2 is for study2
#' @param exclude_var excluded specific parameter names where units do not apply eg. scores
#'
#' @return data.frame with non-matching units or empty data.frame
#' @export
#'
#' @examples
#' df<-data.frame(
#' STUDYID=c("CDISK-01","CDISK-01","CDISK-02","CDISK-02","CDISK-02"),
#' USUBJID=paste0("CDISC01.10000",c(1:5)),
#' PARAM=c("CDISK dosing","Hemoglobin","Glucose","Hemoglobin","Glucose"),
#' ECDOSEU=c("mg","","","",""),
#' EXDOSEU=c("","","","",""),
#' AVALU=c("","g/dl","mmol/L","g/dl","mmol/L")
#' )
#'check_units(df)
check_units<-function(data,col1,col2,exclude_var){

  check<-data %>%
    select(STUDYID,USUBJID,PARAM,ECDOSEU,EXDOSEU,AVALU) %>%
    group_by(STUDYID, PARAM) %>%
    summarise(check_AVALU=AVALU,check_EXDOSEU=EXDOSEU,check_ECDOSEU=ECDOSEU) %>%
    mutate(units=coalesce(check_AVALU,check_EXDOSEU,check_ECDOSEU)) %>%
    ungroup() %>%
    mutate_all(list(~tolower(.))) %>%
    select(-check_AVALU,-check_EXDOSEU,-check_ECDOSEU) %>%
    group_by() %>%
    distinct() %>%
    ungroup() %>%
    pivot_wider(names_from="STUDYID", values_from="units",values_fn = list) %>%
    suppressWarnings(unnest()) %>%
    suppressWarnings(mutate_each(funs(replace(., is.na(.), ""))))


  cols<-names(check)[!names(check) %in% "PARAM"]


  check_df<-check %>%
    rename(STUDY=cols) %>%
    rowwise() %>%
    unnest(cols = c({{col1}}, {{col2}})) %>%
    mutate(test=case_when(
      {{col1}} == {{col2}}~ TRUE,
      TRUE ~ FALSE )) %>%
    verify(!duplicated(PARAM)) %>%
    filter(!test %in% TRUE) %>%
    verify(!test %in% FALSE)

  check_df


}

utils::globalVariables(c("STUDYID","USUBJID","ECDOSEU","PARAM","EXDOSEU","AVALU","check_AVALU",
                         "check_EXDOSEU","check_ECDOSEU",".","test"))


