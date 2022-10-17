#' Checks derived variables
#'
#' @param data is a data.frame with derived variables
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df<-data.frame(
#' STUDYID=c("CDISK-01","CDISK-01","CDISK-02","CDISK-02"),
#' PARAM=c("Hemoglobin","Glucose","Hemoglobin","Glucose"),
#' PARAMN=c(1,2,1,2),
#' STUDYIDN=c(1,2,1,2)
#' )
#' check_derived_variables(df)
check_derived_variables<-function(data){

  check<-data %>% select(STUDYID,PARAM,PARAMN,STUDYIDN) %>%
    mutate(PARAMN_check=as.integer(factor(PARAM)),STUDYIDN_check=as.integer(factor(STUDYID))) %>%
    distinct(PARAMN,STUDYIDN,PARAMN_check,STUDYIDN_check) %>%
    summarise(PARAMN=max(PARAMN),STUDYIDN=max(STUDYIDN),
               PARAMN_check=max(PARAMN_check),STUDYIDN_check=max(STUDYIDN_check)) %>%
    data.frame()


  return(check)

}

utils::globalVariables(c("STUDYID","USUBJID","STUDYIDN","PARAM","PARAMN",
                         "PARAMN_check","STUDYIDN_check"))
