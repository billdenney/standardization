#' A standardization function
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
check_derived_variables<-function(data){

  check<-data %>% select(STUDYID,PARAM,PARAMN,STUDYIDN) %>%
    mutate(PARAMN_check=as.integer(factor(PARAM)),STUDYIDN_check=as.integer(factor(STUDYID))) %>%
    distinct(PARAMN,STUDYIDN,PARAMN_check,STUDYIDN_check) %>%
    summarise(PARAMN=max(PARAMN),STUDYIDN=max(STUDYIDN),
               PARAMN_check=max(PARAMN_check),STUDYIDN_check=max(STUDYIDN_check)) %>%
    data.frame()


  return(check)

}
