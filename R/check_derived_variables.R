#' Ensures no duplicate parameter names exist. Compares output dataset with specifications file for number of elements in columns PARAMN and PARAM.
#'
#' @param data Data.frame with PARAM and PARAMN columns.
#' @param specs specifications file with 'Recode PARAM' sheet required.
#' @return Data.frame with number of unique elements in PARAM and PARAMN.
#' @description Compares output dataset with specifications file for number of elements in columns PARAMN and PARAM.
#' @export
#' @examples
#' df<-data.frame(
#' PARAM=c("Hemoglobin","Glucose","Hemoglobin","Glucose"),
#' PARAMN=c(1,2,1,2)
#' )
#' list1<-data.frame(
#' PARAM=c("Hemoglobin","Glucose"),
#' PARAMN=c(1,2)
#' )
#' specification<-list(list1)
#' names(specification)<-"Recode PARAM"
#' check_derived_variables(df,specification)
check_derived_variables<-function(data,specs){

  check<-data %>% select(PARAM,PARAMN) %>%
    mutate(PARAMN_check=as.integer(factor(PARAM))) %>%
    distinct(PARAM,PARAMN,PARAMN_check,) %>%
    summarise(PARAMN=max(PARAMN),
              PARAMN_check_df=max(PARAMN_check)) %>%
    data.frame()

  check_specs<-specs[["Recode PARAM"]] %>% select(PARAM,PARAMN) %>%
    mutate(PARAMN_check=as.integer(factor(PARAM))) %>%
    distinct(PARAM,PARAMN,PARAMN_check) %>%
    summarise(PARAMN=max(PARAMN),
              PARAMN_check_specs=max(PARAMN_check)) %>%
    data.frame()

  check_data_specs<-check %>% cbind(check_specs) %>%
    select(-PARAMN) %>%
    dplyr::group_by_all() %>%
    verify(PARAMN_check_df == PARAMN_check_specs)

  check_data_specs


  }

utils::globalVariables(c("PARAM","PARAMN","PARAMN_check",'PARAMN_check_specs','PARAMN_check_df'))
