#' A standardization function
#'
#' @param data
#' @param col1
#' @param col2
#'
#' @return
#' @export
#'
#' @examples
#' check_units()
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
    mutate(compare=case_when(
      {{col1}} == {{col2}}~ TRUE,
      TRUE ~ FALSE )) %>%
    filter(!PARAM %in% exclude_var) %>%
    filter(!compare %in% TRUE)

  print(check_df)

}
