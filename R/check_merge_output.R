#' Checks for duplicate rows
#'
#' @name check_merge_output
#' @param data is a data frame
#' @param study is a character STUDYID pattern to check data frame for duplicate rows for each study
#' @param ... are grouping variables
#' @description checks for duplicate rows
#'
#' @return prints duplicate rows and throws an error if duplicate rows are present
#' @export
#' @examples
#' ex<-data.frame(
#' STUDYID=rep("S-CDSK-01",4),
#' DOMAIN=rep("EX",4),
#' USUBJID=c("CDISC01.001","CDISC01.002","CDISC01.003","CDISC01.008"),
#' EXTRT=rep("THEOPHYLLINE",4),
#' EXROUTE=rep("ORAL",4),
#' EXDOSE=c(4.02,4.4,4.53,4.53),
#' EXDOSU=rep("mg/kg",4),
#' EXSTDTC=rep("29-04-2003",4)
#' )
#' ae<-data.frame(
#'STUDYID=rep("S-CDSK-01",3),
#'DOMAIN=rep("AE",3),
#'USUBJID=c("CDISC01.008","CDISC01.008","CDISC01.001"),
#'AEDECOD=c("Agitation","Anxiety","Anxiety"),
#'AESTDTC=c("29-04-2002","29-04-2001","16-10-2003")
#')
#' merge_df<-dplyr::full_join(ae,ex)
#' check_merge_output(merge_df,study ="S-CDSK-01",USUBJID,AEDECOD,AESTDTC)
check_merge_output <- function(data, study, ...) {
  group_var <- enquos(...)
  for (i in seq_along(study)) {
    print(paste0("Checking study: ", study[i]))
    check_dupl <-
      data %>%
      ungroup() %>%
      filter(STUDYID %in% study[i]) %>%
      group_by(!!!group_var) %>%
      mutate(row = row_number()) %>%
      verify(row %in% 1)
  }
}

utils::globalVariables(c("STUDYID","row_number"))
