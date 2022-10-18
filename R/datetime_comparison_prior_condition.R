
#' Compares datetime with earliest dosing datetime to extract prior condition
#'
#' @name datetime_comparison_prior_condition
#' @param file_name1 data.frame with dosing datetime variable
#' @param file_name2 second data.frame (another domain eg. AE) with datetime variable
#' @param datetimecol1 is datetime variable (ADTC) in first data.frame
#' @param datetimecol2 is datetime variable (ADTC) in second data.frame
#' @param datetimecol3 is second datetime variable (AENDTC) in second data.frame
#' @param colwithconditions is variable with multiple disease or conditions (eg. AEDECOD)
#' @param filter_val character vector to filter colwithconditions
#' @param condition1 new variable name for subjects with prior disease or condition (eg. prior_diabetes)
#' @description compares datetime with earliest dosing datetime to extract prior condition
#' @return data.frame with prior disease/condition variable
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
#' STUDYID=rep("S-CDSK-01",3),
#' DOMAIN=rep("AE",3),
#' USUBJID=c("CDISC01.008","CDISC01.008","CDISC01.001"),
#' AEDECOD=c("Agitation","Anxiety","Anxiety"),
#' AESTDTC=c("29-04-2002","29-04-2001","16-10-2003")
#' )
#'datetime_comparison_prior_condition(ex, ae, "EXSTDTC", "AESTDTC",
#'colwithconditions = "AEDECOD",
#'filter_val = "Agitation",
#'condition1 = "PRIOR_Agitation"
#')

datetime_comparison_prior_condition <-
  function(file_name1, file_name2, datetimecol1, datetimecol2, datetimecol3,
           colwithconditions, filter_val, condition1) {
    name1 <- enquo(file_name1)
    name2 <- enquo(file_name2)
    datetimecol1 <- as.name(datetimecol1)
    datetimecol2 <- as.name(datetimecol2)
    colwithconditions <- as.name(colwithconditions)
    conditon1 <- as.name(condition1)

    name1 <- file_name1 %>% select(STUDYID, USUBJID, datetimecol1)
    name2 <- file_name2 %>% select(STUDYID, USUBJID, {{ datetimecol2 }}, {{ datetimecol3 }}, {{ colwithconditions }})

      data_ret <-
      full_join(name1, name2) %>%
      filter({{ colwithconditions }} %in% filter_val) %>%
      select(STUDYID, USUBJID, {{ colwithconditions }}, {{ datetimecol1 }}, {{ datetimecol2 }}) %>%
      group_by(STUDYID, USUBJID, {{ colwithconditions }}) %>%
      arrange({{ datetimecol2 }}, {{ datetimecol1 }}) %>%
      slice(1L) %>%
      mutate(
        STATUS = case_when(
          is.na({{ datetimecol2 }}) | is.na({{ datetimecol1 }}) ~ "None",
          TRUE ~ case_when(
            {{ datetimecol2 }} == "" | {{ datetimecol1 }} == "" ~ "not eligible",
            TRUE ~ case_when(
              {{ datetimecol2 }} >= {{ datetimecol1 }} ~ "after",
              TRUE ~ case_when(
                {{ datetimecol2 }} == {{ datetimecol1 }} ~ "same",
                TRUE ~ "before"
              )
            )
          )
        )
      ) %>%
      filter(STATUS %in% "before") %>%
      ungroup() %>%
      mutate({{ condition1 }} := {{ colwithconditions }}) %>%
      select(STUDYID, USUBJID, {{ condition1 }}, datetimecol1, datetimecol2)

      data_ret

  }

utils::globalVariables(c("STUDYID","USUBJID","STATUS"))
