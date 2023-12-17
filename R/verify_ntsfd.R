#' Ensures there are no missing values in NTSFD column
#'
#' @param data Data.frame with STUDYID, USUBJID, VISIT, VISITDY, ATPTN, NTSFD variables.
#' @param expected_missing Specifies VISIT data for which missing values are expected in NTSFD (e.g. "Unscheduled").
#' @description Verifies there are no missing values in NTSFD column.
#' @return Data.frame with data rows where NTSFD is missing.
#' @export
#'
#' @examples
#' df1<-
#'   data.frame(
#'     STUDYID = rep("CDISK-01", 6),
#'     USUBJID = paste0("CDISC01.10000", c(1:6)),
#'     NTSFD=c(NA,"0","-7272","-384","0","0"),
#'     VISITDY=c(NA,"1","31","8","1","1"),
#'     VISIT=c("Unscheduled","1","Day 31","WEEK 1","Day 1","DAY 1"),
#'     ATPTN=c("3384","0","-7992","-552",	"0","0"),
#'     ATPT =c("141","0","-333","-23","Pre-Dose","0")
#'   )
#' verify_ntsfd(df1, expected_missing=c("Unscheduled"))
#'
verify_ntsfd<-function(data, expected_missing){
  check<-
    data %>%
    select(STUDYID,USUBJID,VISIT,VISITDY,ATPTN,NTSFD) %>%
    mutate(check=case_when(
      VISITDY %in% 0 ~ FALSE,
      !is.na(NTSFD) ~TRUE,
      is.na(NTSFD) & is.na(VISIT) & is.na(VISITDY) ~ TRUE,
      is.na(NTSFD) & VISIT %in% expected_missing ~ TRUE,
      TRUE~FALSE
    )) %>%
    filter(check %in% FALSE)

  if(!length(check$USUBJID)==0){
    stop("missing values in NTSFD")
  }

}


utils::globalVariables(c(
  "STUDYID", "USUBJID", "NTSFD", "VISITDY", "VISIT", "ATPTN", "ATPT","check"
))
