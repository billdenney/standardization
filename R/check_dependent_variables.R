#' A standardization function
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' check_dependent_variables()
check_dependent_variables<-function(data){

  print("checking NTSFD")
  check_var<-data %>%
    mutate(chk_NTSFD=case_when(
      !NTSFD %in% NA_integer_~1,
      is.na(VISITDY) & is.na(ATPTN) & is.na(NTSFD)  ~1,
      is.na(VISITDY) & is.na(NTSFD) & !is.na(ATPTN) ~1,
      is.na(ATPTN) & is.na(NTSFD) & !is.na(VISITDY) & VISITDY>0 ~0,
      VISITDY < 0 & is.na(NTSFD) ~1,
      TRUE ~ 0
    )) %>%
    verify(!chk_NTSFD %in% "0")

  print("checking NTSFM")
  check_var<-check_var %>%
    mutate(chk_NTSFM=case_when(
      !is.na(NTSFM)~1,
      !NTSFM %in% NA_integer_~1,
      is.na(VISITDY) & is.na(ATPTN) & is.na(NTSFM)  ~1,
      is.na(VISITDY) & is.na(NTSFM) & !is.na(ATPTN)~1,
      is.na(ATPTN) & is.na(NTSFM) & !is.na(VISITDY)& VISITDY>0 ~0,
      VISITDY < 0 & is.na(NTSFM) ~1,
      TRUE ~ 0
    )) %>%
    verify(!chk_NTSFM %in% "0")

  print("checking TSFM")
  check_var<-check_var %>%
    mutate(chk_TSFM=case_when(
      !is.na(TSFM) ~1,
      is.na(ADTC) & is.na(TSFM)~1,
      !is.na(ADTC) & is.na(TSFM) & is.na(ADTC_IMPUTED) ~1,
      TRUE ~0
    )) %>%
    verify(!chk_TSFM %in% "0")

  print("checking TSFD")
  check_var<-check_var %>%
    group_by(STUDYID, USUBJID) %>%
    mutate(chk_TSFD=case_when(
      !is.na(TSFD) ~1,
      is.na(TSFM) & is.na(TSFD)~1,
      length(TSFM[EVID %in% 1] %in% NA_integer_) %in% 0 & is.na(TSFD)  ~1,
      suppressWarnings(!length(TSFM[EVID %in% 1] %in% NA_integer_) %in% 0 & min(TSFM[EVID %in% 1]) %in% NA_integer_ & is.na(TSFD)) ~1,
      TRUE~0)) %>%
    ungroup() %>%
    verify(!chk_TSFD %in% "0")

  print("checking NTAD")
  check_var<-check_var %>%
    group_by(STUDYID,USUBJID) %>%
    mutate(chk_NTAD=case_when(
      !is.na(NTAD) ~1,
      is.na(NTSFM) & is.na(NTAD)~1,
      min_no_na(NTSFM[EVID %in% 1]) %in% NA_integer_ & is.na(NTAD)~1,
      TRUE~0
    )) %>%
    ungroup() %>%
    verify(!chk_NTAD %in% "0")

  print("checking TAD")
  check_var<-check_var %>%
    group_by(STUDYID,USUBJID) %>%
    mutate(chk_TAD=case_when(
      !is.na(TAD) ~1,
      is.na(TSFM) & is.na(TAD)~1,
      suppressWarnings( min_no_na(TSFM[EVID %in% 1]) %in% NA_integer_  & is.na(TAD)) ~1,
      TRUE~0
    )) %>%
    ungroup() %>%
    verify(!chk_TAD %in% "0") %>%
    select(ADTC,VISITDY, ATPTN, TSFM,NTSFM,TSFD,NTSFD,TAD,NTAD,
           chk_NTSFD,chk_NTSFM,chk_TSFM,chk_TSFD,chk_NTAD,chk_TAD)

  return(check_var)
}
