#' checks time variables
#'
#' @param data is a data.frame with variables NTSFD,TSFD,NTSFM,TSFM,NTAD,TAD
#' @description verifies variables for unwarranted missing values
#' @return returns data.frame with verification variables:chk_NTSFD, chk_TSFD, chk_TSFM, chk_NTSFM, chk_NTAD, chk_TAD
#' @export
#' @examples
#'df_1<-data.frame(
#'  STUDYID=c(rep("S-CDSK-02",6)),
#'  USUBJID=c(rep("CDISC01.10001",6)),
#'  NTSFD=c(0,168,0,0,168,168),
#'  NTSFM=c(0,168,0,0,168,168),
#'  EVID=c(1,1,0,0,0,0),
#'  TSFD=c(0,167.3667,-0.7667,-1.0167,167.2667,167.2667),
#'  TSFM=c(1320.3667,1487.7333,1319.6,1319.35,1487.6333,1487.6333),
#'  NTAD=c(0,0,0,0,0,0),
#'  TAD=c(0,0,-0.7667,-1.0167,167.2667,167.2667),
#'  VISITDY=c(1,8,1,1,8,8),
#'  ATPTN=c(0,0,"","","",""),
#'  ADTC=c(
#'    "2019-07-22T09:41:UN","2019-07-29T09:03:UN","2019-07-22T08:55:UN","2019-07-22T08:40:UN",
#'    "2019-07-29T08:57:UN","2019-07-29TUN:UN:UN"),
#'  ADTC_IMPUTED=c("2019-07-22T09:41","2019-07-29T09:03","2019-07-22T08:55","2019-07-22T08:40",
#'                 "2019-07-29T08:57","2019-07-29")
#')
#' check_time_variables(df_1)
check_time_variables<-function(data){

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
    assertr::verify(!chk_NTSFD %in% "0")

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
     assertr::verify(!chk_NTSFM %in% "0")

  print("checking TSFM")
  check_var<-check_var %>%
    mutate(chk_TSFM=case_when(
      !is.na(TSFM) ~1,
      is.na(ADTC) & is.na(TSFM)~1,
      !is.na(ADTC) & is.na(TSFM) & is.na(ADTC_IMPUTED) ~1,
      TRUE ~0
    )) %>%
     assertr::verify(!chk_TSFM %in% "0")

  print("checking TSFD")
  check_var<-check_var %>%
    dplyr::group_by(STUDYID, USUBJID) %>%
    dplyr::mutate(chk_TSFD=case_when(
      !is.na(TSFD) ~1,
      is.na(TSFM) & is.na(TSFD)~1,
      length(TSFM[EVID %in% 1] %in% NA_integer_) %in% 0 & is.na(TSFD)  ~1,
      suppressWarnings(!length(TSFM[EVID %in% 1] %in% NA_integer_) %in% 0 & min(TSFM[EVID %in% 1]) %in% NA_integer_ & is.na(TSFD)) ~1,
      TRUE~0)) %>%
    dplyr::ungroup() %>%
     assertr::verify(!chk_TSFD %in% "0")

  print("checking NTAD")
  check_var<-check_var %>%
    dplyr::group_by(STUDYID,USUBJID) %>%
    dplyr::mutate(chk_NTAD=case_when(
      !is.na(NTAD) ~1,
      is.na(NTSFM) & is.na(NTAD)~1,
      suppressWarnings(min(NTSFM[EVID %in% 1]) %in% NA_integer_) & is.na(NTAD)~1,
      TRUE~0
    )) %>%
    dplyr::ungroup() %>%
     assertr::verify(!chk_NTAD %in% "0")

  print("checking TAD")
  check_var<-check_var %>%
    dplyr::group_by(STUDYID,USUBJID) %>%
    dplyr::mutate(chk_TAD=case_when(
      !is.na(TAD) ~1,
      is.na(TSFM) & is.na(TAD)~1,
      suppressWarnings(min(TSFM[EVID %in% 1]) %in% NA_integer_) & is.na(TAD) ~1,
      TRUE~0
    )) %>%
    dplyr::ungroup() %>%
     assertr::verify(!chk_TAD %in% "0") %>%
    dplyr::select(ADTC,VISITDY, ATPTN, TSFM,NTSFM,TSFD,NTSFD,TAD,NTAD,
           chk_NTSFD,chk_NTSFM,chk_TSFM,chk_TSFD,chk_NTAD,chk_TAD)

 check_var


}

utils::globalVariables(c("chk_NTSFD","chk_TSFD","chk_NTSFM","chk_TSFM","chk_NTAD","chk_TAD",
                         "NTSFD","TSFD","NTSFM","TSFM","NTAD","TAD","STUDYID","USUBJID",
                         "ADTC","VISITDY","ATPTN"))
