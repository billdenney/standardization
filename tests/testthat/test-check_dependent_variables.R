
options(error=NULL)
test_that("dependent variables", {
  
  
  df_1<-data.frame(
    STUDYID=c(rep("S-CDSK-02",6)),
    USUBJID=c(rep("CDISC01.10001",6)),
    NTSFD=c(0,168,0,0,168,168),
    NTSFM=c(0,168,0,0,168,168),
    EVID=c(1,1,0,0,0,0),
    TSFD=c(0,167.3667,-0.7667,-1.0167,167.2667,167.2667),
    TSFM=c(1320.3667,1487.7333,1319.6,1319.35,1487.6333,1487.6333),
    NTAD=c(0,0,0,0,0,0),
    TAD=c(0,0,-0.7667,-1.0167,167.2667,167.2667),
    VISITDY=c(1,8,1,1,8,8),
    ATPTN=c(0,0,"","","",""),
    ADTC=c(
      "2019-07-22T09:41:UN","2019-07-29T09:03:UN","2019-07-22T08:55:UN","2019-07-22T08:40:UN",
      "2019-07-29T08:57:UN","2019-07-29TUN:UN:UN"),
    ADTC_IMPUTED=c("2019-07-22T09:41","2019-07-29T09:03","2019-07-22T08:55","2019-07-22T08:40",
                   "2019-07-29T08:57","2019-07-29")
    )
  
  df_2<-data.frame(
    STUDYID=c(rep("S-CDSK-02",6)),
    USUBJID=c(rep("CDISC01.10002",6)),
    NTSFD=c(0,168,0,0,168,168),
    NTSFM=c(0,168,0,0,168,168),
    EVID=c(1,1,0,0,0,0),
    TSFD=c(0,167.3667,-0.7667,-1.0167,167.2667,167.2667),
    TSFM=c(1320.3667,"",1319.6,1319.35,1487.6333,1487.6333),
    NTAD=c(0,0,0,0,0,0),
    TAD=c(0,0,-0.7667,-1.0167,167.2667,167.2667),
    VISITDY=c(1,8,1,1,8,8),
    ATPTN=c(0,0,"","","",""),
    ADTC=c(
      "2019-07-22T09:41:UN","2019-07-29T09:03:UN","2019-07-22T08:55:UN","2019-07-22T08:40:UN",
      "2019-07-29T08:57:UN","2019-07-29TUN:UN:UN"),
    ADTC_IMPUTED=c("2019-07-22T09:41","2019-07-29T09:03","2019-07-22T08:55","2019-07-22T08:40",
                   "2019-07-29T08:57","2019-07-29")
  )
  
  df_3<-data.frame(
    STUDYID=c(rep("S-CDSK-02",10)),
    USUBJID=c(rep("CDISC01.10002",10)),
    NTSFD=c(0,0,0,0,"","","","","",""),
    NTSFM=c(0,0,0,0,"","","","","",""),
    EVID=c(rep(0,10)),
    TSFD=c(rep("",10)),
    TSFM=c(839.95,839.8833,839.95,840,1.5833,0, 648,1.2667,792,216),
    NTAD=c(rep("",10)),
    TAD=c(rep("",10)),
    VISITDY=c(1,1,1,1,-42,-42,-42,-42,-42,-42),
    ATPTN=c(rep("",10)),
    ADTC=c(
      "2019-12-11TUN:UN:UN","2019-12-11T10:05:UN","2019-12-11T10:09:UN","2019-12-11T10:12:UN",
      "2019-11-06T11:47:UN","2019-11-06TUN:UN:UN","2019-12-03TUN:UN:UN","2019-11-06T11:28:UN",
      "2019-12-09TUN:UN:UN","2019-11-15TUN:UN:UN"),
    ADTC_IMPUTED=c("2019-12-11","2019-12-11T10:05","2019-12-11T10:09","2019-12-11T10:12",
      "2019-11-06T11:47","2019-11-06","2019-12-03","2019-11-06T11:28",
      "2019-12-09","2019-11-15")
  )
  
  check_dependent_var_1<-check_dependent_variables(df_1)
  check_dependent_var_2<-check_dependent_variables(df_2)
  check_dependent_var_3<-check_dependent_variables(df_3)
  expect_error(check_dependent_variables(df_1),NA)
  expect_error(check_dependent_variables(df_2),NA)
  expect_error(check_dependent_variables(df_3),NA)
  expect_setequal(check_dependent_var_1 %>% select(contains("chk")) %>% 
                    distinct() %>% pull() %>% unique(),1)
  expect_setequal(check_dependent_var_2 %>% select(contains("chk")) %>% 
                    distinct() %>% pull() %>% unique(),1)
  expect_setequal(check_dependent_var_3 %>% select(contains("chk")) %>% 
                    distinct() %>% pull() %>% unique(),1)
  
  
})
