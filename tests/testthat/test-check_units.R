lb <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/lb.csv")
specs <- "..../data-raw/specs.xlsx"

options(error=NULL)

test_that("check_units works", {
  lb_study1<-lb %>%
    mutate(PARAM=LBTEST,AVALU=LBSTRESU,ECDOSEU=NA_character_,EXDOSEU=NA_character_) %>%
    select(STUDYID,USUBJID,PARAM,AVALU,EXDOSEU,ECDOSEU)
  lb_study2<-lb %>% mutate(STUDYID="S-CDSK-02") %>%
    mutate(PARAM=LBTEST,AVALU=LBSTRESU,ECDOSEU=NA_character_,EXDOSEU=NA_character_) %>%
    select(STUDYID,USUBJID,PARAM,AVALU,EXDOSEU,ECDOSEU)

  lb_dat_trial1<-lb_study1 %>% dplyr::bind_rows(lb_study2)
  lb_dat_trial2<-lb_study1 %>% dplyr::bind_rows(lb_study2) %>%
    mutate(AVALU=case_when(
      PARAM %in% "Glucose" ~"mmol/L",
      TRUE ~ AVALU
    ))

  units_trial2<-check_units(lb_dat_trial2,STUDY1,STUDY2,exclude_var = "")


  expect_setequal(names(units_trial2),c("PARAM","STUDY1","STUDY2","test"))
  expect_error(check_units(lb_dat_trial1,STUDY1,STUDY2,exclude_var = ""),
               "assertr stopped execution")
  expect_warning(check_units(lb_dat_trial2,STUDY1,STUDY2,exclude_var = ""),
               "The result of evaluating '!test %in% FALSE' has length zero")

})
