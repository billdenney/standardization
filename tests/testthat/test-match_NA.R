test_that("match_NA works", {


  source_1<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06",
                                      "XYZ","2015-04-06T09:10",
                                      "Cycle 1","."," "))
  output_1<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06T",
                              "XYZ dosing","2015-04-06 09:10",
                              "Cycle 1",".","Unknown assessment time"))
  match_NA_1<-match_NA(source_1,output_1)


  source_2<-data.frame(USUBJID=1:12,ETHNIC=c(2,3,"<3.5","2015-04-06",
                                                "XYZ","2015-04-06T09:10",
                                                "Cycle 1"," "," "," ","NA",1))
  output_2<-data.frame(USUBJID=1:12,ETHNIC=c(2,3,"<3.5","2015-04-06T",
                                                "XYZ dosing","2015-04-06 09:10",
                                                "Cycle 1","U","Unknown","UNKNOWN","NA","NA"))
  match_NA_2<-suppressWarnings(match_NA(source_2,output_2))

  check_values_1<-match_NA_1 %>% mutate(check=col1_Output %in% col1_Input) %>%
    select(check) %>% distinct() %>% pull()
  match_values_source_1<-sum(is.na(source_1))
  match_values_output_1<-sum(is.na(output_1))

  match_values_source_2<-sum(is.na(source_2$USUBJID))
  match_values_output_2<-sum(is.na(output_2$USUBJID))
  match_values_source_2_ETHNIC<-sum(grepl(" ",source_2$ETHNIC ))
  match_values_output_2_ETHNIC<-sum(grepl("NA|Unknown|U",output_2$ETHNIC,ignore.case = TRUE ))
  check_values_2<-match_NA_2 %>%  mutate(check=ETHNIC_Output %in% ETHNIC_Input) %>%
    select(check) %>% distinct() %>% pull()

  expect_setequal(match_values_source_1 %in% match_values_output_1, TRUE)
  expect_setequal(match_values_source_2 %in% match_values_output_2,TRUE)
  expect_setequal(match_values_source_2_ETHNIC %in% match_values_output_2_ETHNIC,FALSE)
  expect_setequal(names(match_NA_1) %in% c("col1_Output" ,"col1_Input" ), TRUE)
  expect_setequal(names(match_NA_2) %in% c("USUBJID_Output" ,"USUBJID_Input",
                                           "ETHNIC_Output" ,"ETHNIC_Input" ), TRUE)
  expect_setequal(check_values_1 %in% TRUE, TRUE)
  expect_setequal(check_values_2 %in% FALSE, TRUE)
  expect_warning(match_NA(source_1,output_1),NA)
  expect_warning(match_NA(source_2,output_2),"ETHNIC_Output & ETHNIC_Input does not match")

})
