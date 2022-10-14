test_that("match_missing_values works", {


  match_values_1<-match_missing_values(input<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06",
                                                                 "XYZ","2015-04-06T09:10",
                                                                 "Cycle 1")),
                                       output<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06T",
                                                                 "XYZ dosing","2015-04-06 09:10",
                                                                 "Cycle 1")))


  match_values_2<-suppressWarnings(match_missing_values(input<-data.frame(col1=c(2,3,"NA",1,"<3.5","2015-04-06",
                                                                                  "XYZ","2015-04-06T09:10",
                                                                                  "Cycle 1")),
                                                        output<-data.frame(col1=c(2,3,"NA",NA,"<3.5","2015-04-06T",
                                                                                  "XYZ dosing","2015-04-06 09:10",
                                                                                  "Cycle 1"))))

  colnames_check_1<-match_values_1 %>% select(contains("check")) %>% colnames()
  list_check_1<-match_values_1 %>%select(all_of(colnames_check_1)) %>% distinct() %>% pull()

  colnames_check_2<-match_values_2 %>% select(contains("check")) %>% colnames()
  list_check_2<-match_values_2 %>%select(all_of(colnames_check_2)) %>% distinct() %>% pull()

  expect_setequal(list_check_1 %in% c(TRUE,NA), TRUE)
  expect_setequal(list_check_2 %in% c(TRUE,NA,FALSE), TRUE)
  expect_warning(match_missing_values(input<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06",
                                                                "XYZ","2015-04-06T09:10",
                                                                "Cycle 1")),
                                      output<-data.frame(col1=c(2,3,"NA","","<3.5","2015-04-06T",
                                                                "XYZ dosing","2015-04-06 09:10",
                                                                "Cycle 1"))),NA)
  expect_warning(match_missing_values(input<-data.frame(col1=c(2,3,"NA",1,"<3.5","2015-04-06",
                                                                "XYZ","2015-04-06T09:10",
                                                                "Cycle 1")),
                                      output<-data.frame(col1=c(2,3,"NA",NA,"<3.5","2015-04-06T",
                                                                "XYZ dosing","2015-04-06 09:10",
                                                                "Cycle 1"))),"col1_check has FALSE")

})


