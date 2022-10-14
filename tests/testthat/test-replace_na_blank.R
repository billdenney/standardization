
test_that("replace_na_blank works ", {

  replace_values<-replace_na_blank(data.frame(col1=c(1,"",NA_character_)),c("col1"),"U")

  check_values<-data.frame(org_col1=c(1,"",NA_character_)) %>% cbind(replace_values)

  match<-check_values %>% filter(org_col1 %in% c("",NA))
  list_org_col1<-match %>% select(org_col1) %>% distinct() %>% pull()
  list_col1<-match %>% select(col1) %>% distinct() %>% pull()

  expect_setequal(list_org_col1,c("", NA))
  expect_setequal(list_col1,"U")
})
