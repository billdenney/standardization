
test_that("verify_no_duplicates works", {
  df_dup <- data.frame(
    USUBJID = c(rep("CDISC01.10001", 6)),
    PARAM = c("pk", "pk", "dosing", "dosing", "dosing", "dosing"),
    TSFD = c("2542.9", "2542.9", "", "", "", ""),
    VISIT = c("WEEK 16", "WEEK 16", "Week 3", "Week 4", "Week 5", "Week 6"),
    ATPT = c("", "", "0 hours", "0 hours", "0 hours", "0 hours"),
    ADTC = c("2020-03-27T10:01:UN", "", "", "", "", "")
  ) %>% dplyr::mutate(TSFD = as.numeric(TSFD))
  
  df_wo_dup <- data.frame(
    USUBJID = c(rep("CDISC01.10001", 6)),
    PARAM = c("pk", "pk", "dosing", "dosing", "dosing", "dosing"),
    TSFD = c(2542.9, 25421.9, 3213, 2543, 2456, 3241),
    VISIT = c("WEEK 16", "WEEK 16", "Week 3", "Week 4", "Week 5", "Week 6"),
    ATPT = c("", "", "0 hours", "0 hours", "0 hours", "0 hours"),
    ADTC = c("2020-03-27T10:01:UN", "", "", "", "", "")
  )
  
  verify_no_duplicates(data = df_wo_dup)
  
  mod_df_dup <-
    df_dup %>%
    filter(!PARAM %in% "pk") %>%
    data.frame()
  
  mod_df_wo_dup <-
    df_dup %>%
    filter(!PARAM %in% "pk") %>%
    data.frame()
  
  expect_error(verify_no_duplicates(data = df_dup, expected = mod_df_dup), "The above rows are not expected to be duplicated, please check them")
  expect_error(verify_no_duplicates(data = df_wo_dup, expected = mod_df_wo_dup), NA)
  expect_error(verify_no_duplicates(data = df_dup), "The above rows are not expected to be duplicated, please check them")
  expect_error(verify_no_duplicates(data = df_wo_dup), NA)
  expect_setequal(class(df_dup$PARAM), "character")
  expect_setequal(class(df_dup$TSFD), "numeric")
  expect_setequal(class(df_dup$USUBJID), "character")
  
  expect_setequal(class(df_wo_dup$PARAM), "character")
  expect_setequal(class(df_wo_dup$TSFD), "numeric")
  expect_setequal(class(df_wo_dup$USUBJID), "character")
  
  expect_setequal(names(verify_no_duplicates(data = df_wo_dup, expected = mod_df_wo_dup)), 
                  c("USUBJID", "PARAM", "TSFD", "VISIT", "ATPT", "ADTC"))
})
