

test_that("date_time_format works ", {
  dtc_df <- data.frame(ADTC = c(
      "2004", "2015-04", "2015---04",
    "2014-02-12", "2014-02-12T13:20", "2014-01-22T09:35:00",
    "", NA,
    "2019-07-22T 09:19","2019-07-22 T09:19","2019-07-22 T 09:19",
    "2019-07-22 T 09:19:20","2019-07-22  09:19:20",
    "2019-07-2209:19:20"
  ))

  format_dtc <- date_time_format(dtc_df, "ADTC")


  check_df <- data.frame(mod_ADTC = c(
    "2004", "2015-04", "2015---04",
    "2014-02-12", "2014-02-12T13:20", "2014-01-22T09:35:00",
    "", NA,
    "2019-07-22T 09:19","2019-07-22 T09:19","2019-07-22 T 09:19",
    "2019-07-22 T 09:19:20","2019-07-22  09:19:20",
    "2019-07-2209:19:20"
  ))

  format_check <- format_dtc %>%
    cbind(check_df) %>%
    mutate(
      check_ADTC_length = nchar(ADTC),
      check_mod_length = nchar(mod_ADTC)
    ) %>%
    rowwise() %>%
    mutate(
      match = case_when(
        check_mod_length %in% 4 & grepl("[0-9]-UN-UNTUN:UN:UN", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 7 & grepl("[0-9]-UNTUN:UN:UN", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 9 & grepl("[0-9]-UNTUN:UN:UN", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 10 & grepl("[0-9]TUN:UN:UN", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 16 & grepl("[0-9]:UN$", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 19 & grepl("[0-9]$", ADTC) ~ paste0(TRUE),
        #addition
        check_mod_length %in% 17 & grepl("[0-9]:UN$", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 18 & grepl("[0-9]:UN$", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 18 & grepl("[0-9]:[0-9]+$", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 20 & grepl("[0-9]$", ADTC) ~ paste0(TRUE),
        check_mod_length %in% 21 & grepl("[0-9]$", ADTC) ~ paste0(TRUE),

        check_mod_length %in% 0 & grepl("", ADTC) ~ paste0(TRUE),
        check_mod_length %in% c("", NA) & grepl("", ADTC) ~ paste0(TRUE),
        TRUE ~ "FALSE"
      )
    )


  dtc_df_formats<-data.frame(ADTC="2014-01-22  09:35:00")


  match_TRUE <- format_check %>%
    select(match) %>%
    distinct() %>%
    pull()
  format_ADTC_lengths <- format_check %>%
    select(check_ADTC_length) %>%
    distinct() %>%
    pull()
  format_mod_lengths <- format_check %>%
    select(check_mod_length) %>%
    distinct() %>%
    pull()

  #expect_warning(date_time_format(dtc_df_formats, "ADTC"), "missing format type 20")
  expect_warning(date_time_format(dtc_df_formats, "ADTC"), NA)
  expect_setequal(match_TRUE, TRUE)
  expect_setequal(format_ADTC_lengths, c(19, 0))
  expect_setequal(format_mod_lengths, c(0 , 4 , 7 , 9, 10 ,16, 17, 18, 19, 20, 21, NA))
  expect_setequal(names(format_check), c("ADTC","mod_ADTC","check_ADTC_length","check_mod_length",
                                         "match"))
  
})
