test_that("recode_ATPT works ", {
  check_ATPT <-
    recode_ATPT(data.frame(ATPT=c("Pre-dose","2 hrs postdose","drug A (2 hours before dosing)","34",
                                  "2 hours prior dose","4 mins before dose","1 day post dose",
                                  " -18 TO -14 HOURS  ",
                                  "-18 TO -14 HOURS","18 TO 14 HOURS",
                                  "24-48HPOST-DOSE","24-48H POST-DOSE",
                                  "24-48HOURS POST-DOSE","24-48HOURS POST-DOSE","96-48 HOURS POST-DOSE")))

  cols_measurement <- colnames(check_ATPT)
  check_values <- check_ATPT %>%   select(ATPTN) %>%   distinct() %>%  pull()

  expect_setequal(check_values %in% c( 0.000,   2.000,  -2.000,  34.000,  -0.067, 24.000, -14.000, -14.000,  
                                       18.000,  48.000,  48.000,
                                       48.000,48.000,96.000),TRUE)
  expect_setequal(c("ATPT", "ATPTN") %in% cols_measurement, TRUE)
  expect_setequal(is.numeric(check_ATPT$ATPTN), TRUE)
})
