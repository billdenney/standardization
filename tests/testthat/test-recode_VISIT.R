test_that("recode_VISIT works ", {
  check_VISIT <- recode_VISIT(data.frame(VISIT=c("Cycle 1 Day 1","cycle 2 day 2","week 10",
                                                 "DAY 10","Period 10 day 2",10)),cycle_length = 21)

  cols_measurement <- colnames(check_VISIT)
  expect_setequal(c("VISIT", "VISITDY", "PERIOD", "PERIODN") %in% cols_measurement, TRUE)
  check_VISITDY <- check_VISIT %>%
    filter(is.na(check_VISIT[, "VISITDY"])) %>%
    select(VISIT) %>%
    distinct() %>%
    pull()
  expect_setequal(length(check_VISITDY) %in% 0, TRUE)
  expect_setequal(grepl(sum(is.na(check_VISIT[, "VISIT"])), sum(is.na(check_VISIT[, "PERIOD"]))), TRUE)

  check_PERIOD <- check_VISIT %>%
    filter(is.na(check_VISIT[, "PERIOD"])) %>%
    select(VISIT) %>%
    distinct() %>%
    pull()
  expect_setequal(length(check_PERIOD) %in% 0, TRUE)
  expect_setequal(grepl(sum(is.na(check_VISIT[, "VISIT"])), sum(is.na(check_VISIT[, "PERIODN"]))), FALSE)

  check_PERIODN <- check_VISIT %>%
    filter(is.na(check_VISIT[, "PERIODN"])) %>%
    select(VISIT) %>%
    distinct() %>%
    pull()
  expect_setequal(check_PERIODN %in% c("week 10" ,"DAY 10",  "10" ), TRUE)

  expect_setequal(class(check_VISIT$VISITDY), "numeric")
  expect_setequal(is_empty(check_VISIT$VISITDY), FALSE)
  expect_setequal(class(check_VISIT$PERIOD), "character")
  expect_setequal(is_empty(check_VISIT$PERIOD), FALSE)
  expect_setequal(class(check_VISIT$PERIODN), "numeric")
  expect_setequal(is_empty(check_VISIT$PERIODN), FALSE)

})
