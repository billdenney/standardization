
ae <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ae.csv")
dm <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/dm.csv")
ae_dm<-full_join(ae,dm)
specs <- "../data-raw/specs.xlsx"




test_that("check_merge output works", {
  expect_error(
    check_merge_output(data = ae_dm,
                       study = "S-CDSK-01",
                       STUDYID, USUBJID, DOMAIN,AESEQ),
    NA)})



check_dupl <-
  ae_dm %>%
  ungroup() %>%
  filter(STUDYID %in% "S-CDSK-01") %>%
  group_by(STUDYID, USUBJID, DOMAIN) %>%
  mutate(row = row_number()) %>%
  filter(row > 1)


test_that("check_merge output works", {
  expect_equal(
    check_merge_output(data = ae_dm,
                       study = "S-CDSK-01",
                       STUDYID, USUBJID, DOMAIN),
  check_dupl)})


