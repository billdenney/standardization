suppressWarnings(library(readxl))
ae <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ae.csv")
dm <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/dm.csv")
ae_dm<-full_join(ae,dm)
specs <- "../data-raw/specs.xlsx"


test_that("check_merge output works", {
  expect_error(check_merge_output(data = ae_dm,
                                     study = c("S-CDSK-01"),
                                     STUDYID, USUBJID, DOMAIN),
  "assertr stopped execution" )})

test_that("check_merge output works", {
  expect_error(
    check_merge_output(data = ae_dm,
                       study = c("S-CDSK-01"),
                       STUDYID, USUBJID, DOMAIN,AESEQ),
    NA)})

