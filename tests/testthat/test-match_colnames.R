ae <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ae.csv")
dm <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/dm.csv")
specs <- "../data-raw/specs.xlsx"


test_that("match_colnames works", {

df<-match_colnames(dm,specs,"Demographics")

expect_setequal(names(df),c("AGE","AGEU","RACE","ETHNIC"))
expect_error(match_colnames(dm,specs,"Demographics"),NA)

})
