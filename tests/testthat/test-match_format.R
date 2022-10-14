ae <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ae.csv")
dm <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/dm.csv")
specs <- "../data-raw/specs.xlsx"


test_that("match_format works", {

  format_df<-match_colnames(dm,specs,c("Demographics")) %>% match_format(specs,c("Demographics"))

  expect_setequal(names(format_df),c("AGE","AGEU","RACE","ETHNIC"))
  expect_setequal(class(format_df$AGE),"numeric")
  expect_setequal(class(format_df$AGEU),"character")
  expect_setequal(class(format_df$RACE),"character")
  expect_setequal(class(format_df$ETHNIC),"character")
  expect_error(match_colnames(dm,specs,c("Demographics")) %>% match_format(specs,c("Demographics")),NA)

})
