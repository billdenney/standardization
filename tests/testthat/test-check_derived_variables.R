
specs<-import_list("../data-raw/specs.xlsx")
test_that("check_derived_variables works", {

  df<-data.frame(
    PARAM=c("Hemoglobin","Glucose"),
    PARAMN=c(1,2)
  )

  expect_error(check_derived_variables(df,specs),NA)


  df1<-data.frame(
    STUDYID=c("CDISK-01","CDISK-01","CDISK-02","CDISK-02","CDISK-02"),
    PARAM=c("Hemoglobin","Glucose","Hemoglobin","Glucose","glucose"),
    PARAMN=c(1,2,1,2,3),
    STUDYIDN=c(1,2,1,2,2)
  )

  expect_error(check_derived_variables(df1,specs),"assertr stopped execution")

})
