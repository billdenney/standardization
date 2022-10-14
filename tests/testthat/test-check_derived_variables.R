test_that("PARAMN and STUDYIDN", {
  
  df<-data.frame(
    PARAM=c("Hemoglobin","Glucose"),
    PARAMN=c(1,2),
    STUDYID=c("S-CDSK-01","S-CDSK-02"),
    STUDYIDN=c(1,2)
  )
  
  check_derived_var<-check_derived_variables(df)
  
  expect_setequal(check_derived_var$PARAMN,2)
  expect_setequal(check_derived_var$STUDYIDN,2)
  expect_error(check_derived_variables(df),NA)
  
})