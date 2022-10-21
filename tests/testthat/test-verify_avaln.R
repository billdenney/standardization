test_that("verify_avaln works", {
  

df2<-data.frame(
  STUDYID=paste("CDISK_00",1:7),
  PARAM=c(rep("Hb",3),rep("Glucose",4)),
  AVALC=c(20.3,21.2,22.5,60.5,55.7,"BLQ","Sample collected"),
  AVALN=c(20.3,21.2,22.5,60.5,55.7,NA_integer_,NA_integer_)
) 


verify_avaln(df2,expected_missing = c("BLQ","Sample collected"))

expect_error(verify_avaln(df2,expected_missing = c("BLQ","Sample collected")),NA)
expect_error(verify_avaln(df2,expected_missing = ""),"The above AVALN rows are not expected to be missing, please check them")
expect_error(verify_avaln(df2,expected_missing = "BLQ"),"The above AVALN rows are not expected to be missing, please check them")

})