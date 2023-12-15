test_that("verify_alloq works", {
  
  df1<-data.frame(
    STUDYID=paste("CDISK_00",1:6),
    PARAM=c(rep("Hb",6)),
    AVALC=c("BLQ","BLOQ","BLQ","<0.973","< 0.973","<"),
    ALLOQ=c(20.3,21.2,NA_integer_,0.973,0.973,NA_integer_)
  ) 
  
  
  verify_alloq(df1,expected_missing = c("BLQ","<"))
  
  expect_error(verify_alloq(df1,expected_missing = c("BLQ","<")),NA)
  expect_error(verify_alloq(df1,expected_missing = ""),"The above ALLOQ rows are not expected to be missing, please check them")
 
})