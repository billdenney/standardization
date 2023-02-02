test_that("pk_variables works ", {
  pk_var <-
    suppressWarnings(pk_variables(data.frame(AVALC=c("<2.30","3.45","BLQ","BQL<2.1",">200.1",">16200",
                                                     "LLQ<1.3","LLOQ<1.5","ulq>1000","uql>2000")),
                                  "AVALC"))

  cols_pk <- colnames(pk_var)
  check_AVALN <- pk_var %>%   select(AVALN) %>%   distinct() %>%  pull()
  check_ALLOQ <- pk_var %>%   select(ALLOQ) %>%   distinct() %>%  pull()
  check_AULOQ <- pk_var %>%   select(AULOQ) %>%   distinct() %>%  pull()

  expect_setequal(check_AVALN %in% c(0.00 ,3.45 ,  NA,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00),TRUE)
  expect_setequal(check_ALLOQ %in% c("2.30" ,""   ,  NA   , ""   ,""   , "2.1", "200.1", "16200", "1.3" , "1.5" ,"1000","2000"),TRUE)
  expect_setequal(check_AULOQ %in% c(""  , "","","",  "200.1", "16200","1000", "2000"),TRUE)
  expect_setequal(c("AVALC", "AVALN" ,"ALLOQ", "AULOQ") %in% cols_pk, TRUE)
  expect_setequal(is.character(pk_var$AVALC), TRUE)
  expect_setequal(is.numeric(pk_var$AVALN), TRUE)

})
