



test_that("check_units works", {
  

  #dosing and analysis data
  df4 <- data.frame(
    STUDYID = c("CDISK-01", "CDISK-01", "CDISK-01", "CDISK-01",
                "CDISK-02", "CDISK-02","CDISK-02", "CDISK-02"),
    USUBJID = paste0("CDISC01.10000", c(1:8)),
    PARAM = c("CDISK dosing", "CDISK dosing", "Glucose", "Glucose", "CDISK dosing", "CDISK dosing", "Glucose", "Glucose"),
    ECDOSEU = c("mg", "mg", NA, NA, "mg", "mg", NA, NA),
    EXDOSEU = c("mg", "mg", NA, NA, "mg", "mg", NA, NA),
    EXDOSE= c(28, 28, NA, NA, 50, 50, NA, NA),
    AVALN=  c(NA, NA, "3.9 ", "4", NA, NA, "5", "75.6"),
    AVALU=  c(NA, NA, "mmol/L", "mmol/L", NA, NA, "mmol/L", "mg/dL")
  )


   expect_output(check_units(df4, nstudies=2),
                "Checking units: Units are not uniform within a parameter name")



   #dosing
   df5 <- data.frame(
     STUDYID = c("CDISK-01", "CDISK-01","CDISK-02", "CDISK-02"),
     USUBJID = paste0("CDISC01.10000", c(1:4)),
     PARAM = c("CDISK dosing", "CDISK dosing", "CDISK dosing", "CDISK dosing"),
     ECDOSEU = c("mg", "mg",  "mg", "mg"),
     EXDOSEU = c("mg", "mg","mg", "mg"),
     EXDOSE= c(28, 28, 50, 50)

   )


   expect_output(check_units(df5, nstudies=2),
                 "Checking units: All units are uniform within a parameter name")


   #analysis data
   df6 <- data.frame(
     STUDYID = c("CDISK-01", "CDISK-01","CDISK-02", "CDISK-02"),
     USUBJID = paste0("CDISC01.10000", c(1:4)),
     PARAM = c("Glucose", "Glucose",  "Glucose", "Glucose"),
     AVALN=  c(3.9 , 4,  5, 75.6),
     AVALU=  c( "mmol/L", "mmol/L", "mmol/L", "mg/dL")
   )


   
   expect_output(check_units(df6, nstudies=2),
                  "Checking units: Units are not uniform within a parameter name")


   
   df7 <- data.frame(
     STUDYID = c("CDISK-01", "CDISK-01","CDISK-02", "CDISK-02"),
     USUBJID = paste0("CDISC01.10000", c(1:4)),
     PARAM = c("Glucose", "Glucose",  "Glucose", "Glucose"),
     AVALN=  c(3.9 , 4,  5, 75.6),
     AVALU=  c( "mmol/L", NA, "mmol/L", NA)
   )


   
   expect_output(check_units(df7, nstudies=2),
                 "Checking units: Units are not uniform within a parameter name")




   #parameter exclusions
   df9 <- data.frame(
     STUDYID = c("CDISK-01", "CDISK-01", "CDISK-01", "CDISK-01", "CDISK-02", "CDISK-02", "CDISK-02", "CDISK-02"),
     USUBJID = paste0("CDISC01.10000", c(1:8)),
     PARAM = c(
       "CDISK dosing", "CDISK dosing", "NAFLD Activity Score", "NAFLD Activity Score",
       "CDISK dosing", "CDISK dosing", "NAFLD Activity Score", "NAFLD Activity Score"
     ),
     ECDOSEU = c("mg", "mg", NA, NA, "mg", "mg", NA, NA),
     EXDOSEU = c("mg", "mg", NA, NA, "mg", "mg", NA, NA),
     EXDOSE = c(28, 28, NA, NA, 50, 50, NA, NA),
     AVALN = c(NA, NA, 3.9, 4, NA, NA, 5, NA),
     AVALU = c(NA, NA, 'score', 'score', NA, NA, 'score', NA)
   )



expect_output(check_units(df9, nstudies = 2),
              "Checking units: All units are uniform within a parameter name")



})
