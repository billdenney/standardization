test_that("verify_ntsfd works", {


  df1<-
    data.frame(
      STUDYID=c(rep("S-CDSK-02",20)),
      USUBJID=c(rep("CDISC01.10002",20)),
      NTSFD=c(	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	"0",	"-7272",	"-384",	"0",	"0",	"0",	"-24",	"-72",	"4",	"8"),
      VISITDY=c(NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA, "1",	"31",	"8",	"1",	"1",	"1",	"1",	"1",	"1",	"1"),
      VISIT=c(	"SCREENING",	"PRE-BASELINE",	"SCREENING",	"EARLY TERMINATION/END OF STUDY",	"Unscheduled",	"SCREENING",	"Unscheduled",	"PRE-BASELINE",	"SCREENING",	"EARLY TERMINATION/END OF STUDY",	"1",	"Day 31",	"WEEK 1",	"Day 1","Day 1",	"Day 1",	"Day 1",		"Day 1",		"Day 1",		"Day 1"),
      ATPT =c("-55",	"-25",	"-6",	"150",	"289",	"-33",	"-25",	"-5",	"-4",	"141",	"0",	"-333",	"-23",	"Pre-Dose",	"0",	NA,	"-1",	"-3",	"4 H", "8 H"),
      ATPTN=c(	"-1320",	"-600",	"-144",	"3600",	"6936",	"-792",	"-600",	"-120",	"-96",	"3384",	"0",	"-7992",	"-552",	"0",	"0",	NA,	"-24",	"-72",	"4",	"8")
    )



   expect_error(verify_ntsfd(df1, expected_missing ="Unscheduled"),"missing values in NTSFD")
   expect_error(
     verify_ntsfd(df1, expected_missing =c("Unscheduled","SCREENING",	"PRE-BASELINE",	"SCREENING",	"EARLY TERMINATION/END OF STUDY"),NA)
   )


})
