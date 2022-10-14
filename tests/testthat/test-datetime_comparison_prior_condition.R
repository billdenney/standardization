ae <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ae.csv")
ex <- read.csv("../data-raw/SDTM_example_files/Harvard-directory/ex.csv")

EX <- ex %>%
  mutate(EXSTDTC = format(as.Date(EXSTDTC, format = "%d-%m-%Y"), format = "%Y-%m-%d")) %>%
  date_time_format("EXSTDTC") %>%
  mutate(USUBJID = paste0(substring(USUBJID, 1, 8), "100", substring(USUBJID, 9, 11)))
AE <- ae %>%
  date_time_format("AESTDTC") %>%
  mutate(
    AESTDTC = case_when(
      AESTDTC %in% "2003-05-UNTUN:UN:UN" ~ "2002-04-29TUN:UN:UN",
      AESTDTC %in% "2003-05-13TUN:UN:UN" ~ "2001-04-29TUN:UN:UN",
      TRUE ~ AESTDTC
    )
  )


output_cond1 <- datetime_comparison_prior_condition(EX, AE, "EXSTDTC", "AESTDTC",
                                                    colwithconditions = "AEDECOD",
                                                    filter_val = c("Agitation"),
                                                    condition1 = "PRIOR_Agitation"
)

output_cond2 <- datetime_comparison_prior_condition(EX, AE, "EXSTDTC", "AESTDTC",
                                                    colwithconditions = "AEDECOD",
                                                    filter_val = c("Anxiety"),
                                                    condition1 = "PRIOR_Anxiety"
)


expect_setequal(names(output_cond1), c("STUDYID", "USUBJID", "PRIOR_Agitation", "EXSTDTC", "AESTDTC"))
expect_setequal(names(output_cond2), c("STUDYID", "USUBJID", "PRIOR_Anxiety", "EXSTDTC", "AESTDTC"))
expect_setequal(is.na(output_cond1$EXSTDTC), FALSE)
expect_setequal(is.na(output_cond1$AESTDTC), FALSE)
expect_setequal(is.na(output_cond2$EXSTDTC), FALSE)
expect_setequal(is.na(output_cond2$AESTDTC), FALSE)
expect_setequal(unique(nchar(output_cond1$EXSTDTC)), c(19))
expect_setequal(unique(nchar(output_cond1$AESTDTC)), c(19))
expect_setequal(unique(nchar(output_cond2$EXSTDTC)), c(19))
expect_setequal(unique(nchar(output_cond2$AESTDTC)), c(19))
