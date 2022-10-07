
#' A standardization function
#'
#' @param data is a data frame containing VISIT variable for recoding
#' @param cycle_length is a numeric vector
#' @return VISITDY when VISIT column is present in data frame
#' @export
#'
#' @examples recode_VISIT(data.frame(VISIT=c("Cycle 1 Day 1","cycle 2 day 2","week 10")),cycle_length = 21)
recode_VISIT <- function(data, cycle_length) {

  recode_ret <- data %>%
    mutate(
      PERIOD = case_when(
        str_detect(VISIT, regex("^Cycle ([A-Z0-9]+) Day (-?[0-9]+)$", ignore_case = T))|
          str_detect(VISIT, regex("^Period ([A-Z0-9]+) Day (-?[0-9]+)$", ignore_case = T))
        ~ str_extract(VISIT, regex("^Cycle (-?[0-9]+)|^Period (-?[0-9]+)", ignore_case = TRUE)),
        str_detect(VISIT, regex("^Day (-?[0-9]+)$", ignore_case = T)) ~ "",
        TRUE ~ ""
      ),
      PERIODN = case_when(
        str_detect(VISIT, regex("^Cycle ([A-Z0-9]+)", ignore_case = T))|
          str_detect(VISIT, regex("^Period ([A-Z0-9]+)", ignore_case = T))
        ~ str_extract(VISIT, "\\ (-?[0-9]+)"),
        str_detect(VISIT, regex("^Day (-?[0-9]+)$", ignore_case = T)) ~ "",
        TRUE ~ ""
      ),
      VISITDY = case_when(
        str_detect(VISIT, regex("^Cycle ([A-Z0-9]+) Day (-?[0-9]+)$",ignore_case = T)) ~ str_extract(VISIT, "(-?[0-9]+)$"),
        str_detect(VISIT, regex("^Day (-?[0-9]+)$|^Period ([A-Z0-9]+) Day (-?[0-9]+)$|^Day (-?[0-9]+)(?=: .*)",ignore_case = T)) ~ str_extract(VISIT, "(-?[0-9]+)$|(-?[0-9]+)(?=: .*)"),
        grepl("^[[:digit:]]+$", VISIT)~VISIT,
        str_detect(VISIT, regex("^Week ([A-Z0-9]+)$",ignore_case = T)) ~ str_extract(VISIT, "(-?[0-9]+)$"),
        str_detect(VISIT, regex("^Week ([A-Z0-9]+)",ignore_case = T)) ~ str_extract(VISIT, "(-?[0-9]+)"),
        TRUE ~ ""
      ),
      PERIODN_ret=PERIODN,
      PERIODN_ret=replace_na(as.numeric(PERIODN_ret), 0),
      VISITDY = as.numeric(VISITDY) + ((as.numeric(PERIODN_ret) - 1) * 0),
      VISITDY = case_when(
        grepl("cycle",VISIT,ignore.case = TRUE)~as.numeric(VISITDY) + ((as.numeric(PERIODN_ret) - 1) * cycle_length),
        TRUE ~ VISITDY),
      VISITDY=case_when(
        grepl("week",VISIT,ignore.case = TRUE)~ VISITDY*7+1,
        TRUE ~ VISITDY
      ),
      PERIODN=as.numeric(PERIODN),
      PERIOD=ifelse(grepl("Cycle 77",VISIT, ignore.case = TRUE),NA_character_,PERIOD),
      PERIODN=ifelse(grepl("Cycle 77",VISIT, ignore.case = TRUE),NA_integer_,PERIODN)
    ) %>%
    select(-PERIODN_ret) %>%
    mutate(
      VISIT = na_if(VISIT, ""))

  return(recode_ret)

}

utils::globalVariables(c('PERIODN','PERIODN_ret','VISITDY','VISIT','PERIOD'))
