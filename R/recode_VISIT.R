
#' Numeric re-coding of VISIT variable
#'
#' @param data A data.frame containing VISIT variable.
#' @param cycle_length A numeric vector specifying length of cycle for conversion of cycle data to days
#' @description Converts VISIT data to numeric format
#' @return Data.frame with the addition of VISITDY variable
#' @export
#' @examples recode_VISIT(data.frame(VISIT=c("Cycle 1 Day 1","cycle 2 day 2","week 10")),cycle_length = 21)
recode_VISIT <- function(data, cycle_length) {

  recode_ret <- data %>%
    mutate(
      VISIT_ret=gsub("^[[:space:]]+|[[:space:]]+$", "", VISIT),
      VISIT_ret=gsub("[/]|[(]|[)]", " ", VISIT_ret),
      PERIOD = case_when(
        str_detect(VISIT_ret, regex("^Cycle ([A-Z0-9]+) Day (-?[0-9]+)$", ignore_case = T))|
          str_detect(VISIT_ret, regex("^Period ([A-Z0-9]+) Day (-?[0-9]+)$", ignore_case = T))
        ~ str_extract(VISIT_ret, regex("^Cycle (-?[0-9]+)|^Period (-?[0-9]+)", ignore_case = TRUE)),
        str_detect(VISIT_ret, regex("^Day (-?[0-9]+)$", ignore_case = T)) ~ "",
        TRUE ~ ""
      ),
      PERIODN = case_when(
        str_detect(VISIT_ret, regex("^Cycle ([A-Z0-9]+)", ignore_case = T))|
          str_detect(VISIT_ret, regex("^Period ([A-Z0-9]+)", ignore_case = T))
        ~ str_extract(VISIT_ret, "\\ (-?[0-9]+)"),
        str_detect(VISIT_ret, regex("^Day (-?[0-9]+)$", ignore_case = T)) ~ "",
        TRUE ~ ""
      ),
      VISITDY = case_when(
        #For cases where string is followed by day data: "Visit 1, to Day 2"
        !grepl("cycle", VISIT_ret,ignore.case=T) & str_detect(VISIT_ret,regex("Days(-?[0-9]+)$|D(-?[0-9]+)$|Day(-?[0-9]+)$|Period ([A-Z0-9]+) Day(-?[0-9]+)$|Day(-?[0-9]+)(?=: .*)",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$|(-?[0-9]+)(?=: .*)"),
        !grepl("cycle", VISIT_ret,ignore.case=T) & str_detect(VISIT_ret,regex("Days[[:space:]]+(-?[0-9]+)$|D[[:space:]]+(-?[0-9]+)$|Day[[:space:]]+(-?[0-9]+)$|Period[[:space:]]+([A-Z0-9]+)[[:space:]]+Day[[:space:]]+(-?[0-9]+)$|Day[[:space:]]+(-?[0-9]+)(?=: .*)",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$|(-?[0-9]+)(?=: .*)"),

        !grepl("cycle", VISIT_ret,ignore.case=T) & str_detect(VISIT_ret,regex("Days[[:space:]]+(-?[0-9]+)[[:space:]]+[A-Za-z]+$|D[[:space:]]+(-?[0-9]+)[[:space:]]+[A-Za-z]+$|Day[[:space:]]+(-?[0-9]+)[[:space:]]+[A-Za-z]+$|Period[[:space:]]+([A-Z0-9]+)[[:space:]]+Day[[:space:]]+(-?[0-9]+)[[:space:]]+[A-Za-z]+$|Day[[:space:]]+(-?[0-9]+)(?=: .*)[[:space:]]+[A-Za-z]+",ignore_case = T))
        ~str_extract(VISIT_ret, "(-?[0-9]+)|(-?[0-9]+)(?=: .*)"),
        !grepl("cycle", VISIT_ret,ignore.case=T) & str_detect(VISIT_ret,regex("Days(-?[0-9]+)[[:space:]]+[A-Za-z]+$|D(-?[0-9]+)[A-Za-z]+$|Day(-?[0-9]+)[A-Za-z]+$|Period([A-Z0-9]+)Day(-?[0-9]+)[A-Za-z]+$|Day(-?[0-9]+)(?=: .*)[A-Za-z]+",ignore_case = T))
        ~str_extract(VISIT_ret, "(-?[0-9]+)|(-?[0-9]+)(?=: .*)"),


        str_detect(VISIT_ret, regex("^Cycle ([A-Z0-9]+) Day (-?[0-9]+)$",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$"),
        #addition: "Days 1", "D 1" (with space)
        str_detect(VISIT_ret, regex("^Days[[:space:]]+(-?[0-9]+)$|^D[[:space:]]+(-?[0-9]+)$|^Day[[:space:]]+(-?[0-9]+)$|^Period[[:space:]]+([A-Z0-9]+)[[:space:]]+Day[[:space:]]+(-?[0-9]+)$|^Day[[:space:]]+(-?[0-9]+)(?=: .*)",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$|(-?[0-9]+)(?=: .*)"),
        grepl("^[[:digit:]]+$", VISIT_ret)~VISIT_ret,
        #addition: "Days1", "D1" (without space)
        str_detect(VISIT_ret, regex("^Days(-?[0-9]+)$|^D(-?[0-9]+)$|^Day(-?[0-9]+)$|^Period ([A-Z0-9]+) Day(-?[0-9]+)$|^Day(-?[0-9]+)(?=: .*)",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$|(-?[0-9]+)(?=: .*)"),
        grepl("^[[:digit:]]+$", VISIT_ret)~VISIT_ret,
        str_detect(VISIT_ret, regex("^Week ([A-Z0-9]+)$|^Week[[:space:]]+([A-Z0-9]+)$|Week([A-Z0-9]+)$|^W[[:space:]]+([A-Z0-9]+)$|^W([A-Z0-9]+)$",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)$"),
        str_detect(VISIT_ret, regex("^Week ([A-Z0-9]+)|^Week[[:space:]]+([A-Z0-9]+)|^Week([A-Z0-9]+)|^W[[:space:]]+([A-Z0-9]+)|^W([A-Z0-9]+)",ignore_case = T)) ~ str_extract(VISIT_ret, "(-?[0-9]+)"),
        TRUE ~ ""
      ),
      PERIODN_ret=PERIODN,
      PERIODN_ret=replace_na(as.numeric(PERIODN_ret), 0),
      VISITDY = as.numeric(VISITDY) + ((as.numeric(PERIODN_ret) - 1) * 0),
      VISITDY = case_when(
        grepl("cycle",VISIT_ret,ignore.case = TRUE)~as.numeric(VISITDY) + ((as.numeric(PERIODN_ret) - 1) * cycle_length),
        TRUE ~ VISITDY),
      VISITDY=case_when(
        grepl("week|W",VISIT_ret,ignore.case = TRUE)~ VISITDY*7+1,
        TRUE ~ VISITDY
      ),
      PERIODN=as.numeric(PERIODN)#,
      #PERIOD=ifelse(grepl("Cycle 77",VISIT, ignore.case = TRUE),NA_character_,PERIOD),
      #PERIODN=ifelse(grepl("Cycle 77",VISIT, ignore.case = TRUE),NA_integer_,PERIODN)
    ) %>%
    select(-PERIODN_ret,-VISIT_ret) #%>%
     # mutate(
     #   VISIT = na_if(VISIT, ""))

  recode_ret

}

utils::globalVariables(c('PERIODN','PERIODN_ret','VISITDY','VISIT','PERIOD','VISIT_ret'))
