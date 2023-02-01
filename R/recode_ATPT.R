#' Numeric recoding of ATPT variable
#'
#' @param data a data.frame with ATPT variable for recoding
#' @return data.frame with ATPTN variable
#' @export
#' @examples recode_ATPT(data.frame(ATPT=c("Pre-dose","2 hrs postdose","drug A (2 hours before)","34")))
recode_ATPT <- function(data) {

  recode_ret <-
   data %>%
    mutate(
      ATPT_mod = gsub("[(]|[)]", "", ATPT),
      ATPT_mod = gsub("^\\s+|\\s+$", "", ATPT_mod)
    ) %>%
    mutate(ATPTN=case_when(
      str_detect(ATPT_mod, regex("^-[0-9]+ TO -[0-9]+", ignore_case = T))~ gsub("(.+) TO (.+) HOURS.*", "\\2", ATPT_mod), 
      str_detect(ATPT_mod, regex("^[0-9]+ TO [0-9]+", ignore_case = T))~gsub("(.+) TO (.+) HOURS.*", "\\2", ATPT_mod), 
      str_detect(ATPT_mod, regex("^PRE-DOSE$|^predose$", ignore_case = T))~"0",

      grepl("^\\d",ATPT_mod) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
      grepl("^-\\d",ATPT_mod) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),
      grepl("^\\d$",ATPT_mod) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
      grepl("-\\d$",ATPT_mod) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),# day -1; dosing of sgn-liv1 day -1
      grepl("^Day\\s(\\d)",ATPT_mod,ignore.case = TRUE) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
      grepl("^Day\\s(-\\d)",ATPT_mod,ignore.case = TRUE) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),


      grepl("(\\d\\s)MINS|(\\d\\s)MINUTES|(\\d\\s)HOUR|(\\d\\s)HOURS|(\\d\\s)HRS",ATPT_mod,ignore.case = TRUE)
      & !grepl("[(]",ATPT_mod)~
        str_extract(ATPT_mod,"^(-\\d+\\.\\d+)[ MINS]|^(-\\d+)[ MINS]|(\\d+)[ MINS]|(\\d+\\.\\d+)[ MINS]"),


      grepl("[(](\\d+)[ MINUTES]|[(](\\d+)[ MINS]|[(](\\d+)[ HOURS]|[(](\\d+)[ HRS]",ATPT_mod, ignore.case = TRUE)~substring(str_extract(ATPT_mod,
                                                                                                                                     "[(](\\d+)[ MINUTES]|[(](\\d+)[ MINS]|[(](\\d+)[ MIN]|
            [(](\\d+)[ minutes]|[(](\\d+)[ mins]|[(](\\d+)[ min]|
            [(](\\d+)[ HOURS]|[(](\\d+)[ HOUR]|[(](\\d+)[ HRS]|[(](\\d+\\.\\d+)[ HR]|
            [(](\\d+)[ hours]|[(](\\d+)[ hour]|[(](\\d+)[ hrs]|[(](\\d+)[ hr]"),2,nchar(.)),

      grepl("[(](\\d+\\.\\d+)[ MINUTES]|[(](\\d+\\.\\d+)[ MINS]|[(](\\d+\\.\\d+)[ HOUR]|
            [(](\\d+\\.\\d+)[ HRS]", ATPT_mod, ignore.case = TRUE)~substring(str_extract(ATPT_mod,
                                                                                     "[(](\\d+\\.\\d+)[ MINUTES]|[(](\\d+\\.\\d+)[ MINS]|[(](\\d+\\.\\d+)[ MIN]|
            [(](\\d+\\.\\d+)[ minutes]|[(](\\d+\\.\\d+)[ mins]|[(](\\d+\\.\\d+)[ min]|
            [(](\\d+\\.\\d+)[ HOURS]|[(](\\d+\\.\\d+)[ HOUR]|[(](\\d+\\.\\d+)[ HRS]|[(](\\d+\\.\\d+)[ HR]|
            [(](\\d+\\.\\d+)[ hours]|[(](\\d+\\.\\d+)[ hour]|[(](\\d+\\.\\d+)[ hrs]|[(](\\d+\\.\\d+)[ hr]"),2,nchar(.)),


      grepl("Cycle", ATPT_mod, ignore.case = TRUE)~ ATPT_mod,
      TRUE ~NA_character_)) %>%

  mutate(ATPTN_ret=case_when(

    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~-1,
    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~1,

    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("minutes|MINS", ATPT, ignore.case = TRUE) ~1/60,
    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("minutes|MINS", ATPT, ignore.case = TRUE) ~-1/60,

    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~24,
    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~-24,
    TRUE ~ 1
  )) %>%
    mutate(ATPTN=round(as.numeric(ATPTN)*ATPTN_ret,3),
           ATPT = na_if(ATPT, "")) %>%
    select(-ATPTN_ret,-ATPT_mod)

  recode_ret
}

utils::globalVariables(c('ATPTN','ATPTN_ret','ATPT','ATPT_mod'))
