#' A standardization function
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
recode_ATPT <- function(data,string, value) {

  recode_ret <-
   data %>%
    mutate(ATPTN=case_when(
      str_detect(ATPT, regex("^PRE-DOSE$", ignore_case = T))~"0",
      #digits
      grepl("^\\d",ATPT) ~str_extract(ATPT,"^\\d+\\.\\d+|\\d+"),
      grepl("^-\\d",ATPT) ~str_extract(ATPT,"^-\\d+\\.\\d+|-\\d+"),
      grepl("^\\d$",ATPT) ~str_extract(ATPT,"^\\d+\\.\\d+|\\d+"),
      grepl("-\\d$",ATPT) ~str_extract(ATPT,"^-\\d+\\.\\d+|-\\d+"),# day -1; dosing of sgn-liv1 day -1
      grepl("^Day\\s(\\d)",ATPT,ignore.case = TRUE) ~str_extract(ATPT,"^\\d+\\.\\d+|\\d+"),
      grepl("^Day\\s(-\\d)",ATPT,ignore.case = TRUE) ~str_extract(ATPT,"^-\\d+\\.\\d+|-\\d+"),

      #digits following text pattern & No bracket
      grepl("(\\d\\s)MINS|(\\d\\s)MINUTES|(\\d\\s)HOUR|(\\d\\s)HOURS|(\\d\\s)HRS",ATPT,ignore.case = TRUE)
      & !grepl("[(]",ATPT)~
        str_extract(ATPT,"^(-\\d+\\.\\d+)[ MINS]|^(-\\d+)[ MINS]|(\\d+)[ MINS]|(\\d+\\.\\d+)[ MINS]"),

      #digits following bracket and followed by minutes/hours
      grepl("[(](\\d+)[ MINUTES]|[(](\\d+)[ MINS]|[(](\\d+)[ HOURS]|[(](\\d+)[ HRS]",ATPT, ignore.case = TRUE)~substring(str_extract(ATPT,
                                                                                                                                     "[(](\\d+)[ MINUTES]|[(](\\d+)[ MINS]|[(](\\d+)[ MIN]|
            [(](\\d+)[ minutes]|[(](\\d+)[ mins]|[(](\\d+)[ min]|
            [(](\\d+)[ HOURS]|[(](\\d+)[ HOUR]|[(](\\d+)[ HRS]|[(](\\d+\\.\\d+)[ HR]|
            [(](\\d+)[ hours]|[(](\\d+)[ hour]|[(](\\d+)[ hrs]|[(](\\d+)[ hr]"),2,nchar(.)),

      grepl("[(](\\d+\\.\\d+)[ MINUTES]|[(](\\d+\\.\\d+)[ MINS]|[(](\\d+\\.\\d+)[ HOUR]|
            [(](\\d+\\.\\d+)[ HRS]", ATPT, ignore.case = TRUE)~substring(str_extract(ATPT,
                                                                                     "[(](\\d+\\.\\d+)[ MINUTES]|[(](\\d+\\.\\d+)[ MINS]|[(](\\d+\\.\\d+)[ MIN]|
            [(](\\d+\\.\\d+)[ minutes]|[(](\\d+\\.\\d+)[ mins]|[(](\\d+\\.\\d+)[ min]|
            [(](\\d+\\.\\d+)[ HOURS]|[(](\\d+\\.\\d+)[ HOUR]|[(](\\d+\\.\\d+)[ HRS]|[(](\\d+\\.\\d+)[ HR]|
            [(](\\d+\\.\\d+)[ hours]|[(](\\d+\\.\\d+)[ hour]|[(](\\d+\\.\\d+)[ hrs]|[(](\\d+\\.\\d+)[ hr]"),2,nchar(.)),

      #text with cycle and digits
      grepl("Cycle", ATPT, ignore.case = TRUE)~ ATPT,
      TRUE ~NA_character_)) %>%

  mutate(ATPTN_ret=case_when(

    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~"-1",
    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~"1",

    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("minutes|MINS", ATPT, ignore.case = TRUE) ~"1/60",
    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("minutes|MINS", ATPT, ignore.case = TRUE) ~"-1/60",

    grepl("post|after", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~"24",
    grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~"-24",
    TRUE ~ "1"
  )) %>%
    mutate(ATPTN=as.numeric(ATPTN)*as.numeric(ATPTN_ret),
           ATPT = na_if(ATPT, "")) %>%
    select(-ATPTN_ret)

  recode_ret
}


