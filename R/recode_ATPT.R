#' Numeric re-coding of ATPT variable.
#'
#' @param data A data.frame with ATPT variable.
#' @return Data.frame with the addition of ATPTN variable.
#' @description Converts time point data in ATPT column to numeric format.
#' @export
#' @examples recode_ATPT(data.frame(ATPT = c("Pre-dose", "2 hrs postdose", "drug A (2 hours before)", "34")))
recode_ATPT <- function(data) {
  recode_ret <-
    data %>%
    mutate(
      ATPT_mod = gsub("[(]|[)]", "", ATPT),
      ATPT_mod = gsub("^[[:space:]]+|[[:space:]]+$", "", ATPT_mod),
      ATPT_mod = gsub("^\\s+|\\s+$", "", ATPT_mod),
      ATPT_mod = gsub("TO", "to", ATPT_mod),
      ATPT_mod = gsub("post-dose", "postdose", ATPT_mod, ignore.case=T)
    ) %>%
    mutate(
      ATPTN = case_when(
        str_detect(ATPT_mod, regex("^PRE-DOSE$|^predose$", ignore_case = T))~"0",

        grepl("^\\d",ATPT_mod) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
        grepl("^-\\d",ATPT_mod) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),
        grepl("^\\d$",ATPT_mod) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
        grepl("-\\d$",ATPT_mod) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),
        grepl("^Day\\s(\\d)",ATPT_mod,ignore.case = TRUE) ~str_extract(ATPT_mod,"^\\d+\\.\\d+|\\d+"),
        grepl("^Day\\s(-\\d)",ATPT_mod,ignore.case = TRUE) ~str_extract(ATPT_mod,"^-\\d+\\.\\d+|-\\d+"),


        grepl("(\\d\\s)MINS|(\\d\\s)MIN||(\\d\\s)MINUTES|(\\d\\s)HOUR|(\\d\\s)HOURS|(\\d\\s)HRS",ATPT_mod,ignore.case = TRUE)
        & !grepl("[(]",ATPT_mod)~
          str_extract(ATPT_mod,"^(-\\d+\\.\\d+)[ MINS]|^(-\\d+)[ MINS]|(\\d+)[ MINS]|(\\d+\\.\\d+)[ MINS]"),

        grepl("Cycle", ATPT_mod, ignore.case = TRUE)~ ATPT_mod,
        TRUE ~NA_character_)) %>%

    mutate(ATPTN_ret=case_when(

      grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~-1,
      grepl("post|after", ATPT, ignore.case = TRUE) & grepl("hr|hour|hrs", ATPT, ignore.case = TRUE) ~1,


      grepl("post|after", ATPT, ignore.case = TRUE) & grepl("minutes|MINS|MIN", ATPT, ignore.case = TRUE) ~1/60,#4 mins post

      grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("minutes|MINS|MIN", ATPT, ignore.case = TRUE) ~-1/60,


      !grepl("post|after", ATPT, ignore.case = TRUE) & grepl("minutes|MINS|MIN", ATPT, ignore.case = TRUE) ~1/60,#4mins

      !grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~24,
      grepl("post|after", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~24,
      grepl("prior|before|pre-dose|predose", ATPT, ignore.case = TRUE) & grepl("day", ATPT, ignore.case = TRUE) ~-24,
      TRUE ~ 1
    )) %>%
    mutate(
      ATPTN_type=case_when(
        str_detect(ATPT_mod, regex("^-[0-9]+[[:space:]]+to[[:space:]]+-[0-9]+|^[0-9]+[[:space:]]+to[[:space:]]+[0-9]+|^-[0-9]+[[:space:]]+-[[:space:]]+-[0-9]+|^[0-9]+[[:space:]]+-[[:space:]]+[0-9]+|^-[0-9]+to-[0-9]+|^[0-9]+to[0-9]+|^-[0-9]+--[0-9]+|^[0-9]+-[0-9]+", ignore_case = T))~"range",
        TRUE~NA_character_),
      ATPTN_range1=case_when(
        ATPTN_type=="range" ~gsub("(.+)[[:space:]]+to[[:space:]]+(.+).*|(.+)[[:space:]]+to[[:space:]]+(.+)[[:space:]]+.*", "\\2",
                                  gsub("(\\d)-(\\d)", "\\1 to \\2", ATPT_mod)),
        TRUE~NA_character_),
      ATPTN_range2=case_when(
        ATPTN_type=="range" ~gsub("(.+)[[:space:]]+to[[:space:]]+(.+).*|(.+)[[:space:]]+to[[:space:]]+(.+)[[:space:]]+.*", "\\1",
                                  gsub("(\\d)-(\\d)", "\\1 to \\2", ATPT_mod)),
        TRUE~NA_character_)) %>%
    transform(ATPTN_range2 = as.numeric(gsub("[A-Za-z]+","",ATPTN_range2)),
              ATPTN_range1 = as.numeric(gsub("[A-Za-z]+","",ATPTN_range1))) %>%
    mutate(ATPTN_range=case_when(
      ATPTN_type=="range" & ATPTN_range2>ATPTN_range1~as.character(ATPTN_range2),
      ATPTN_type=="range" & ATPTN_range1>ATPTN_range2~as.character(ATPTN_range1),
      TRUE~ATPTN
    )) %>%
    mutate(
      ATPTN = round(as.numeric(ATPTN) * ATPTN_ret, 3),
      ATPT = na_if(ATPT, ""),
      ATPTN = case_when(
        ATPTN_type == "range" ~ as.numeric(ATPTN_range),
        TRUE ~ ATPTN
      )
    ) %>%
    select(-ATPTN_ret, -ATPT_mod, -ATPTN_type, -ATPTN_range, -ATPTN_range1, -ATPTN_range2)

  recode_ret
}

utils::globalVariables(c("ATPTN", "ATPTN_ret", "ATPT", "ATPT_mod",
                         "ATPTN_range2", "ATPTN_range1", "ATPTN_range", "ATPTN_type"))
