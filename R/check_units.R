#' Compares and checks for uniformity of units within a parameter name across two studies
#'
#' @param data is a data.frame with STUDYID,USUBJID,PARAM,ECDOSEU,EXDOSEU,AVALU variables
#' @param col1 specifies column name for study1
#' @param col2 specifies column name for for study2
#' @param exclude_var excludes specific parameter names where units do not apply
#' @return data.frame with matching units
#' @export
#' @examples
#' df <- data.frame(
#'   STUDYID = c("CDISK-01", "CDISK-02", "CDISK-01", "CDISK-02", "CDISK-02", "CDISK-01"),
#'   USUBJID = paste0("CDISC01.10000", c(1:6)),
#'   PARAM = c("CDISK dosing", "CDISK dosing", "Hemoglobin", "Glucose", "Hemoglobin", "Glucose"),
#'   ECDOSEU = c("mg", "mg", "", "", "", ""),
#'   EXDOSEU = c("mg", "mg", "", "", "", ""),
#'   AVALU = c("", "", "g/dl", "mmol/L", "g/dl", "mmol/L")
#' )
#' check_units(df,STUDY1,STUDY2,exclude_var = "")
check_units <- function(data, col1, col2, exclude_var) {
  check <- data %>%
    select(STUDYID, USUBJID, PARAM, ECDOSEU, EXDOSEU, AVALU) %>%
    group_by(STUDYID, PARAM) %>%
    summarise(check_AVALU = AVALU, check_EXDOSEU = EXDOSEU, check_ECDOSEU = ECDOSEU) %>%
    replace_na_blank(columns = c("check_AVALU", "check_EXDOSEU", "check_ECDOSEU"), replacement = NA_character_) %>%
    mutate(units = coalesce(check_AVALU, check_EXDOSEU, check_ECDOSEU)) %>%
    ungroup() %>%
    mutate_all(~tolower(.)) %>% 
    select(-check_AVALU, -check_EXDOSEU, -check_ECDOSEU) %>%
    group_by() %>%
    distinct() %>%
    ungroup() %>%
    pivot_wider(names_from = "STUDYID", values_from = "units", values_fn = list) %>%
    unnest(cols=c(2,3)) %>%
    mutate_each(~(replace(., is.na(.), "")))


  cols <- names(check)[!names(check) %in% "PARAM"]


  check_df <- check %>%
    rename(STUDY = cols) %>%
    rowwise() %>%
    unnest(cols = c({{ col1 }}, {{ col2 }})) %>%
    filter(!PARAM %in% exclude_var) %>% 
    mutate(test = case_when(
      {{ col1 }} == {{ col2 }} ~ "TRUE",
      TRUE ~ "FALSE"
    )) %>%
    verify(!duplicated(PARAM)) %>%
    verify(!test %in% FALSE)

  check_df
}

utils::globalVariables(c(
  "STUDYID", "USUBJID", "ECDOSEU", "PARAM", "EXDOSEU", "AVALU", "check_AVALU",
  "check_EXDOSEU", "check_ECDOSEU", ".", "test"
))
