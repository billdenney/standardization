#' Checks for uniformity of units within a parameter name across multiple studies.
#'
#' @param data Data.frame with STUDYID,USUBJID,PARAM,EXDOSEU,AVALU variables.
#' @param nstudies Specifies number of studies.
#' @return Data.frame with summarized parameters and their respective units. Also returns a message for parameter names with multiple or missing units.
#' @export
#' @examples
#' vignette(package="standardization",all=TRUE)
#' df <- data.frame(
#'   STUDYID = c("CDISK-01", "CDISK-01", "CDISK-01", "CDISK-01",
#'               "CDISK-02", "CDISK-02","CDISK-02", "CDISK-02"),
#'   USUBJID = paste0("CDISC01.10000",1:8),
#'   PARAM = c("CDISK dosing", "CDISK dosing", "Glucose", "Glucose",
#'             "CDISK dosing", "CDISK dosing", "Glucose", "Glucose"),
#'   EXDOSEU = c("mg", "mg", NA, NA,
#'               "mg", "mg", NA, NA),
#'   EXDOSE=c(28, 28, NA, NA,
#'            50, 50, NA, NA),
#'   AVALN=c(NA, NA, 3.9 , 4,
#'           NA, NA, 5, NA),
#'   AVALU = c(NA, NA, "mmol/L", "mmol/L",
#'             NA, NA, "mmol/L", NA)
#' )
#' check_units(df, nstudies=2)
check_units <- function(data, nstudies) {

  ret_cols<-c("STUDYID", "USUBJID", "PARAM", "EXDOSE", "EXDOSEU", "AVALN", "AVALU")
  check_cols<-c("check_AVALU", "check_EXDOSEU")

  ret <- data %>%
    select(tidyselect::any_of(ret_cols))


  if(any(grepl("AVALU",names(ret))) & any(grepl("EXDOSEU",names(ret)))){

    ret_dat<-ret %>%
      group_by(STUDYID, PARAM, AVALN, EXDOSE, AVALU, EXDOSEU) %>%
      mutate(check_analysis=is.na(AVALN),
             check_dosing=is.na(EXDOSE)
      ) %>%
      mutate(AVALU=case_when(
        check_analysis %in% FALSE & is.na(AVALU) ~"missing",
        TRUE~AVALU
      )) %>%
      mutate(EXDOSEU=case_when(
        check_dosing %in% FALSE& is.na(EXDOSEU)~"missing",
        TRUE~EXDOSEU
      )) %>%
      reframe(check_AVALU = AVALU,check_EXDOSEU = EXDOSEU) %>%
      replace_na_blank(columns = c("check_AVALU", "check_EXDOSEU"), replacement = NA_character_) %>%
      mutate(units = coalesce(check_AVALU, check_EXDOSEU)) %>%
      ungroup() %>%
      select(-AVALN, -EXDOSE, -AVALU, -EXDOSEU)



  } else if(any(grepl("AVALU",names(ret))) & any(!grepl("EXDOSEU",names(ret)))){

    ret_dat<-ret %>%
      group_by(STUDYID, PARAM, AVALN, AVALU) %>%
      mutate(check_analysis=is.na(AVALN)
      ) %>%
      mutate(AVALU=case_when(
        check_analysis %in% FALSE & is.na(AVALU) ~"missing",
        TRUE~AVALU
      )) %>%
      reframe(check_AVALU = AVALU) %>%
      replace_na_blank(columns = "check_AVALU", replacement = NA_character_) %>%
      mutate(units = coalesce(check_AVALU)) %>%
      ungroup() %>%
      select(-AVALN, -AVALU)

  } else{

    ret_dat<-ret %>%
      group_by(STUDYID, PARAM, EXDOSE, EXDOSEU) %>%
      mutate(check_dosing=is.na(EXDOSE)) %>%
      mutate(EXDOSEU=case_when(
        check_dosing %in% FALSE& is.na(EXDOSEU)~"missing",
        TRUE~EXDOSEU
      )) %>%
      reframe(check_EXDOSEU = EXDOSEU) %>%
      replace_na_blank(columns = "check_EXDOSEU", replacement = NA_character_) %>%
      mutate(units = coalesce(check_EXDOSEU)) %>%
      ungroup() %>%
      select(-EXDOSE, -EXDOSEU)

  }

  check<-ret_dat %>%
    ungroup() %>%
    mutate_all(~tolower(.)) %>%
    select(-any_of(check_cols)) %>%
    group_by() %>%
    distinct() %>%
    ungroup() %>%
    pivot_wider(names_from = "STUDYID", values_from = "units", values_fn = list) %>%
    unnest(cols=names(.))

  cols <- names(check)[!names(check) %in% "PARAM"]
  columns<-paste0("STUDY",1:nstudies)
  pivot_cols<-c("PARAM","units")

  check_df <- check %>%
    rename(STUDY = cols) %>%
    unnest(cols=columns) %>%
    ungroup() %>%
    tidyr::pivot_longer(values_to="units",cols=setdiff(names(.),"PARAM")) %>%
    select(all_of(pivot_cols)) %>%
    unique() %>%
    filter(!units %in% NA)  #For data rows with missing AVALN



  if (any(check_df$units %in% "missing" | duplicated(check_df$PARAM))) {
    print("Checking units: Units are not uniform within a parameter name")
  } else{
    print("Checking units: All units are uniform within a parameter name")
  }

  print(check_df ,n=1000)

}

utils::globalVariables(c(
  "STUDYID", "USUBJID", "PARAM", "EXDOSEU", "AVALU", "check_AVALU",
  "check_EXDOSEU","AVALN","EXDOSE"
))
