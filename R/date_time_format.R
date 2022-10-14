
#' Title
#'
#' @name date_time_format
#' @param data is a data frame with datetime variables
#' @param col is datetime variable
#' @description converts datetime variable to ISO 8601
#' @return datetime variable in ISO 8601 format with UN for missing date and time parts
#' @export
#'
#' @examples date_time_format(data.frame(ADTC=c("2012-04-05T09:10")),"ADTC")
date_time_format <- function(data, col) {

  col <- as.name(col)
  mod_col <- paste0("mod_", col)
  mod_col <- as.name(mod_col)
  date_col <- paste0("date_", col)
  date_col <- as.name(date_col)
  time_col <- paste0("time_", col)
  time_col <- as.name(time_col)
  NA_col <- paste0("NA_", col)
  NA_col <- as.name(NA_col)
  ret_col <- paste0("ret_", col)
  ret_col <- as.name(ret_col)


  pre_dat <- data
  if (is.character(pre_dat$col) %in% "FALSE") {
    ret_dat <- data %>%
      mutate({{ col }} := as.character({{ col }})) %>%
      mutate(length_format = nchar({{ col }}))
  } else {
    NULL
  }

  NA_ret_dat <- ret_dat %>%
    select({{ col }}) %>%
    summarise_all(~ sum(is.na(.)))
  NA_dat <- data %>%
    select({{ col }}) %>%
    summarise_all(~ sum(is.na(.)))

  if (NA_ret_dat %in% NA_dat) {
    NULL
  } else {
    print(paste0("NA's due to class conversion"))
  }

  format_list <- ret_dat %>%
    select({{ col }}) %>%
    mutate(format = nchar({{ col }})) %>%
    select(format) %>%
    distinct() %>%
    pull()
  format_len <- c(0, 4, 7, 8, 9, 10, 16, 19, NA)
  match_format_len <- as.vector(setdiff(format_list, format_len))

  if (!is_empty(match_format_len)) {
    warning(paste("missing format type", match_format_len))
  } else if (is_empty(match_format_len)) {
    NULL
  }


  modified_dat <-
    ret_dat %>%
    mutate(
      "length_format" = nchar({{ col }}),
      {{ ret_col }} :=({{ col }}),
      {{ ret_col }} :=case_when( ({{ret_col}}) %in% ""|({{ret_col}}) %in% NA_character_ ~ as.character(Sys.Date()),
                                 TRUE ~ ({{ret_col}}))) %>%
    #replace missing date parts with UN
    mutate({{date_col}}:= case_when(
      length_format %in% 4  ~ format(as.Date(({{ col }}), format = "%Y"), format = "%Y-UN-UN"), # format type:"2012-UN-UN"
      length_format %in% 7  ~ format(as.Date(paste0(({{ret_col}}), "-01"), format = "%Y-%m-%d"), format = "%Y-%m-UN"),# format type:"2015-04-UN"
      length_format %in% 9  ~ format(as.Date(paste0(sub("---","-",{{ret_col}}), "-01"), format = "%Y-%m-%d"), format = "%Y-%m-UN"),# format type:"2015-04-UN"
      length_format %in% 10 ~ substring(format_ISO8601(as.POSIXct(({{ col }}), format = "%Y-%m-%d")),1,10), # format type:"2014-01-22"
      length_format %in% 16 ~ substring(format_ISO8601(as.POSIXct(({{ col }}), format = "%Y-%m-%d")),1,10) , # format type:"2014-02-12T13:20"
      length_format %in% 19 ~ substring(format_ISO8601(as.POSIXct(({{ col }}), format = "%Y-%m-%d")),1,10),# format type: 2014-01-22T09:35:00
      TRUE ~"")
    )  %>%
    #replace missing time parts with UN
    mutate({{ time_col }} := case_when(
      length_format %in% 4 ~ format(as.POSIXct({{ date_col }}, format = "%Y"), format = "TUN:UN:UN"), # Output type:TUN:UN:UN
      length_format %in% 7 ~   format(as.POSIXct(format(as.Date(paste0(({{ret_col}}), "-01"),format = "%Y-%m-%d"), format = "%Y-%m-%d"),
                                                 format = "%Y-%m-%d"), format = "TUN:UN:UN"), # Output type:TUN:UN:UN
      length_format %in% 9 ~   format(as.POSIXct(format(as.Date(paste0(sub("---","-",{{ret_col}}), "-01"),format = "%Y-%m-%d"), format = "%Y-%m-%d"),
                                                 format = "%Y-%m-%d"), format = "TUN:UN:UN"), # Output type:TUN:UN:UN
      length_format %in% 10 ~ format(as.POSIXct({{ col }}, format = "%Y-%m-%d"), format = "TUN:UN:UN"), # Output type:TUN:UN:UN
      length_format %in% 16 & grepl("T|t", ({{ col }}))~ format(as.POSIXct({{ col }}, format = "%Y-%m-%dT%H:%M"), format = "T%H:%M:UN"), # Output type:T09:35:UN
      length_format %in% 16 & !grepl("T|t", ({{ col }}))~ format(as.POSIXct({{ col }}, format = "%Y-%m-%d %H:%M"), format = "T%H:%M:UN"), # Output type:T09:35:UN

      length_format %in% 19 & grepl("T|t", ({{ col }}))~ format(as.POSIXct({{ col }}, format = "%Y-%m-%dT%H:%M:%S"), format = "T%H:%M:%S"), # Output type:"09:35:00"
      length_format %in% 19 & !grepl("T|t", ({{ col }}))~ format(as.POSIXct({{ col }}, format = "%Y-%m-%d %H:%M:%S"), format = "T%H:%M:%S"), # Output type:"T09:35:00"
      TRUE~"")
    ) %>%
    #combine date and time parts
    mutate({{mod_col}}:=paste0(({{date_col}}), ({{time_col}})),
           {{NA_col}}:=case_when(
             length_format %in% 4 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_4",
             length_format %in% 7 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_7",
             length_format %in% 9 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_9",
             length_format %in% 10 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_10",
             length_format %in% 16 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_16",
             length_format %in% 19 & is.na({{date_col}})|is.na({{time_col}}) & !is.na({{col}})~ "check_19",
             TRUE ~""
           )) %>%

    verify(!({{NA_col}}) %in% c("check_4","check_7","check_9","check_10","check_16","check_19") ) %>%
    mutate({{col}}:=({{ mod_col }})) %>%
    select(-{{ mod_col }}) %>%
    select(-{{ NA_col }},-{{ date_col }},-{{ time_col }},-{{ ret_col }},-{{ ret_col }}, -length_format)


}


utils::globalVariables(c("length_format","match_format_len"))

