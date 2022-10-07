
#' A standardization function
#'
#' @name datetime_comparison_prior_condition
#' @param file_name1
#' @param file_name2
#' @param datetimecol1
#' @param datetimecol2
#' @param datetimecol3
#' @param colwithconditions
#' @param filter_val
#' @param condition1
#' @description compares datetime with earliest dosing datetime to extract prior condition
#' @return
#' @export
#'
#' @examples
datetime_comparison_prior_condition <-
  function(file_name1, file_name2, datetimecol1, datetimecol2, datetimecol3,
           colwithconditions, filter_val, condition1) {
    name1 <- enquo(file_name1)
    name2 <- enquo(file_name2)
    datetimecol1 <- as.name(datetimecol1)
    datetimecol2 <- as.name(datetimecol2)
    colwithconditions <- as.name(colwithconditions)
    conditon1 <- as.name(condition1)

    name1 <- file_name1 %>% select(STUDYID, USUBJID, datetimecol1)
    name2 <- file_name2 %>% select(STUDYID, USUBJID, {{ datetimecol2 }}, {{ datetimecol3 }}, {{ colwithconditions }})

      data_ret <-
      full_join(name1, name2) %>%
      filter({{ colwithconditions }} %in% filter_val) %>%
      select(STUDYID, USUBJID, {{ colwithconditions }}, {{ datetimecol1 }}, {{ datetimecol2 }}) %>%
      group_by(STUDYID, USUBJID, {{ colwithconditions }}) %>%
      arrange({{ datetimecol2 }}, {{ datetimecol1 }}) %>%
      slice(1L) %>%
      mutate(
        STATUS = case_when(
          is.na({{ datetimecol2 }}) | is.na({{ datetimecol1 }}) ~ "None",
          TRUE ~ case_when(
            {{ datetimecol2 }} == "" | {{ datetimecol1 }} == "" ~ "not eligible",
            TRUE ~ case_when(
              {{ datetimecol2 }} >= {{ datetimecol1 }} ~ "after",
              TRUE ~ case_when(
                {{ datetimecol2 }} == {{ datetimecol1 }} ~ "same",
                TRUE ~ "before"
              )
            )
          )
        )
      ) %>%
      filter(STATUS %in% "before") %>%
      ungroup() %>%
      mutate({{ condition1 }} := {{ colwithconditions }}) %>%
      select(STUDYID, USUBJID, {{ condition1 }}, datetimecol1, datetimecol2)


  }
