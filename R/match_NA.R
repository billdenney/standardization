#' A standardization function
#'
#' @param data1
#' @param data2
#' @param output_var
#' @param input_var
#'
#' @return
#' @export
#'
#' @examples
match_NA<-function(data1, data2){

  common_names <- intersect(names(data2),names(data1))

  input_dat<-data1 %>% select(all_of(common_names)) %>% group_by() %>% distinct() %>% arrange_all() %>% ungroup()

  input_colnames<-paste0(common_names,"_Input")
  colnames(input_dat)<-input_colnames

  output_dat<- data2 %>% select(all_of(common_names)) %>% group_by() %>% distinct() %>% arrange_all() %>% ungroup()

  output_colnames<-paste0(common_names,"_Output")
  colnames(output_dat)<-output_colnames

  data_ret<- output_dat %>% cbind(input_dat)


  if(any(grepl("RACE|ETHNIC", output_colnames, ignore.case = TRUE)) %in% TRUE &
     any(data_ret[,grepl("RACE|ETHNIC", colnames(data_ret))] %>%
         summarise(across(everything(), ~sum(is.na(.)|grepl("^ *$|^[.]$",(.))))))>0 %in% TRUE
  ){
    NA_check_common<-data_ret %>% select(!contains(c("ETHNIC"))) %>%
      summarise(across(everything(), ~sum(is.na(.)|grepl("^ *$|^[.]$|Unknown assessment time",(.)))))
    NA_check_add<-data_ret %>% select(contains(c("ETHNIC"))) %>%
      summarise(across(everything(), ~sum(is.na(.)|grepl("^.$|^ *$|^[.]$|U|Unknown|UNKNOWN",(.)))))
    NA_check_df<- NA_check_common %>% cbind(NA_check_add)
    NA_check_df
  }else {
    NA_check_df_common<-data_ret %>%
      summarise(across(everything(),~sum(is.na(.)|grepl("^ *$|^[.]$|Unknown assessment time",(.)))))
    NA_check_df<- NA_check_df_common
    NA_check_df
  }



  for (i in 1: length(output_colnames)){
    ifelse(!any(NA_check_df[,output_colnames[i]] %in% NA_check_df[,input_colnames[i]]),
           print(paste0(output_colnames[i] ," & ",input_colnames[i]," does not match")),
           list(NULL))
  }

  NA_check_df

}
