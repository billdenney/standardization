#' Compares sum of missing values between corresponding output and input dataset columns.
#'
#' @param input input dataset
#' @param output Output dataset
#' @return Draws a comparison of total number of missing values between two datasets and returns an error message for unequal matches.
#' @export
#' @examples match_NA(input<-data.frame(col1=c(2,3,"NA","")), output<-data.frame(col1=c(2,3,"NA","")))
match_NA<-function(input, output){

  common_names <- intersect(names(output),names(input))

  input_dat<-input %>% select(all_of(common_names)) %>% group_by() %>% distinct() %>% arrange_all() %>% ungroup()

  input_colnames<-paste0(common_names,"_Input")
  colnames(input_dat)<-input_colnames

  output_dat<- output %>% select(all_of(common_names)) %>% group_by() %>% distinct() %>% arrange_all() %>% ungroup()

  output_colnames<-paste0(common_names,"_Output")
  colnames(output_dat)<-output_colnames

  data_ret<- output_dat %>% cbind(input_dat)


  if(any(grepl("RACE|ETHNIC", output_colnames, ignore.case = TRUE)) %in% TRUE &
     any(data_ret[,grepl("RACE|ETHNIC", colnames(data_ret))] %>%
         summarise(across(everything(), ~sum(is.na(.)|grepl("^ *$|^[.]$",(.))))))>0 %in% TRUE
  ){
    NA_check_common<-data_ret %>% select(!contains("ETHNIC")) %>%
      summarise(across(everything(), ~sum(is.na(.)|grepl("^ *$|^[.]$|Unknown assessment time",(.)))))
    NA_check_add<-data_ret %>% select(contains("ETHNIC")) %>%
      summarise(across(everything(), ~sum(is.na(.)|grepl("^.$|^ *$|^[.]$|U|Unknown|UNKNOWN",(.)))))
    NA_check_df<- NA_check_common %>% cbind(NA_check_add)
    NA_check_df
  }else {
    NA_check_df_common<-data_ret %>%
      summarise(across(everything(),~sum(is.na(.)|grepl("^ *$|^[.]$|Unknown assessment time",(.)))))
    NA_check_df<- NA_check_df_common
    NA_check_df
  }



  for (i in seq_along(output_colnames)){

    if(!any(NA_check_df[,output_colnames[i]] %in% NA_check_df[,input_colnames[i]])){
      warning(paste0(output_colnames[i] ," & ",input_colnames[i]," does not match"))
      }else{
        list(NULL)}


  }

  NA_check_df

}
