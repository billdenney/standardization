#' Rowwise comparison of missing values between two data.frames
#'
#' @name match_missing_values
#' @param data1 first data.frame
#' @param data2 second data.frame
#' @param ... are grouping variables used to arrange the data
#' @description  rowwise comparison of missing values between data.frames to identify accidentally occurring missing values
#' @return for each pair of corresponding source and output columns a check column with true/false values returned within a data.frame
#' @export
#'
#' @examples match_missing_values(source<-data.frame(col1=c(2,3,"NA")), output<-data.frame(col1=c(2,3,"NA")))
match_missing_values<-function(data1,data2,...){

  common_names <- intersect(names(data2),names(data1))

  input_dat<-data1 %>% select(all_of(common_names)) %>%
    group_by() %>% distinct() %>% arrange(...) %>% ungroup()

  input_colnames<-paste0(common_names,"_Input")
  colnames(input_dat)<-input_colnames

  output_dat<- data2 %>% select(all_of(common_names)) %>%
    group_by() %>% distinct() %>% arrange(...) %>% ungroup()

  output_colnames<-paste0(common_names,"_Output")
  colnames(output_dat)<-output_colnames

  data_ret<- output_dat %>% cbind(input_dat) %>% mutate_all(list(~tolower(.)))

  outputnames_common<-common_names


  for (names in outputnames_common) {
    outputcolumn_name <- paste0(names,"_Output")
    input_column_name <- paste0(names, "_Input")
    check_column_name <- paste0(names,"_check")

    data_ret <-
      data_ret %>%
      mutate_all(na_if,"") %>%
      rowwise() %>%
      mutate(!!check_column_name:=case_when(
        grepl("^[[:digit:]]+$", !!sym(input_column_name)) ~ grepl(!!sym(input_column_name),!!sym(outputcolumn_name), fixed=TRUE),
        grepl("cycle|day", !!sym(input_column_name), ignore.case = TRUE)  ~ !!sym(outputcolumn_name)==!!sym(input_column_name),
        grepl('(?:\b| )dosing(?:\b|)',!!sym(outputcolumn_name), ignore.case = TRUE)
        ~grepl(sub('(?:\b| )dosing(?:\b|)', "\\1",!!sym(input_column_name)),
               sub('(?:\b| )dosing(?:\b|)', "\\1",!!sym(outputcolumn_name)), fixed=TRUE),
        str_detect(!!sym(input_column_name),"[0-9]{4}-[0-9]{2}-[0-9]{2}[t]{1}[0-9]{2}:[0-9]{2}")
        ~grepl(sub("t"," ",!!sym(input_column_name)),sub("t"," ",!!sym(outputcolumn_name)), fixed=TRUE),
        TRUE ~grepl(!!sym(input_column_name),!!sym(outputcolumn_name), fixed=TRUE)
      ))

    ifelse(grepl("^NA$",data_ret[,check_column_name]) %in% (grepl("^NA$|Unknown|U$",data_ret[,outputcolumn_name], ignore.case=FALSE) &
                                                              grepl("^NA$",data_ret[,input_column_name], ignore.case=FALSE)),
           list(NULL),
           warning(paste(check_column_name,  "has accidental NA")))


    check_ret<-data_ret %>% select_if(grepl("check",names(.))) %>% colnames()
    ifelse(grepl("FALSE",data_ret[,check_ret]),warning(paste(check_ret,"has FALSE")),
           list(NULL))

  }

  data_ret

}

utils::globalVariables('.')
