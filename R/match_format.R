#' Matches and changes format of variables as per specifications
#'
#' @param data data.frame that serves as a source dataset
#' @param file is a specifications file(list or .xlsx) that informs selection of variables
#' @param coltype is column type specification for baseline, demog, measurement, or dosing variables
#' @description checks if variables are in specified format(numeric or character) and converts as required
#' @return a data.frame with specified variables converted to required format
#' @export
#' @examples
#' df<-data.frame(STUDYID=rep("S-CDSK-01",3),
#' DOMAIN=rep("YEARS",3),
#' USUBJID=paste0("CDISC01.10000",c(1:3)),
#' Age=c(72,66,78)
#' )
#' list1<-data.frame(
#'   `Column Name`=c("AGE","AGEU","RACE","ETHNIC"),
#'   `Column Name Variants`=c("Age","","",""),
#'   Labels=c("Age in years at baseline","Units for AGE","Race","Ethnic group"),
#'   `Column Type`=rep("Demographics",4),
#'   Format=c("Numeric","Character","Character","Character")
#')
#'specification<-list(list1)
#'names(specification)<-"Specification-Source Data"
#'format_df<-match_format(df,file=specification,coltype="Demographics")
match_format <- function(data, file, coltype) {
  d_specification <- file
  source_cols <- d_specification["Specification-Source Data"] %>%
    as.data.frame() %>%
    select(Specification.Source.Data.Column.Name, Specification.Source.Data.Column.Name.Variants, Specification.Source.Data.Column.Type) %>%
    mutate(
      spec_names = Specification.Source.Data.Column.Name,
      spec_namevariants = Specification.Source.Data.Column.Name.Variants,
      col_type = Specification.Source.Data.Column.Type
    ) %>%
    filter(col_type %in% coltype) %>%
    select(Specification.Source.Data.Column.Name) %>%
    pull()

  spec_names <- source_cols
  source_data_names <- c(colnames(data))


  d_spec <- d_specification["Specification-Source Data"]
  spec_df <- as.data.frame(d_spec) %>%
    select(
      Specification.Source.Data.Column.Name, Specification.Source.Data.Column.Name.Variants,
      Specification.Source.Data.Column.Type,
      Specification.Source.Data.Format
    ) %>%
    mutate(
      spec_names = Specification.Source.Data.Column.Name,
      spec_namevariants = Specification.Source.Data.Column.Name.Variants,
      col_type = Specification.Source.Data.Column.Type,
      source_data_format = Specification.Source.Data.Format
    ) %>%
    filter(col_type %in% coltype) %>%
    group_by(spec_names, col_type, spec_namevariants) %>%
    mutate(spec_namevariants = strsplit(spec_namevariants, ", |,| ,")) %>%
    unnest(cols = spec_namevariants) %>%
    ungroup()

  spec_cols_df <-
    spec_df %>%
    mutate(select_col = case_when(
      spec_names %in% source_data_names ~ spec_names,
      spec_namevariants %in% source_data_names ~ spec_namevariants,
      TRUE ~ ""
    )) %>%
    mutate(
      final_name = case_when(
        !(is.na(select_col) | select_col == "") ~ spec_names,
        TRUE ~ ""
      )
    ) %>%
    select(
      spec_names, spec_namevariants, col_type, select_col, final_name, source_data_format # ,column_format
    ) %>%
    mutate(source_data_format = case_when(
      grepl("ADTC|AENDTC", final_name) ~ "Character",
      TRUE ~ source_data_format
    ))

  names_df <-
    spec_cols_df %>%
    select(select_col) %>%
    filter(!(select_col %in% "")) %>%
    distinct() %>%
    pull(select_col)


  format_df <-
    spec_cols_df %>%
    filter(!(select_col %in% "")) %>%
    select(select_col, source_data_format) %>%
    distinct() %>%
    select(source_data_format) %>%
    mutate_all(funs(tolower)) %>%
    pull(source_data_format)



  for (i in seq_along(names_df)) {
    if (!(class(data[, names_df[i]]) %in% format_df[i])) {
      data[, names_df[i]] <- eval(call(paste0("as.", format_df[i]), data[, names_df[i]]))
    } else {
      NULL
    }
  }
  data.frame(data)
}

utils::globalVariables(c(
  "Specification.Source.Data.Column.Name",
  "Specification.Source.Data.Column.Name.Variants",
  "Specification.Source.Data.Column.Type",
  "col_type", "spec_namevariants", "select_col", "final_name",
  "spec_namevariants", "source_data_format",
  "Specification.Source.Data.Format",
  "."
))
