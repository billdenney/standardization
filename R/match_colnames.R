
#' Selects SDTM column names or their name variants and renames them to preferred column name in the output
#'
#' @name match_colnames
#' @param data A data.frame that serves as a source dataset
#' @param file Specifications can be provided as list object in R or .xlsx file path, which informs selection of variables
#' @param coltype Column type specification: 'baseline', 'demog', 'measurement', or 'dosing' variables
#' @description Selects SDTM column names or their name variants and renames them to preferred column name in the output.
#' @return A data.frame with specified variables selected and renamed to preferred name
#' @export
#' @examples
#' df<-data.frame(STUDYID=rep("S-CDSK-01",3),DOMAIN=rep("YEARS",3),Age=c(72,66,78))
#' list1<-data.frame(`Column Name`="AGE",
#' `Column Name Variants`="Age",
#' Labels="Age in years at baseline",
#' `Column Type`="Demographics",
#' Format="Numeric")
#' specification<-list(list1)
#' names(specification)<-"Specification-Source Data"
#' format_df<-match_colnames(df,file=specification,coltype="Demographics")
match_colnames <- function(data, file, coltype) {

  cols<-c("Specification.Source.Data.Column.Name", "Specification.Source.Data.Column.Name.Variants",
          "Specification.Source.Data.Column.Type")

  d_specification <- file
  source_cols <-
    d_specification["Specification-Source Data"] %>%
    as.data.frame() %>%
    select(all_of(cols)) %>%
    mutate(
      spec_names = Specification.Source.Data.Column.Name,
      spec_namevariants = Specification.Source.Data.Column.Name.Variants,
      col_type = Specification.Source.Data.Column.Type
    ) %>%
    filter(col_type %in% coltype) %>%
    dplyr::select(Specification.Source.Data.Column.Name) %>%
    dplyr::pull()

  spec_names <- source_cols
  source_data_names <- c(colnames(as.data.frame(data)))

  d_spec <- d_specification["Specification-Source Data"]
  spec_df <- as.data.frame(d_spec) %>%
    select(Specification.Source.Data.Column.Name, Specification.Source.Data.Column.Name.Variants, Specification.Source.Data.Column.Type) %>%
    mutate(
      spec_names = Specification.Source.Data.Column.Name,
      spec_namevariants = Specification.Source.Data.Column.Name.Variants,
      col_type = Specification.Source.Data.Column.Type
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
    select(spec_names, spec_namevariants, col_type, select_col, final_name)

  names_df <-
    spec_cols_df %>%
    select(select_col) %>%
    filter(!(select_col %in% "")) %>%
    distinct() %>%
    pull(select_col)

  rename_df <-
    spec_cols_df %>%
    select(final_name) %>%
    filter(!(final_name %in% "")) %>%
    distinct() %>%
    pull(final_name)

  ifelse(!duplicated(rename_df) %in% TRUE, print(rename_df[duplicated(rename_df) | duplicated(rename_df, fromLast = TRUE)]),
    NULL
  )

  data_ret <- as.data.frame(data) %>% select(all_of(names_df))
  colnames(data_ret) <- rename_df
  print(paste0(colnames(data_ret), " :present"))

  addmissingcols <- setdiff(spec_names, colnames(data_ret))

  print(paste0(setdiff(spec_names, colnames(data_ret)), ":Assigned to NA"))

  data_ret[, addmissingcols] <- NA


  data_ret
}


utils::globalVariables(c(
  "Specification.Source.Data.Column.Name",
  "Specification.Source.Data.Column.Name.Variants",
  "Specification.Source.Data.Column.Type",
  "col_type", "spec_namevariants", "select_col", "final_name",
  "."
))
