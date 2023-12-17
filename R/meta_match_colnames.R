#' Selects variables based on metadata labels and renames them to preferred column name in the output.
#'
#' @param data A data.frame that serves as source dataset.
#' @param file 'Specifications' can be provided as list object in R or .xlsx file path, which informs selection of variables.
#' @param coltype Column type specification: 'baseline', 'demog', 'measurement', or 'dosing' to select all related variables.
#' @description Selects SDTM column names based on their metadata labels and renames them to preferred column name in the output.
#' @return A data frame with specified variables selected and renamed to preferred name.
#' @export
#' @examples
#' df<-data.frame(STUDYID=rep("S-CDSK-01",3),DOMAIN=rep("YEARS",3),Age=c(72,66,78))
#' list1<-data.frame(`Column Name`="AGE",
#' `Column Name Variants`="Age",
#' Labels="Age in years at baseline",
#' `Column Type`="Demographics",
#' Format="Numeric")
#' labelled::var_label(df[,"Age"]) <- "Age in years at baseline"
#' specification<-list(list1)
#' names(specification)<-"Specification-Source Data"
#' format_df<-meta_match_colnames(df,file=specification,coltype="Demographics")
meta_match_colnames <- function(data, file, coltype) {
  meta_df <- Hmisc::contents(data)
  meta_dat <- data.frame(meta_df[1])
  meta_file <- tibble::rownames_to_column(meta_dat, "Contents.Columns") %>% select(Contents.Columns, contents.Labels)
  colnames(meta_file) <- substring(colnames(meta_file), 10, 17)

  d_specification <- file
  source_cols <- d_specification["Specification-Source Data"] %>%
    as.data.frame() %>%
    select(Specification.Source.Data.Column.Name, Specification.Source.Data.Column.Type) %>%
    mutate(
      spec_names = Specification.Source.Data.Column.Name,
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
      Specification.Source.Data.Column.Name, Specification.Source.Data.Labels,
      Specification.Source.Data.Column.Type
    ) %>%
    mutate(
      "Columns_spec" = Specification.Source.Data.Column.Name,
      "Labels" = Specification.Source.Data.Labels,
      "col_type" = Specification.Source.Data.Column.Type
    ) %>%
    filter(col_type %in% coltype) %>%
    select(Columns_spec, Labels, col_type) %>%
    group_by(Columns_spec, col_type, Labels) %>%
    mutate(Labels = strsplit(Labels, ", |,| ,")) %>%
    unnest(cols = Labels) %>%
    ungroup()

  spec <- full_join(spec_df, meta_file, by = "Labels")


  spec_cols_df <-
    spec %>%
    mutate(select_col = case_when(
      Columns_spec %in% source_data_names ~ Columns_spec,
      TRUE ~ ""
    )) %>%
    mutate(
      select_col = case_when(
        !(is.na(Columns) | Columns == "") & !(is.na(Columns_spec) | Columns_spec == "") &
          select_col == "" ~ Columns,
        TRUE ~ select_col
      ),
      final_name = case_when(
        !(is.na(select_col) | select_col == "") ~ Columns_spec,
        TRUE ~ ""
      )
    )


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


  if (any(!duplicated(rename_df) %in% TRUE) ) {
    print(rename_df[duplicated(rename_df) | duplicated(rename_df, fromLast = TRUE)])
  } else{
    NULL
  }

  data_ret <- data %>% select(all_of(names_df))
  colnames(data_ret) <- rename_df
  print(paste0(colnames(data_ret), " :present"))

  addmissingcols <- setdiff(spec_names, colnames(data_ret))

  print(paste0(setdiff(spec_names, colnames(data_ret)), ":Assigned to NA"))

  data_ret[, addmissingcols] <- NA

  data_ret
}

utils::globalVariables(c(
  "Specification.Source.Data.Column.Name",
  "Specification.Source.Data.Column.Type",
  "col_type", "select_col", "final_name",
  ".",
  "Specification.Source.Data.Column.Name",
  "Specification.Source.Data.Labels",
  "Specification.Source.Data.Column.Type",
  "Contents.Columns",
  "Columns_spec",
  "Labels",
  "contents.Labels"
))
