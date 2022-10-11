#' A standardization function
#'
#' @param data is a data frame
#' @param file is path for specifications file that informs selection of variables
#' @param coltype is column type specification for baseline, demog, measurement, or dosing variables
#' @description matches for specified format(numeric or character) of the variable and converts variables to required format
#' @return a data frame with specified variables converted to required format
#' @export
#'
#' @examples
match_format <- function(data, file, coltype) {
  d_specification <- import_list(file_in(file), guess_max = 1e6)
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



  for (i in 1:length(names_df)) {
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
