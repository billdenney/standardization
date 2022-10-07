
#' A standardization function
#'
#' @name match_colnames
#' @param data
#' @param file
#' @param coltype
#' @description matches for standard column names and selects from source dataset and prints columns not present in source dataset
#' @return data frame with selected columns
#' @export
#'
#' @examples
#' match_colnames()
match_colnames <- function(data, file, coltype) {
  d_specification <- import_list(file_in(file), guess_max = 1e6)
  source_cols <-
    get_data_manage_standard_cols(
      d_specification[["Specification-Source Data"]],
      coltype
    )

  spec_names <- source_cols
  source_data_names <- c(colnames(data))

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

  data_ret <- data %>% select(all_of(names_df))
  colnames(data_ret) <- rename_df
  print(paste0(colnames(data_ret), " :present"))

  addmissingcols<-setdiff(spec_names, colnames(data_ret))

  print(paste0(setdiff(spec_names, colnames(data_ret)), ":Assigned to NA"))

  data_ret[,addmissingcols] <- NA


  return(data_ret)
}
