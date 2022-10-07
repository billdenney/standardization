
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
#' @importFrom dplyr filter
match_colnames <- function(data, file, coltype) {
  d_specification <- import_list(file_in(file), guess_max = 1e6)
  source_cols <-
    bsd.report::get_data_manage_standard_cols(
      d_specification[["Specification-Source Data"]],
      coltype
    )

  spec_names <- source_cols
  source_data_names <- c(colnames(data))

  d_spec <- d_specification["Specification-Source Data"]
  spec_df <- as.data.frame(d_spec) %>%
    dplyr::select(Specification.Source.Data.Column.Name, Specification.Source.Data.Column.Name.Variants, Specification.Source.Data.Column.Type) %>%
    dplyr::mutate(
      spec_names = Specification.Source.Data.Column.Name,
      spec_namevariants = Specification.Source.Data.Column.Name.Variants,
      col_type = Specification.Source.Data.Column.Type
    ) %>%
    dplyr::filter(col_type %in% coltype) %>%
    dplyr::group_by(spec_names, col_type, spec_namevariants) %>%
    dplyr::mutate(spec_namevariants = strsplit(spec_namevariants, ", |,| ,")) %>%
    purr::unnest(cols = spec_namevariants) %>%
    ungroup()


  spec_cols_df <-
    spec_df %>%
    dplyr::mutate(select_col = case_when(
      spec_names %in% source_data_names ~ spec_names,
      spec_namevariants %in% source_data_names ~ spec_namevariants,
      TRUE ~ ""
    )) %>%
    dplyr::mutate(
      final_name = case_when(
        !(is.na(select_col) | select_col == "") ~ spec_names,
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(spec_names, spec_namevariants, col_type, select_col, final_name)

  names_df <-
    spec_cols_df %>%
    dplyr::select(select_col) %>%
    dplyr::filter(!(select_col %in% "")) %>%
    dplyr::distinct() %>%
    dplyr::pull(select_col)

  rename_df <-
    spec_cols_df %>%
    select(final_name) %>%
    dplyr::filter(!(final_name %in% "")) %>%
    dplyr::distinct() %>%
    dplyr::pull(final_name)

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
