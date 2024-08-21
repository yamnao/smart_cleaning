#' Function to select the information in the logfile as country name etc..
#'
#' @param log_file log_file file
#' @param item name of the folder linked to the log_file
#' @return [data.frame] Clean cluster data
#' @export
f_add_clean_infos <- function(item, log_file){
  ## Find the name of the file
  cut_name <- strsplit(item, '/')[[1]]
  name_file <- cut_name[[length(cut_name)]]
  ## Filter the right line of the log file
  select_data <- log_file |> dplyr::filter(new_name == name_file) |>
    dplyr::select(old_name, new_name, admin_level_survey, admin_name_survey,
                  type_survey, min_day_survey, min_month_survey, min_year_survey,
                  max_day_survey, max_month_survey, max_year_survey,
                  access_nutrition, 
                  access_mortality, access_clusters, recall_days, 
                  inj_code, viol_code, unk_code)
  select_data <- select_data[1,]
  ## Save the data in the information
  name_column <- colnames(select_data)
  value_column <- c(apply(select_data, 1, function(row) as.vector(as.character(row))))
  information<- data.frame(type_info = name_column,
                     values = value_column)
  return(information)
}
