#' Function to generate table analysis per team
#'
#' @param folder_name name of the folder where the data are available
#' @return plot
#' @export

table_analysis_per_team <- function(folder_name){
  
  ## Find the right file
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  nutrition_file <- paste(folder_name,'/', 
                          all_files[grepl(pattern = 'nutrition_data_clean', x=all_files)],
                          sep="")
  nutrition_data <- rio::import(nutrition_file)
  res <- nutrition_data |> 
    dplyr::mutate(MONTHS= as.numeric(MONTHS)) |>
    dplyr::group_by(TEAM) |>
    dplyr::summarize(count = round(as.integer(dplyr::n()), 0), 
                     flag_whz = round(sum(flag_WHZ), 0),
                     flag_haz = round(sum(flag_HAZ), 0),
                     flag_waz = round(sum(flag_WAZ), 0),
                     ratio_age = round(sum(MONTHS < 30, na.rm = TRUE) / sum(MONTHS >= 30, na.rm = TRUE), 3),
                     .groups = 'drop',
                     )
    res <- data.frame(t(res))
    colnames(res) <- res[1,]
    res <- res[-1,]
    rownames(res) <- c('Number of observations', 
                       '% of flagged WHZ',
                       '% of flagged HAZ', 
                       '% of flagged WAZ', 
                       'Age ratio')
    res$Team <- rownames(res)
    res <- res |> dplyr::select(Team, dplyr::everything())
    return(res)
}