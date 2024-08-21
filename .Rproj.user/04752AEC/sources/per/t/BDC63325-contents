#' Function to clean the different folders
#'
#' @param folder_name folder containing all the data
#' @return [data.frame] Clean cluster data
#' @export
f_cleaning <- function(all_survey_folder, cleaning_criteria='who_2006'){
  # List all items (files/folders) in the current folder
  items <- list.files(all_survey_folder, 
                      full.names = TRUE)
  log_file <- rio::import(paste(all_survey_folder, '/metadata.csv',sep = ''))
  # Generate and create a metadata file  for the raw folder
  metadata <- generate_metadata_file(all_survey_folder)
  # Loop through each item
  for (item in items) {
    # Check if the item is a directory
    if (file.info(item)$isdir) {
      print(item)
      if(length(item[grepl(x=item, pattern='smart_with_issue')]) == 0){
        print(item)
        # Clean information in the log_file
        information <- f_add_clean_infos(item, log_file)
        # Clean and save cluster data
        f_clean_and_export_cluster_data(item, information)
        # Clean and save mortality data
        f_clean_and_export_mortality_data(item, information)
        # Clean and save nutrition data
        information <- f_clean_and_export_nutritition_data(item, information, cleaning_criteria)
        information <- information[[1]]
        # Generate plausibility score and report
        information <- f_calcul_and_report_plausibility_score(item, information)
        # Calculate the different mortality metadata (lshtm scores)
        information <- f_calculate_mortality_metadata(item, information)
        # Save the all the information in the metadata
        information <- data.frame(t(information))
        rownames(information) <- NULL
        colnames(information) <- information[1,]
        information <- information[-1,]
        metadata <- rbind(metadata, information)
      }
    }
  }
  write.table(metadata, 
              file = paste(all_survey_folder, 'metadata_clean.csv', sep=""),
              append = FALSE, 
              sep=',', 
              col.names=TRUE, 
              row.names = FALSE)
}

#' Function to generate metadata file
#'
#' @return [data.frame] metadata
generate_metadata_file <- function(all_survey_folder){
  if(length(which(list.files(all_survey_folder) == 'metadata_clean.csv')) == 0){ ##if metadata doesnt exist
    metadata_file <- data.frame()
  }else{
    metadata_file <- read.csv(paste(all_survey_folder, '/metadata_clean.csv', sep=""))
  }
  return(metadata_file)
}
