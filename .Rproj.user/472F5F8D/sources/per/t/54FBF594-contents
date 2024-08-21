#' Function to read and save the different nutrition data
#'
#' @param folder_name Folder name
#' @return [data.frame] Save nutrition data in the right folder
#' @export
f_clean_and_export_nutritition_data <- function(folder_name, information, cleaning_criteria){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'nutrition', x=all_files)]
  ## But one without any cleaning process
  unwanted <- all_files[grepl(pattern = 'clean', x=all_files)]
  wanted <- base::setdiff(all_files, unwanted)
  ## If there is a cluster file - we can clean it
  if(length(wanted) != 0){
    if(information[information$type_info == 'access_nutrition',]$values == TRUE){
      # Extract name of the data
      path_data <- paste(folder_name,'/', wanted[1], sep="")
      name_data <- strsplit(path_data, split='_nutrition')[[1]][1]
      # List containing 2 dataframe - first one is the nutrition data and second one information updated
      output_nutrition_information <- f_clean_nutrition_data(path_data,  
                                                     information, 
                                                     cleaning_criteria)
      clean_nutrition_data <- output_nutrition_information[1]
      write.table(clean_nutrition_data, 
                  file = paste(name_data, '_nutrition_data_clean.csv', sep=""),
                  append = FALSE, 
                  sep=',', 
                  col.names=TRUE, 
                  row.names = FALSE)
      return(output_nutrition_information[2])
    }
  }
  return(information)
}

#' Function to generaye dico cleaning criteria as developed in Marko paper
#'
#' @return [dict] List of list containing the cleaning criteria
#' @export
f_dico_cleaning_criteria <- function(){
  dico_cleaning <- list()
  # TRUE - because we don't want to excluse any
  dico_cleaning[['who_2006']] <- list(haz_low = -6, 
                                      haz_up = 6,
                                      waz_low = -6,
                                      waz_up = 5,
                                      whz_low = -5,
                                      whz_up = 5, 
                                      bio_haz_up = TRUE,
                                      bio_whz_low = TRUE,
                                      bio_haz_low = TRUE,
                                      bio_whz_up = TRUE)
  dico_cleaning[['smart_flag']] <- list(haz_low = -3, 
                                        haz_up = 3,
                                        waz_low = -3,
                                        waz_up = 3,
                                        whz_low = -3,
                                        whz_up = 3, 
                                        bio_haz_up = TRUE,
                                        bio_whz_low = TRUE,
                                        bio_haz_low = TRUE,
                                        bio_whz_up = TRUE)
  dico_cleaning[['who_1995_survey']] <- list(haz_low = -4, 
                                        haz_up = 3,
                                        waz_low = -4,
                                        waz_up = 4,
                                        whz_low = -4,
                                        whz_up = 4, 
                                        bio_haz_up = TRUE,
                                        bio_whz_low = TRUE,
                                        bio_haz_low = TRUE,
                                        bio_whz_up = TRUE)
  dico_cleaning[['who_1995_growth']] <- list(haz_low = -5, 
                                        haz_up = 3,
                                        waz_low = -5,
                                        waz_up = 5,
                                        whz_low = -4,
                                        whz_up = 5, 
                                        bio_haz_up = TRUE,
                                        bio_whz_low = TRUE,
                                        bio_haz_low = TRUE,
                                        bio_whz_up = TRUE)
  dico_cleaning[['epi_info']] <- list(haz_low = -6, 
                                             haz_up = 6,
                                             waz_low = -6,
                                             waz_up = 6,
                                             whz_low = -4,
                                             whz_up = 6, 
                                             bio_haz_up = 3.09,
                                             bio_whz_low = -3.09,
                                             bio_haz_low = -3.09,
                                             bio_whz_up = 3.09)
  return(dico_cleaning)
}

#' Function to clean nutrition data: add flag in the nutrition data + 
#' generate dataframe with clean nutrition data - without the one flagged +
#' modify the information by calculating the flag coefficient
#'
#' @param path_data Path for nutrition data
#' @return [list] Clean nutrition data + information update
#' @export
f_clean_nutrition_data <- function(path_data, information, 
                                   cleaning_criteria){
  # Read nutrition data
  nutrition_data <- rio::import(path_data)
  # Remove WHZ, WAZ or HAZ with NA values
  nutrition_data <- nutrition_data |> 
    dplyr::filter_at(dplyr::vars(WAZ, HAZ, WHZ),dplyr::all_vars(!is.na(.)))
  if(nrow(nutrition_data) == 0){
    # Sum the number of flag data for each criteria and add it to the information
    information <- rbind(information, c('flag_whz', NA))
    information <- rbind(information, c('flag_waz', NA))
    information <- rbind(information, c('flag_haz', NA))
    information <- rbind(information, c('flag_bio', NA))
    information <- rbind(information, c('sample_size_nutrition', NA))
    information <- rbind(information, c('nb_boys', NA))
    information <- rbind(information, c('nb_girls', NA))
    information <- rbind(information, c('prevalence_sam', NA))
    information <- rbind(information, c('prevalence_gam', NA))
    return(list(nutrition_data, information))
  }
  # Flag line depending of the criteria -- so add 3 fl
  # Select the cleaning criteria
  criteria <- f_dico_cleaning_criteria()[[cleaning_criteria]]
  # First flag WHZ
  mean_WHZ <- mean(nutrition_data$WHZ)
  nutrition_data['flag_WHZ'] <- ifelse(nutrition_data$WHZ > criteria$whz_up + mean_WHZ | 
                                         nutrition_data$WHZ < mean_WHZ + criteria$whz_low , 1, 0)
  # Second flag WAZ
  mean_WAZ <- mean(nutrition_data$WAZ)
  nutrition_data['flag_WAZ'] <- ifelse(nutrition_data$WAZ > criteria$waz_up + mean_WAZ | 
                                         nutrition_data$WAZ < mean_WAZ + criteria$waz_low , 1, 0)
  # Third flag HAZ
  mean_HAZ <- mean(nutrition_data$HAZ)
  nutrition_data['flag_HAZ'] <- ifelse(nutrition_data$HAZ > criteria$haz_up + mean_HAZ | 
                                         nutrition_data$HAZ < mean_HAZ + criteria$haz_low , 1, 0)
  # Fourth flag bio criteria
  nutrition_data['flag_bio'] <- ifelse(criteria$bio_haz_up == TRUE | 
                                         !((nutrition_data$HAZ > criteria$bio_haz_up + mean_HAZ) & (nutrition_data$WHZ < criteria$bio_whz_low + mean_WHZ)) |
                                         !((nutrition_data$HAZ < criteria$bio_haz_low + mean_HAZ) & (nutrition_data$WHZ > criteria$bio_whz_up + mean_WHZ)), 
                                       0, 1)
  # Calculate GAM and SAM - 1 if existing, 0 else 
  nutrition_data['edema'] <- ifelse(nutrition_data$EDEMA %in% c('No', 'n', 'no', 2, '2'), yes=0, no=1)
  nutrition_data <- nutrition_data |>
    dplyr::mutate(
      SAM = ifelse(WHZ < -3 | MUAC < 115 | edema == 1, 1, 0),
      GAM = ifelse(WHZ < -2 | MUAC < 125 | edema == 1, 1, 0)
    )
  # Sum the number of flag data for each criteria and add it to the information
  information <- rbind(information, c('flag_whz', ifelse(nrow(nutrition_data) != 0, yes = sum(nutrition_data$flag_WHZ, na.rm=TRUE)/length(nutrition_data$flag_WHZ)*100, no = 0)))
  information <- rbind(information, c('flag_waz', ifelse(nrow(nutrition_data) != 0, yes = sum(nutrition_data$flag_WAZ, na.rm=TRUE)/length(nutrition_data$flag_WHZ)*100, no= 0)))
  information <- rbind(information, c('flag_haz', ifelse(nrow(nutrition_data) != 0, yes = sum(nutrition_data$flag_HAZ, na.rm=TRUE)/length(nutrition_data$flag_WHZ)*100, no=0)))
  information <- rbind(information, c('flag_bio', ifelse(nrow(nutrition_data) != 0, yes = sum(nutrition_data$flag_bio, na.rm=TRUE)/length(nutrition_data$flag_WHZ)*100, no=0)))
  information <- rbind(information, c('sample_size_nutrition', ifelse(nrow(nutrition_data) != 0, yes = nrow(nutrition_data), no = NA)))
  information <- rbind(information, c('nb_boys', ifelse(nrow(nutrition_data) != 0, yes = nrow(nutrition_data |> dplyr::filter(SEX == 'm')), no = NA)))
  information <- rbind(information, c('nb_girls', ifelse(nrow(nutrition_data) != 0, yes = nrow(nutrition_data |> dplyr::filter(SEX == 'f')), no = NA)))
  information <- rbind(information, c('prevalence_sam', ifelse(nrow(nutrition_data) != 0, yes = round(sum(nutrition_data$SAM) / nrow(nutrition_data) * 100, 2), no = NA)))
  information <- rbind(information, c('prevalence_gam', ifelse(nrow(nutrition_data) != 0, yes = round(sum(nutrition_data$GAM) / nrow(nutrition_data) * 100, 2), no = NA)))
  # Save nutrition data with flag column
  write.table(nutrition_data, 
              file = path_data,
              append = FALSE, 
              sep=',', 
              col.names=TRUE, 
              row.names = FALSE)
  nutrition_data_clean <- nutrition_data |> dplyr::filter(flag_WHZ == 0 & flag_WAZ == 0 & flag_HAZ ==0 & flag_bio==0)
  # Remove the line with flag data - and save the new nutrition data
  return(list(nutrition_data_clean, information))
  
}

