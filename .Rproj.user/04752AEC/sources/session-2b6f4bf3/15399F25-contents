#' Function to clean mortality data
#'
#' @param path_data Path for morality data
#' @return [data.frame] Clean mortality data
#' @export
f_clean_mortality_data <- function(path_data, 
                                   information){
  ## Add cleaning process for mortality data
  mortality_data <- rio::import(path_data)
  ## First need to check of the mortality data are aggregate or at individual level
  if(information[information$type_info == 'type_survey',]$values == 'aggregate'){
    return(clean_data_aggregate(mortality_data, information))
  }else if(information[information$type_info == 'type_survey',]$values == 'individual'){
    data <- rotate_data(mortality_data)
    return(clean_individual_data(data, information))
  }else{
    return(data.frame())
  }
}

#' Function to rotate mortality when individual data
#'
#' @param df Mortality data 
#' @return [data.frame] Rotate mortality data
#' @export
rotate_data <- function(df){
  df <- df |> tidyr::drop_na(HH, Cluster)
  if(nrow(df) ==0){
    return(df)
  }
  df[colnames(dplyr::select(df, contains('join')))] <- ifelse(dplyr::select(df, contains('join')) == 'y' & is.na(dplyr::select(df, contains('join'))) == FALSE, 1, 0)
  for(col in colnames(df |> dplyr::select(contains('age')))){
    df[, col] <- as.numeric(df[, col])
  }
  for(col in colnames(df |> dplyr::select(contains('cause')))){
    df[, col] <- as.numeric(df[, col])
  }
  
  transpose_data <- dplyr::select(df, contains(c('sex', 'HH', 'Cluster', 'Team'))) |>
    tidyr::pivot_longer(cols=contains('sex'), cols_vary='slowest', names_to= c('personindex'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('age'))) |>
                                                          tidyr::pivot_longer(cols=contains('age'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('join'))) |>
                                                          tidyr::pivot_longer(cols=contains('join'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('left'))) |>
                                                          tidyr::pivot_longer(cols=contains('left'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('born'))) |>
                                                          tidyr::pivot_longer(cols=contains('born'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('died'))) |>
                                                          tidyr::pivot_longer(cols=contains('died'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('cause'))) |>
                                                          tidyr::pivot_longer(cols=contains('cause'), cols_vary='slowest', names_to= c('age')), 'value'))
  
  colnames(transpose_data) <- c('HH', 'Cluster', 'Team', 'PersonIndex', 'Sex', 'Age', 'Join', 'Left', 'Born', 'Died', 'Cause')
  transpose_data$Sex <- ifelse(transpose_data$Sex == 'm', 1, 0)
  transpose_data$Join <- ifelse(transpose_data$Join == 'y', 1, 0)
  transpose_data$Left <- ifelse(transpose_data$Left == 'y', 1, 0)
  transpose_data$Born <- ifelse(transpose_data$Born == 'y', 1, 0)
  transpose_data$Died <- ifelse(transpose_data$Died == 'y', 1, 0)
  transpose_data$newIndex <- paste(transpose_data$Cluster,transpose_data$HH, sep='_')
  return(transpose_data)
}

#' Function to clean mortality when individual data
#'
#' @param data Mortality data 
#' @return [data.frame] Rotate mortality data
#' @export
clean_individual_data <- function(data, information){
  if(nrow(data) == 0){
    return(data)
  }
  if(length(unique(data$Cluster)) > 1){
    data <- data[which(is.na(data$Cluster) == FALSE),]
  }
  recall_period <- as.integer(information[information$type_info == 'recall_days',]$values)
  inj_code <- as.integer(information[information$type_info == 'inj_code',]$values)
  unk_code <- as.integer(information[information$type_info == 'unk_code',]$values)
  viol_code <- as.integer(information[information$type_info == 'viol_code',]$values)
  data[which(is.na(data$Join)), 'Join'] <- 0
  data[which(is.na(data$Left)), 'Left'] <- 0
  data[which(is.na(data$Died)), 'Died'] <- 0
  data[which(is.na(data$Born)), 'Born'] <- 0
  data[which(is.na(data$Cause)), 'Cause'] <- 0
  data['eligeable'] <- ifelse(is.na(data$Sex) == FALSE & is.na(data$Age) == FALSE, 1, 0)
  results <- data[which(data$eligeable == 1),] |> dplyr::group_by(newIndex) |>
    dplyr::summarise(n = sum(eligeable), n_m = sum(Sex), n_f = sum(Sex == 0), n_u5 = sum(Age < 5),
                     n_5 = sum(Age == 5), n_u5_m = sum(Age < 5 & Sex), n_u5_f = sum(Age <5 & Sex == 0),
                     n_join = sum(Join), n_join_m = sum(Join & Sex), n_join_f = sum(Join&Sex==0),
                     n_join_u5=sum(Join & Age <5), n_join_5 = sum(Join & Age == 5),
                     n_left = sum(Left), n_left_m = sum(Left & Sex), n_left_f = sum(Left & Sex ==0),
                     n_left_u5 = sum(Left & Age < 5), n_left_5 = sum(Left & Age == 5),
                     n_born = sum(Born), n_born_m = sum(Born & Sex), n_born_f = sum(Born & Sex ==0),
                     n_died = sum(Died), n_died_m = sum(Died & Sex), n_died_f = sum(Died & Sex == 0),
                     n_died_u5 = sum(Died & Age <5), n_died_5 = sum(Died & Age == 5), n_died_u1 = sum(Died & Age < 1),
                     n_died_inj =sum(Died & Cause == inj_code), n_died_inj_m = sum(Died & Cause == inj_code & Sex),
                     n_died_inj_f = sum(Died & Cause == inj_code & Sex ==0), n_died_inj_u5 = sum(Died & Cause == inj_code& Age <5),
                     n_died_unk = sum(Died & Cause == unk_code),
                     n_died_viol = sum(Died & Cause == viol_code),
                     n_died_viol_u18 = sum(Died & Cause == viol_code & Age < 18),
                     n_died_viol_o18 = sum(Died & Cause == viol_code & Age > 18 & Sex == 0),
                     n_died_oth = sum(Died) - n_died_inj- n_died_unk - n_died_viol,
                     p_time = (n - 0.5 * n_join + 0.5 * n_left - 0.5 * n_born + 0.5 * n_died ) * recall_period,
                     p_time_f = (n_f - 0.5 * n_join_f + 0.5 * n_left_f - 0.5 * n_born_f + 0.5 * n_died_f) * recall_period,
                     p_time_m = (n_m - 0.5 * n_join_m + 0.5 * n_left_m - 0.5 * n_born_m + 0.5 * n_died_m) * recall_period,
                     p_time_u5 = (n_u5 - 0.5 * n_join_u5 + 0.5 * n_left_u5 - 0.5 * n_born + 0.5 * n_died_u5) * recall_period +
                       (n_5 - 0.5 * n_join_5 + 0.5 * n_left_5 + 0.5 * n_died_5 ) * recall_period / 365
    )
  results <- results |> tidyr::separate_wider_delim(newIndex, '_', names=c('Cluster', 'HH'))
  return(results)
}


#' Function to clean mortality data
#'
#' @param mortality_data Aggregate mortality data
#' @return [data.frame] Clean mortality data
#' @export
clean_data_aggregate <- function(data, information){
  data <- lapply(data, as.numeric)
  data <- replace(data, is.na(data), 0)
  data <- as.data.frame(data)
  colnames(data) <- c("HH", "Cluster", "n", "n_u5" ,"n_join", "n_join_u5", "n_left", "n_left_u5", "n_born", "n_died", "n_died_u5")
  data['n_join'][which(is.na(data$n_join)),] <- 0
  data['n_join_u5'][which(is.na(data$n_join_u5)),] <- 0
  data['n_left'][which(is.na(data$n_left)),] <- 0
  data['n_left_u5'][which(is.na(data$n_left_u5)),] <- 0
  data['n_born'][which(is.na(data$n_born)),] <- 0
  data['n_died'][which(is.na(data$n_died)),] <- 0
  data['n_died_u5'][which(is.na(data$n_died_u5)),] <- 0
  data <- subset(data, data$n >= data$n_join + data$n_born + data$n_join_u5 +  data$n_u5 - data$n_died - data$n_died_u5 - data$n_left - data$n_left_u5)
  data <- apply(data, c(1,2), as.character)
  data <- as.data.frame(apply(t(data), 1, readr::parse_vector, readr::col_integer() ))
  if (length(unique(data$Cluster)) > 1) { data <- subset(data, ! is.na(Cluster) ) }
  data[is.na(data)] <- 0
  recall_period <- as.integer(information[information$type_info == 'recall_days',]$values)
  if(length(recall_period) ==2){recall_period <- recall_period[2]}
  results <- data.frame()
  data[, 'p_time'] <- (data$n - data$n_join * 0.5 + data$n_left * 0.5 - data$n_born * 0.5 + data$n_died * 0.5 )* recall_period
  data[, 'p_time_u5'] <- (data$n_u5 - data$n_join_u5 * 0.5 + data$n_left_u5 * 0.5 - data$n_born * 0.5 + data$n_died_u5 * 0.5 )* recall_period
  # add blank columns for variables that aggregate surveys don't collect
  data[, c("n_f", "n_m", "n_5", "n_u5_m","n_u5_f", "n_join_f", "n_join_m", "n_left_f", "n_left_m", "n_born_f",
           "n_born_m", "n_died_m", "n_died_f", "n_died_u1", "n_died_5", "n_died_inj", "n_died_inj_f", "n_died_inj_m", "n_died_inj_u5",
           "n_died_unk", "n_died_oth","n_join_5", "n_left_5", "n_died_viol", "n_died_viol_u18", "n_died_viol_o18", "p_time_f", "p_time_m")] <- NA
  return(data)
}

#' Function to read and save the different mortality data
#'
#' @param folder_name Folder name
#' @return [data.frame] Save mortality data in the right folder
#' @export
f_clean_and_export_mortality_data <- function(folder_name, information){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'mortality', x=all_files)]
  ## But one without any cleaning process
  unwanted <- all_files[grepl(pattern = 'clean', x=all_files)]
  wanted <- base::setdiff(all_files, unwanted)
  ## If there is a cluster file - we can clean it
  if(length(wanted) != 0){
    if(information[information$type_info == 'access_mortality',]$values == TRUE){
      # Extract name of the data
      path_data <- paste(folder_name,'/', wanted[1], sep="")
      name_data <- strsplit(path_data, split='_mortality_data')[[1]][1]
      clean_mortality_data <- f_clean_mortality_data(path_data,  
                                                     information)
      write.table(clean_mortality_data, 
                  file = paste(name_data, '_mortality_data_clean.csv', sep=""),
                  append = FALSE, 
                  sep=',', 
                  col.names=TRUE, 
                  row.names = FALSE)
    }
  }
}