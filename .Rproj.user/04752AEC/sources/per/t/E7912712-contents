#' Function to clean clusters data
#'
#' @param path_data Path for cluster data
#' @return [data.frame] Clean cluster data
#' @export
f_clean_cluster_data <- function(path_data){
  cluster_data <- rio::import(path_data)
  ## if the file is empty
  if(nrow(cluster_data) < 3){
    return(data.frame())
  }else{
    if(ncol(cluster_data) == 2){
      colnames(cluster_data) <- c('settlements', 'nb_cluster')
    }else{
      colnames(cluster_data) <- c('settlements', 'pop', 'nb_cluster')
    }
    cluster_data <- dplyr::select(cluster_data, c('settlements', 'nb_cluster'))
    cluster_data <- cluster_data |>
      tidyr::separate_rows(nb_cluster, sep = ",")
    cluster_data[which(cluster_data$nb_cluster == ""),]$nb_cluster <- NA
    cluster_data[which(cluster_data$nb_cluster == "RC"),]$nb_cluster <- NA
    cluster_data <- tidyr::drop_na(cluster_data)
    return(cluster_data)
  }
}

#' Function to read and save the different cluster data
#'
#' @param folder_name Folder name
#' @return [data.frame] Save cluster data in the right folder
#' @export
f_clean_and_export_cluster_data <- function(folder_name, information){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'clusters', x=all_files)]
  ## But one without any cleaning process
  unwanted <- all_files[grepl(pattern = 'clean', x=all_files)]
  wanted <- base::setdiff(all_files, unwanted)
  ## If there is a cluster file - we can clean it
  if(length(wanted) != 0){
    if(information[information$type_info == 'access_clusters',]$values == TRUE){
      path_data <- paste(folder_name, '/', wanted[1], sep="")
      name_data <- strsplit(path_data, split='_clusters')[[1]][1]
      clean_cluster_data <- f_clean_cluster_data(path_data)
      write.table(clean_cluster_data, 
                  file = paste(name_data, '_clusters_data_clean.csv', sep=""),
                  append = FALSE, 
                  sep=',', 
                  col.names=TRUE, 
                  row.names = FALSE)
    }
  }
}