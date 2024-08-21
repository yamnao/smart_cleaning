#' Function to plot anthro results could be gaussian curve or probability plot or distribution
#'
#' @param type_of_plot could be gaussian_curve_WHZ, gaussian_curve_WAZ, gaussian_curve_HAZ,
#' or cum_muac, or prob_plot_WHZ, or prob_plot_HAZ, or prob_plot_WAZ, or
#' distr_WHZ_2 or distr_WHZ_3 
#' NOT IMPLEMENTED FOR THE MOMENT distr_GAM or distr_SAM
#' @return plot
#' @export

plot_anthro <- function(folder_name, 
                        exclusion, 
                        type_of_plot, 
                        group = 'All'){
  
  ## Find the right file
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  
  ## If we are using the clean nutrition data
  if(exclusion == 'with'){
    ## Find the cluster one
    all_files <- all_files[grepl(pattern = 'nutrition', x=all_files)]
    ## But one without any cleaning process
    unwanted <- all_files[grepl(pattern = 'clean', x=all_files)]
    wanted <- base::setdiff(all_files, unwanted)
    # Extract name of the data
    nutrition_file <- paste(folder_name,'/', wanted[1], sep="")
  }else{
    ## Find the cluster one
    nutrition_file <- paste(folder_name,'/', 
                            all_files[grepl(pattern = 'nutrition_data_clean', x=all_files)],
                            sep="")
  }
  
  ## Then plot the right option between the different possibilities
  if(grepl(pattern = 'gaussian', x = type_of_plot)){
    anthro_feature <- stringr::str_split(type_of_plot, '_')[[1]][3]
    plot <- plot_gaussian_curve(nutrition_file, 
                                anthro_feature,
                                group)
    return(plot)
  }else if(grepl(pattern = 'prob', x= type_of_plot)){
    anthro_feature <- stringr::str_split(type_of_plot, '_')[[1]][3]
    plot <- plot_proba_curve(nutrition_file, 
                                anthro_feature,
                                group)
    return(plot)
  }else if(type_of_plot == 'cum_muac'){
    plot <- plot_cumulative_curve(nutrition_file)
    return(plot)
  }else if(type_of_plot == 'distr_WHZ_2'){
    plot <- plot_distribution_per_cluster(nutrition_file = nutrition_file,  
                                          threshold = -2)
    return(plot)
  }else if(type_of_plot == 'distr_WHZ_3'){
    plot <- plot_distribution_per_cluster(nutrition_file = nutrition_file,  
                                          threshold = -3)
    return(plot)
  }else if(type_of_plot == 'age_team'){
    return(plot_age_distr(nutrition_file, group_by_TEAM = TRUE))
  }else if(type_of_plot == 'age'){
    return(plot_age_distr(nutrition_file, group_by_TEAM = FALSE))
  }
  
 
  
}