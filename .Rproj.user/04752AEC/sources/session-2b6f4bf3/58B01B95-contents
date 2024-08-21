#' Function to update metadata with mortality metadata
#'
#' @param item name of the folder linked to the log_file
#' @param information metadatada of the file selected
#' @return [data.frame] Clean metadda with mortality ones
#' @export
f_calculate_mortality_metadata <- function(item, information){
  # Read nutrition data
  ## Select all the files in the folder
  all_files <- list.files(path = item, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'mortality', x=all_files)]
  ## But one without any cleaning process
  wanted <- all_files[grepl(pattern = 'clean', x=all_files)]
  ## If there is a cluster file - we can clean it
  if(length(wanted) != 0){
    # Extract name of the data
    path_data <- paste(item,'/', wanted[1], sep="")
    mortality_data <- rio::import(path_data)
    if(nrow(mortality_data) == 0){
      information <- f_update_info_no_mortality_data(information)
      return(information)
    }
    information <- f_update_info_mortality(mortality_data, information)
    return(information)
  }else{
    information <- f_update_info_no_mortality_data(information)
    return(information)
  }
  ## need to return something here too
}

#' Function to update metadata when mortality data is empty
#'
#' @param information metadatada of the file selected
#' @return [data.frame] Clean metadda with mortality ones
#' @export
f_update_info_no_mortality_data <- function(information){
  information <- rbind(information, c('lshtm_cdr_est', NA))
  information <- rbind(information, c('lshtm_cdr_log_se', NA))
  information <- rbind(information, c('lshtm_cdr_lci', NA))
  information <- rbind(information, c('lshtm_cdr_uci', NA))
  information <- rbind(information, c('lshtm_cdr_u5_est', NA))
  information <- rbind(information, c('lshtm_cdr_u5_log_se', NA))
  information <- rbind(information, c('lshtm_cdr_u5_lci', NA))
  information <- rbind(information, c('lshtm_cdr_u5_uci', NA))
  information <- rbind(information, c('lshtm_cdr_f_est', NA))
  information <- rbind(information, c('lshtm_cdr_f_log.se', NA))
  information <- rbind(information, c('lshtm_cdr_f_lci', NA))
  information <- rbind(information, c('lshtm_cdr_f_uci', NA))
  information <- rbind(information, c('lshtm_cdr_m_est', NA))
  information <- rbind(information, c('lshtm_cdr_m_log.se', NA))
  information <- rbind(information, c('lshtm_cdr_m_lci', NA))
  information <- rbind(information, c('lshtm_cdr_m_uci', NA))
  information <- rbind(information, c('lshtm_cdr_inj_est', NA))
  information <- rbind(information, c('lshtm_cdr_inj_log_se', NA))
  information <- rbind(information, c('lshtm_cdr_viol_est', NA))
  information <- rbind(information, c('lshtm_cdr_viol_log_se', NA))
  information <- rbind(information, c('lshtm_cbr_est', NA))
  information <- rbind(information, c('lshtm_in_migration_rate_est', NA))
  information <- rbind(information, c('lshtm_out_migration_rate_est', NA))
  information <- rbind(information, c('lshtm_net_migration_rate_est', NA))
  information <- rbind(information, c('lshtm_mean_n', NA))
  information <- rbind(information, c('lshtm_prop_f', NA))
  information <- rbind(information, c('lshtm_prop_u5', NA))
  information <- rbind(information, c('lshtm_prop_unk', NA))
  information <- rbind(information, c('lshtm_prop_inj', NA))
  information <- rbind(information, c('lshtm_prop_oth', NA))
  information <- rbind(information, c('lshtm_prop_viol', NA))
  information <- rbind(information, c('lshtm_prop_died_u1', NA))
  information <- rbind(information, c('lshtm_prop_inj_m', NA))
  information <- rbind(information, c('lshtm_prop_viol_u18', NA))
  information <- rbind(information, c('lshtm_prop_viol_f_o18', NA))
  information <- rbind(information, c('average_hh', NA))
  information <- rbind(information, c('sample_size_mortality', NA))
  information <- rbind(information, c('sum_p_time', NA))
  return(information)
}

#' Function to update metadata when mortality data is not empty
#'
#' @param information metadatada of the file selected
#' @return [data.frame] Clean metadda with mortality ones
#' @export
f_update_info_mortality <-function(mortality_data, information){
  
  unk_code <- as.integer(information[information$type_info == 'unk_code', ]$values)
  inj_code <- as.integer(information[information$type_info == 'inj_code', ]$values)
  viol_code <- as.integer(information[information$type_info == 'viol_code', ]$values)
  
  ##Calculate the lshtm design to produce cdr update
  if(length(unique(mortality_data$Cluster)) == 1 | length(unique(mortality_data$Cluster)) == 0){
    lshtm_survey_design <- "SRS or exhaustive"
    survey_design <- survey::svydesign(id = ~0, data = subset(mortality_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~0, data = subset(mortality_data, p_time_u5 > 0))
    survey_design_f <- survey::svydesign(id = ~0, data = subset(mortality_data, p_time_f > 0))
    survey_design_m <- survey::svydesign(id = ~0, data = subset(mortality_data, p_time_m > 0) )
  }else{
    lshtm_survey_design <- "multi-stage cluster"
    survey_design <- survey::svydesign(id = ~Cluster, data = subset(mortality_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~Cluster, data = subset(mortality_data, p_time_u5 > 0))
    survey_design_f <- survey::svydesign(id = ~Cluster, data = subset(mortality_data, p_time_f > 0))
    survey_design_m <- survey::svydesign(id = ~Cluster, data = subset(mortality_data, p_time_m > 0) )
  }
  fit <- survey::svyglm(n_died~NULL, survey_design, family="poisson", offset=log(p_time))
  information <- rbind(information, c('lshtm_cdr_est', round(exp(summary(fit)$coefficients[[1]])* 10000,3)))
  information <- rbind(information, c('lshtm_cdr_log_se', round(summary(fit)$coefficients[[2]],3)))
  information <- rbind(information, c('lshtm_cdr_lci', round(exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  information <- rbind(information, c('lshtm_cdr_uci', round(exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  
  fit <- survey::svyglm(n_died_u5~NULL, survey_design_u5, family="poisson", offset=log(p_time_u5) )
  information <- rbind(information, c('lshtm_cdr_u5_est', round(exp(summary(fit)$coefficients[[1]])* 10000,3)))
  information <- rbind(information, c('lshtm_cdr_u5_log_se', round(summary(fit)$coefficients[[2]],3)))
  information <- rbind(information, c('lshtm_cdr_u5_lci', round(exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  information <- rbind(information, c('lshtm_cdr_u5_uci', round(exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  
  if(all(is.na(mortality_data$p_time_f))){
    information <- rbind(information, c('lshtm_cdr_f_est', NA))
    information <- rbind(information, c('lshtm_cdr_f_log.se', NA))
    information <- rbind(information, c('lshtm_cdr_f_lci', NA))
    information <- rbind(information, c('lshtm_cdr_f_uci', NA))
  }else{
    fit <- survey::svyglm(n_died_f~NULL, survey_design_f, family="poisson", offset=log(p_time_f))
    information <- rbind(information, c('lshtm_cdr_f_est', round(exp(summary(fit)$coefficients[[1]])* 10000,3)))
    information <- rbind(information, c('lshtm_cdr_f_log.se', round(summary(fit)$coefficients[[2]],3)))
    information <- rbind(information, c('lshtm_cdr_f_lci', round(exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
    information <- rbind(information, c('lshtm_cdr_f_uci', round(exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  }
  
  if(all(is.na(mortality_data$p_time_m))){
    information <- rbind(information, c('lshtm_cdr_m_est', NA))
    information <- rbind(information, c('lshtm_cdr_m_log.se', NA))
    information <- rbind(information, c('lshtm_cdr_m_lci', NA))
    information <- rbind(information, c('lshtm_cdr_m_uci', NA))
  }else{
    fit <- survey::svyglm(n_died_m~NULL, survey_design_m, family="poisson", offset=log(p_time_m) )
    information <- rbind(information, c('lshtm_cdr_m_est', round(exp(summary(fit)$coefficients[[1]])* 10000,3)))
    information <- rbind(information, c('lshtm_cdr_m_log.se', round(summary(fit)$coefficients[[2]],3)))
    information <- rbind(information, c('lshtm_cdr_m_lci', round(exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
    information <- rbind(information, c('lshtm_cdr_m_uci', round(exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000,3)))
  }
  
  if (is.na(inj_code) == FALSE & !(all(is.na(mortality_data$n_died_inj)))){
    fit <- survey::svyglm(n_died_inj~NULL, survey_design, family="poisson", offset=log(p_time) )
    information <- rbind(information, c('lshtm_cdr_inj_est', eround(exp(summary(fit)$coefficients[[1]] ) * 10000,3)))
    information <- rbind(information, c('lshtm_cdr_inj_log_se', round(summary(fit)$coefficients[[2]]),3))
  }else{
    information <- rbind(information, c('lshtm_cdr_inj_est', NA))
    information <- rbind(information, c('lshtm_cdr_inj_log_se', NA))
  }
  
  if (is.na(viol_code)==FALSE & !(all(is.na(mortality_data$n_died_viol)))) {
    fit <- survey::svyglm(n_died_viol~NULL, survey_design, family="poisson", offset=log(p_time) )
    information <- rbind(information, c('lshtm_cdr_viol_est', round(exp(summary(fit)$coefficients[[1]] ) * 10000,3)))
    information <- rbind(information, c('lshtm_cdr_viol_log_se', round(summary(fit)$coefficients[[2]]),3))
  }else{
    information <- rbind(information, c('lshtm_cdr_viol_est', NA))
    information <- rbind(information, c('lshtm_cdr_viol_log_se', NA))
  }
  
  fit <- survey::svyglm(n_born~NULL, survey_design, family="poisson", offset=log(p_time) )
  information <- rbind(information, c('lshtm_cbr_est', round(exp(summary(fit)$coefficients[[1]] ) * 1000 * 365,3)))
  
  fit <- survey::svyglm(n_join~NULL, survey_design, family="poisson", offset=log(p_time) )
  information <- rbind(information, c('lshtm_in_migration_rate_est', round(exp(summary(fit)$coefficients[[1]] ) * 1000 * 365,3)))
  
  fit <- survey::svyglm(n_left~NULL, survey_design, family="poisson", offset=log(p_time) )
  information <- rbind(information, c('lshtm_out_migration_rate_est', round(exp(summary(fit)$coefficients[[1]] ) * 1000 * 365,3)))
  
  information <- rbind(information, c('lshtm_net_migration_rate_est', 
                                      round(as.integer(information[information$type_info == 'lshtm_in_migration_rate_est', ]$values) - 
                                        as.integer(information[information$type_info == 'lshtm_out_migration_rate_est', ]$values), 3)))
  
  information <- rbind(information, c('lshtm_mean_n', round(mean(mortality_data$n - mortality_data$n_left - mortality_data$n_died, na.rm = TRUE), 3)))
  information <- rbind(information, c('lshtm_prop_f', round(sum(mortality_data$p_time_f) / sum(mortality_data$p_time), 3)))
  information <- rbind(information, c('lshtm_prop_u5', round(sum(mortality_data$p_time_u5) / sum(mortality_data$p_time), 3)))

  sum_ <- sum(mortality_data$n_died_unk) + sum(mortality_data$n_died_inj) + sum(mortality_data$n_died_oth)
  information <- rbind(information, c('lshtm_prop_unk', round(sum(mortality_data$n_died_unk) / sum_, 3)))
  information <- rbind(information, c('lshtm_prop_inj', round(sum(mortality_data$n_died_inj) / sum_, 3)))
  information <- rbind(information, c('lshtm_prop_oth', round(sum(mortality_data$n_died_oth) / sum_, 3)))
  information <- rbind(information, c('lshtm_prop_viol', round(sum(mortality_data$n_died_viol) / sum_, 3)))
  information <- rbind(information, c('lshtm_prop_died_u1', round(sum(mortality_data$n_died_u1) / sum(mortality_data$n_died_u5), 3)))
  information <- rbind(information, c('lshtm_prop_inj_m', round(sum(mortality_data$n_died_inj_m) / sum(mortality_data$n_died_inj), 3)))
  information <- rbind(information, c('lshtm_prop_viol_u18', round(sum(mortality_data$n_died_viol_u18) / sum(mortality_data$n_died_viol), 3)))
  information <- rbind(information, c('lshtm_prop_viol_f_o18', round(sum(mortality_data$n_died_viol_o18) / sum(mortality_data$n_died_viol), 3)))
  information <- rbind(information, c('average_hh', round(mean(mortality_data$n, na.rm = TRUE), 0)))
  information <- rbind(information, c('sample_size_mortality', nrow(mortality_data)))
  information <- rbind(information, c('sum_p_time', sum(mortality_data$p_time, na.rm = TRUE)))
  return(information)
}