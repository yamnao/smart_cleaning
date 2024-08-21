#' Function to plot age distribution
#'
#' @param nutrition_file path of the nutrition file
#' @return plot
plot_age_distr <- function(nutrition_file, group_by_TEAM = FALSE){
  # First read the file
  nutrition_data <- rio::import(nutrition_file)
  ## First define the different factor for month
  nutrition_data <- nutrition_data |>
    dplyr::mutate(MONTHS = as.numeric(MONTHS)) |>
    dplyr::filter(!is.na(MONTHS)) |>
    dplyr::mutate(Interval = dplyr::case_when(
      MONTHS < 6 ~ "<6",
      MONTHS >= 6 & MONTHS < 12 ~ "6 to 12",
      MONTHS >= 12 & MONTHS < 18 ~ "12 to 18",
      MONTHS >= 18 & MONTHS < 24 ~ "18 to 24",
      MONTHS >= 24 & MONTHS < 30 ~ "24 to 30",
      MONTHS >= 30 & MONTHS < 36 ~ "30 to 36",
      MONTHS >= 36 & MONTHS < 42 ~ "36 to 42",
      MONTHS >= 42 & MONTHS < 48 ~ "42 to 48",
      MONTHS >= 48 & MONTHS < 54 ~ "48 to 54",
      MONTHS >= 54 ~ ">=54"
    ))|>
    # Reorder the factor levels of Interval
    dplyr::mutate(Interval = factor(Interval, 
                                    levels = c("<6", 
                                               "6 to 12", 
                                               "12 to 18", 
                                               "18 to 24",
                                               "24 to 30",
                                               "30 to 36", 
                                               "36 to 42", 
                                               "42 to 48",
                                               "48 to 54",
                                               ">=54")))
  if(group_by_TEAM == FALSE){
    # Create the bar plot
    plot <- ggplot2::ggplot(nutrition_data, ggplot2::aes(x = Interval)) +
      ggplot2::geom_bar(fill = "skyblue", alpha = 0.6, colour="black") +
      ggplot2::labs(x = "Age (months)",
                    y = "Number of children") +
      ggplot2::theme_bw()
    return(plot)
  }else{ # if we want to plot per Teams
    nutrition_data <- nutrition_data |>
      dplyr::mutate(TEAM = factor(TEAM))
    
    plot <- ggplot2::ggplot(nutrition_data, ggplot2::aes(x = Interval, fill = TEAM)) +
      ggplot2::geom_bar(position = ggplot2::position_stack(reverse = FALSE), alpha = 0.6, colour="black") +
      ggplot2::scale_fill_brewer(palette="viridis")+
      ggplot2::labs(x = "Age (months)",
                    y = "Number of children",
                    fill = "Team number:") +
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=40, hjust=1, vjust=1))
    return(plot)
  }
}