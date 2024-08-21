#' Function to plot probbility plot for anthro results
#'
#' @param anthro_feature WHZ or WAZ, or HAZ
#' @param group NA or SEX
#' @param nutrition_file path of the nutrition file
#' @return plot
plot_proba_curve <- function(nutrition_file, 
                             anthro_feature,
                             group){
  # First read the file and change the SEX type
  nutrition_data <- rio::import(nutrition_file) |>
    dplyr::mutate(SEX = dplyr::recode(SEX,
                                      'm' = '0',
                                      'f' = '1'), 
                  SEX = as.numeric(SEX), 
                  AGE = as.numeric(MONTHS)*(365.25/12))
  if(anthro_feature == 'WHZ'){
    title = 'Probability for WHZ'
  }else if(anthro_feature == 'WAZ'){
    title = 'Probability for WAZ'
  }else{
    title = 'Probability for HAZ'
  }
  ## Then check if it's group by sex or not
  if(group == 'All'){
    plot <- ggplot2::ggplot()+
      ggplot2::stat_qq(data = nutrition_data,
                       ggplot2::aes(sample = .data[[anthro_feature]]),
                             size=2, color='#56B4E9', size = 0.2, alpha = 0.6) + 
      ggplot2::stat_qq_line(data = nutrition_data,
                            ggplot2::aes(sample = .data[[anthro_feature]]),
                            color='#E69F00', size=1.5, alpha = 0.6)+
      ggplot2::theme_bw() + 
      ggplot2::labs(y='Normal quantiles - observed',
                    x ='Normal quantiles - theorical', 
                    title=title) +
      ggplot2::scale_y_continuous(limits = c(-5, 5), n.breaks = 11) +
      ggplot2::scale_x_continuous(limits = c(-5, 5), n.breaks = 11)+
      ggh4x::coord_axes_inside(labels_inside = TRUE)
  }else if(group == 'SEX'){
    # Separate both data
    boys_nutrition_data <- nutrition_data |> dplyr::filter(SEX == 0)
    girls_nutrition_data <-  nutrition_data |> dplyr::filter(SEX == 1)
    # Then plot using 3 colors
    plot <- ggplot2::ggplot()+
      ggplot2::stat_qq(data = boys_nutrition_data,
                       ggplot2::aes(sample = .data[[anthro_feature]], 
                                    color='Boys Values'),
                       size=2) + 
      ggplot2::stat_qq_line(data = boys_nutrition_data,
                            ggplot2::aes(sample = .data[[anthro_feature]],
                                         color='Boys Values')
                            )+
      ggplot2::stat_qq(data = girls_nutrition_data,
                       ggplot2::aes(sample = .data[[anthro_feature]], 
                                    color='Girls Values'),
                       size=2) + 
      ggplot2::stat_qq_line(data = girls_nutrition_data,
                            ggplot2::aes(sample = .data[[anthro_feature]],
                                         color='Girls Values'))+
      ggplot2::theme_bw() + 
      ggplot2::labs(y=ggplot2::element_blank(),
                    x = ggplot2::element_blank(), 
                    title=title) +
      ggplot2::scale_y_continuous(limits = c(-5, 5), n.breaks = 11) +
      ggplot2::scale_x_continuous(limits = c(-5, 5), n.breaks = 11)+
      ggh4x::coord_axes_inside(labels_inside = TRUE) +
      ggplot2::theme(legend.position = c(0.8, 0.8))+
      ggplot2::scale_color_manual(name = title, 
                                  breaks=c('Boys Values',
                                           'Girls Values'),
                                  values=c('Girls Values'= 'firebrick1', 
                                           'Boys Values' = 'blue'))
  }
  return(plot)
}