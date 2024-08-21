#' Function to plot gaussian curve for anthro results
#'
#' @param anthro_feature WHZ or WAZ, or HAZ
#' @param group NA or SEX
#' @param nutrition_file path of the nutrition file
#' @return plot
plot_gaussian_curve <- function(nutrition_file, 
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
    title = 'Weight-for-Height (WHZ)'
  }else if(anthro_feature == 'WAZ'){
    title = 'Weight-for-Age (WAZ)'
  }else{
    title = 'Height-for-Age (HAZ)'
  }
  ## Then check if it's group by sex or not
  if(group == 'All'){
    plot <- ggplot2::ggplot(nutrition_data, ggplot2::aes(x = .data[[anthro_feature]], color = 'Survey Values'))+
      ggplot2::stat_function(fun = dnorm, 
                             args = list(mean=mean(nutrition_data[[anthro_feature]]), sd=sd(nutrition_data[[anthro_feature]])),
                             size=2, alpha=0.6) + 
      ggplot2::scale_y_continuous(labels = function(x) x * 100) + 
      ggplot2::stat_function(data = data.frame(x = c(-5, 5)), ggplot2::aes(x, color = 'WHO standards'),
                             fun = dnorm, n = nrow(nutrition_data), 
                             args = list(mean = 0, sd = 1), size=2, alpha=0.6) + 
      ggplot2::scale_x_continuous(n.breaks = 11) +
      ggplot2::theme_bw() + 
      ggplot2::labs(y=paste('% of Children (n=', nrow(nutrition_data), ')'),
                    x = 'Z-score', 
                    title=title) +
      ggplot2::theme(legend.position = c(0.8, 0.8), 
                     legend.title=ggplot2::element_blank())+
      ggplot2::scale_color_manual(name = title, 
                                  breaks=c('WHO standards', 
                                           'Survey Values'),
                                  values=c('Survey Values'= '#56B4E9', 
                                           'WHO standards'= '#E69F00'))
  }else if(group == 'SEX'){
    # Separate both data
    boys_nutrition_data <- nutrition_data |> dplyr::filter(SEX == 0)
    girls_nutrition_data <-  nutrition_data |> dplyr::filter(SEX == 1)
    # Then plot using 3 colors
    plot <- ggplot2::ggplot() +
      ggplot2::stat_function(data =boys_nutrition_data, 
                             ggplot2::aes(x = .data[[anthro_feature]],
                                          color = 'Boys Values'), 
                             fun = dnorm, 
                             args = list(mean=mean(boys_nutrition_data[[anthro_feature]]), 
                                         sd=sd(boys_nutrition_data[[anthro_feature]])),
                             size=2, alpha = 0.6) + 
      ggplot2::stat_function(data = data.frame(x = c(-5, 5)), 
                             ggplot2::aes(x, color = 'WHO standards'),
                             fun = dnorm, n = nrow(nutrition_data), 
                             args = list(mean = 0, sd = 1), size=2, alpha = 0.6) + 
      ggplot2::stat_function(data = girls_nutrition_data, 
                             ggplot2::aes(x = .data[[anthro_feature]],
                             color = 'Girls Values'),
                             fun = dnorm, 
                             n = nrow(nutrition_data), 
                             args = list(mean=mean(girls_nutrition_data[[anthro_feature]]), 
                                         sd=sd(girls_nutrition_data[[anthro_feature]])), 
                             size=2, alpha = 0.6) + 
      ggplot2::scale_y_continuous(labels = function(x) x * 100) + 
      ggplot2::scale_x_continuous(n.breaks = 11) +
      ggplot2::theme_bw() + 
      ggplot2::labs(y=paste('% of Children (n=', nrow(nutrition_data), ')'),
                    x = 'Z-score', 
                    title=title) +
      ggplot2::theme(legend.position = c(0.8, 0.8), 
                     legend.title=ggplot2::element_blank())+
      ggplot2::scale_color_manual(name = title, 
                                  breaks=c('WHO standards', 
                                           'Boys Values',
                                           'Girls Values'),
                                  values=c('Girls Values'= '#56B4E9', 
                                           'Boys Values' = '#009E73',
                                            'WHO standards'= '#E69F00'))
  }
  return(plot)
}