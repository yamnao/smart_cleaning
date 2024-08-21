#' Function to plot cumulative distribution
#'
#' @param nutrition_file path of the nutrition file
#' @return plot
plot_cumulative_curve <- function(nutrition_file){
  # First read the file and change the SEX type
  nutrition_data <- rio::import(nutrition_file)
  plot <- ggplot2::ggplot(nutrition_data, ggplot2::aes(MUAC))+
    ggplot2::stat_ecdf(geom = 'step', color='#56B4E9', size=2) + 
    ggplot2::scale_y_continuous(labels = function(x) x * 100) +
    ggplot2::scale_x_continuous(n.breaks = 11) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x='MUAC (mm)',
                  y = '%')
  return(plot)
}