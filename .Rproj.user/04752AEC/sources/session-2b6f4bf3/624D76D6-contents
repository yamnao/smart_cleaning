#' Function to plot distribution per cluster
#'
#' @param anthro_feature WHZ or WAZ, or HAZ
#' @param threshold NA or 2 or 3
#' @param nutrition_file path of the nutrition file
#' @return plot
plot_distribution_per_cluster <- function(nutrition_file, 
                              threshold){
  # Import the data
  nutrition_data <- rio::import(nutrition_file) 
  nutrition_data$bool_case <- ifelse(nutrition_data$WHZ < threshold, yes=1, no=0)
  
  if(threshold == -2){
    title = 'Moderate acute malnutrition (WHZ < -2Z)'
  }else{
    title = 'Severe acute malnutrition (WHZ < -3Z)'
  }
  
  # Supposons que vos colonnes sont 'cluster' et 'WHZ'
  # Identifier les patients malades (WHZ < -2)
  nutrition_data$malade <- ifelse(nutrition_data$WHZ < threshold, 1, 0)
  
  # Compter le nombre de malades par cluster
  cluster_counts <- nutrition_data |>
    dplyr::group_by(CLUSTER) |>
    dplyr::summarise(n_malades = sum(malade))
  
  # Compter combien de clusters ont un certain nombre de malades
  counts_by_n_malades <- cluster_counts |>
    dplyr::group_by(n_malades) |>
    dplyr::summarise(n_clusters = dplyr::n())
  
  # Estimation par la loi de Poisson
  lambda <- mean(cluster_counts$n_malades)
  mu <- mean(cluster_counts$n_malades, na.rm = TRUE)
  var <- var(cluster_counts$n_malades, na.rm = TRUE)
  size <- mu^2 / (var - mu)
  
  poisson_estimation <- data.frame(
    n_malades = 0:max(cluster_counts$n_malades),
    estimated_n_clusters = dpois(0:max(cluster_counts$n_malades), lambda) * sum(counts_by_n_malades$n_clusters)
  )
  
  # Estimation par la loi binomiale négative
  negative_binomial_estimation <- data.frame(
    n_malades = 0:max(cluster_counts$n_malades, na.rm = TRUE),  # Gérer NA dans max()
    estimated_n_clusters_neg = dnbinom(0:max(cluster_counts$n_malades, na.rm = TRUE), size = size, mu = mu) * sum(counts_by_n_malades$n_clusters, na.rm = TRUE)
  )
  
  # Fusionner les données observées et les estimations de Poisson pour le graphique
  plot_data <- counts_by_n_malades |>
    dplyr::full_join(poisson_estimation, by = "n_malades") |>
    dplyr::full_join(negative_binomial_estimation, by='n_malades')
  
  plot_data[is.na(plot_data$n_clusters),'n_clusters'] <- 0
  
  # Bar plot avec les estimations de Poisson superposées
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n_malades)) +
    ggplot2::geom_bar(ggplot2::aes(y = n_clusters), stat = "identity", fill = "#56B4E9", alpha = 0.6, colour="black") +
    ggplot2::geom_point(ggplot2::aes(y = estimated_n_clusters, color = "Poisson"), size = 3) +
    ggplot2::geom_line(ggplot2::aes(y = estimated_n_clusters, color = "Poisson")) +
    ggplot2::geom_point(ggplot2::aes(y = estimated_n_clusters_neg, color = "Negative Binomial"), size = 3) +
    ggplot2::geom_line(ggplot2::aes(y = estimated_n_clusters_neg, color = "Negative Binomial")) +
    ggplot2::labs(x = "Number of cases per cluster", y = "Number of cluster", title = paste(title, sep="")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_blank())+
    ggplot2::scale_y_continuous(name = "Number of clusters", sec.axis = ggplot2::sec_axis(~ .)) +
    ggplot2::scale_color_manual(values = c("Poisson" = "#E69F00", "Negative Binomial" = "#009E73")) 
  return(plot)
}