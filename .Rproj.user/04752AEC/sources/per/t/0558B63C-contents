#' Function to generate word document with plausibility score
#'
#' @param path_data Path for cluster data
#' @return [data.frame] Clean cluster data
#' @export
generate_word_doc<- function(item, information, score_list, flagged_data){
  ## Define typology caption 
  # Define the text properties
  caption_text_style <- officer::fp_text(
    font.size = 10,        # Font size
    font.family = "Arial", # Font family
    bold = FALSE,           # Bold text
    color = "#000000"      # Text color (black)
  )
  
  
  # Load the reference document with Arial font styles
  
  doc <- officer::read_docx(system.file("extdata", "reference_doc.docx", package = "smartcleaning"))
  
  # # Create the word document
  # doc <- officer::read_docx()
  
  # Generate the title
  doc <- doc |>                                                                                                                                                                                                                                     
    officer::body_add_par(paste("REASSURE ", sep=""), style = "heading 1") |>
    officer::body_add_par(paste("SMART survey anthropometric data quality report ", sep=""), style = "heading 1") |>
    officer::body_add_par(" ", style = "Normal")
  
  # Add few information about the surveu
  doc <- doc |>
    officer::body_add_par(paste('Country: ', 
                                stringr::str_split(information[information$type_info == 'new_name',]$values, "_")[[1]][1], 
                                sep=''), 
                          style='Normal') |>
    officer::body_add_par(paste('Admin Level: ', 
                                information[information$type_info == 'admin_name_survey',]$values, 
                                sep=''), 
                          style='Normal') |>
    officer::body_add_par(paste('Month and year in which survey was completed: ', 
                                month.name[as.numeric(information[information$type_info == 'max_month_survey',]$values)],
                                ' ',
                                as.numeric(information[information$type_info == 'max_year_survey',]$values), 
                                sep=''), 
                          style='Normal') |>
    officer::body_add_par(paste('Raw dataset filename: ', 
                                information[information$type_info == 'old_name',]$values, 
                                sep=''), 
                          style='Normal')|>
    officer::body_add_par(" ", style = "Normal") 
  
  # officer::body_add_par(paste("REASSURE ", information[information$type_info == 'old_name',]$values, sep=""), style = "heading 1")
  ## OVERALL SECTION - TABLE ------- 
  doc <- officer::body_add_break(doc)
  doc <- doc |>
    officer::body_add_par("Overall anthropometric data plausibility score", style = "heading 2")|>
    officer::body_add_par(" ", style = "Normal")
  score_list_table <- f_generate_score_table_word(score_list)
  doc <- f_modify_table_style(doc, score_list_table)
  doc <- doc |>
    officer::body_add_par(paste("The overall score of this survey is ",
                                100-score_list$overall_score,
                                '% (100 - ', score_list$overall_score, 'penalty_points): This is ', f_type_score(score_list$overall_score), sep=""), style = "Normal") |>
    officer::body_add_par(" ", style = "Normal") |>
    officer::body_add_par(paste("Note: the above table reproduces the plausibility score calculations performed by the ENA software.", 
                          "For each criterion, a certain number of penalty percentage points are deducted from the best-possible score of 100%.", 
                          "The best-quality surveys are those with the smallest number of penalty points.", sep=" "))|>
    officer::body_add_par(" ", style = "Normal")
  
  ## FLAGGED SECTION - TABLE -------
  doc <- officer::body_add_break(doc)
  doc <- doc |>
    officer::body_add_par("List of flagged anthropometric observations", style = "heading 2") |>
    officer::body_add_par(" ", style = "Normal")
  doc <- f_modify_table_style(doc, flagged_data)|>
    officer::body_add_par(" ", style = "Normal")
  
  ## AGE SECTION - TABLE + VISUZALITIOn-------
  doc <- officer::body_add_break(doc)
  doc <- doc |>
    officer::body_add_par("Age completeness and distribution", style = "heading 2") |>
    officer::body_add_par(" ", style = "Normal")
  
  doc <- doc |>
    officer::body_add_par(paste("Percentage of children with no exact birthday: ",
                                score_list$percent_no_exact_birth, '%.', sep=""), style = "Normal") |>
    officer::body_add_par(" ", style = "Normal")

  plot1 <- plot_anthro(folder_name = item,
                       exclusion = 'without',
                       type_of_plot = 'age',
                       group = 'All')
  doc <- doc |> officer::body_add_gg(value = plot1 ,
                                     height = 4,
                                     width = 6)|> 
    officer::body_add_fpar(officer::fpar(officer::ftext("Figure 1: Age Distribution", prop = caption_text_style)), style = "Normal")
  
  
  ## DATA QUALITY SECTIOn -- PLOTS --------------------------------
  doc <- officer::body_add_break(doc)
  doc <- doc |>
    officer::body_add_par("Visualization of data quality patterns", style = "heading 2") |>
    officer::body_add_par(" ", style = "Normal")
  doc <- doc |>
    officer::body_add_par("Distribution of anthropometric indices (before exclusion of flagged observations)", style = "heading 3") |>
    officer::body_add_par(" ", style = "Normal")

  #Concat anthro plot
  plot1 <- plot_anthro(folder_name = item,
                       exclusion = 'with',
                       type_of_plot = 'gaussian_curve_WHZ',
                       group = 'All')
  plot2 <- plot_anthro(folder_name = item,
                      exclusion = 'with',
                      type_of_plot = 'gaussian_curve_HAZ',
                      group = 'All')
  plot3 <- plot_anthro(folder_name = item,
                      exclusion = 'with',
                      type_of_plot = 'gaussian_curve_WAZ',
                      group = 'All')

  get_legend <- function(my_plot) {
    tmp <- ggplot2::ggplotGrob(my_plot)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  legend <- get_legend(plot1)

  plot1 <- plot1 + ggplot2::theme(legend.position = "none")
  plot2 <- plot2 + ggplot2::theme(legend.position = "none")
  plot3 <- plot3 + ggplot2::theme(legend.position = "none")

  combined_plot <- cowplot::plot_grid(plot1, plot2, plot3,
                                      ncol = 2,
                                      legend)
  #Plot anthro plot
  doc <- doc |> officer::body_add_gg(value = combined_plot)|> 
    officer::body_add_fpar(officer::fpar(officer::ftext("Figure 2: Distribution of WHZ, HAZ, WAZ (before exclusion of flagged observations)", prop = caption_text_style)), style = "Normal")|>
    officer::body_add_par(" ", style = "Normal")
  
  #Concat gaussian curve after exclusion
  doc <- doc |>
    officer::body_add_par("Distribution of anthropometric indices (after exclusion of flagged observations)", 
                          style = "heading 3") |>
    officer::body_add_par(" ", style = "Normal")
  plot1 <- plot_anthro(folder_name = item,
                      exclusion = 'without',
                      type_of_plot = 'gaussian_curve_WHZ',
                      group = 'All')
  plot2 <- plot_anthro(folder_name = item,
                      exclusion = 'without',
                      type_of_plot = 'gaussian_curve_HAZ',
                      group = 'All')
  plot3 <- plot_anthro(folder_name = item,
                      exclusion = 'without',
                      type_of_plot = 'gaussian_curve_WAZ',
                      group = 'All')
  legend <- get_legend(plot1)

  plot1 <- plot1 + ggplot2::theme(legend.position = "none")
  plot2 <- plot2 + ggplot2::theme(legend.position = "none")
  plot3 <- plot3 + ggplot2::theme(legend.position = "none")

  combined_plot <- cowplot::plot_grid(plot1, plot2, plot3,
                                      ncol = 2,
                                      legend)

  doc <- doc |> officer::body_add_gg(value = combined_plot)|> 
    officer::body_add_fpar(officer::fpar(officer::ftext("Figure 3: Distribution of WHZ, HAZ, WAZ (after exclusion of flagged observations)", prop = caption_text_style)), style = "Normal")|>
    officer::body_add_par(" ", style = "Normal")
  
    #Concat gaussian curve after exclusion group by sex
    doc <- doc |>
      officer::body_add_par("Distribution of anthropometric indices by sex (after exclusion of flagged observations)",
                            style = "heading 3") |>
      officer::body_add_par(" ", style = "Normal")
  
    plot1 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'gaussian_curve_WHZ',
                        group = 'SEX')
    plot2 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'gaussian_curve_HAZ',
                        group = 'SEX')
    plot3 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'gaussian_curve_WAZ',
                        group = 'SEX')
    legend <- get_legend(plot1)
  
    plot1 <- plot1 + ggplot2::theme(legend.position = "none")
    plot2 <- plot2 + ggplot2::theme(legend.position = "none")
    plot3 <- plot3 + ggplot2::theme(legend.position = "none")
  
    combined_plot <- cowplot::plot_grid(plot1, plot2, plot3,
                                        ncol = 2,
                                        legend)
  
    doc <- doc |> officer::body_add_gg(value = combined_plot)|> 
      officer::body_add_fpar(officer::fpar(officer::ftext("Figure 4: Distribution of WHZ, HAZ, WAZ by Sex (after exclusion of flagged observations)", prop = caption_text_style)), style = "Normal")|>
      officer::body_add_par(" ", style = "Normal")
    
    #Concat muac plot
    doc <- doc |>
      officer::body_add_par("Cumulative distribution of MUAC", style = "heading 3") |>
      officer::body_add_par(" ", style = "Normal")
    
    plot <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'cum_muac',
                        group = 'All')
    doc <- doc |> officer::body_add_gg(value = plot,
                                width = 4,
                                height = 3)|> 
      officer::body_add_fpar(officer::fpar(officer::ftext("Figure 5: Cumulative distribution of MUAC", prop = caption_text_style)), style = "Normal")|>
      officer::body_add_par(" ", style = "Normal")
      
    #Concat probability plot
    doc <- doc |>
      officer::body_add_par("Quantile-quantile plots of anthropometric indices (after exclusion of flagged observations)", 
                            style = "heading 3") |>
      officer::body_add_par(" ", style = "Normal")
    
    plot1 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'prob_plot_WHZ',
                        group = 'All')
    plot2 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'prob_plot_HAZ',
                        group = 'All')
    plot3 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                        type_of_plot = 'prob_plot_WAZ',
                        group = 'All')
  
    combined_plot <- cowplot::plot_grid(plot1, plot2, plot3,
                                        ncol = 2)
    doc <- doc |> officer::body_add_gg(value = combined_plot)|> 
      officer::body_add_fpar(officer::fpar(officer::ftext("Figure 6: Quantile-quantile plots of WHZ, WAZ and HAZ", prop = caption_text_style)), style = "Normal")|>
      officer::body_add_par(" ", style = "Normal")|>
      officer::body_add_par(paste("Note: Quantile-quantile plots are a good way to check whether data are distributed according to a normal distribution,", 
                                  "which is expected for anthropometric data, and suggests good data quality.", 
                                  "If the dots in the graphs align closely with the diagonal line, the data are distributed normally.", sep=" "))|>
      officer::body_add_par(" ", style = "Normal")
    
    
    
    #Concat cluster distribution
    doc <- doc |>
      officer::body_add_par("Distribution of moderate and severe acute malnutrition cases by survey cluster",
                            style = "heading 3") |>
      officer::body_add_par(" ", style = "Normal")
    plot1 <- plot_anthro(folder_name = item,
                         exclusion = 'without',
                         type_of_plot = 'distr_WHZ_2',
                         group = 'All')
    plot2 <- plot_anthro(folder_name = item,
                         exclusion = 'without',
                         type_of_plot = 'distr_WHZ_3',
                         group = 'All')
    legend <- get_legend(plot1)
  
    plot1 <- plot1 + ggplot2::theme(legend.position = "none")
    plot2 <- plot2 + ggplot2::theme(legend.position = "none")
  
    combined_plot <- cowplot::plot_grid(plot1, plot2,
                                        ncol = 1,
                                        rel_heights = c(2, 2))
    combined_plot <- cowplot::plot_grid(
      combined_plot,
      legend,
      ncol = 2,
      rel_widths = c(3.5, 1)  # Adjust widths
    )
  
    doc <- doc |> officer::body_add_gg(value = combined_plot,
                                       height = 8,
                                       width = 6)|> 
      officer::body_add_fpar(officer::fpar(officer::ftext("Figure 7: Distribution of moderate and severe acute malnutrition cases", prop = caption_text_style)), style = "Normal")|>
      officer::body_add_par(" ", style = "Normal")|>
      officer::body_add_par(paste("Note: If MAM and SAM cases are randomly distributed across the survey clusters, they will be distributed more or less according to a Poisson distribution; if, on the other hand, they are clustered in specific locations where data were collected, a negative binomial distribution will fit the data better.", 
                                  "Clustering of SAM and MAM cases may reflect genuinely worse conditions in some survey clusters, but it could also be a sign of problems with the quality of data collection in some locations.",
                                  sep=" "))|>
      officer::body_add_par(" ", style = "Normal")
    
  ## ANALYSIS BY TEAM -TABLE  --------------------------------
  doc <- officer::body_add_break(doc)
  doc <- doc |>
    officer::body_add_par("Indicators by survey team", style = "heading 2") |>
    officer::body_add_par(" ", style = "Normal")
  # Add table
  table <- table_analysis_per_team(folder_name = item)
  
  doc <- f_modify_table_style(doc, table) |> 
    officer::body_add_par(paste("Note: Obvious differences among survey teams may indicate data collection problems, and warrant further investigation.", 
                                sep=" "))|>
    officer::body_add_par(" ", style = "Normal")
  
  plot2 <- plot_anthro(folder_name = item,
                        exclusion = 'without',
                       type_of_plot = 'age_team',
                       group = 'All')
  
  doc <- doc |> officer::body_add_gg(value = plot2 ,
                                     height = 4,
                                     width = 6)|> 
    officer::body_add_fpar(officer::fpar(officer::ftext("Figure 8: Age Distribution per Team", prop = caption_text_style)), style = "Normal")|>
    officer::body_add_par(" ", style = "Normal")
  
  #SAVE THE DOC -------------------
  name <- gsub("[ .-]", "_", strsplit(information[information$type_info == 'old_name',]$values, split = ".as")[[1]][1])
  name <- substr(name, start=1, stop=30)
  print(doc, target = paste(item, '/', "plausibility_check_", name,".docx", sep=""))
}