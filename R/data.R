# path/to/your/smartcleaning/R/data_access.R

#' Get the path to the reference document
#'
#' @return A character string with the path to the reference_doc.docx
#' @export
get_reference_doc <- function() {
  system.file("extdata", "reference_doc.docx", package = "smartcleaning")
}