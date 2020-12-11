#' @title pdf_txt_df to az table
#' @param pdf_txt_df result of pdf_data function of pdftools
#' @export
pdf_txt_to_az_df <- function(pdf_txt_df) {
  pdf_txt_df %>% dplyr::rename(w = width, h = height)
}