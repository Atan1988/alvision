library(zeallot)
library(alvision)
library(tictoc)
library(imager)

#test docker
reticulate::use_condaenv('computer_vision')
#reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readxl::read_excel("inst/creds/azure creds.xlsx")
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/Large Loss Detail_Redacted.pdf"
#pdf_file <- 'inst/raw_data/tmp.pdf'

imgs <- pdftools::pdf_convert(pdf_file, dpi = 400, pages = 1:17)
tictoc::tic()
imgs %>% purrr::map(function(im_fl){
  im <- imager::load.image(im_fl ) %>% imager::imrotate(90) %>% imager::grayscale() 
  dims <- dim(im)
  if (max(dims)>4000){
    scale <- 4000 / max(dims)
    im <- im %>% imager::resize(size_x = scale * dims[1], size_y = scale * dims[2])
  }
  im %>% imager::save.image(im_fl)
})
tictoc::toc()

im_fl <- imgs[1]
col_header <- c('Suffix', 'Lust', "Type", 'Site', 'Owner', "Name", 'Reserve', 'Paid', 
                'Incurred', 'Hours', 'S')
MO_large_loss_read <- function(im_fl, col_header){
  analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                            endpoint = azure_creds$endpoint,
                            image_path = normalizePath(im_fl))
  saveRDS(analysis_res, 'analysis_res.rds')
  analysis_res <- readr::read_rds('analysis_res.rds')
  analysis_res$recognitionResult$lines -> res_lines
  tidy_az_res <- alvision::az_lines_to_df(res_lines)
  res <- tidytbl_to_r(tidy_az_res, col_header)
  colnames(res) <- tolower(c('row_id', as.vector(res[1, ]) %>% .[-1]))
  res <- res[-1, ]
  colnames(res) <- make.names(stringr::str_squish(colnames(res)), unique = T)
  return(res)
}

MO_large_loss_read_safely <- purrr::safely(MO_large_loss_read)

tictoc::tic()
large_loss_df <- imgs[1:17] %>% 
  purrr::map(function(x){
    res <- MO_large_loss_read_safely(x, col_header)
    result <- res$result
    if (is.null(result)) {print(x); print(res$error)}
    if (!is.null(result)) result <- result %>% dplyr::mutate(pg = x)
    return(result)
  }) %>% dplyr::bind_rows()
tictoc::toc()
