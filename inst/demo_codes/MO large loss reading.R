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

hmax = 300; cropped_tm_dir; azure_creds;
box_push_to_az = F; box_highlight = F; remove_fl = F; dpi = 400
main_cl <- 1; sub_cl <- 3


imgs <- pdftools::pdf_convert(pdf_file, dpi = 400)
tictoc::tic()
imgs %>% purrr::map(function(im_fl)
  imager::load.image(im_fl ) %>% imager::imrotate(90) %>% imager::grayscale() %>% imager::save.image(im_fl))
tictoc::toc()

im_fl <- imgs[1]
tictoc::tic()
analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                          endpoint = azure_creds$endpoint,
                          image_path = normalizePath(im_fl))
saveRDS(analysis_res, 'analysis_res.rds')
analysis_res <- readr::read_rds('analysis_res.rds')
analysis_res$recognitionResult$lines -> res_lines
tictoc::toc()
