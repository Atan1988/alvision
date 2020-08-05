library(zeallot)
library(alvision)
library(tictoc)

#test docker
#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readxl::read_excel("inst/creds/azure creds.xlsx")
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/Large Loss Detail_Redacted.pdf"

tic()
image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = dpi)
toc()