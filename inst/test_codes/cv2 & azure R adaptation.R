library(reticulate)
library(dplyr)
library(zeallot)
library(alvision)

img_file = "inst\\raw_data\\ACE Contrractors Pollution_2.png"
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
# Read the image
cropped_tm_dir <- 'inst/data/tmp_cropped/'
parse_df <- ocr_img_wrapper(img_file = "inst\\raw_data\\ACE Contrractors Pollution_2.png", hmax = 100,
                            cropped_tm_dir = 'inst/data/tmp_cropped/', azure_creds = azure_creds)

#parse_df <- readr::read_rds('inst/data/azure parsed results/ace page2.rds')
parse_df %>%
  purrrlyr::by_row(
    function(row) {

  })
