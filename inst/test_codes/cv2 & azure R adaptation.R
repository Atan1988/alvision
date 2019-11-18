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
parse_df1 <- parse_df %>%
  dplyr::group_by(row) %>%
  dplyr::mutate(col = seq(1, dplyr::n(), 1)) %>%
  purrrlyr::by_row(
    function(row) {
      df <- row$.out[[1]]
      if (nrow(df) == 0) return("")
      df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
  }, .to = '.txt') %>% tidyr::unnest(cols = '.txt') %>% dplyr::select(-`.txt`)

parse_df2 <- parse_df1 %>% select(row, col, txt) %>%
  tidyr::pivot_wider(names_from = col, values_from = txt)
