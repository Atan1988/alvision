#library(reticulate)
library(dplyr)
library(zeallot)
library(alvision)

#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/ACE Contrractors Pollution.pdf"
image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL)

img_file <- image_files[2]

# Read the image
resize_fl <- paste0('resize-full ', img_file)
raw_img <- magick::image_read(img_file)
raw_img %>% magick::image_resize('3500x3500') %>%
  magick::image_quantize(colorspace = 'gray') %>%
  magick::image_write('resize-full 2.png')

main_img <-  'resize-full 2.png'
analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                          endpoint = azure_creds$endpoint,
                          image_path = normalizePath(main_img ))
analysis_res$recognitionResult$lines -> res_lines

crop_out_boxes(main_img, hmax = 100) %->% c(img, img_bin, img_final_bin,
                                      contours, bounds_df, hierarchy)

bounds_df1 <- az_to_cv2_box(bounds_df, res_lines)

bounds_df2 <- vec_post_cropped_azure(bounds_df1, cropped_tm_dir = cropped_tm_dir, img,
                          azure_creds, box_highlight = F, remove_fl = F)

bounds_df3 <- vec_get_cropped_azure(bounds_df2)

#parse_df <- readr::read_rds('inst/data/azure parsed results/ace page2.rds')
parse_df1 <- bounds_df3 %>%
  purrrlyr::by_row(
    function(row) {
      df <- row$get_res[[1]]
      if (nrow(df) == 0) return("")
      df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
    }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')

parse_df2 <- parse_df1 %>% select(row, col, txt) %>%
  tidyr::pivot_wider(names_from = col, values_from = txt)
