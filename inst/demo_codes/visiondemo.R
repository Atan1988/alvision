#library(reticulate)
library(dplyr)
library(zeallot)
library(alvision)

img_file = "inst/raw_data/ACE Contrractors Pollution_2.png"
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
# Read the image
cropped_tm_dir <- 'inst/data/tmp_cropped/'
reticulate::use_condaenv('computer_vision')

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
bounds_list <- bbox_df_to_c(bounds_df)

match_idx <- res_lines %>% purrr::map(~pts_to_wh(.$boundingBox)) %>%
  purrr::map_dbl(function(x) {
    res <- bounds_list %>% purrr::map_lgl(~chk_box_in(., x, 10)) %>% which(.)
    if (length(res) == 0) return(NA)
    return(res)
  })


# cv2$imwrite('img.png', img)
# cv2$imwrite('img bin.png', img_bin)
# cv2$imwrite('img final bin.png', img_final_bin)
bounds_df1 <- add_rc_bbox(bbox_df = bounds_df)
bounds_df1$az <- 1:nrow(bounds_df1) %>%  purrr::map(
  function(x) {
    idx <- which(match_idx == x)
    if (length(idx) == 0) return(list())
    return(res_lines[idx])
  }
)

pb <- dplyr::progress_estimated(nrow(bounds_df1))
bounds_df2 <- bounds_df1 %>% .[1:10, ] %>%
  purrrlyr::by_row(
    function(row) {
      res <- get_ocr_azure(row, cropped_dir_path = cropped_tm_dir,
                           img, azure_creds, remove_fl = F)
      pb$tick()$print()
      return(res)
    }
  )

#parse_df <- readr::read_rds('inst/data/azure parsed results/ace page2.rds')
parse_df1 <- bounds_df2 %>%
  purrrlyr::by_row(
    function(row) {
      df <- row$.out[[1]]
      if (nrow(df) == 0) return("")
      df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
    }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')

parse_df2 <- parse_df1 %>% select(row, col, txt) %>%
  tidyr::pivot_wider(names_from = col, values_from = txt)
