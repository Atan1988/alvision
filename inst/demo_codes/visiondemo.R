#library(reticulate)
library(dplyr)
library(zeallot)
library(alvision)

img_file = "inst\\raw_data\\ACE Contrractors Pollution_2.png"
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
# Read the image
cropped_tm_dir <- 'inst/data/tmp_cropped/'
reticulate::use_condaenv('computer_vision')

crop_out_boxes(img_file, hmax = 100) %->% c(img, img_bin, img_final_bin,
                                      contours, bounds_df, hierarchy)

des <- density(bounds_df$y, bw = 8, n = nrow(bounds_df), kernel = 'rectangular')
des_df <- tibble::tibble(x = des$x, y = des$y) %>%
  dplyr::mutate(peak = ifelse(y > dplyr::lead(y, default = 0) & y > dplyr::lag(y, default = 0), 1, 0),
                trough = ifelse(y < dplyr::lead(y, default = 0) & y < dplyr::lag(y, default = 0), 1, 0)
  ) %>%
  dplyr::filter(peak == 1)

bounds_df1 <- bounds_df %>%
  dplyr::left_join(
    purrr::cross_df(list(y = unique(bounds_df$y),
                         row = ceiling(des_df$x))) %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(dist = abs(y-row)) %>%
      dplyr::filter(abs(dist) == min(dist)),
    by = "y"
  ) %>%
  dplyr::arrange(row, x) %>%
  dplyr::group_by(row) %>%
  dplyr::mutate(col = seq(1, dplyr::n(), 1)) %>% dplyr::ungroup() %>%
  dplyr::mutate(idx = seq(1, dplyr::n(), 1))

pb <- dplyr::progress_estimated(nrow(bounds_df1))
bounds_df2 <- bounds_df1 %>% .[1:10, ] %>%
  purrrlyr::by_row(
    function(row) {
      res <- get_ocr_azure(row, cropped_dir_path = cropped_tm_dir, img, azure_creds, F)
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
