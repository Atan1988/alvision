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

crop_out_boxes(normalizePath(img_file)) %->% c(img, img_bin, img_final_bin, contours, hierarchy)

cv2$imwrite("Image_bin.jpg", img_bin)
# Sort all the contours by top to bottom.
sort_contours(contours, method="top-to-bottom") %->% c(contours_sorted, boundingBoxes)

bounds <- get_crop_bounds(contours_sorted, 100)
names(bounds) <- c('x', 'y', 'w', 'h')
bounds_df <- bounds %>% tibble::as_tibble()

des <- density(bounds_df1$y, bw = 8, n = nrow(bounds_df1), kernel = 'rectangular')
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
  dplyr::arrange(row, x)

pb <- dplyr::progress_estimated(nrow(bounds_df1))
bounds_df2 <- bounds_df1 %>% .[1:10, ] %>%
  purrrlyr::by_row(
    function(row) {
      res <- get_ocr_azure(row, cropped_dir_path = cropped_tm_dir, img, azure_creds)
      #Sys.sleep(2)
      pb$tick()$print()
      return(res)
    }
  )
