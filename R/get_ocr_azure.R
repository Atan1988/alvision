#'@title get ocr from azure
#'@param df the data frame with contour info
#'@param cropped_dir_path tmp path to output cropped img
#'@param img original image
#'@param azure_creds credential for azure app
#'@export
get_ocr_azure <- function(df, cropped_dir_path, img, azure_creds) {
  cropped_img <- output_cropped_img(normalizePath(cropped_dir_path), img,
                                    df$idx, df$x, df$y, df$w, df$h)

  analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                            endpoint = azure_creds$endpoint, image_path = normalizePath(cropped_img))

  tidy_res <- analysis_res$recognitionResult$lines %>%
    purrr::map_df(function(x){
      boxes <- x$boundingBox; txt <- x$text
      x <- boxes[1]; y <- boxes[2]
      w <- mean(abs(boxes[1] - boxes[3]), abs(boxes[5] - boxes[7]))
      h <- mean(abs(boxes[2] - boxes[8]), abs(boxes[4] - boxes[6]))
      tibble::tibble(txt = txt, x = x, y = y, w = w, h = h)
    })
  unlink(cropped_img)
  return(tidy_res)
}

#' @title ocr image wrapper
#' @param img_file img file path
#' @param hmax maximum box height to include
#' @param cropped_tm_dir temporary dir for cropped img
#' @param azure_creds azure credential
#' @export
ocr_img_wrapper <- function(img_file, hmax = 100, cropped_tm_dir, azure_creds) {
  crop_out_boxes(normalizePath(img_file), hmax) %->% c(img, img_bin, img_final_bin,
                                                       contours, bounds, hierarchy)
  #cv2$imwrite("Image_bin.jpg", img_bin)
  # Sort all the contours by top to bottom.
  #sort_contours(contours, method="top-to-bottom") %->% c(contours_sorted, boundingBoxes)

  #bounds <- get_crop_bounds(contours_sorted, hmax)
  names(bounds) <- c('x', 'y', 'w', 'h')
  bounds_df <- bounds %>% tibble::as_tibble()

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
  bounds_df2 <- bounds_df1 %>% #.[1:10, ] %>%
    purrrlyr::by_row(
      function(row) {
        res <- get_ocr_azure(row, cropped_dir_path = cropped_tm_dir, img, azure_creds)
        #Sys.sleep(2)
        pb$tick()$print()
        return(res)
      }
    )
  return(bounds_df2)
}
