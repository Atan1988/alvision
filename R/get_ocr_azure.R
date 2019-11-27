#'@title get ocr from azure
#'@param df the data frame with contour info
#'@param cropped_dir_path tmp path to output cropped img
#'@param img original image
#'@param azure_creds credential for azure app
#'@param box_highlight whether to have the additional step of flagging the cropped image
#'@param remove_fl whether to remove images
#'@export
get_ocr_azure <- function(df, cropped_dir_path, img, azure_creds,
                          box_highlight = T, remove_fl = T) {
  cropped_img <- output_cropped_img(normalizePath(cropped_dir_path), img,
                                    df$idx, df$x, df$y, df$w, df$h)

  if (box_highlight) {
    c(flagged_img, flag_cnts) %<-% crop_out_obj(image_file = cropped_img,
                                          output_cropped = F, output_dir = NULL)

    cv2$imwrite(cropped_img, flagged_img)

    az_area <- df$az[[1]] %>% purrr::map_dbl(az_line_area) %>% sum()
    flag_area <- flag_cnts %>% purrr::map_dbl(~cv2$contourArea(.)) %>% sum()
    if (az_area >= flag_area) {
      line_res <- df$az %>% .[[1]]
    } else {
      analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                                endpoint = azure_creds$endpoint,
                                image_path = normalizePath(cropped_img))
      line_res <- analysis_res$recognitionResult$lines
    }
  } else {
    analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                              endpoint = azure_creds$endpoint,
                              image_path = normalizePath(cropped_img))
    line_res <- analysis_res$recognitionResult$lines
  }

  tidy_res <- line_res %>%
    purrr::map_df(function(x){
      boxes <- x$boundingBox; txt <- x$text
      x <- boxes[1]; y <- boxes[2]
      w <- mean(abs(boxes[1] - boxes[3]), abs(boxes[5] - boxes[7]))
      h <- mean(abs(boxes[2] - boxes[8]), abs(boxes[4] - boxes[6]))
      tibble::tibble(txt = txt, x = x, y = y, w = w, h = h)
    })
  if(remove_fl) unlink(cropped_img)
  return(tidy_res)
}


#'@title post cropped image to azure
#'@param df the data frame with contour info
#'@param cropped_dir_path tmp path to output cropped img
#'@param img original image
#'@param azure_creds credential for azure app
#'@param push_to_az whether to push to azure
#'@param box_highlight whether to have the additional step of flagging the cropped image
#'@param remove_fl whether to remove images
#'@export
post_cropped_azure  <- function(df, cropped_dir_path, img, azure_creds, push_to_az = T,
                                box_highlight = T, remove_fl = T) {
  if (!push_to_az) {
    line_res <- list(flag = "not run", result = df$az %>% .[[1]],
                     time = Sys.time() - start)
    return(line_res)
  }
  start <- Sys.time()
  cropped_img <- output_cropped_img(normalizePath(cropped_dir_path), img,
                                    df$idx, df$x, df$y, df$w, df$h)

  if (box_highlight) {

    c(flagged_img, flag_cnts) %<-% crop_out_obj(image_file = cropped_img,
                                                output_cropped = F, output_dir = NULL)

    cv2$imwrite(cropped_img, flagged_img)


    az_area <- df$az[[1]] %>% purrr::map_dbl(az_line_area) %>% sum()
    flag_area <- flag_cnts %>% purrr::map_dbl(~cv2$contourArea(.)) %>% sum()

    if (az_area >= flag_area) {
      line_res <- list(flag = "not run", result = df$az %>% .[[1]],
                       time = Sys.time() - start)
    } else {
      tictoc::tic()
      line_res <- list(flag = 'run',
                       result = azure_post(subscription_key = azure_creds$subscription_key,
                                          endpoint = azure_creds$endpoint,
                                          image_path = normalizePath(cropped_img)),
                       time = Sys.time() - start
                       )
      tictoc::toc()
    }
    return(line_res)
  }
  line_res <- list(flag = 'run',
                   result = azure_post(subscription_key = azure_creds$subscription_key,
                                       endpoint = azure_creds$endpoint,
                                       image_path = normalizePath(cropped_img)),
                   time = Sys.time() - start
  )
  return(line_res)
}

#'@title vectorized post azure
#'@param df bounds dataframe
#'@param cropped_tm_dir temporary dir to output cropped img
#'@param img image
#'@param azure_cred azure credentials
#'@param box_highlight whether to have the additional step of flagging the cropped image
#'@param remove_fl whether to remove cropped files
#'@export
vec_post_cropped_azure <- function(df, cropped_tm_dir, img,
                                   azure_creds, box_highlight = F, remove_fl = F) {
  tictoc::tic()
  pb <- dplyr::progress_estimated(nrow(df))
  df2 <- df %>%
    purrrlyr::by_row(
      function(row) {
        res <- post_cropped_azure(row, cropped_dir_path = cropped_tm_dir,
                                  img, azure_creds, box_highlight = box_highlight,
                                  remove_fl = remove_fl)
        pb$tick()$print()
        return(res)
      }
    )
  tictoc::toc()
  return(df2)
}

#'@title post cropped image to azure
#'@param df the data frame with contour info
#'@export
get_cropped_azure <- function(res) {
  if (res$flag == "not run") lines <- res$result
  lines <-azure_get(response = res$result[[1]],
                headers = res$result[[2]])$recognitionResult$lines
  lines %>%
         purrr::map_df(function(x){
                boxes <- x$boundingBox; txt <- x$text
                x <- boxes[1]; y <- boxes[2]
                w <- mean(abs(boxes[1] - boxes[3]), abs(boxes[5] - boxes[7]))
                h <- mean(abs(boxes[2] - boxes[8]), abs(boxes[4] - boxes[6]))
                tibble::tibble(txt = txt, x = x, y = y, w = w, h = h)
          })
}

#'@title get response from azure api, vectorized
#'@param df the data frame from vec_post_cropped_azure
#'@export
vec_get_cropped_azure <- function(df) {
  tictoc::tic()
  pb <- dplyr::progress_estimated(nrow(df))
  df3 <- df %>%
    purrrlyr::by_row(
      function(row) {
        res <- get_cropped_azure(row$.out[[1]]); pb$tick()$print()
        return(res)
      }, .to = 'get_res')
  tictoc::toc()
  return(df3)
}

#' @title ocr image wrapper
#' @param img_file img file path
#' @param hmax maximum box height to include
#' @param cropped_tm_dir temporary dir for cropped img
#' @param azure_creds azure credential
#' @export
ocr_img_wrapper <- function(img_file, hmax = 100, cropped_tm_dir, azure_creds) {
  crop_out_boxes(img_file, hmax) %->% c(img, img_bin, img_final_bin,
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
