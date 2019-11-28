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
  start <- Sys.time()
  if (!push_to_az) {
    line_res <- list(flag = "not run", result = df$az %>% .[[1]],
                     time = Sys.time() - start)
    return(line_res)
  }

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
#'@param push_to_az whether to push to azure
#'@param box_highlight whether to have the additional step of flagging the cropped image
#'@param remove_fl whether to remove cropped files
#'@export
vec_post_cropped_azure <- function(df, cropped_tm_dir, img,
                                   azure_creds, push_to_az = T,
                                   box_highlight = F, remove_fl = F) {
  tictoc::tic()
  pb <- dplyr::progress_estimated(nrow(df))
  df2 <- df %>%
    purrrlyr::by_row(
      function(row) {
        res <- post_cropped_azure(row, cropped_dir_path = cropped_tm_dir,
                                  img, azure_creds, push_to_az = push_to_az,
                                  box_highlight = box_highlight,
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
  if (res$flag == "not run") {
    lines <- res$result
  } else {
    lines <-azure_get(response = res$result[[1]],
                      headers = res$result[[2]])$recognitionResult$lines
  }

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
#' @param box_push_to_az whether to push the individual boxes to azure
#' @param box_highlight whether to highlight boxes, this allows the check to use results from the main push if it seems to work well
#' @param remove_fl whether to remove files written by the box level push
#' @export
ocr_img_wrapper <- function(img_file, hmax = 100, cropped_tm_dir, azure_creds,
                            box_push_to_az = F, box_highlight = F, remove_fl = F) {
  main_img <- resize_png(img_file)
  ##push image to azure for overall ocr
  analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                            endpoint = azure_creds$endpoint,
                            image_path = normalizePath(main_img ))
  analysis_res$recognitionResult$lines -> res_lines

  ##crop out boxes if document is a form
  crop_out_boxes(main_img, hmax = hmax) %->% c(img, img_bin, img_final_bin,
                                              contours, bounds_df, hierarchy)

  ##match results from main azure api push to the cropped boxes
  bounds_df1 <- az_to_cv2_box(bounds_df, res_lines)

  ##post individual boxes to azure api
  bounds_df2 <- vec_post_cropped_azure(df = bounds_df1,
                    cropped_tm_dir = cropped_tm_dir, img = img,
                    azure_creds = azure_creds, push_to_az = box_push_to_az,
                    box_highlight = box_highlight, remove_fl = remove_fl)

  ##get ocr results from azure api
  bounds_df3 <- vec_get_cropped_azure(bounds_df2)

  ##creating texts from lines in the get_res column
  parse_df1 <- bounds_df3 %>%
    purrrlyr::by_row(
      function(row) {
        df <- row$get_res[[1]]
        if (nrow(df) == 0) return("")
        df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
      }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')

  return(parse_df1)
}
