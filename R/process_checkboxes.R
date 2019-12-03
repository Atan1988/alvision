#'@title get checkboxes
#'@param img_file file of image or np array of image
#'@export
identify_chkboxes <- function(img_file){
  # Read the image
  #tictoc::tic()
  if (!"numpy.ndarray" %in% class(img_file)) {
    image <-  cv2$imread(normalizePath(img_file)) %>% reticulate::np_array(dtype = "uint8")
    gray <- cv2$cvtColor(image, cv2$COLOR_BGR2GRAY) %>% reticulate::np_array(dtype = "uint8")
  } else {
    gray <- cv2$cvtColor(img_file, cv2$COLOR_BGR2GRAY) %>% reticulate::np_array(dtype = "uint8")
  }
  
  gray <-  cv2$GaussianBlur(gray, reticulate::tuple(7L, 7L), 0L) %>%
    reticulate::np_array(dtype = "uint8")
  
  # threshold the image
  c(ret, thresh1) %<-% cv2$threshold(gray ,128,255,
                                     bitwOr(cv2$THRESH_BINARY, cv2$THRESH_OTSU))
  thresh1 <- 255-thresh1
  # find contours in the image
  c(cnts, hirachy) %<-% cv2$findContours(thresh1 %>%
                                           reticulate::np_array(dtype = "uint8"),
                                         cv2$RETR_EXTERNAL, cv2$CHAIN_APPROX_NONE)
  #tictoc::toc()
  orig <-  gray
  i <-  0
  threshold_max_area <- 3000;
  threshold_min_area <- 200
  #tictoc::tic()
  checkboxes_cnts <- cnts %>% purrr::map(
    function(c) {
      area <-  cv2$contourArea(c)
      if (area > threshold_max_area | area < threshold_min_area) return(NULL)
      peri <- cv2$arcLength(c, T)
      approx <- cv2$approxPolyDP(c, 0.05 * peri, T)
      c(x, y, w, h) %<-% cv2$boundingRect(approx)
      aspect_ratio <- w / h
      if (aspect_ratio < 0.95 | aspect_ratio > 1.05) return(NULL)
      sqaure_chk <- ptwise_chk_approx(approx)
      if (sqaure_chk$flag){
        return(c)
      } else {
        return(NULL)
      }
    }
  )
  #tictoc::toc()
  
  if (length(checkboxes_cnts) > 0) {
    checkboxes_cnts[sapply(checkboxes_cnts, is.null)] <- NULL
    if (length(checkboxes_cnts) == 0) return(NULL)
    checkboxes_df <- checkboxes_cnts %>%
      purrr::map_df(
        function(c) {
          mat <- c %>% cv2$boundingRect() %>%
            unlist() %>% t() %>% tibble::as_tibble()
          colnames(mat) <- c('x', 'y', 'w', 'h')
          return(mat)
        })
    return(checkboxes_df %>% 
             dplyr::mutate(chkbox_id = seq(1, dplyr::n(), 1)))
  }
  # 1:length(cnts) %>% purrr::map(
  #   function(i) {
  #     c(x, y, w, h) %<-% cv2$boundingRect(cnts[[i]])
  #     new_img <- orig %>% reticulate::py_to_r() %>% .[y:(y+h), x:(x+w)]
  #     cv2$imwrite(file.path('tmp1', paste0(i,'.png')), new_img)
  #   }
  # )
}

#'@title get checkbox options
#'@param chkbox_df data frame with checkbox information
#'@param words_df data frame with all the words info from azure
#'@export
get_chkbox_options <- function(chkbox_df, words_df) {
  preceding_word_df <- purrr::cross_df(list(combo_id = words_df$combo_id,
                                            chkbox_id = chkbox_df$chkbox_id)) %>%
    dplyr::left_join(words_df, by = 'combo_id') %>%
    dplyr::left_join(chkbox_df, by = 'chkbox_id') %>%
    dplyr::mutate(
      diffx = x.x  - x.y - w.y, diffy = y.x - y.y,
      dist = sqrt(diffx^2 + diffy^2)
    ) %>% 
    dplyr::mutate(text = gsub("\\[|\\]", "", text) %>% 
                    stringr::str_squish()) %>% 
    dplyr::filter(text != "") %>%
    dplyr::group_by(chkbox_id) %>%
    dplyr::filter(dist == min(dist)) %>%
    dplyr::distinct()
  return(preceding_word_df)
}