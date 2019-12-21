#'@title get checkboxes
#'@param img_file file of image or np array of image
#'@export
identify_chkboxesR <- function(img_file){
  # Read the image
  #tictoc::tic()
  if (!"Rcpp_Image" %in% class(img_file)) {
    gray <- Rvision::image(img_file)
  } else {
    gray <- img_file
  }
  
  gray_blur <-  Rvision::gaussianBlur(gray, k_height = 7, k_width = 7, 0)
  # threshold the image
  thresh1 <- Rvision::adaptiveThreshold(gray_blur, threshold_type ='inverse')
  # find contours in the image
  c(cnts1, hirachy1) %<-% Rvision::findContours( thresh1,
                                  mode = "external", method = 'none')
  cnts1 <- base::split(cnts1, cnts1$id)
  img_max_y <- dim(gray)[1]
  
  #tictoc::toc()
  orig <-  gray
  i <-  0
  threshold_max_area <- 3000;
  threshold_min_area <- 900
  #tictoc::tic()
  checkboxes_cnts <- cnts1 %>% purrr::map(
    function(c) {
      #area <-  cv2$contourArea(c)
      #if (area > threshold_max_area | area < threshold_min_area) return(NULL)
      #peri <- cv2$arcLength(c, T)
      #approx <- cv2$approxPolyDP(c, 0.05 * peri, T)
      bounds <- boundingRect(c, img_max_y)
      c(x, y, w, h) %<-% purrr::map(c('x', 'y', 'w', 'h'), ~bounds[[.]][1])
      if (h>100) return(NULL)
      if (w * h < threshold_min_area | w * h > threshold_max_area) return(NULL)
      aspect_ratio <- w / h
      if (aspect_ratio < 0.90 | aspect_ratio > 1.1) return(NULL)
      approx <- c; approx[, 'y1'] <- approx[, 'y']; 
      approx[, 'y'] <- img_max_y - approx[, 'y']
      sqaure_chk <- ptwise_chk_approx(approx, mode = 'r')
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
      purrr::map(boundingRect, img_max_y = img_max_y) %>% dplyr::bind_rows()
    return(checkboxes_df %>%
             dplyr::mutate(chkbox_id = seq(1, dplyr::n(), 1)))
  }
}

#'@title get checkboxes by parts
#'@param bounds_df identify bounds data frame
#'@param removed_img image with hw removed
#'@param cl number of cores to use
#'@export
identify_chkboxes_by_partsR <- function(bounds_df, removed_img, cl = 1) {
  res_main <- removed_img %>% identify_chkboxesR() #%>% arrange(y, x)
  img_max_y <- dim(removed_img)[1]
  chkbox_cnts <- 1:nrow(bounds_df) %>%
    pbapply::pblapply(function(l) {
      row <- bounds_df[l, ]
      part_img <- quick_img_chkR(row, removed_img, NULL)
      res <- part_img %>% identify_chkboxesR()
      if (!is.null(res)) res <- res %>%
        dplyr::mutate(x = x + row$x, y = y + row$y) %>% 
        dplyr::mutate(y1 = img_max_y - y - h)
    }, cl = cl) %>% dplyr::bind_rows() %>%
    dplyr::mutate(chkbox_id = seq(1, dplyr::n(), 1))
  
  if (is.null(res_main)) return(chkbox_cnts)
  
  res_ids <- purrr::cross_df(list(id1 = res_main$chkbox_id, id2 = chkbox_cnts$chkbox_id)) %>% 
    dplyr::inner_join(res_main %>% dplyr::select(id1:=chkbox_id, x, y), by = 'id1') %>%
    dplyr::inner_join(chkbox_cnts %>% dplyr::select(id2:=chkbox_id, x, y), by = 'id2') %>% 
    dplyr::mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>% 
    dplyr::group_by(id1) %>% dplyr::summarise(dist = min(dist)) %>% 
    dplyr::filter(dist > 50) %>% dplyr::pull(id1)
  
  chkbox_cnts1 <- dplyr::bind_rows(
    chkbox_cnts, 
    res_main %>% dplyr::filter(chkbox_id %in% res_ids)
  )%>% dplyr::mutate(chkbox_id = seq(1, dplyr::n(), 1))
  
  chkbox_cnts1$chked_txt <- 1:nrow(chkbox_cnts1) %>% 
    pbapply::pblapply(
      function(x){
        row <- chkbox_cnts1[x, ]
        tmp <- paste0(tempfile(), '.png')
        chkbox_img <- quick_img_chkR(row, removed_img, tmp)
        txt = tesseract::ocr(tmp); unlink(tmp)
        return(txt)
      }, cl = cl) %>% unlist() %>% gsub('\\\n', "", .) %>% stringr::str_squish()
  return(chkbox_cnts1)
}

#'@title get checkboxes questions, options, selections
#'@param chkbox_df data frame with checkbox information
#'@param words_df  data frame with all the words info from azure
#'@param lines_df  data frame with all the lines info from azure
#'@param img       the image np array of the page
#'@param cl        number of cores being used
#'@export
get_chkbox_wrapperR <- function(chkbox_df, words_df, lines_df, img, cl = 1) {
  
  ##find out the question
  question_df <- get_chkbox_questions(chkbox_df = chkbox_df,
                                      lines_df = lines_df)
  ##find choice options
  preceding_word_df <- get_chkbox_options(chkbox_df = chkbox_df,
                                          words_df = words_df, question_df = question_df)
  
  question_df1 <- question_df %>%
    dplyr::left_join(preceding_word_df %>% dplyr::select(chkbox_id, text),
                     by = "chkbox_id") %>%
    dplyr::left_join(chkbox_df, by = "chkbox_id")
  
  question_df1$box <- 1:nrow(question_df1) %>%
    purrr::map(function(x) { quick_img_chkR(question_df1[x, ], img, NULL)})
  
  question_df1$box_mu <- 1:nrow(question_df1) %>%
    purrr::map_dbl(function(x) mean(question_df1$box[[x]]$toR())) 
  
  question_df1 <- question_df1 %>%
    dplyr::mutate(selected = ifelse(box_mu < mean(box_mu), T, F))
  
  return(question_df1)
}
