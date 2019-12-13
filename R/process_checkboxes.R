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
    gray <- cv2$cvtColor(img_file, cv2$COLOR_BGR2GRAY) %>%
      reticulate::np_array(dtype = "uint8")
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
      purrr::map(
        function(c) {
          mat <- c %>% cv2$boundingRect() %>%
            unlist() %>% t() %>% tibble::as_tibble()
          colnames(mat) <- c('x', 'y', 'w', 'h')
          return(mat)
        }) %>% dplyr::bind_rows()
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

#'@title get checkboxes by parts
#'@param bounds_df identify bounds data frame
#'@param color_img colored image
#'@export
identify_chkboxes_by_parts <- function(bounds_df, color_img) {
  removed_img <- remove_color(color_img) %>% reticulate::np_array('uint8')
  res_main <- removed_img %>% identify_chkboxes() %>% arrange(y, x)
  chkbox_cnts <- 1:nrow(bounds_df) %>%
    purrr::map(function(l) {
      row <- bounds_df[l, ]
      part_img <- quick_img_chk(row, removed_img, NULL)
      res <- part_img %>% identify_chkboxes()
      if (!is.null(res)) res <- res %>%
        dplyr::mutate(x = x + row$x, y = y + row$y)
    }) %>% dplyr::bind_rows() %>%
    dplyr::mutate(chkbox_id = seq(1, dplyr::n(), 1))
  
  res_ids <- purrr::cross_df(list(id1 = res_main$chkbox_id, id2 = chkbox_cnts$chkbox_id)) %>% 
    dplyr::inner_join(res_main %>% dplyr::select(id1:=chkbox_id, x, y), by = 'id1') %>%
    dplyr::inner_join(chkbox_cnts %>% dplyr::select(id2:=chkbox_id, x, y), by = 'id2') %>% 
    dplyr::mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>% 
    dplyr::group_by(id1) %>% dplyr::summarise(dist = min(dist)) %>% 
    dplyr::filter(dist > 50) %>% dplyr::pull(id1)
  
  chkbox_cnts1 <- dplyr::bind_rows(
    chkbox_cnts, 
    res_main %>% filter(chkbox_id %in% res_ids)
  )
  return(chkbox_cnts1)
}

#'@title get checkbox options
#'@param chkbox_df data frame with checkbox information
#'@param words_df data frame with all the words info from azure
#'@param question_df data frame with all the questions
#'@export
get_chkbox_options <- function(chkbox_df, words_df, question_df) {
  chkbox_df1 <- chkbox_df %>% dplyr::inner_join(question_df, by = 'chkbox_id')%>%
    dplyr::group_by(line_text) %>% dplyr::mutate(row = floor(mean(y))) %>%
    dplyr::arrange(line_text, x) %>%
    dplyr::mutate(leadx = dplyr::lead(x, 1, default = Inf)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-line_text)

  preceding_word_df <- purrr::cross_df(list(combo_id = words_df$combo_id,
                                            chkbox_id = chkbox_df1$chkbox_id)) %>%
    dplyr::left_join(words_df, by = 'combo_id') %>%
    dplyr::left_join(chkbox_df1, by = 'chkbox_id') %>%
    dplyr::mutate(
      diffx = x.x  - x.y - w.y, diffy = y.x - y.y,
      dist = sqrt(diffx^2 + diffy^2)
    ) %>%
    dplyr::mutate(text = gsub("\\[|\\]", "", text) %>%
                    stringr::str_squish()) %>%
    dplyr::filter(text != "") %>%
    dplyr::filter(abs(diffy) <= 25, diffx > -25, x.x <= leadx)%>%
    dplyr::distinct() %>%
    dplyr::group_by(chkbox_id) %>%
    dplyr::summarise(text = paste(text, collapse = " "))

  return(preceding_word_df)
}

#'@title get checkbox questions
#'@param chkbox_df data frame with checkbox information
#'@param lines_df data frame with all the lines info from azure
#'@export
get_chkbox_questions <- function(chkbox_df, lines_df) {
  question_df <- purrr::cross_df(list(line_id = lines_df$line_id,
                                      chkbox_id = chkbox_df$chkbox_id)) %>%
    dplyr::left_join(lines_df, by = 'line_id') %>%
    dplyr::left_join(chkbox_df, by = 'chkbox_id') %>%
    dplyr::mutate(
      diffx = x.x  - x.y - w.y, diffy = y.x - y.y,
      dist = sqrt(diffx^2 + diffy^2)
    ) %>%
    dplyr::mutate(text = gsub("\\[|\\]", "", text) %>%
                    stringr::str_squish()) %>%
    dplyr::filter(text != "")  %>%
    dplyr::group_by(chkbox_id) %>%
    dplyr::filter(diffy >= -25, diffy <= 25) %>%
    dplyr::filter(!grepl('yes no', ignore.case = T, text)) %>%
    dplyr::arrange(x.x) %>%
    dplyr::distinct() %>%
    dplyr::summarise(line_text = paste(text, collapse = " "))
  return(question_df)
}

#'@title get checkboxes questions, options, selections
#'@param chkbox_df data frame with checkbox information
#'@param words_df  data frame with all the words info from azure
#'@param lines_df  data frame with all the lines info from azure
#'@param img       the image np array of the page
#'@export
get_chkbox_wrapper <- function(chkbox_df, words_df, lines_df, img) {

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
    purrr::map(~quick_img_chk(question_df1[., ], img, NULL))

  question_df1$box_mu <- 1:nrow(question_df1) %>%
    purrr::map_dbl(~quick_img_chk(question_df1[., ], img, NULL)$mean() %>%
                     reticulate::py_to_r() %>% as.numeric())

  question_df1 <- question_df1 %>%
    dplyr::mutate(selected = ifelse(box_mu < mean(box_mu), T, F))

  return(question_df1)
}
