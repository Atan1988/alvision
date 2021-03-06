#' @title fix str, bankstatement
#' @param x the text
#' @export
fix_str_bankstatement <- function(x) {
 x %>% gsub('([0-9]) (,) ([0-9])', "\\1\\2\\3", .) %>%
    gsub('([0-9]) (\\.) ([0-9])', "\\1\\2\\3", .) %>%
    gsub('(\\$) ([0-9])', "\\1\\2", .) %>%
    gsub(" (,) | (\\.) ", "\\1 ", .) %>%
    gsub(" (,)| (\\.)", "\\1", .)
}


#' @title turning bounding obj to x,y coordinates
#' @param x the bounding obj from python
#' @export
parse_coord <- function(x) {
  x %>% reticulate::py_str() %>% strsplit('\\\n') %>% .[[1]] %>%
    stringr::str_squish() %>% .[grepl(":",.)] -> xy_txt
  xy_df <- tibble::tibble(x = xy_txt %>% .[seq(1,7,2)] %>% gsub('x: ', "", .),
                          y = xy_txt %>% .[seq(2,8,2)] %>% gsub('y: ', "", .)) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(pos = dplyr::case_when(
      x %in% head(sort(x), 2) & y %in% head(sort(y), 2) ~ '0',
      x %in% tail(sort(x), 2) & y %in% head(sort(y), 2) ~ '1',
      x %in% tail(sort(x), 2) & y %in% tail(sort(y), 2) ~ '2',
      TRUE ~ '3'
    )
    ) %>% dplyr::arrange(pos)
  xy_df %>% tidyr::gather(key, val, -pos) %>%
    tidyr::pivot_wider(names_from = c('key', 'pos'), values_from = 'val')
}

#' @title convert point coords to x,y, w, h
#' @param boundingbox the bounding box returned by azure api
#' @export
pts_to_wh  <- function(boundingbox) {
  x <- boundingbox[1]; y <- boundingbox[2]
  w <- mean(c(abs(boundingbox[3] - boundingbox[1]),
              abs(boundingbox[5]- boundingbox[7])))
  h <- mean(abs(c(boundingbox[8] - boundingbox[2],
                  boundingbox[4] - boundingbox[6])))
  return(c(x, y, w, h))
}

#' @title check whether bounding box is in another bounding box
#' @param box1 first box
#' @param box2 second box
#' @param err fuzzy around several ptx
#' @export
chk_box_in <- function(box1, box2, err = 5) {
   pt_inside_corner <- box2[1] >= (box1[1] - err)  & box2[2] >= (box1[2] - err)
   w_within <- (box1[1] + box1[3]) >= ((box2[1] + box2[3]) - err)
   h_within <- (box1[2] + box1[4]) >= ((box2[2] + box2[4]) - err)
   return(pt_inside_corner & w_within & h_within )
}

#' @title convert bbox df format to vector format
#' @param df bbox df
#' @export
bbox_df_to_c  <- function(df) {
   res_list <- 1:nrow(df) %>% purrr::map(
     ~df[., ] %>% dplyr::select(x, y, w, h) %>% t() %>% as.vector()
   )
   if (length(res_list) == 1) res_list <- res_list[[1]]
   return(res_list)
}

#' @title get the column or row based on density
#' @param x x or y vector
#' @export
get_col_row <- function(x) {
  des <- density(x, bw = 8, n = length(x),
                 kernel = 'rectangular')
  des_df <- tibble::tibble(x = des$x, y = des$y) %>%
    dplyr::mutate(peak = ifelse(y > dplyr::lead(y, default = 0) &
                                  y > dplyr::lag(y, default = 0) &
                                  y > 1e-6, 1, 0),
                  trough = ifelse(y < dplyr::lead(y, default = 0) &
                                    y < dplyr::lag(y, default = 0)&
                                    y > 1e-6, 1, 0)
    ) %>%
    dplyr::filter(peak == 1)

  if(nrow(des_df) == 0) des_df <- tibble::tibble(x = des$x, y = des$y) %>%
                             dplyr::filter(x == min(x)) %>%
                             dplyr::mutate(peak = 1, trough = 0)

  return(des_df)
}

#' @title add row and cols to bounding box df
#' @param bbox_df bounding box df
#' @export
add_rc_bbox <- function(bbox_df) {
  rows <- get_col_row(bbox_df$y)
  cols <- get_col_row(bbox_df$x)

  bbox_df1 <- bbox_df %>%
    dplyr::left_join(
      purrr::cross_df(list(x = unique(bbox_df$x),
                           col = ceiling(cols$x))) %>%
        dplyr::group_by(x) %>%
        dplyr::mutate(dist = abs(x-col)) %>%
        dplyr::filter(abs(dist) == min(dist)),
      by = "x"
    ) %>%
    dplyr::left_join(
      purrr::cross_df(list(y = unique(bbox_df$y),
                           row = ceiling(rows$x))) %>%
        dplyr::group_by(y) %>%
        dplyr::mutate(dist = abs(y-row)) %>%
        dplyr::filter(abs(dist) == min(dist)),
      by = "y"
    ) %>%
    dplyr::arrange(row, col) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idx = seq(1, dplyr::n(), 1))

  return(bbox_df1)
}

#' @title line areas calcualtion for azure
#' @param line line obj returnef from azure api
#' @export
az_line_area  <- function(line) {
 line$words %>% purrr::map_dbl(
   function(x) {bbox <- x$boundingBox %>% pts_to_wh();
                bbox[3] * bbox[4]}) %>% sum()
}

#'@title get cv2 image dimension
#'@param np image
#'@export
get_img_dim <- function(np) {
  raw_dim <- np$shape 
  if (class(raw_dim)[1] != 'list') raw_dim <- raw_dim %>% reticulate::py_to_r() 
  raw_dim <- raw_dim %>% unlist()
  return(raw_dim)
}

#'@title check whether two approx points are really close
#'@param approx approximated vertex points of a contour
#'@param mode  'r' or 'py'
#'@export
ptwise_chk_approx <- function(approx, mode = 'py') {
  if(mode == 'py') {
    ptdf <- tibble::tibble(
      x = approx[,,1],
      y = approx[,,2]
    ) 
  } else {
    ptdf <- tibble::as_tibble(approx[,c('x', 'y')])
  }
  
  ptdf <- ptdf %>%
    dplyr::mutate(
      nwdist = sqrt((x - min(x))^2 + (y - min(y))^2),
      nedist = sqrt((x - max(x))^2 + (y - min(y))^2),
      swdist = sqrt((x - min(x))^2 + (y - max(y))^2),
      sedist = sqrt((x - max(x))^2 + (y - max(y))^2)
    )
  nw <- ptdf %>% dplyr::filter(nwdist == min(nwdist)) %>% .[1, ]
  ne <- ptdf %>% dplyr::filter(nedist == min(nedist)) %>% .[1, ]
  sw <- ptdf %>% dplyr::filter(swdist == min(swdist)) %>% .[1, ]
  se <- ptdf %>% dplyr::filter(sedist == min(sedist)) %>% .[1, ]

  ##nw and ne at similar hight
  chk1 <- abs(ne$y - nw$y) <= 3
  ##sw and se at similar hight
  chk2 <- abs(se$y - sw$y) <= 3
  ##nw and sw at similar x coord
  chk3 <- abs(nw$x - sw$x) <= 3
  ##ne and se at similar x coord
  chk4 <- abs(ne$x - se$x) <= 3
  ##check no contour points significantly higher
  chk5 <- abs(min(ptdf$y) - min(ne$y, nw$y)) <= 3
  ##check no contour points significantly lower
  chk6 <- abs(max(ptdf$y) - max(se$y, sw$y)) <= 3
  ##check no contour points significantly to the left
  chk7 <- abs(min(ptdf$x) - min(sw$x, nw$x)) <= 3
  ##check no contour points significantly to the right
  chk8 <- abs(max(ptdf$x) - max(se$x, ne$x)) <= 3

  return(list(flag = chk1&chk2&chk3&chk4&chk5&chk6&chk7&chk8, ptdf = ptdf))
}

#'@title quick image chk
#'@param df x, y, w, h df
#'@param img raw img
#'@param out_fl output file
#'@export
quick_img_chk <- function(df, img, out_fl = 'new.png') {
  y <- df$y; x <- df$x; w <- df$w; h <- df$h
  img_dim <- get_img_dim(img)
  if (length(img_dim) == 2) {
    new_img <- img %>% reticulate::py_to_r() %>% .[y:(y+h), x:(x+w)] %>%
      reticulate::np_array('uint8')
  }
  if (length(img_dim) == 3) {
    new_img <- img %>% reticulate::py_to_r() %>% .[y:(y+h), x:(x+w), ] %>%
      reticulate::np_array('uint8')
  }

  if (!is.null(out_fl)) cv2$imwrite(out_fl, new_img)
  return(new_img)
}

#'@title quick image chk
#'@param df x, y, w, h df
#'@param img raw img
#'@param out_fl output file
#'@param dev_r how much room to cut into the contour to remove the outside box
#'@export
quick_img_chkR <- function(df, img, out_fl = 'new.png', dev_r = 10) {
  y <- df$y1; x <- df$x; w <- df$w; h <- df$h
  vertical_dev <-  round(h/dev_r, 0)
  horizontal_dev <- round(w/dev_r, 0)
  new_img <- img[(y + vertical_dev) :(y+h - vertical_dev), (x + horizontal_dev):(x+w - horizontal_dev), ]
  if(length(dim(new_img)) == 2) dim(new_img)[3] <- 1
  new_img <- Rvision::image(new_img)
  if (!is.null(out_fl)) Rvision::write.Image(new_img, out_fl)
  return(new_img)
}

#'@title get bounding box of contour matrix
#'@param cnt_mat contour matrix
#'@param img_max_y the max y variable of image
#'@export
boundingRect <- function(cnt_mat, img_max_y){
  tibble::tibble(x = min(cnt_mat[, 'x']), y1 = min(cnt_mat[, 'y']),
    w = max(cnt_mat[,'x']) - min(cnt_mat[,'x']), 
    h = max(cnt_mat[,'y']) - min(cnt_mat[,'y']), y = img_max_y - y1 - h)
}

#'@title extend lines in case lines are broken
#'@param img Rvision::image obj
#'@export
extend_horizontal_lines <- function(img){
  img <- img$toR()
  rows <- apply(img, 1, mean)
  cols <- apply(img, 2, mean)
  rows_to_over <- which(rows < 100)
  cols_to_over <- which(cols < 200); 
  min_col <- min(cols_to_over); max_col <- max(cols_to_over)
  if (is.infinite(min_col) | is.infinite(max_col)) return(Rvision::image(img))
  img[rows_to_over, min_col:max_col, 1] <- 0
  img <- Rvision::image(img)
  return(img)
}