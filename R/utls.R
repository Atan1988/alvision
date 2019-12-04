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

#'@title get magick image dimension
#'@param x magick image
#'@export
get_img_dim <- function(x) {
  magick::image_info(x) %>% dplyr::select(width, height) %>%
    .[1,] %>% t() %>% as.vector()
}

#'@title check whether two approx points are really close
#'@param approx approximated vertex points of a contour
#'@export
ptwise_chk_approx <- function(approx) {
  ptdf <- tibble::tibble(
    x = approx[,,1],
    y = approx[,,2]
  ) %>%
    dplyr::mutate(
      nwdist = sqrt((x - min(x))^2 + (y - min(y))^2),
      nedist = sqrt((x - max(x))^2 + (y - min(y))^2),
      swdist = sqrt((x - min(x))^2 + (y - max(y))^2),
      sedist = sqrt((x - max(x))^2 + (y - max(y))^2)
    )
  nw <- ptdf %>% dplyr::filter(nwdist == min(nwdist))
  ne <- ptdf %>% dplyr::filter(nedist == min(nedist))
  sw <- ptdf %>% dplyr::filter(swdist == min(swdist))
  se <- ptdf %>% dplyr::filter(sedist == min(sedist))

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
  new_img <- img %>% reticulate::py_to_r() %>% .[y:(y+h), x:(x+w)] %>%
    reticulate::np_array('uint8')
  if (!is.null(out_fl)) cv2$imwrite(out_fl, new_img)
  return(new_img)
}
