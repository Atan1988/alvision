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
