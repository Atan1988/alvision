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
