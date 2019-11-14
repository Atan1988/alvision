bounds_to_df <- function(bounds, feature) {
  1:length(bounds) %>%
    purrr::map_df(function(x) {
      idx <- x
      bounds[[x]] %>% py_str() %>% strsplit('\\\n') %>% .[[1]] %>%
        stringr::str_squish() %>% .[grepl(":",.)] -> xy_txt
      xy_df <- tibble(x = xy_txt %>% .[seq(1,7,2)] %>% gsub('x: ', "", .),
                      y = xy_txt %>% .[seq(2,8,2)] %>% gsub('y: ', "", .)) %>%
        mutate_all(as.numeric) %>%
        mutate(pos = case_when(
          x %in% head(sort(x), 2) & y %in% head(sort(y), 2) ~ '0',
          x %in% tail(sort(x), 2) & y %in% head(sort(y), 2) ~ '1',
          x %in% tail(sort(x), 2) & y %in% tail(sort(y), 2) ~ '2',
          TRUE ~ '3'
        )
        ) %>% arrange(pos) %>% mutate(!!sym(feature) := !!x)
    })
}

strcture_words <- function(obj){
  obj %>% purrr::map_df(function(x) {
    names(x) <- c('text', 'confidence');
    return(as_tibble(x))
  }) %>% mutate(WORD = seq(1, n(), 1))
}
