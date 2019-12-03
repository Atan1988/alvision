#'@title convert azure line words to df
#'@param line line of azure result
#'@param type word or line
#'@export
az_words_to_df <- function(line, type = 'word') {
  if (type == 'word') {
    line <- line$words
  }
  
  line %>% purrr::map_df(function(x) {
    tmp_df <- tibble::tibble(text = x$text)
    bbox_df <- x$boundingBox %>% pts_to_wh() %>% t() %>%
      tibble::as_tibble(); colnames(bbox_df) <- c('x', 'y', 'w', 'h')
    return(dplyr::bind_cols(tmp_df, bbox_df) %>%
             dplyr::mutate(word_id = seq(1, dplyr::n(), 1)))
  })
}

#'@title convert azure lines to df
#'@param lines result lines
#'@export
az_lines_to_df <- function(lines) {
  1:length(lines) %>%
    purrr::map_df(function(x){
      #print(x)
      l_id <- x
      res <- az_words_to_df(lines[[x]]) %>%
        dplyr::mutate(line_id = l_id)
      return(res)
    })
}



