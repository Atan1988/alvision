#'@title convert azure line words to df
#'@param line line of azure result
#'@export
az_words_to_df <- function(line) {
  line$words %>% purrr::map_df(function(x) {
    tmp_df <- tibble::tibble(text = x$text)
    bbox_df <- x$boundingBox %>% pts_to_wh() %>% t() %>%
      tibble::as_tibble(); colnames(bbox_df) <- c('x', 'y', 'w', 'h')
    return(dplyr::bind_cols(tmp_df, bbox_df))
  })
}

#'@title convert azure lines to df
#'@param lines result lines
#'@export
az_lines_to_df <- function(lines) {
  1:length(lines) %>%
    purrr::map_df(function(x){
      print(x)
      res <- az_words_to_df(lines[[x]])
      return(res)
    })
}
