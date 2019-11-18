#' @title parser from tidy res
#' @param df tidy data frame
#' @export
parser_std <- function(df){
  df %>%
    dplyr::mutate(
      w_h = y_2 - y_1
    ) %>%
    dplyr::mutate(
      mean_w_h = ceiling(mean(w_h))
    ) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      y_3a = ceiling(y_3 / mean_w_h) * mean_w_h
    ) %>% dplyr::arrange(y_3a, x_0) %>%
    dplyr::group_by(y_3a) %>% dplyr::summarise(text = paste(text, collapse = " ")) %>%
    dplyr::mutate(text = fix_str_bankstatement(text)) %>% View()
}
