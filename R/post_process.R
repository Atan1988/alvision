#'@title get tidy text
#'@param results results from the ocr_pdf function
#'@export
get_text_df <- function(results) {
  1:length(results) %>%
    purrr::map_df(function(pg) results[[pg]][[1]] %>% dplyr::mutate(page = pg))
}

#'@title get tidy questions
#'@param results results from the ocr_pdf function
#'@export
get_question_df <- function(results) {
  1:length(results) %>%
    purrr::map_df(function(pg) results[[pg]][[2]] %>% dplyr::mutate(page = pg)) 
}

#'@title conver to tabular form
#'@param texts texts df
#'@param pg page
#'@export
crt_tabular <- function(texts, pg = NULL) {
  tmp_text <- texts
  if (!is.null(pg) & pg %in% unique(tmp_text$page)) tmp_text <- tmp_text %>% filter(page == pg)
  tmp_text %>% dplyr::arrange(row, col, x, y) %>%
    dplyr::group_by(row, col) %>% dplyr::summarise(txt = paste0(txt, collapse = ";")) %>%
    dplyr::select(row, col, txt) %>%
    dplyr::arrange(col, row) %>%
    tidyr::pivot_wider(names_from = col, values_from = txt )%>%
    dplyr::arrange(row)
}