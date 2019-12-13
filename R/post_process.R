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
    purrr::map_df(function(pg) results[[pg]][[2]] %>% dplyr::mutate(page = pg)) %>%
    dplyr::filter(w > (mean(w) - 3))
}