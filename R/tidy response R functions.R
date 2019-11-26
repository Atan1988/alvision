#' @title python bounds to df
#' @description convert python bounds object to tidy dataframe
#' @param bounds python bounds option
#' @param feature what type of features, py$FEATURE class
#' @export
bounds_to_df <- function(bounds, feature) {
  1:length(bounds) %>%
    purrr::map_df(function(x) {
      idx <- x
      bounds[[x]] %>% reticulate::py_str() %>% strsplit('\\\n') %>% .[[1]] %>%
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
        ) %>% dplyr::arrange(pos) %>% dplyr::mutate(!!sym(feature) := !!x)
    })
}

#' @title python words object to df
#' @description convert python words object to tidy dataframe
#' @param obj python word level info object
#' @export
strcture_words <- function(obj){
  obj %>% purrr::map_df(function(x) {
    names(x) <- c('text', 'confidence');
    return(as_tibble(x))
  }) %>% dplyr::mutate(WORD = seq(1, n(), 1))
}

#' @title python contents object to df
#' @description convert python contents object to tidy dataframe
#' @param obj python block, paragraph, word level info object
#' @export
strcture_doc <- function(contents){

  1:length(contents) %>%
    purrr::map_df(
      function(x) {
        block <- contents[[x]]
        1:length(block) %>%
          purrr::map_df(
            function(y) {
              paragraphs <- block[[y]]
              paragraphs %>% purrr::map_df(
                function(x) {
                  names(x) <- c('text', 'confidence', 'property', 'bounding');
                  x[[3]] <- reticulate::py_str(x[[3]])
                  x[[4]] <- parse_coord(x[[4]])
                  dplyr::bind_cols(x[1:3] %>% tibble::as_tibble(), x[[4]])
                }) %>% dplyr::mutate(WORD = seq(1, dplyr::n(), 1), PARA = y)
            }) %>% dplyr::mutate(BLOCK = x)
      })
}
