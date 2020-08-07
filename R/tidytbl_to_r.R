#' @title tidy result to row and column table
#' @param tidy_res tidied result, could be from azure after tidying or pdf_data
#' @param row_skip row to skip
#' @param col_header column header to help identifying the tabular structure
#' @export
tidytbl_to_r <- function(tidy_res, col_header) {
  mean_h <- mean(tidy_res$h)
  dens <- density(tidy_res$y, bw = mean_h/10)
  dens_tibbl <- tibble::tibble(x = dens$x, y = dens$y) %>% 
    dplyr::mutate(locl_max = slider::slide_dbl(y, 
                                               ~max(.x), .before = 2, .after = 2)) %>% 
    dplyr::filter(y == locl_max) %>% 
    dplyr::mutate(row_id = seq_len(dplyr::n()))
  
  ##setup row map
  row_map <- purrr::cross_df(
    list(y = unique(tidy_res$y), row_y = unique(dens_tibbl$x))
  ) %>% 
    dplyr::arrange(y, row_y) %>% 
    dplyr::group_by(y) %>% 
    dplyr::filter(min(abs(y - row_y)) == abs(y - row_y)) %>% 
    dplyr::distinct() %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      dens_tibbl %>% dplyr::select(row_y := x, row_id), by = 'row_y'
    )
  
  ##process rows
  tidy_res <- tidy_res %>% 
    dplyr::left_join(row_map, by = 'y') #%>% 
  #dplyr::filter(row_id > row_skip)
  
  ###get column table
  col_tbl <- tidy_res %>% 
    dplyr::filter(tolower(text) %in% tolower(col_header))
  
  title_row <-  Mode(col_tbl$row_id)   
  col_tbl <- col_tbl %>% dplyr::filter(row_id == title_row) %>% 
    dplyr::mutate(col_x = x + w) %>% 
    dplyr:::select(text, col_x1 := x, col_x) %>% 
    dplyr::mutate(col_id = seq_len(dplyr::n()))
  tidy_res <- tidy_res %>% dplyr::filter(row_id >= title_row)
  ###setup columnn map
  col_map <- purrr::cross_df(
    list(x = unique(tidy_res$x), col_x = unique(col_tbl$col_x))
  ) %>% 
    dplyr::arrange(x, col_x) %>% 
    dplyr::group_by(x) %>% 
    dplyr::filter(x <= col_x) %>% 
    dplyr::filter(min(abs(x - col_x)) == abs(x - col_x)) %>% 
    dplyr::distinct() %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      col_tbl %>% dplyr::select(col_x, col_id), by = 'col_x'
    )
  
  ##process cols
  tidy_res <- tidy_res %>% 
    dplyr::left_join(col_map, by = 'x') 
  
  res <- tidy_res %>% dplyr::group_by(row_id, col_id) %>%
    dplyr::summarise(text = paste(text, collapse = '  ') %>% stringr::str_squish()) %>% 
    tidyr::spread(col_id, text)
  
  return(res)
}

#' @title calculate mode
#' @param x the value vector
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}