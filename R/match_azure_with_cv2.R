#' @title azure lines into cv2 bounding boxes
#' @param bounds_df bounds_df result from crop_out_boxes function
#' @param res_lines lines result from the azure api reading the whole page
#' @export
az_to_cv2_box <- function(bounds_df, res_lines) {
  bounds_list <- bbox_df_to_c(bounds_df)

  match_idx <- res_lines %>% purrr::map(~pts_to_wh(.$boundingBox)) %>%
    purrr::map_dbl(function(x) {
      res <- bounds_list %>% purrr::map_lgl(~chk_box_in(., x, 10)) %>% which(.)
      if (length(res) == 0) return(NA)
      return(res[1])
    })

  bounds_df$az <- 1:nrow(bounds_df) %>%  purrr::map(
    function(x) {
      idx <- which(match_idx == x)
      if (length(idx) == 0) return(list())
      return(res_lines[idx])
    }
  )

  not_matched_idx <- which(is.na(match_idx))
  not_matched_bounds_df <- not_matched_idx %>%
    purrr::map_df(function(x) {
      box_ref <- res_lines[[x]]$boundingBox %>% pts_to_wh() %>% t()
      box_ref <- tibble::as_tibble(box_ref)
      names(box_ref) <- c('x', 'y', 'w', 'h')
      #box_ref$az <- list(res_lines[[x]])
      return(box_ref)
    })
  not_matched_bounds_df$az <- not_matched_idx %>% purrr::map(~res_lines[.])

  bounds_dfb <- bind_rows(bounds_df, not_matched_bounds_df)
  bounds_df1 <- add_rc_bbox(bbox_df = bounds_dfb)

  return(bounds_df1)
}

