#' @title azure lines into cv2 bounding boxes
#' @param bounds_df bounds_df result from crop_out_boxes function
#' @param res_lines lines result from the azure api reading the whole page
#' @export
az_to_cv2_box <- function(bounds_df, res_lines) {
  bounds_df1 <- add_rc_bbox(bbox_df = bounds_df)
  bounds_list <- bbox_df_to_c(bounds_df1)

  match_idx <- res_lines %>% purrr::map(~pts_to_wh(.$boundingBox)) %>%
    purrr::map_dbl(function(x) {
      res <- bounds_list %>% purrr::map_lgl(~chk_box_in(., x, 10)) %>% which(.)
      if (length(res) == 0) return(NA)
      return(res)
    })

  bounds_df1$az <- 1:nrow(bounds_df1) %>%  purrr::map(
    function(x) {
      idx <- which(match_idx == x)
      if (length(idx) == 0) return(list())
      return(res_lines[idx])
    }
  )
  return(bounds_df1)
}

