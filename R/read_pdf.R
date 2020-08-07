#' @title ocr a pdf file
#' @description wrap of the ocr wrapper for images
#' @param pdf_file pdf file location
#' @param hmax maximum box height to include
#' @param cropped_tm_dir temporary dir for cropped img
#' @param azure_creds azure credential
#' @param box_push_to_az whether to push the individual boxes to azure
#' @param box_highlight whether to highlight boxes, this allows the check to use results from the main push if it seems to work well
#' @param remove_fl whether to remove files written by the box level push
#' @param dpi dpi of the converted images
#' @export
ocr_pdf <- function(pdf_file, hmax = 200, cropped_tm_dir, azure_creds,
                    box_push_to_az = F, box_highlight = F, remove_fl = F, dpi = 400) {
  ##convert pdf to images
  image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = dpi)

  res <- image_files %>% purrr::map(~ocr_img_wrapper(img_file = ., hmax = hmax,
                    cropped_tm_dir = cropped_tm_dir, azure_creds = azure_creds,
                    box_push_to_az = box_push_to_az, box_highlight = box_highlight,
                    remove_fl = remove_fl))
  return(res)
}

#' @title ocr a pdf file via R 
#' @description wrap of the ocr wrapper for images
#' @param pdf_file pdf file location
#' @param hmax maximum box height to include
#' @param cropped_tm_dir temporary dir for cropped img
#' @param azure_creds azure credential
#' @param box_push_to_az whether to push the individual boxes to azure
#' @param box_highlight whether to highlight boxes, this allows the check to use results from the main push if it seems to work well
#' @param remove_fl whether to remove files written by the box level push
#' @param dpi dpi of the converted images
#' @export
ocr_pdfR <- function(pdf_file, hmax = 200, cropped_tm_dir, azure_creds,
                    box_push_to_az = F, box_highlight = F, remove_fl = F, dpi = 400) {
  ##convert pdf to images
  image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = dpi)
  
  res <- image_files %>% purrr::map(~ocr_img_wrapper(img_file = ., hmax = hmax,
                                                     cropped_tm_dir = cropped_tm_dir, azure_creds = azure_creds,
                                                     box_push_to_az = box_push_to_az, box_highlight = box_highlight,
                                                     remove_fl = remove_fl))
  return(res)
}


#' @title ocr a pdf file via R with multicore support
#' @description wrap of the ocr pdf with multicore support
#' @param pdf_file pdf file location
#' @param hmax maximum box height to include
#' @param cropped_tm_dir temporary dir for cropped img
#' @param azure_creds azure credential
#' @param box_push_to_az whether to push the individual boxes to azure
#' @param box_highlight whether to highlight boxes, this allows the check to use results from the main push if it seems to work well
#' @param remove_fl whether to remove files written by the box level push
#' @param dpi dpi of the converted images
#' @param main_cl number of cores for main processes
#' @param sub_cl number of cores for sub processes
#' @export
ocr_pdfR_fr <- function(pdf_file, hmax = 300, cropped_tm_dir, azure_creds,
                        box_push_to_az = F, box_highlight = F, remove_fl = F, dpi = 400,
                        main_cl = 1, sub_cl = 3){
  ##convert pdf to images
  image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = dpi)
  
  ##resize images
  resized_images <- pbapply::pblapply(image_files, resize_png, cl = 1)  
  resized_images[sapply(resized_images, is.null)] <- NULL
  
  ##remove handwritten color from images
  removed_images <- pbapply::pblapply(resized_images, 
                        function(x) {remove_colorR(x[[4]])}, cl = main_cl)
  removed_images <- purrr::map(removed_images, ~Rvision::image(.))
  
  ##get ocr from azure api
  analysis_res_list <- pbapply::pblapply(resized_images, function(x){
    azure_vis(subscription_key = azure_creds$subscription_key,
              endpoint = azure_creds$endpoint,
              image_path = normalizePath(x[[1]]))
  }, cl = 1)
  
  res_lines_list <- pbapply::pblapply(analysis_res_list, 
                function(x){
                  res_lines <- x$recognitionResult$lines
                  if (length(res_lines) == 0) return(list(tibble::tibble(), tibble::tibble()))
                  return(res_lines) }, cl = 1)
  
  res_lines_df_list <- pbapply::pblapply(res_lines_list, az_lines_to_df, cl = 1)
  
  res_lines_only_df_list <- pbapply::pblapply(res_lines_list, 
                function(x)az_words_to_df(x, type = 'line'), cl = 1)
  
  ###cropped out box structures in the image
  cropped_objs <- pbapply::pblapply(removed_images, 
                function(x) crop_out_boxesR(x, hmax = 300), cl = main_cl)
  
  ###get checkbox questions
  chkbox_cnts <-  pbapply::pblapply(1:length(removed_images),
                        function(x) {identify_chkboxes_by_partsR(cropped_objs[[x]]$bounds_df, 
                          removed_images[[x]], cl = 1)},  cl = main_cl)
  
  chkbox_cnts1 <- pbapply::pblapply(chkbox_cnts, 
                        function(x){
                           if (is.null(x)) return(x)
                               cutoff <- (x$h %>% mean()) * 0.95
                               x %>% dplyr::filter(h >= cutoff) -> x2
                               x %>% dplyr::filter(h < cutoff) -> x1
                               return(x2)}, cl = 1)
  
  question_df1 <- pbapply::pblapply(1:length(chkbox_cnts1), 
                        function(x){get_chkbox_wrapperR(
                            chkbox_df = chkbox_cnts1[[x]],
                            words_df = res_lines_df_list[[x]], 
                            lines_df = res_lines_only_df_list[[x]],
                            img = resized_images[[x]]$gray_img, cl = sub_cl)}, cl = main_cl)
  
  ##match results from main azure api push to the cropped boxes
  bounds_df1 <- pbapply::pblapply(1:length(res_lines_df_list), 
                  function(x){ az_to_cv2_box(cropped_objs[[x]]$bounds_df, res_lines_list[[x]])}, cl = 1)
  
  ##post individual boxes to azure api
  bounds_df2 <- pbapply::pblapply(1:length(bounds_df1), 
                  function(x){
                    vec_post_cropped_azure(
                           df = bounds_df1[[x]],
                           cropped_tm_dir = cropped_tm_dir, 
                           img = resized_images[[x]]$gray_img,
                           azure_creds = azure_creds, push_to_az = box_push_to_az,
                           box_highlight = box_highlight, remove_fl = remove_fl)}, cl = 1)
  
  ##get ocr results from azure api
  bounds_df3 <- pbapply::pblapply(bounds_df2, vec_get_cropped_azure)
  
  
  parse_df1 <- pbapply::pblapply(bounds_df3,  function(x) {
    x %>%
      purrrlyr::by_row(
        function(row) {
          df <- row$get_res[[1]]
          if (nrow(df) == 0) return("")
          df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
        }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')
  }, cl = 1)
  
  1:length(  image_files) %>% 
    purrr::map(~list(parse_df1[[.]], question_df1[[.]]))
}

