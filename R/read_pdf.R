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
