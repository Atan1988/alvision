#' @title create png from pdf
#' @param pdf_file pdf file
#' @param pages pages to create, default to NULL
#' @param dpi dpi, quality of the image being created
#' @export
crt_png_from_pdf <- function(pdf_file, pages = NULL, dpi = 350) {
  parent_folder <- dirname(pdf_file)
  converted_img <- pdftools::pdf_convert(pdf = pdf_file, pages = pages, dpi = dpi)
  file.copy(from = converted_img, to = file.path(parent_folder, converted_img),
            overwrite = T)
  return(file.path(parent_folder, converted_img))
}

#'@title resize the image to meet azure criteria
#'@param img_file original file path
#'@param new_size in the form of YxY
#'@export
resize_png  <- function(img_file, new_size = '3500x3500') {
  parent_folder <- dirname(img_file)
  resize_fl <- file.path(parent_folder,
                  paste0('resize-full ',
                      strsplit(img_file, '/') %>% .[[1]] %>% .[length(.)])
              )
  raw_img <- magick::image_read(img_file)
  raw_img %>% magick::image_resize(new_size) %>%
    magick::image_quantize(colorspace = 'gray') %>%
    magick::image_write(resize_fl)
  return(resize_fl)
}
