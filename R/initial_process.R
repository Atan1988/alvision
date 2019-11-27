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
#'@param file_size_limit file size limit in mbs
#'@export
resize_png  <- function(img_file, file_size_limit = 3.8) {
  parent_folder <- dirname(img_file)
  resize_fl <- file.path(parent_folder,
                  paste0('resize-full ',
                      strsplit(img_file, '/') %>% .[[1]] %>% .[length(.)])
              )
  ###first convert to gray
  raw_img <- magick::image_read(img_file)

  raw_img %>%
    magick::image_quantize(colorspace = 'gray') %>%
    magick::image_write(resize_fl)

  ###check image size
  img_sz <- file.size(resize_fl) / (1024^2)
  raw_dim <- raw_img %>% magick::image_data() %>% dim()

  if (img_sz > file_size_limit) {
     scale <- sqrt(file_size_limit / img_sz)
     sz <- floor(max(raw_dim) * scale)
     magick::image_read(resize_fl) %>%
       magick::image_quantize(colorspace = 'gray') %>%
       magick::image_resize(paste0(sz, "x", sz)) %>%
       magick::image_write(resize_fl)
  }

  return(resize_fl)
}
