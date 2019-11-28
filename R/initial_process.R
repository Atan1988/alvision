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
  unlink(converted_img)
  return(file.path(parent_folder, converted_img))
}

#'@title resize the image to meet azure criteria
#'@param pdf_file original pdf file path
#'@param page which page to read in
#'@param dpi density for the image_read_pdf function
#'@param file_size_limit file size limit in mbs
#'@export
resize_png  <- function(pdf_file, page, dpi = 400, file_size_limit = 3.8) {
  parent_folder <- dirname(pdf_file)
  resize_fl <- file.path(parent_folder,
                  paste0('resize-full ',
                      strsplit(pdf_file, '/') %>% .[[1]] %>% .[length(.)])
              ) %>% gsub('\\.pdf', '\\.png', .)
  ###first convert to gray
  raw_img <- magick::image_read_pdf(pdf_file, pages = 2, density = 400)

  magick::image_convert(raw_img, colorspace = 'gray') -> grayed

  ###check dimension
  raw_dim <- get_img_dim(grayed)

  if (max(raw_dim) > 4000) {
    dim_scale <- 4000 / max(raw_dim)
    dim_sz <- floor(max(raw_dim) * dim_scale)
    grayed <- grayed %>%
      magick::image_resize(paste0(dim_sz, "x", dim_sz))
    raw_dim <- grayed %>% magick::image_data() %>% dim()
  }

  ##write gray image out
  magick::image_write(grayed, resize_fl)

  ###check image size
  img_sz <- file.size(resize_fl) / (1024^2)

  ###convert the image size if it's larger than 4M
  if (img_sz > file_size_limit) {
     scale <- sqrt(file_size_limit / img_sz)
     sz <- floor(max(raw_dim) * scale)
     magick::image_read(resize_fl) %>%
       magick::image_convert(colorspace = 'gray')%>%
       magick::image_resize(paste0(sz, "x", sz)) %>%
       magick::image_write(resize_fl)
  }

  return(resize_fl)
}
