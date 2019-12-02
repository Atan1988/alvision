#' @title create png from pdf
#' @param pdf_file pdf file
#' @param pages pages to create, default to NULL
#' @param dpi dpi, quality of the image being created
#' @export
crt_png_from_pdf <- function(pdf_file, pages = NULL, dpi = 350) {
  tictoc::tic()
  if (is.null(pages)) {f <- NULL; l <- NULL} else {
    f <- min(pages); l <- max(pages)
  }
  images <- pdf2image$convert_from_path(pdf_file, dpi=dpi,
                                        first_page=f, last_page=l)
  parent_folder <- dirname(pdf_file)
  image_files <- file.path(parent_folder,
                           paste0(gsub(paste0(parent_folder, "/|\\.pdf"), "", pdf_file), "_",
                                  1:length(images), '.png'))
  1:length(images) %>%
    purrr::map(~images[[.]]$save(image_files[.]))
  tictoc::toc()
  return(image_files)
}

#'@title resize the image to meet azure criteria
#'@param img_file original pdf file path
#'@param file_size_limit file size limit in mbs
#'@export
resize_png  <- function(img_file, file_size_limit = 3.8) {
  parent_folder <- dirname(img_file)
  resize_fl <- file.path(parent_folder,
                  paste0('resize-full ',
                      strsplit(img_file, '/') %>% .[[1]] %>% .[length(.)])
              ) %>% gsub('\\.pdf', '\\.png', .)
  resize_fl1 <- file.path(parent_folder,
                         paste0('clr resize-full ',
                                strsplit(img_file, '/') %>% .[[1]] %>% .[length(.)])
  ) %>% gsub('\\.pdf', '\\.png', .)
  ###first convert to gray
  orig_img <- cv2$imread(normalizePath(img_file), 1L) %>%
    reticulate::np_array(dtype = "uint8")
  grayed <- cv2$imread(normalizePath(img_file), 0L) %>%
    reticulate::np_array(dtype = "uint8")

  ###check dimension
  raw_dim <- grayed$shape %>% reticulate::py_to_r() %>% unlist()

  if (max(raw_dim) > 4000) {
    dim_scale <- 4000 / max(raw_dim)
    dim_sz <- floor( raw_dim * dim_scale) %>% as.integer()
    grayed <- grayed %>% cv2$resize(reticulate::tuple(dim_sz[2], dim_sz[1])) %>%
      reticulate::np_array(dtype = "uint8")
    orig_img <- orig_img %>% cv2$resize(reticulate::tuple(dim_sz[2], dim_sz[1])) %>%
      reticulate::np_array(dtype = "uint8")
    raw_dim <- grayed$shape %>% reticulate::py_to_r() %>% unlist()
  }

  ##write gray image out
  cv2$imwrite(resize_fl, grayed)
  cv2$imwrite(resize_fl1, orig_img)

  ###check image size
  img_sz <- file.size(resize_fl) / (1024^2)

  ###convert the image size if it's larger than 4M
  if (img_sz > file_size_limit) {
     scale <- sqrt(file_size_limit / img_sz)
     sz <- floor(raw_dim * scale) %>% as.integer()
     cv2$imread(normalizePath(resize_fl), 0L) %>%
       reticulate::np_array(dtype = "uint8") %>%
       cv2$resize(reticulate::tuple(sz[2], sz[1])) %>%
       reticulate::np_array(dtype = "uint8") %>%
       cv2$imwrite(resize_fl, .)
    
     ##write out fixed original color image
     orig_img %>%
       cv2$resize(reticulate::tuple(sz[2], sz[1])) %>%
       reticulate::np_array(dtype = "uint8")%>%
       cv2$imwrite(resize_fl1, .)
  }

  return(resize_fl)
}
