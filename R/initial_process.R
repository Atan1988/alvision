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
  orig_img1 <- Rvision::image(img_file)
  grayed1 <- Rvision::changeColorSpace(orig_img1, 'GRAY')

  ###check dimension
  raw_dim <- grayed1$dim()

  if (max(raw_dim) > 4000) {
    dim_scale <- 4000 / max(raw_dim)
    dim_sz <- floor( raw_dim * dim_scale) %>% as.integer()
    
    grayed1 <- Rvision::resize(grayed1, height = dim_sz[1], width = dim_sz[2])
      
    orig_img1 <- Rvision::resize(orig_img1, height = dim_sz[1], width = dim_sz[2])
    
    raw_dim <- grayed1$dim()
  }

  ##write gray image out
  Rvision::write.Image(grayed1, resize_fl)
  Rvision::write.Image(orig_img1, resize_fl1)
  ###check image size
  img_sz <- file.size(resize_fl) / (1024^2)

  ###convert the image size if it's larger than 4M
  if (img_sz > file_size_limit) {
     scale <- sqrt(file_size_limit / img_sz)
     sz <- floor(raw_dim * scale) %>% as.integer()

     grayed1 %>% 
       Rvision::resize(height = sz[1], width = sz[2]) %>% 
       Rvision::write.Image(resize_fl)

     ##write out fixed original color image
     orig_img1 %>% 
       Rvision::resize(height = sz[1], width = sz[2]) %>% 
       Rvision::write.Image(resize_fl1)
  }

  return(c(resize_fl, resize_fl1))
}
