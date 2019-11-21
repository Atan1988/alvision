#'@title crop out boxes
#'@param img_file file of image
#'@param hmax max height allowed for boxes
#'@export
crop_out_boxes <- function(img_file, hmax){
  # Read the image
  img <-  cv2$imread(normalizePath(img_file), 0L) %>%
    reticulate::np_array(dtype = "uint8")
  # Thresholding the image
  c(thresh, img_bin) %<-% cv2$threshold(img, 128, 255, bitwOr(cv2$THRESH_BINARY, cv2$THRESH_OTSU))
  # Invert the image
  img_bin <- 255-img_bin
  # Defining a kernel length
  kernel_length <- ((np$array(img) %>% dim() %>% .[2])%/% 100) %>% as.integer()
  # A verticle kernel of (1 X kernel_length), which will detect all the verticle lines from the image.
  verticle_kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, tuple(1L, kernel_length))%>%
    reticulate::np_array(dtype = "uint8")
  # A horizontal kernel of (kernel_length X 1), which will help to detect all the horizontal line from the image.
  hori_kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, tuple(kernel_length, 1L)) %>%
    reticulate::np_array(dtype = "uint8")
  # A kernel of (3 X 3) ones.
  kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, tuple(3L, 3L)) %>%
    reticulate::np_array(dtype = "uint8")
  # Morphological operation to detect vertical lines from an image
  img_temp1 <- cv2$erode(img_bin%>% reticulate::np_array(dtype = "uint8"), verticle_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  verticle_lines_img <- cv2$dilate(img_temp1, verticle_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  #cv2.imwrite("verticle_lines.jpg",verticle_lines_img)
  # Morphological operation to detect horizontal lines from an image
  img_temp2 <- cv2$erode(img_bin %>% reticulate::np_array(dtype = "uint8"), hori_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  horizontal_lines_img <- cv2$dilate(img_temp2, hori_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  #cv2.imwrite("horizontal_lines.jpg",horizontal_lines_img)
  # Weighting parameters, this will decide the quantity of an image to be added to make a new image.
  alpha <- 0.5
  beta <- 1.0 - alpha
  # This function helps to add two image with specific weight parameter to get a third image as summation of two image.
  img_final_bin <- cv2$addWeighted(verticle_lines_img, alpha, horizontal_lines_img, beta, 0.0)
  dim1 <- dim(img_final_bin)
  img_final_bin <- cv2$erode(matrix(bitwNot(img_final_bin), nrow = dim1[1]) %>%
                               reticulate::np_array(dtype = "uint8"),
                             kernel, iterations=2L) %>% reticulate::np_array(dtype = "uint8")
  c(thresh, img_final_bin) %<-% cv2$threshold(img_final_bin, 128,255,
                                              bitwOr(cv2$THRESH_BINARY, cv2$THRESH_OTSU))
  #cv2$imwrite("img_final_bin.jpg",img_final_bin)
  # Find contours for image, which will detect all the boxes
  c(im2, contours, hierarchy) %<-% cv2$findContours(img_final_bin%>% reticulate::np_array(dtype = "uint8"),
                                                    cv2$RETR_TREE, cv2$CHAIN_APPROX_SIMPLE)
  c(contours, boundingBoxes) %<-% sort_contours(contours, "top-to-bottom", hmax = hmax)
  #bounds <-  get_crop_bounds(contours, hmax)
  return(list(img, img_bin, img_final_bin, contours, boundingBoxes, hierarchy))
}

#' @title sort contours
#' @param cnts contours
#' @param method 'left-to-right' or 'bottom-to-top'
#' @param hmax max height to include
#' @export
sort_contours <- function(cnts, method="left-to-right", hmax = 100){
  # initialize the reverse flag and sort index
  reverse <-  F
  i <-  'x'
  # handle if we need to sort in reverse
  if (method == "right-to-left" | method == "bottom-to-top") reverse = T
  # handle if we are sorting against the y-coordinate rather than
  # the x-coordinate of the bounding box
  if (method == "top-to-bottom" | method == "bottom-to-top") i = 'y'
  # construct the list of bounding boxes and sort them from top to
  # bottom
  boundingBoxes <-  1:length(cnts) %>% purrr::map_df(function(y) {
    x <- cnts[[y]]
    b <- cv2$boundingRect(x); names(b) <- c('x', 'y', 'w', 'h');
    tibble::as_tibble(b) %>% dplyr::mutate(cnum = !!y)
    }) %>% dplyr::arrange_at(i)

  if (reverse)  boundingBoxes <-  boundingBoxes %>% .[nrow(.):1, ]
  boundingBoxes <- boundingBoxes %>% dplyr::filter(h < hmax)
  # (cnts, boundingBoxes) = zip(*sorted(zip(cnts, boundingBoxes),
  #                                     key=lambda b: b[1][i], reverse=reverse))
  # return the list of sorted contours and bounding boxes
  return(list(cnts[boundingBoxes$cnum], boundingBoxes))
}

#' @title output_cropped_img
#' @param cropped_dir_path temporary path for cropped out img
#' @param img original image
#' @param idx index of image file
#' @param x  x coord
#' @param y  y coord
#' @param w  width
#' @param h  height
#' @export
output_cropped_img <- function(cropped_dir_path, img, idx, x, y, w, h){
  new_img <- img %>% reticulate::py_to_r() %>% .[y:(y+h), x:(x+w)]
  cv2$imwrite(paste0(cropped_dir_path, idx, '.png'), new_img)
  return(paste0(cropped_dir_path, idx, '.png'))
}

