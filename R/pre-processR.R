#'@title crop out boxes
#'@param img_file file of image
#'@param hmax max height allowed for boxes
#'@export
crop_out_boxesR <- function(img_file, hmax){
  # Read the image
  if ("Rcpp_Image" %in% class(img_file)) {
    img1 <- img_file
  } else {
    img1 <- Rvision::image(img_file)
  }

  img1 <- Rvision::changeBitDepth(img1, '8U')
  # Thresholding the image
  img_bin1 <- Rvision::adaptiveThreshold(img1)
 
  # Defining a kernel length
  kernel_length1 <- as.integer(img1$dim()[2] %/% 100)
  # A verticle kernel of (1 X kernel_length), which will detect all the verticle lines from the image.
  verticle_kernel1 <- matrix(rep(1, kernel_length1), ncol = 1)
  # A horizontal kernel of (kernel_length X 1), which will help to detect all the horizontal line from the image.
  hori_kernel1 <- matrix(rep(1, kernel_length1), nrow = 1)
  # A kernel of (3 X 3) ones.
  kernel1 <- matrix(rep(1, 9), nrow =3)
  # Morphological operation to detect vertical lines from an image
  img_temp1_1 <- Rvision::morph(img_bin1, operation = 'erode',
                            kernel = verticle_kernel1, iterations = 3)
  
  verticle_lines_img1 <- Rvision::morph(img_temp1_1, operation = 'dilate',
                            kernel = verticle_kernel1, iterations = 3)
  #cv2$imwrite("verticle_lines.jpg",verticle_lines_img)
  # Morphological operation to detect horizontal lines from an image
  img_temp2_1 <- Rvision::morph(img_bin1, operation = 'erode',
                                kernel = hori_kernel1, iterations = 3)
  
  horizontal_lines_img1 <- Rvision::morph(img_temp2_1, operation = 'dilate',
                            kernel = hori_kernel1, iterations = 3)
  #cv2$imwrite("horizontal_lines.jpg",horizontal_lines_img)
  # Weighting parameters, this will decide the quantity of an image to be added to make a new image.
  alpha <- 0.5
  beta <- 1.0 - alpha
  # This function helps to add two image with specific weight parameter to get a third image as summation of two image.
  img_final_bin1 <- Rvision::addWeighted(verticle_lines_img1, 
                      horizontal_lines_img1, c(alpha, beta))
  dim1_1 <- dim(img_final_bin1)
  
  img_mat <- img_final_bin1$toR()
  #mat <- matrix(bitwNot(img_mat), nrow = dim1_1[1])
  mat <- matrix(255 - img_mat, nrow = dim1_1[1])
  dim(mat) <- c(dim(mat), 1)
  
  if (mean(mat) == -1){
    boundingBoxes1 <- tibble::tibble(x = 1, y1 = 1, w = 1, h = 1, y = 1, cnum = as.integer(1)) %>% .[0, ]
    contours1 <- list()
    return(list(contours = contours1, bounds_df = boundingBoxes1))
  }
  
  img_final_bin1 <- Rvision::morph(Rvision::image(mat),
                           operation = 'erode', kernel = kernel1, iterations = 2)
  #img_final_bin1  <- Rvision::image(img_mat)
  img_final_bin1 <- Rvision::changeBitDepth(img_final_bin1, '8U')
  
  img_final_bin1 <- Rvision::adaptiveThreshold(img_final_bin1, 
                      threshold_type ='binary')
  
  img_final_bin1 <- extend_horizontal_lines(img_final_bin1)
  #cv2$imwrite("img_final_bin.jpg",img_final_bin)
  # Find contours for image, which will detect all the boxes
  img_final_bin1 <- Rvision::changeBitDepth(img_final_bin1, '8U')
  c(contours1, hierarchy1) %<-% Rvision::findContours(img_final_bin1,
                                      mode = "tree", method = 'simple')
  contours1 <- base::split(contours1, contours1$id)
  c(contours1, boundingBoxes1) %<-%  sort_contoursR(contours1, 
                                "top-to-bottom", hmax = hmax, img_max_y = dim(img1)[1])

  #bounds <-  get_crop_bounds(contours, hmax)
  #return(list(img1, img_bin1, img_final_bin1, contours1, boundingBoxes1, hierarchy1))
  return(list(contours = contours1, bounds_df = boundingBoxes1))
}

#' @title sort contours
#' @param cnts contours
#' @param method 'left-to-right' or 'bottom-to-top'
#' @param hmax max height to include
#' @param img_max_y the max y variable of image
#' @export
sort_contoursR <- function(cnts, method="left-to-right", hmax = 100, img_max_y){
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
    boundingRect(cnts[[y]], img_max_y) %>% dplyr::mutate(cnum = !!y)
  }) %>% dplyr::arrange_at(i)
  if (reverse)  boundingBoxes <-  boundingBoxes %>% .[nrow(.):1, ]
  boundingBoxes <- boundingBoxes %>% dplyr::filter(h < hmax, h > 20)

  return(list(cnts[boundingBoxes$cnum], boundingBoxes))
}
  
#' @title remove non black and white color from an image to remove handwritten
#' @param img_file the file path of the image
#' @param min_clr minimum color number to filter
#' @param max_clr maximum color number to filter
#' @export
remove_colorR <- function(img_file, min_clr = 100, max_clr = 250) {
  if ("Rcpp_Image" %in% class(img_file)) {
    image <- img_file$toR()
  } else {
    image <- Rvision::image(img_file)
    image <- image$toR()
  }
  
  ch1 <- which(image[,,1] > min_clr & image[,,1] < max_clr)
  ch2 <- which(image[,,2] > min_clr & image[,,2] < max_clr)
  ch3 <- which(image[,,3] > min_clr & image[,,3] < max_clr)
  ch <- c(ch1, ch2, ch3) %>% unique()
  image[,,1][ch] <- 255
  image[,,2][ch] <- 255
  image[,,3][ch] <- 255
  
  image <- Rvision::image(image)
  image <-  Rvision::changeColorSpace(image, 'GRAY')
  #return(image$toR())
  return(image$toR())
}