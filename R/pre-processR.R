#'@title crop out boxes
#'@param img_file file of image
#'@param hmax max height allowed for boxes
#'@export
crop_out_boxes <- function(img_file, hmax){
  # Read the image
  
  img <-  cv2$imread(normalizePath(img_file), 0L) %>%
    reticulate::np_array(dtype = "uint8")

  tic()
  img1 <- Rvision::image(img_file)
  toc()
  # Thresholding the image
  tic()
  c(thresh, img_bin) %<-% cv2$threshold(img, 128, 255, bitwOr(cv2$THRESH_BINARY, cv2$THRESH_OTSU))
  # Invert the image
  img_bin <- 255-img_bin
  toc()
  
 
  # Defining a kernel length
  kernel_length <- ((np$array(img) %>% dim() %>% .[2])%/% 100) %>% as.integer()
  # A verticle kernel of (1 X kernel_length), which will detect all the verticle lines from the image.
  verticle_kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, reticulate::tuple(1L, kernel_length))%>%
    reticulate::np_array(dtype = "uint8")
  # A horizontal kernel of (kernel_length X 1), which will help to detect all the horizontal line from the image.
  hori_kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, reticulate::tuple(kernel_length, 1L)) %>%
    reticulate::np_array(dtype = "uint8")
  # A kernel of (3 X 3) ones.
  kernel <- cv2$getStructuringElement(cv2$MORPH_RECT, reticulate::tuple(3L, 3L)) %>%
    reticulate::np_array(dtype = "uint8")
  # Morphological operation to detect vertical lines from an image
  img_temp1 <- cv2$erode(img_bin%>% reticulate::np_array(dtype = "uint8"), verticle_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  verticle_lines_img <- cv2$dilate(img_temp1, verticle_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  #cv2$imwrite("verticle_lines.jpg",verticle_lines_img)
  # Morphological operation to detect horizontal lines from an image
  img_temp2 <- cv2$erode(img_bin %>% reticulate::np_array(dtype = "uint8"), hori_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  horizontal_lines_img <- cv2$dilate(img_temp2, hori_kernel, iterations=3L)%>%
    reticulate::np_array(dtype = "uint8")
  #cv2$imwrite("horizontal_lines.jpg",horizontal_lines_img)
  # Weighting parameters, this will decide the quantity of an image to be added to make a new image.
  alpha <- 0.5
  beta <- 1.0 - alpha
  # This function helps to add two image with specific weight parameter to get a third image as summation of two image.
  img_final_bin <- cv2$addWeighted(verticle_lines_img, alpha,
                                   horizontal_lines_img, beta, 0.0)
  dim1 <- dim(img_final_bin)
  img_final_bin <- cv2$erode(matrix(bitwNot(img_final_bin), nrow = dim1[1]) %>%
                               reticulate::np_array(dtype = "uint8"),
                             kernel, iterations=2L) %>% reticulate::np_array(dtype = "uint8")
  c(thresh, img_final_bin) %<-% cv2$threshold(img_final_bin, 128,255,
                                              bitwOr(cv2$THRESH_BINARY, cv2$THRESH_OTSU))
  #cv2$imwrite("img_final_bin.jpg",img_final_bin)
  # Find contours for image, which will detect all the boxes
  c(contours, hierarchy) %<-% cv2$findContours(img_final_bin%>% reticulate::np_array(dtype = "uint8"),
                                               cv2$RETR_TREE, cv2$CHAIN_APPROX_SIMPLE)
  c(contours, boundingBoxes) %<-% sort_contours(contours, "top-to-bottom", hmax = hmax)
  #bounds <-  get_crop_bounds(contours, hmax)
  return(list(img, img_bin, img_final_bin, contours, boundingBoxes, hierarchy))
}