#'@title azure post
#'@param subscription_key azure key
#'@param endpoint azure service end point
#'@param image_path path of image to recognize
#'@export
azure_post <- function(subscription_key, endpoint, image_path){
  vision_base_url <- paste0(endpoint, "vision/v2.0/")
  text_recognition_url <- paste0(vision_base_url, "recognizeText")
  
  #check the size of the image if not meeting 50X50, resize it
  loaded_img <- cv2$imread(image_path, 0L)
  dims <- dim(loaded_img)
  if (min(dims[1:2]) < 50) {
    scale <- 50 / min(dims[1:2]) 
    resize_dim <- as.integer(ceiling(c(dims[2], dims[1]) * scale))
    loaded_img <- cv2$resize(loaded_img %>% reticulate::np_array('uint8'), 
                              reticulate::tuple(resize_dim[1], resize_dim[2]))
    cv2$imwrite(image_path, loaded_img)
  } 
  
  # Read the image into a byte array
  image_data <-  py_built$open(image_path, "rb")$read()
  headers = list('Ocp-Apim-Subscription-Key'= subscription_key,
    'Content-Type'= 'application/octet-stream')
  params   = list('mode' = 'Handwritten')
  response = requests$post(
    text_recognition_url, headers=headers, params = params, data=image_data)
  response$raise_for_status()
  return(list(response, headers))
}

#'@title azure get
#'@param response result of azure post
#'@param headers  also result of azure post
#'@export
azure_get <- function(response, headers){
  # The recognized text isn't immediately available, so poll to wait for completion.
  try_idx = 0
  for (i in 1:61)
  {
    response_final <- requests$get(response$headers["Operation-Location"], headers=headers)
    analysis <- response_final$json()
    if (grepl("recognitionResult", names(analysis)) %>% sum() >= 1) {break }
    if (grepl("status", names(analysis)) %>% sum() <1 ) {

    }else {
      if ((grepl("status", names(analysis)) %>% sum() >= 1) & analysis[['status']] == 'Failed') break
    }
    if (i >= 0) {
      print(i)
      print(paste0('sleep ', i+1, 's'))
      Sys.sleep(i+1)
      print(analysis)
    }
   }
  return(analysis)
}

#'@title azure vision call function
#'@param subscription_key azure key
#'@param endpoint azure service end point
#'@param image_path path of image to recognize
#'@export
azure_vis <- function(subscription_key, endpoint, image_path){
  c(response, headers) %<-% azure_post(subscription_key, endpoint, image_path)
  return(azure_get(response, headers))
}

