#'@title azure post
#'@param subscription_key azure key
#'@param endpoint azure service end point
#'@param image_path path of image to recognize
#'@export
azure_post <- function(subscription_key, endpoint, image_path){
  vision_base_url <- paste0(endpoint, "vision/v2.0/")
  text_recognition_url <- paste0(vision_base_url, "recognizeText")
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
  poll = T
  try_idx = 0
  while (poll)
  {
    response_final <- requests$get(response$headers["Operation-Location"], headers=headers)
    analysis <- response_final$json()
    if (grepl("recognitionResult", names(analysis)) %>% sum() >= 1) poll <- F
    if ((grepl("status", names(analysis)) %>% sum() >= 1) & analysis[['status']] == 'Failed') poll <- F
    try_idx = try_idx + 1
    if (try_idx >= 5) poll = F
    if (poll) Sys.sleep(1)
   }
  return(analysis)
}

#'@title azure vision call function
#'@param subscription_key azure key
#'@param endpoint azure service end point
#'@param image_path path of image to recognize
#'@export
zure_vis <- function(subscription_key, endpoint, image_path){
  c(response, headers) %<-% azure_post(subscription_key, endpoint, image_path)
  return(azure_get(response, headers))
}
