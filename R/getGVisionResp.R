

getGVisionResp <- function (imagePath, feature = "LABEL_DETECTION", numResults = 5)
{
  txt <- imageToText(imagePath)
  if (is.numeric(numResults)) {
    body <- paste0("{  \"requests\": [    {   \"image\": { \"content\": \"",
                   txt, "\" }, \"features\": [  { \"type\": \"",
                   feature, "\", \"maxResults\": ", numResults,
                   "} ],  }    ],}")
  }
  else {
    body <- paste0("{  \"requests\": [    {   \"image\": { \"content\": \"",
                   txt, "\" }, \"features\": [  { \"type\": \"",
                   feature, "\" } ],  }    ],}")
  }
  simpleCall <- gar_api_generator(baseURI = "https://vision.googleapis.com/v1/images:annotate",
                                  http_header = "POST")
  pp <- simpleCall(the_body = body)
  if (ncol(pp$content$responses) > 0) {
    res <- extractResponse(pp, feature)
  }
  else {
    res <- data.frame(error = "No features detected!")
  }
  return(res)
}
