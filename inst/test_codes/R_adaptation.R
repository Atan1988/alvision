library(reticulate)
library(jsonlite)
library(dplyr)
library(purrr)

gcp <- import('google')
vision <- gcp$cloud$vision
types <- vision$types
service_account <- gcp$oauth2$service_account
io <- import('io')
os <- import('os')
json <- import('json')
PIL <- import('PIL')
Image <- PIL$Image
ImageDraw <- PIL$ImageDraw
enum <- import('enum')$Enum

image_file <- '~/800px-Wachovia_National_Bank_1906_statement.jpg'
image  <-  Image$open(normalizePath(image_file))

info <- jsonlite::read_json('C:/Users/allen/Documents/tranlator-95964dd00b2f.json')
credentials <-  service_account$Credentials$from_service_account_info(info)

client = vision$ImageAnnotatorClient(credentials=credentials)
image_file1 <- io$open(normalizePath(image_file), 'rb')
content <-  image_file1$read()
content_image <-  types$Image(content=content)

###run api
# response = client$document_text_detection(image=content_image)
# document = response$full_text_annotation

###pull from archived object, save $$$$
document <- py_load_object('inst/data/bank statement resp.pyobj')
parsed_txt <- document$text

bounds_blocks <-  get_document_bounds(document, FeatureType$BLOCK) %>%
  bounds_to_df('block')

bounds_paragraph <-  get_document_bounds(document, FeatureType$PARA) %>%
  bounds_to_df('PARA')

bounds_words <- get_document_bounds(document, FeatureType$WORD) %>%
  bounds_to_df('WORD')

words_content <- get_words_content(document) %>%
  strcture_words()
