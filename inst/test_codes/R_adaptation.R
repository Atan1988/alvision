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
document <- reticulate::py_load_object('inst/data/bank statement resp.pyobj')
parsed_txt <- document$text

contents <- get_doc_contents(document)
txt_structure_df <- get_doc_contents(document) %>% strcture_doc()



txt_structure_df %>% dplyr::group_by(BLOCK, PARA) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  dplyr::mutate(text = fix_str_bankstatement(text)
                )

library(dplyr)
bounds_blocks <-  get_document_bounds(document, FeatureType$BLOCK) %>%
  bounds_to_df('block')

bounds_paragraph <-  get_document_bounds(document, FeatureType$PARA) %>%
  bounds_to_df('PARA')

bounds_words <- get_document_bounds(document, FeatureType$WORD) %>%
  bounds_to_df('WORD')

words_content <- get_words_content(document) %>%
  strcture_words()

small_df <- bounds_words
large_df <- bounds_paragraph
small_var <- 'WORD'
large_var <- 'PARA'

small_df %>% dplyr::group_by_at(vars(small_var)) %>%
  tidyr::nest() %>%
  purrrlyr::by_row(function(row){
    row$data[[1]] %>% dplyr::inner_join(large_df, by = 'pos') %>%
      dplyr::arrange_at(vars(large_var)) %>%
      dplyr::mutate(chk = dplyr::case_when(
        pos == 0 & x.x >= x.y & y.x >= y.y ~ 1,
        pos == 1 & x.x <= x.y & y.x >= y.y ~ 1,
        pos == 2 & x.x <= x.y & y.x <= y.y ~ 1,
        pos == 3 & x.x >= x.y & y.x <= y.y ~ 1,
        TRUE ~ 0
      )) %>% dplyr::group_by_at(vars(large_var)) %>%
      dplyr::summarise_at(vars(chk), sum) %>% dplyr::filter(chk == 4) %>% select(large_var)
  }) %>% dplyr::select(-data) %>% tidyr::unnest(cols = '.out')


