#library(reticulate)
library(dplyr)
library(zeallot)
library(alvision)

#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/ACE Contrractors Pollution.pdf"
#image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = 400)
#saveRDS(image_files, 'image_files.rds')
image_files <- readr::read_rds('image_files.rds')

tictoc::tic()
results <- image_files %>%
  purrr::map(~ocr_img_wrapper(img_file = .,
                                     hmax = 300, cropped_tm_dir = cropped_tm_dir,
                                     azure_creds = azure_creds, box_push_to_az = F,
                                     box_highlight = F, remove_fl = F))
tictoc::toc()

img_file <- image_files[3]

#Read the image
tictoc::tic()
c(main_img, color_img) %<-% resize_png(img_file)
tictoc::toc()

tictoc::tic()
# analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
#                           endpoint = azure_creds$endpoint,
#                           image_path = normalizePath(main_img ))
# saveRDS(analysis_res, 'analysis_res.rds')
analysis_res <- readr::read_rds('analysis_res.rds')
analysis_res$recognitionResult$lines -> res_lines
tictoc::toc()

tictoc::tic()
res_lines_df <- az_lines_to_df(res_lines)
res_lines_only_df <- az_words_to_df(res_lines, type = 'line')
tictoc::toc()

tictoc::tic()
crop_out_boxes(main_img, hmax = 300) %->% c(img, img_bin, img_final_bin,
                                      contours, bounds_df, hierarchy)
tictoc::toc()

tictoc::tic()
chkbox_cnts <- identify_chkboxes_by_parts(bounds_df, color_img)
tictoc::toc()

chkbox_cnts %>% filter(h >= 35) -> chkbox_cnts2
chkbox_cnts %>% filter(h < 35) -> chkbox_cnts1
#
# question_df1 <- get_chkbox_wrapper(chkbox_df = chkbox_cnts2,
#           words_df = res_lines_df, lines_df = res_lines_only_df, img = img)
#
# tictoc::tic()
# bounds_df1 <- az_to_cv2_box(bounds_df, res_lines)
#
# bounds_df2 <- vec_post_cropped_azure(df = bounds_df1, cropped_tm_dir = cropped_tm_dir,
#                   img = img, azure_creds = azure_creds, push_to_az = F,
#                   box_highlight = F, remove_fl = F)
#
# bounds_df3 <- vec_get_cropped_azure(bounds_df2)
#
# #parse_df <- readr::read_rds('inst/data/azure parsed results/ace page2.rds')
# parse_df1 <- bounds_df3 %>%
#   purrrlyr::by_row(
#     function(row) {
#       df <- row$get_res[[1]]
#       if (nrow(df) == 0) return("")
#       df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
#     }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')
# tictoc::toc()

# parse_df2 <- parse_df1 %>% dplyr::arrange(row, col, x, y) %>%
#   dplyr::group_by(row, col) %>% dplyr::summarise(txt = paste0(txt, collapse = ";")) %>%
#   dplyr::select(row, col, txt) %>%
#   dplyr::arrange(col, row) %>%
#   tidyr::pivot_wider(names_from = col, values_from = txt )%>%
#   dplyr::arrange(row)
#
# parse_df3 <- parse_df1 %>% dplyr::arrange(row, col, x, y) %>%
#   dplyr::group_by(row) %>% dplyr::summarise(txt = paste0(txt, collapse = "; ")) %>%
#   dplyr::select(row, txt)

tictoc::tic()
c(parse_df1b, question_df1b) %<-% ocr_img_wrapper(img_file = image_files[7],
                                                  hmax = 300,
                        cropped_tm_dir = cropped_tm_dir, azure_creds = azure_creds,
                        box_push_to_az = F, box_highlight = F, remove_fl = F)
tictoc::toc()

parse_df2b <- parse_df1b %>% dplyr::arrange(row, col, x, y) %>%
  dplyr::group_by(row, col) %>% dplyr::summarise(txt = paste0(txt, collapse = ";")) %>%
  dplyr::select(row, col, txt) %>%
  dplyr::arrange(col, row) %>%
  tidyr::pivot_wider(names_from = col, values_from = txt )%>%
  dplyr::arrange(row)

parse_df3b <- parse_df1b %>% dplyr::arrange(row, col, x, y) %>%
  dplyr::group_by(row) %>% dplyr::summarise(txt = paste0(txt, collapse = "; ")) %>%
  dplyr::select(row, txt)




text_data <- 1:length(results) %>%
    purrr::map_df(function(pg) results[[pg]][[1]] %>% dplyr::mutate(page = pg))

question_data <- 1:length(results) %>%
  purrr::map_df(function(pg) results[[pg]][[2]] %>% dplyr::mutate(page = pg)) %>%
  dplyr::filter(w > (mean(w) - 3))

library(profvis)


tictoc::tic()
#profvis({
results <- ocr_pdf(pdf_file = pdf_file, hmax = 300, cropped_tm_dir, azure_creds,
                   box_push_to_az = F, box_highlight = F, remove_fl = F, dpi = 400)
#})
tictoc::toc()
