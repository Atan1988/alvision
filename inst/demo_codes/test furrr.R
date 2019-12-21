library(zeallot)
library(alvision)

#test docker
#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
cropped_tm_dir <- 'inst/data/tmp_cropped/'

image_files <- readr::read_rds('image_files.rds')
img_file <- image_files[3]

#library(profvis)
tic()
tictoc::tic()
c(main_img_fl, color_img_fl, main_img, color_img) %<-% resize_png(img_file)
tictoc::toc()

tic()
removed_img %<-% remove_colorR(color_img_fl)
toc()

tic()
analysis_res <- azure_vis(subscription_key = azure_creds$subscription_key,
                          endpoint = azure_creds$endpoint,
                          image_path = normalizePath(main_img_fl))
analysis_res$recognitionResult$lines -> res_lines
if (length(res_lines) == 0) return(list(tibble::tibble(), tibble::tibble()))
toc()

res_lines_df <- az_lines_to_df(res_lines)
res_lines_only_df <- az_words_to_df(res_lines, type = 'line')

##crop out boxes if document is a form
tictoc::tic()
crop_out_boxesR(removed_img, hmax = 300) %->% c(img, img_bin, img_final_bin,
                                             contours, bounds_df, hierarchy)
tictoc::toc()
###get checkbox questions
tictoc::tic()
chkbox_cnts <- identify_chkboxes_by_partsR(bounds_df, removed_img, cl = 5)
tictoc::toc()

cutoff <- (chkbox_cnts$h %>% mean()) * 0.95
chkbox_cnts %>% dplyr::filter(h >= cutoff) -> chkbox_cnts2
chkbox_cnts %>% dplyr::filter(h < cutoff) -> chkbox_cnts1

tictoc::tic()
question_df1 <- get_chkbox_wrapperR(chkbox_df = chkbox_cnts2,
                            words_df = res_lines_df, lines_df = res_lines_only_df,
                            img = main_img, cl = 5)
tictoc::toc()

##match results from main azure api push to the cropped boxes
tic()
bounds_df1 <- az_to_cv2_box(bounds_df, res_lines)
toc()

##post individual boxes to azure api
box_push_to_az <- F; remove_fl = T;box_highlight = F
tic()
bounds_df2 <- vec_post_cropped_azure(df = bounds_df1,
                                     cropped_tm_dir = cropped_tm_dir, img = main_img,
                                     azure_creds = azure_creds, push_to_az = box_push_to_az,
                                     box_highlight = box_highlight, remove_fl = remove_fl)
toc()
##get ocr results from azure api
tic()
bounds_df3 <- vec_get_cropped_azure(bounds_df2)
toc()

##creating texts from lines in the get_res column
tic()
parse_df1 <- bounds_df3 %>%
  purrrlyr::by_row(
    function(row) {
      df <- row$get_res[[1]]
      if (nrow(df) == 0) return("")
      df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
    }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')
toc()
toc()