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
tictoc::tic()
c(main_img_fl, color_img_fl, main_img, color_img) %<-% resize_png(img_file)
tictoc::toc()

orig_img <- cv2$imread(normalizePath(main_img), 0L) %>%
  reticulate::np_array(dtype = "uint8")

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
crop_out_boxesR(removed_img_fl, hmax = 300) %->% c(img, img_bin, img_final_bin,
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
question_df1 <- get_chkbox_wrapper(chkbox_df = chkbox_cnts2,
                                   words_df = res_lines_df, lines_df = res_lines_only_df,
                                   img = orig_img)
tictoc::toc()