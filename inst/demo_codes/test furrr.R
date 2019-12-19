library(zeallot)
library(alvision)

#test docker
#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
cropped_tm_dir <- 'inst/data/tmp_cropped/'

image_files <- readr::read_rds('image_files.rds')
img_file <- image_files[1]

#library(profvis)
tictoc::tic()
#profvis({
c(main_img, color_img) %<-% resize_png(img_file)
#})
tictoc::toc()

##crop out boxes if document is a form
tictoc::tic()
crop_out_boxes(main_img, hmax = 300) %->% c(img, img_bin, img_final_bin,
                                             contours, bounds_df, hierarchy)
tictoc::toc()

###get checkbox questions
future::plan(future::multiprocess(workers = 6))
tictoc::tic()
chkbox_cnts <- identify_chkboxes_by_parts(bounds_df, color_img)
tictoc::toc()