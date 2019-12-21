library(zeallot)
library(alvision)
library(tictoc)

#test docker
#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readr::read_rds('inst/creds/azure credential.rds')
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/ACE Contrractors Pollution.pdf"; 
hmax = 300; cropped_tm_dir; azure_creds;
box_push_to_az = F; box_highlight = F; remove_fl = F; dpi = 400
main_cl <- 2; sub_cl <- 3

tic()
image_files <- crt_png_from_pdf(pdf_file = pdf_file, pages = NULL, dpi = dpi)
toc()

tic()
resized_images <- pbapply::pblapply(image_files, resize_png, cl = 1)  
toc()
resized_images[sapply(resized_images, is.null)] <- NULL

tic()
removed_images <- pbapply::pblapply(resized_images, 
                    function(x) {remove_colorR(x[[4]])}, cl = 1)
toc()

tic()
analysis_res_list <- pbapply::pblapply(resized_images, function(x){
  azure_vis(subscription_key = azure_creds$subscription_key,
            endpoint = azure_creds$endpoint,
            image_path = normalizePath(x[[1]]))
}, cl = 1)
toc()   

tic()
res_lines_list <- pbapply::pblapply(analysis_res_list, 
                        function(x){
        res_lines <- x$recognitionResult$lines
        if (length(res_lines) == 0) return(list(tibble::tibble(), tibble::tibble()))
        return(res_lines)
}, cl = 1)
toc()

tic()
res_lines_df_list <- pbapply::pblapply(res_lines_list, az_lines_to_df, cl = 1)
toc()
tic()
res_lines_only_df_list <- pbapply::pblapply(res_lines_list, 
              function(x)az_words_to_df(x, type = 'line'), cl = 1)
toc()

tictoc::tic()
#c(img, img_bin, img_final_bin, contours, bounds_df, hierarchy)
cropped_objs <- pbapply::pblapply(removed_images, 
                  function(x) crop_out_boxesR(x, hmax = 300), cl = 1)
tictoc::toc()
###get checkbox questions
tictoc::tic()
chkbox_cnts <-  pbapply::pblapply(1:length(removed_images),
    function(x) {identify_chkboxes_by_partsR(cropped_objs[[x]]$bounds_df, 
                        removed_images[[x]], cl = 4)},  cl =1)
tictoc::toc()