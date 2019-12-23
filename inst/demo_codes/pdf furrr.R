library(zeallot)
library(alvision)
library(tictoc)

#test docker
#reticulate::use_condaenv('computer_vision')
reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
azure_creds <- readxl::read_excel("inst/creds/azure creds.xlsx")
cropped_tm_dir <- 'inst/data/tmp_cropped/'

pdf_file <- "inst/raw_data/ACE Contrractors Pollution.pdf"; 
hmax = 300; cropped_tm_dir; azure_creds;
box_push_to_az = F; box_highlight = F; remove_fl = F; dpi = 400
main_cl <- 2; sub_cl <- 3

tic()
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
                        removed_images[[x]], cl = 3)},  cl =2)
tictoc::toc()

tic()
chkbox_cnts1 <- pbapply::pblapply(chkbox_cnts, 
                  function(x){
                    cutoff <- (x$h %>% mean()) * 0.95
                    x %>% dplyr::filter(h >= cutoff) -> x2
                    x %>% dplyr::filter(h < cutoff) -> x1
                    return(x2)
                  }, cl = 1)
toc()


tic()
question_df1 <- pbapply::pblapply(1:length(chkbox_cnts1), 
    function(x){get_chkbox_wrapperR(chkbox_df = chkbox_cnts1[[x]],
                                     words_df = res_lines_df_list[[x]], 
                                     lines_df = res_lines_only_df_list[[x]],
                                     img = resized_images[[x]]$gray_img, cl = 5) 
}, cl = 1)
toc()


##match results from main azure api push to the cropped boxes
tic()
bounds_df1 <- pbapply::pblapply(1:length(res_lines_df_list), 
                function(x){
                  az_to_cv2_box(cropped_objs[[x]]$bounds_df, res_lines_list[[x]])
          }, cl = 1)
toc()

##post individual boxes to azure api
tic()
bounds_df2 <- pbapply::pblapply(1:length(bounds_df1), 
              function(x){
                vec_post_cropped_azure(df = bounds_df1[[x]],
                    cropped_tm_dir = cropped_tm_dir, 
                    img = resized_images[[x]]$gray_img,
                    azure_creds = azure_creds, push_to_az = box_push_to_az,
                    box_highlight = box_highlight, remove_fl = remove_fl)
              }, cl = 1)

toc()
##get ocr results from azure api
tic()
bounds_df3 <- pbapply::pblapply(bounds_df2, vec_get_cropped_azure)
toc()

tic()
parse_df1 <- pbapply::pblapply(bounds_df3,  function(x) {
  x %>%
    purrrlyr::by_row(
      function(row) {
        df <- row$get_res[[1]]
        if (nrow(df) == 0) return("")
        df %>% dplyr::summarise(txt = paste(stringr::str_squish(txt), collapse = "  "))
      }, .to = '.txt') %>% tidyr::unnest(cols = '.txt')
  }, cl = 1)
toc()


toc()