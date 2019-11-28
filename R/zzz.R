cv2 <- NULL
ipywidgets <- NULL
np <- NULL
requests <- NULL
py_time <- NULL
py_built <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  # py_main <<- reticulate::py_run_file('inst/py_scripts/extract info from resp.py')
  # get_doc_contents <<- py_main$get_doc_content
  # get_document_bounds <<- py_main$get_document_bounds
  # assemble_word <<- py_main$assemble_word
  # get_words_content <<- py_main$get_words_content
  # FeatureType <<- py_main$FeatureType
  cv2 <<- reticulate::import('cv2', delay_load = TRUE)
  pdf2image <<- reticulate::import('pdf2image', delay_load = TRUE)
  ipywidgets <<- reticulate::import('ipywidgets', delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
  requests <<- reticulate::import('requests', delay_load = TRUE)
  py_time <<- reticulate::import('time', delay_load = TRUE)
  py_built <<- reticulate::import('builtins', delay_load = T)
  # py_main <<- reticulate::import_from_path('pre-process', path = 'inst/py_scripts/')
  # crop_out_boxes <<- py_main$crop_out_boxes
  # sort_contours <<-  py_main$sort_contours
  # get_crop_bounds <<-  py_main$get_crop_bounds
  # output_cropped_img <<- py_main$output_cropped_img
  # #
  # azure_py <<- reticulate::import_from_path('azure_vision_py', path = 'inst/py_scripts/')
  # azure_post <<- azure_py$azure_post
  # azure_get <<- azure_py$azure_get
  # azure_vis <<- azure_py$azure_vis
}
