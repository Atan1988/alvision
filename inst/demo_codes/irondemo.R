reticulate::use_virtualenv('/opt/virtualenvs/r-tensorflow')
py_main <- reticulate::import_from_path('pre-process', path = 'inst/py_scripts/')

library(zeallot)
img_file <- "inst/raw_data/ACE Contrractors Pollution_2.png"

c(img, img_bin, img_final_bin, contours, bounds, hierarchy) %<-%
  py_main$crop_out_boxes(img_file, hmax = 300)

py_main$cv2$imwrite("r_py_boxes.png", img_final_bin)
