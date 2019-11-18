library(reticulate)
library(jsonlite)
library(dplyr)
library(purrr)
library(zeallot)

cv2 <- import('cv2')
np <- import('numpy')

img_for_box_extraction_path <- 'inst/raw_data/ACE Contrractors Pollution_2.png'
# Read the image
img <-  cv2$imread(normalizePath(img_for_box_extraction_path), 0L)

# Thresholding the image
c(thresh, img_bin) `%<-%` cv2$threshold(img, 128, 255, cv2$THRESH_BINARY)

|     cv2$THRESH_OTSU
# Invert the image
img_bin = 255-img_bin
cv2.imwrite("Image_bin.jpg",img_bin)
