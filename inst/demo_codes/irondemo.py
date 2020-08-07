img_file = "inst/raw_data/ACE Contrractors Pollution_2.png"
img, img_bin, img_final_bin, contours, bounds, hierarchy = crop_out_boxes(img_file, hmax = 300)

cv2.imwrite('py_boxes.png', img_final_bin)
