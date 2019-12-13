import cv2
import numpy as np
from ipywidgets import interact, interactive, fixed, interact_manual
import ipywidgets as widgets

#document is result of the vision api, feature is one of the options from FeatureType
def crop_out_boxes(img_file, hmax):
    # Read the image
    img = cv2.imread(img_file, 0)
    # Thresholding the image
    (thresh, img_bin) = cv2.threshold(img, 128, 255,cv2.THRESH_BINARY|     cv2.THRESH_OTSU)
    # Invert the image
    img_bin = 255-img_bin
    #cv2.imwrite("Image_bin.jpg",img_bin)
    #plt.axis('off')
    #plt.imshow(img_bin);
    # Defining a kernel length
    kernel_length = np.array(img).shape[1]//100
    # A verticle kernel of (1 X kernel_length), which will detect all the verticle lines from the image.
    verticle_kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (1, kernel_length))
    # A horizontal kernel of (kernel_length X 1), which will help to detect all the horizontal line from the image.
    hori_kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (kernel_length, 1))
    # A kernel of (3 X 3) ones.
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    # Morphological operation to detect vertical lines from an image
    img_temp1 = cv2.erode(img_bin, verticle_kernel, iterations=3)
    verticle_lines_img = cv2.dilate(img_temp1, verticle_kernel, iterations=3)
    #cv2.imwrite("verticle_lines.jpg",verticle_lines_img)
    # Morphological operation to detect horizontal lines from an image
    img_temp2 = cv2.erode(img_bin, hori_kernel, iterations=3)
    horizontal_lines_img = cv2.dilate(img_temp2, hori_kernel, iterations=3)
    #cv2.imwrite("horizontal_lines.jpg",horizontal_lines_img)
    # Weighting parameters, this will decide the quantity of an image to be added to make a new image.
    alpha = 0.5
    beta = 1.0 - alpha
    # This function helps to add two image with specific weight parameter to get a third image as summation of two image.
    img_final_bin = cv2.addWeighted(verticle_lines_img, alpha, horizontal_lines_img, beta, 0.0)
    img_final_bin = cv2.erode(~img_final_bin, kernel, iterations=2)
    (thresh, img_final_bin) = cv2.threshold(img_final_bin, 128,255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)
    #cv2.imwrite("img_final_bin.jpg",img_final_bin)
    # Find contours for image, which will detect all the boxes
    contours, hierarchy = cv2.findContours(img_final_bin, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    contours, boundingBoxes = sort_contours(contours, "bottom-to-top")
    bounds = get_crop_bounds(contours, hmax)
    return (img, img_bin, img_final_bin, contours, bounds, hierarchy)

def sort_contours(cnts, method="left-to-right"):
    # initialize the reverse flag and sort index
    reverse = False
    i = 0
    # handle if we need to sort in reverse
    if method == "right-to-left" or method == "bottom-to-top":
        reverse = True
    # handle if we are sorting against the y-coordinate rather than
    # the x-coordinate of the bounding box
    if method == "top-to-bottom" or method == "bottom-to-top":
        i = 1
    # construct the list of bounding boxes and sort them from top to
    # bottom
    boundingBoxes = [cv2.boundingRect(c) for c in cnts]
    (cnts, boundingBoxes) = zip(*sorted(zip(cnts, boundingBoxes),
                                        key=lambda b: b[1][i], reverse=reverse))
    # return the list of sorted contours and bounding boxes
    return (cnts, boundingBoxes)

def get_crop_bounds(cnts, hmax = 300):
    X = []
    Y = []
    W = []
    H = []
    for c in cnts:
        # Returns the location and width,height for every contour
        x, y, w, h = cv2.boundingRect(c)
        # If the box height is greater then 20, widht is >80, then only save it as a box in "cropped/" folder.
        #if (w > 80 and h > 20) and w > 3*h:
        if (h < hmax):
            X.append(x)
            Y.append(y)
            W.append(w)
            H.append(h)
    return (X, Y, W, H)

def output_cropped_img(cropped_dir_path, img, idx, x, y, w, h):
    new_img = img[y:y+h, x:x+w]
    cv2.imwrite(cropped_dir_path+str(idx) + '.png', new_img)
    return cropped_dir_path+str(idx) + '.png'
