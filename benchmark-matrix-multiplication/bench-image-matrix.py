import cv2
import math
import time

img = cv2.imread('tiger_low_constrast.jpg', cv2.IMREAD_GRAYSCALE)
contrast = 2

start_time = time.time()
img = img * contrast
img[img > 255] = 255
elapsed_time = time.time() - start_time

print(elapsed_time)
