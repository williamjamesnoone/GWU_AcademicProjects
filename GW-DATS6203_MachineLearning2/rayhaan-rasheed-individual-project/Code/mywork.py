# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Importing and Processing for Malaria CNN
# Author: Rayhaan Rasheed
# Date Created: 4/11/2019
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import numpy as np
import scipy.ndimage as ndi
from skimage.color import rgb2gray
import matplotlib.pyplot as plt
from PIL import Image
import os

DIR1 = '/home/ubuntu/Deep-Learning/cell_images/Parasitized'
DIR2 = '/home/ubuntu/Deep-Learning/cell_images/Uninfected'

# ---------- Import the Ininfected Images -------------
main_address='cell_images/Parasitized'
images_address=os.listdir(main_address)
images_address.remove('Thumbs.db')
parasitized_rgb=np.zeros((len(images_address),100,100,3),dtype=np.uint8)

print("Importing Parasitized Data...")
for ind,img_address in enumerate(images_address):
    img=Image.open(main_address+'/'+img_address)
    img=img.resize((100,100),Image.ANTIALIAS)
    img=np.asarray(img)
    img=img.astype(np.uint8)
    parasitized_rgb[ind]=img
print("Done Importing Parasitized Data!")

parasitized_grey = rgb2gray(parasitized_rgb)

print(parasitized_rgb.shape)
print(parasitized_rgb[4].shape)
print(type(parasitized_rgb))
plt.imshow(parasitized_rgb[4])
plt.show()


print(parasitized_grey.shape)
print(parasitized_grey[4].shape)
print(type(parasitized_grey))
plt.imshow(parasitized_grey[4], cmap='gray')
plt.show()




# ---------- Import the Unininfected Images -------------
address='cell_images/Uninfected'
imaddress=os.listdir(address)
imaddress.remove('Thumbs.db')
uninfected_rgb=np.zeros((len(images_address),100,100,3),dtype=np.uint8)

print("Importing Uninfected Data...")
for ind,imadd in enumerate(imaddress):
    img=Image.open(address+'/'+imadd)
    img=img.resize((100,100),Image.ANTIALIAS)
    img=np.asarray(img)
    img=img.astype(np.uint8)
    uninfected_rgb[ind]=img
print("Done Importing Uninfected Data!")

uninfected_grey=rgb2gray(uninfected_rgb)

print(uninfected_rgb.shape)
print(uninfected_rgb[4].shape)
print(type(uninfected_rgb))
plt.imshow(uninfected_rgb[4])
plt.show()

print(uninfected_grey.shape)
print(uninfected_grey[4].shape)
print(type(uninfected_grey[4]))
plt.imshow(uninfected_grey[4], cmap='gray')
plt.show()



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                               Image Processing Step for Grey Scale Images
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for img in range(0, len(parasitized_grey)-1):
    # Apply Gaussian Smoothing filter with sigma = 1
    img_smooth = ndi.gaussian_filter(parasitized_grey[img], sigma=1)
    # Generate mask based on intensity
    mask = img_smooth <= .45
    # Apply mask to image
    parasitized_grey[img] = np.where(mask, img_smooth, 0)
    
plt.imshow(parasitized_grey[4], cmap='gray')
plt.show()

for img in range(0, len(uninfected_grey)-1):
    # Apply Gaussian Smoothing filter with sigma = 1
    img_smooth = ndi.gaussian_filter(uninfected_grey[img], sigma=1)
    # Generate mask based on intensity
    mask = img_smooth <= .45
    # Apply mask to image
    uninfected_grey[img] = np.where(mask, img_smooth, 0)

plt.imshow(uninfected_grey[4], cmap='gray')
plt.show()

