
# Data Importing and Processing for Malaria CNN
# Authors: Rayhaan Rasheed, William Noone, Samuel Abogaye
# Date Created: 4/11/2019

#%% Import Libraries

# file system libraries
import os
import zipfile
import pathlib
import random
import shutil
from os import path, listdir

# mathematics & data libraries
import numpy as np
import scipy.ndimage as ndi
import pandas as pd

# image processing libraries
from PIL import ImageFile
from PIL import Image
from skimage.color import rgb2gray
import imageio


#%%  File Directory Operations
"""
First, Download zip fle from NIH/Kaggle
This operation:
1.  Yields source folders containing imaages by class: Parasitized, Uninfected
2.  Unzips contents to working directory
3.  Creates data frame of master index, class label index, image files names, class labels
4.  Writes to .csv file
"""

# set root_path and data_path (source & destination for zip contents)
root_path = "D:\\GW Programs\\GW MS Data Science\\DATS 6203 - Machine Learning II\\Projects\\GCP_DeepLearning\\Final_Project"
data_path = "D:\\GW Programs\\GW MS Data Science\\DATS 6203 - Machine Learning II\\Projects\\GCP_DeepLearning\\Final_Project\\malaria_data"
os.chdir(root_path)
#os.mkdir(data_path)
#os.getcwd()

# unzip files from zip_source_path to data_path, allow 1-2 min for unzip
zip_source_path = "D:\\GW Programs\\GW MS Data Science\\DATS 6203 - Machine Learning II\\Final_Project\\malaria_data\\cell_images.zip"
zipfile.ZipFile(zip_source_path).extractall(data_path)

# create parent and child directory paths
parent_directory = data_path + "\\cell_images"
child_directory_1 = parent_directory + "\\" + os.listdir(parent_directory)[0]
child_directory_2 = parent_directory + "\\" + os.listdir(parent_directory)[1]
child_1_contents = os.listdir(child_directory_1)
child_2_contents = os.listdir(child_directory_2)

# create dataframe1 of indexed child_directory files w/class labels
child_1_df = pd.DataFrame(child_1_contents, columns = ["image"])
child_1_df["class_label"] = os.listdir(parent_directory)[0]
child_1_df.drop(child_1_df.tail(1).index, inplace=True)

# create dataframe2 of indexed child_directory files w/class labels
child_2_df = pd.DataFrame(child_2_contents, columns = ["image"])
child_2_df["class_label"] = os.listdir(parent_directory)[1]
child_2_df.drop(child_2_df.tail(1).index, inplace=True)

# combine into master dataframe
frames = [child_1_df, child_2_df]
master_image_targets = pd.concat(frames)
master_image_targets.reset_index(inplace = True)
master_image_targets.rename(columns={'index': 'class_index'}, inplace=True)
master_image_targets.index.names = ["image_index"]

# write to .csv
master_image_targets.to_csv(root_path + "\\master_image_targets.csv", header = True)


#%% Create Unprocessed Images - No Color Alteration

"""
This operation:
1.  Resizes raw images to 100 * 100
2.  Converts from .png to .jpg
3.  Saves to class label folders under parent directory 
"""

ImageFile.LOAD_TRUNCATED_IMAGES = True
# Parasitized images
print("Resizing Parasitized Images...")
parasitized_path = parent_directory + "\\Parasitized"
parasitized_destination = parent_directory + "\\Resized_Images\\Parasitized_Resized\\"
parasitized_images = os.listdir(parasitized_path)
parasitized_images.remove('Thumbs.db')
#parasitized_data = np.zeros((len(images_address), 100, 100, 3), dtype=np.uint8)
for i, image in enumerate(parasitized_images):
    img = Image.open(parasitized_path + '/' + image)
    img = img.resize((100, 100), Image.ANTIALIAS)
    image_jpg = image.replace(".png", ".jpg")
    img.save(parasitized_destination + image_jpg, 'JPEG')
print("Operation Complete")


ImageFile.LOAD_TRUNCATED_IMAGES = True
# Uninfected images
print("Resizing Uninfected Images...")
uninfected_path = parent_directory + "\\Uninfected"
uninfected_destination = parent_directory + "\\Resized_Images\\Uninfected_Resized\\"
uninfected_images = os.listdir(uninfected_path)
uninfected_images.remove('Thumbs.db')
#uninfected_data = np.zeros((len(images_address), 100, 100, 3), dtype=np.uint8)
for i, image in enumerate(uninfected_images):
    img = Image.open(uninfected_path + '/' + image)
    img = img.resize((100, 100), Image.ANTIALIAS)
    image_jpg = image.replace(".png", ".jpg")
    img.save(uninfected_destination + image_jpg, 'JPEG')
print("Operation Complete")



#%% Create Processed Images - Apply Greyscale, Filters, Mask

"""
This operation:
1.  Imports image and converts to np.ndarray 
2.  Converts to grey scale
3.  Converts back to .jpg
4.  Saves to class label folders under parent directory 
"""

# Parasitized images
print("Importing Parasitized Images...")
resized_parasitized_path = data_path + "\\cell_images_altered\\Resized_Images\\Parasitized_Resized"
parasitized_grey_destination = data_path + "\\cell_images_altered\\Greyscale_Images\\Parasitized_Greyscale\\"
resized_parasitized_images = os.listdir(resized_parasitized_path)
#parasitized_images.remove('Thumbs.db')

for ind, img_address in enumerate(resized_parasitized_images):
    img = imageio.imread(resized_parasitized_path + "\\" + img_address)
    img = np.asarray(img)
    img = img.astype(np.uint8)
    # Convert to greyscale
    img_g = rgb2gray(img)
    # Apply Gaussian Smoothing filter
    img_smooth = ndi.gaussian_filter(img_g, sigma=1)
    # Generate mask based on intensity
    mask = img_smooth <= .45
    # Apply mask to image
    img_g = np.where(mask, img_smooth, 0)
    imageio.imwrite(parasitized_grey_destination + img_address, img_g)
print("Operation Complete")

# Uninfected images
print("Importing Uninfected Images...")
resized_uninfected_path = data_path + "\\cell_images_altered\\Resized_Images\\Uninfected_Resized"
uninfected_grey_destination = data_path + "\\cell_images_altered\\Greyscale_Images\\Uninfected_Greyscale\\"
resized_uninfected_images = os.listdir(resized_uninfected_path)
#parasitized_images.remove('Thumbs.db')

for ind, img_address in enumerate(resized_uninfected_images):
    img = imageio.imread(resized_uninfected_path + "\\" + img_address)
    img = np.asarray(img)
    img = img.astype(np.uint8)
    # Convert to greyscale
    img_g = rgb2gray(img)
    # Apply Gaussian Smoothing filter
    img_smooth = ndi.gaussian_filter(img_g, sigma=1)
    # Generate mask based on intensity
    mask = img_smooth <= .45
    # Apply mask to image
    img_g = np.where(mask, img_smooth, 0)
    imageio.imwrite(uninfected_grey_destination + img_address, img_g)
print("Operation Complete")

#%%  Create Functions for Train & Validation Image Sets

"""
These functions:
1.  Imports images from a path & shuffles
2.  Splits images into Train and Validation sets
3.  Creates folders for Train and Validation sets by class 
4.  Saves images to new Train and Validation directories
"""
# parameters
seed = 123
ratio_split = (.7, .3)

# Function returns directories within path
# directory = input_test
# list_dirs(directory)
def list_dirs(directory):
    # Returns all directories in a given directory
    return [f for f in pathlib.Path(directory).iterdir() if f.is_dir()]


# Function returns files within path
# directory_files = input_test_files
# list_files(directory_files)
def list_files(directory):
    # Returns all files in a given directory
    return [f for f in pathlib.Path(directory).iterdir() if f.is_file() and not f.name.startswith('.')]

# Function shuffles and returns files within a path
# directory_files = input_test_files
# seed = random_seed
# setup_files(directory_files, seed)
def setup_files(directory, seed):
    # Returns shuffled files
    # make sure its reproducible
    random.seed(seed)
    files = list_files(directory)
    files.sort()
    random.shuffle(files)
    return files

# Function splits files along training index and returns train files and validation file lists
# files = setup_files(directory_files, seed)
# split_train = split_train
# split_val = split_val
# use_test = False
# split_files(files, split_train, split_val, use_test)
def split_files(files, split_train, split_val, use_test):
    # Splits the files along the provided indices
    files_train = files[:split_train]
    files_val = files[split_train:split_val] if use_test else files[split_train:]
    list = [(files_train, 'train'), (files_val, 'validation')]
    # optional test folder
    if use_test:
        files_test = files[split_val:]
        li.append((files_test, 'test'))
    return list

# Function creates train/validation folders, populates with train/validation files to the output folder
# files_type = split_files(files, split_train, split_val, use_test) # files by test vs validation type
# input_files = input_test_files
# output = output_test
# copy_files(files_type, input_files, output)
def copy_files(files_type, input, output):
    class_name = path.split(input)[1]   # get the last part within the file
    for (files, folder_type) in files_type:
        full_path = path.join(output, folder_type, class_name)
        pathlib.Path(full_path).mkdir(
            parents=True, exist_ok=True)
        for f in files:
            shutil.copy2(f, full_path)

# Function combines all functions to create train/validation images sets in folders
#directory_files = input_test_files
# output = output_test
#ratio_split = (0.7, 0.3)
# seed = random_seed
# split_class_dir_ratio(directory_files, output, ratio_split, seed)
def split_class_dir_ratio(directory, output, ratio_split, seed):
    # shuffles and returns files within a path
    files = setup_files(directory, seed)
    # initializes train index for split based upon ratio
    split_train = int(ratio_split[0] * len(files))
    # initializes validation index for split based upon ratio
    split_val = split_train + int(ratio_split[1] * len(files))
    # splits files along training index and returns train files and validation file lists
    list = split_files(files, split_train, split_val, len(ratio_split) == 3)
    # creates train/validation folders, populates with train/validation files to the output folder
    copy_files(list, class_dir, output)


#%% Initialize Training and Validation Directories

# Unprocessed Images - directories
input_unprocessed = data_path + "\\cell_images_altered\\Resized_Images"
output_unprocessed = data_path + "\\Unprocessed_Directory"
for class_dir in list_dirs(input_unprocessed):
    split_class_dir_ratio(class_dir, output_unprocessed, ratio_split, seed)

# Processed Images - directories
input_processed = data_path + "\\cell_images_altered\\Greyscale_Images"
output_processed = data_path + "\\Processed_Directory"
for class_dir in list_dirs(input_processed):
    split_class_dir_ratio(class_dir, output_processed, ratio_split, seed)




