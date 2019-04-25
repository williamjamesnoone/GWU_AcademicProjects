
README

In reference to the project:
Analysis of Image Processing Techniques in Malaria Cell Classification using Convolutional Neural Networks
authors:  Rayhaan Rasheed, William Noone, and Samuel Aboagye
The George Washington University
Department of Data Science, MS

Please save all files to a working directory
Run the .py files in following order.  
Brief descriptions of function included.

1.  data_preparation_malaria.py
		#%%  File Directory Operations
		"""
		First, Download zip fle from NIH/Kaggle
		This operation:
		1.  Yields source folders containing imaages by class: Parasitized, Uninfected
		2.  Unzips contents to working directory
		3.  Creates data frame of master index, class label index, image files names, class labels
		4.  Writes to .csv file
		"""
		#%% Create Unprocessed Images - No Color Alteration
		"""
		This operation:
		1.  Resizes raw images to 100 * 100
		2.  Converts from .png to .jpg
		3.  Saves to class label folders under parent directory 
		"""
		#%% Create Processed Images - Apply Greyscale, Filters, Mask
		"""
		This operation:
		1.  Imports image and converts to np.ndarray 
		2.  Converts to grey scale
		3.  Converts back to .jpg
		4.  Saves to class label folders under parent directory 
		"""
		#%%  Create Functions for Train & Validation Image Sets
		"""
		These functions:
		1.  Imports images from a path & shuffles
		2.  Splits images into Train and Validation sets
		3.  Creates folders for Train and Validation sets by class 
		4.  Saves images to new Train and Validation directories
		"""	
		#%% Initialize Training and Validation Directories
	
2.  create_lmdb_processed.py
		"""
		This operation:
		1.  Creates a .lmdb package for the processed images
		"""

3.  create_lmdb_unprocessed.py
		"""
		This operation:
		1.  Creates a .lmdb package for the unprocessed images
		"""
	
4.  train_unprocessed_model1.py
	Unprocessed_Model1_solver.prototxt
	Unprocessed_Model1_train_test.prototxt
		"""
		This operation:
		1.  Builds & trains the Caffe model 
		2.  Renders associated performance visualizations
		"""
5.  train_processed_model1.py
	Processed_Model1_solver.prototxt
	Processed_Model1_train_test.prototxt
		"""
		This operation:
		1.  Builds & trains the Caffe model 
		2.  Renders associated performance visualizations
		"""
6.  train_unprocessed_model2.py
	Unprocessed_Model2_solver.prototxt
	Unprocessed_Model2_train_test.prototxt
		"""
		This operation:
		1.  Builds & trains the Caffe model 
		2.  Renders associated performance visualizations 
		"""		
7.  train_processed_model2.py
	Processed_Model2_solver.prototxt
	Processed_Model2_train_test.prototxt
		"""
		This operation:
		1.  Builds & trains the Caffe model 
		2.  Renders associated performance visualizations
		"""
8.  train_unprocessed_model3.py
	Unprocessed_Model3_solver.prototxt
	Unprocessed_Model3_train_test.prototxt
		"""
		This operation:
		1.  Builds & trains the Caffe model 
		2.  Renders associated performance visualizations
		"""		
