# Final-Project-Group8
### Aurthors: Rayhaan Rasheed, William Noone, Samuel Aboagye
#### The George Washinton University

#### Abstract
Malaria is a mosquito-borne disease caused by a parasite. People with malaria often experience fever, chills, and flu-like illness. Left untreated, they may develop severe complications and die. In 2016 an estimated 216 million cases of malaria occurred worldwide and 445,000 people died, mostly children in the African Region. Malaria can only be diagnosed by examining a stained blood film of the patient with a microscope. Three different Convolutional Neural Networks(CNN)  were trained in classifying cells that are parasitic and non parasitic. The models used various configurations of Convolution, Pooling, and ReLU layers along with two fully connected layers and a SoftMax at the end. Based on the average test accuracy across all three models, unprocessed images performed best with 91.7% with training loss decay oscillating the tighter bounds. Further analysis allowed the average test accuracy to eventually become 93.7%.

#### Data
The images used in this study were pulled from the U.S. National LIbrary of Medicine’s Communications Engineering Branch. There are a total of 27,558 images of single red blood cells from Giemsa stained thin blood slides. These samples were taken from patients at the Chittagong Medical College hospital in Bangladesh where patients were either normal or infected with the P. falciparum parasite. Each image is roughly the same size and is given one of two classes: “Uninfected” or “Parasitized”.

#### Code
Look at code repository README. Follow the direction to process the image data, upload into LMDB, and run on our various CNN configurations. 
