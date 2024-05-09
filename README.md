# Cell-Morphome

Dependencies
•	numpy 1.21.6
•	tensorflow-gpu 2.5.0
•	cudnn 8.2.1
•	pillow 9.2.0
•	python 3.7.12
•	cudatoolkit 11.3.1
•	keras-preprocessing  1.1.2
•	matplotlib 3.2.2
•	opencv-python  4.1.2.30
•	scikit-image 0.16.2              
•	scikit-learn  1.0.2                   
•	scipy  1.7.3

Preprocessing
Our trained data are cropped into 256 x 256 for training using crop_code_whole.R.  And the dataset can be converted into 16-bit, 8-bit or binary form for ablation study through binarize_image.R.

Train a Pix2pix model
The general training setup is layed out in GAN-pix2pix.py. The trained models in the form of 16-bit are listed below:
C2C12_d_model_000960_cyto.h5
C2C12_d_model_000960_nucl.h5
C2C12_d_model_000960_grad.h5
C2C12_g_model_000960_cyto.h5
C2C12_g_model_000960_nucl.h5
C2C12_g_model_000960_grad.h5

Perform Watershed and Voronoi
After saving the predicted images from pix2pix model, they can be input to WatershedVoronoiPropagatePix2PixData.R to perform instance segmentation.

Metrics Calculation
With saved instance segmentation and cooperated with ground truth, Precisions, Recall, F1 Score ad mAP can be calculated under different IOU thresholds through cal6-IOU, mAP, AP, Recall calculation.py.

Mask RCNN
Downloading and running in the MaskRCNN folder, train.py can be used to trained model in tensorflow 2.5.0 in GPU.

UNet
Converting the data into binary, Unet-single output.py can used to trained UNet model for both benchmarking as well as ablation study.

Transfer Learning
Using the pre-trained model of large dataset in ./models, small amounts of dataset can be imported into transfer_learning_code.py for further training to fit the new types of feature.



