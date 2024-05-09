from tensorflow.keras import Input, Model
from tensorflow.keras.backend import flatten, sum
from tensorflow.keras.layers import Activation, BatchNormalization, concatenate, Conv2D, Conv2DTranspose, Dropout, MaxPool2D
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score 

import matplotlib.pyplot as plt
import numpy as np
import os
import tensorflow as tf
import time


from numpy import zeros, ones,  asarray, load
from tensorflow.keras.preprocessing.image import img_to_array
from tensorflow.keras.preprocessing.image import load_img
import os
                    
# load all images in a directory into memory
def load_images(path, size=(256,256)):
    src_list = list()
    for filename in os.listdir(path):
        if filename.endswith(".tif"):
            file_path=path +'/' +filename
            pixels = load_img(file_path, target_size=size, color_mode = "grayscale")
            pixels = img_to_array(pixels)
            src_list.append(pixels)
    return asarray(src_list)



# dataset path
#path1 = 'D:/Data/Tingwei/001+008_Unet_16bit/training_x/train'
path1 = 'Z:/01_Project01_AI/ablation study for pix2pix/01dataset/Whole_Dataset_cropped_16bit/training_x/train'
train_image_generator = load_images(path1)
train_image_generator=train_image_generator/65535

path2 = r'Z:\01_Project01_AI\ablation study for pix2pix\01dataset\Whole_Dataset_cropped_16bit\training_z1\train'
train_mask_generator = load_images(path2)
train_mask_generator=train_mask_generator/65535

path3 = 'Z:/01_Project01_AI/ablation study for pix2pix/01dataset/Whole_Dataset_cropped_16bit/validation_x/val'
validation_image_generator = load_images(path3)
validation_image_generator=validation_image_generator/65535

path4 = 'Z:/01_Project01_AI/ablation study for pix2pix/01dataset/Whole_Dataset_cropped_16bit/validation_z1/val'
validation_mask_generator = load_images(path4)
validation_mask_generator=validation_mask_generator/65535




#unet model
def conv_block(tensor, nfilters, size=3, padding='same', 
               initializer="he_normal"):
    x = Conv2D(filters=nfilters, kernel_size=(size, size), padding=padding, 
               kernel_initializer=initializer)(tensor)
    x = BatchNormalization()(x)
    x = Activation("relu")(x)
    x = Conv2D(filters=nfilters, kernel_size=(size, size), padding=padding, 
               kernel_initializer=initializer)(x)
    x = BatchNormalization()(x)
    x = Activation("relu")(x)
    return x


def deconv_block(tensor, residual, nfilters, size=3, padding='same', 
                 strides=(2, 2)):
    y = Conv2DTranspose(nfilters, kernel_size=(size, size), strides=strides, 
        padding=padding)(tensor)
    y = concatenate([y, residual], axis=3)
    y = conv_block(y, nfilters)
    return y


def Unet(img_height, img_width, nclasses=1, filters=64):
#down
    input_layer = Input(shape=(img_height, img_width, 1), name='image_input')
    conv1 = conv_block(input_layer, nfilters=filters)
    conv1_out = MaxPool2D(pool_size=(2, 2))(conv1)
    conv2 = conv_block(conv1_out, nfilters=filters*2)
    conv2_out = MaxPool2D(pool_size=(2, 2))(conv2)
    conv3 = conv_block(conv2_out, nfilters=filters*4)
    conv3_out = MaxPool2D(pool_size=(2, 2))(conv3)
    conv4 = conv_block(conv3_out, nfilters=filters*8)
    conv4_out = MaxPool2D(pool_size=(2, 2))(conv4)
    conv5 = conv_block(conv4_out, nfilters=filters*16)
    conv5_out = MaxPool2D(pool_size=(2, 2))(conv5)
    conv5_out = Dropout(0.5)(conv5_out)
    conv6 = conv_block(conv5_out, nfilters=filters*32)
    conv6 = Dropout(0.5)(conv6)
#up
    deconv7 = deconv_block(conv6, residual=conv5, nfilters=filters*16)
    deconv7 = Dropout(0.5)(deconv7)
    deconv8 = deconv_block(deconv7, residual=conv4, nfilters=filters*8)
    deconv8 = Dropout(0.5)(deconv8)
    deconv9 = deconv_block(deconv8, residual=conv3, nfilters=filters*4)
    deconv9 = Dropout(0.5)(deconv9) 
    deconv10 = deconv_block(deconv9, residual=conv2, nfilters=filters*2)
    deconv11 = deconv_block(deconv10, residual=conv1, nfilters=filters)
#output
    output_layer = Conv2D(filters=nclasses, kernel_size=(1, 1))(deconv11)
    output_layer = BatchNormalization()(output_layer)
    output_layer = Activation('sigmoid')(output_layer)
    model = Model(inputs=input_layer, outputs=output_layer, name='Unet')
    return model



#loss functions
def dice_coeff(y_true, y_pred):
    smooth = 1
    y_true_f = flatten(y_true)
    y_pred_f = flatten (y_pred)
    intersection = sum(y_true_f * y_pred_f)
    score = (2. * intersection + smooth) / (sum(y_true_f) + sum(y_pred_f) + smooth)
    return score

def dice_loss(y_true, y_pred):
    loss = 1 - dice_coeff(y_true, y_pred)
    return loss


#unet model
unet = Unet(img_height = img_height, img_width = img_width, nclasses=1, filters=64)
unet.summary()
print(unet.output_shape)


#train model
opt = tf.keras.optimizers.Adam(learning_rate=0.005, epsilon = 0.1)
unet.compile(optimizer=opt, loss=dice_loss, metrics=[dice_coeff])


import time
start_time=time.time()

history = unet.fit(train_image_generator, train_mask_generator,
                   batch_size=48, 
                   epochs = epochs, validation_data = (validation_image_generator, validation_mask_generator))



end_time=time.time()
print(f'Time elapsed is {(end_time - start_time):g} seconds.')



#plot model history
dice_coeff = history.history['dice_coeff']
val_dice_coeff = history.history['val_dice_coeff']
loss=history.history['loss']
val_loss=history.history['val_loss']
epochs_range = range(epochs)
plt.figure(figsize=(8, 8))
acc_plot=plt.subplot(1, 2, 1)
acc_plot.set_ylim([0,1])
plt.plot(epochs_range, dice_coeff, label='Training Dice Coefficient')
plt.plot(epochs_range, val_dice_coeff, label='Validation Dice Coefficient')
plt.legend(loc='lower right')
plt.title('Training and Validation Dice Coefficient')
acc_plot2 = plt.subplot(1, 2, 2)
acc_plot2.set_ylim([0,1])
plt.plot(epochs_range, loss, label='Training Loss')
plt.plot(epochs_range, val_loss, label='Validation Loss')
plt.legend(loc='upper right')
plt.title('Training and Validation Loss')
plt.show()


#save model
unet.save(r'Z:\01_Project01_AI\DataAndAnalysis\ResearchRecord\2023.6.20\unet_bo\unet_200.h5')


# load model
from tensorflow.keras.models import load_model
model = load_model('Z:\\01_Project01_AI\\DataAndAnalysis\\ResearchRecord\\2023.5.19\\unet nuclei\\unet_nuclei_16bit.h5', custom_objects={'dice_loss': dice_loss, 'dice_coeff': dice_coeff})
