from numpy import zeros, ones,  asarray, load
from numpy.random import randint
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.initializers import RandomNormal
from tensorflow.keras import Input, Model
from tensorflow.keras.layers import Activation, BatchNormalization, concatenate, Conv2D, Conv2DTranspose, Dropout, MaxPool2D
from tensorflow.keras.layers import LeakyReLU
from matplotlib import pyplot as plt
from tensorflow.keras.utils import plot_model
from tensorflow.keras.models import load_model
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras import losses
from tensorflow.keras import regularizers

import matplotlib.pyplot as plt
import numpy as np
import os
import tensorflow as tf
import cv2


from tensorflow.keras.preprocessing.image import img_to_array
from tensorflow.keras.preprocessing.image import load_img
import os


#set path for training and validation generators
PATH = r'E:\Zhang Xu\Data\02 Computer vision\Transfer learning\hMSC_Whole data set_2' #need to change for other computer
trainx_dir = os.path.join(PATH, 'training_x')
trainy_dir = os.path.join(PATH, 'training_y2')
validationx_dir = os.path.join(PATH, 'validation_x')
validationy_dir = os.path.join(PATH, 'validation_y2')
testx_dir = os.path.join(PATH, 'test_x')
testy_dir = os.path.join(PATH, 'test_y2')


#set up parameters
batch_size = 216
epochs = 30
#epochs = 1
img_height = 256
img_width = 256


trainx_dir_num = len(os.listdir(os.path.join(trainx_dir, 'train')))
trainy_dir_num = len(os.listdir(os.path.join(trainy_dir, 'train')))
validatex_dir_num = len(os.listdir(os.path.join(validationx_dir, 'validation')))
validatey_dir_num = len(os.listdir(os.path.join(validationy_dir, 'validation')))
testx_dir_num = len(os.listdir(os.path.join(testx_dir, 'test')))
testy_dir_num = len(os.listdir(os.path.join(testy_dir, 'test')))

total_train = trainx_dir_num
total_validate = validatex_dir_num
total_test = testx_dir_num
total_train // batch_size
total_validate // batch_size
total_test // batch_size

#use keras api data generators
#we create two instances with the same arguments
train_data_gen_args = dict(rescale=1, vertical_flip=True,
                     horizontal_flip=True)
validate_data_gen_args = dict(rescale=1)
test_data_gen_args = dict(rescale=1)
train_image_datagen = ImageDataGenerator(**train_data_gen_args)
train_mask_datagen = ImageDataGenerator(**train_data_gen_args)
validate_image_datagen = ImageDataGenerator(**validate_data_gen_args)
validate_mask_datagen = ImageDataGenerator(**validate_data_gen_args)
test_image_datagen = ImageDataGenerator(**test_data_gen_args)
test_mask_datagen = ImageDataGenerator(**test_data_gen_args)


#Provide the same seed and keyword arguments to the fit and flow methods
seed = 1
train_image_generator = train_image_datagen.flow_from_directory(trainx_dir,
                        target_size=(img_height, img_width), color_mode = 
                        "grayscale", class_mode=None, batch_size=batch_size, 
                        seed=seed, shuffle=False)
train_mask_generator = train_mask_datagen.flow_from_directory(trainy_dir,
                       target_size=(img_height, img_width), color_mode = 
                       "grayscale", class_mode=None, batch_size=batch_size,
                       seed=seed, shuffle=False)
validation_image_generator = validate_image_datagen.flow_from_directory(
                             validationx_dir, target_size=(img_height, 
                             img_width), color_mode = "grayscale", 
                             batch_size=batch_size, class_mode=None, 
                             seed=seed, shuffle=False)
validation_mask_generator = validate_mask_datagen.flow_from_directory(
                            validationy_dir, target_size=(img_height, 
                            img_width), color_mode = "grayscale", 
                            class_mode=None, batch_size=batch_size, 
                            seed=seed, shuffle=False)
test_image_generator = test_image_datagen.flow_from_directory(
                             testx_dir, target_size=(img_height, 
                             img_width), color_mode = "grayscale", 
                             batch_size=batch_size, class_mode=None, 
                             seed=seed, shuffle=False)
test_mask_generator = test_mask_datagen.flow_from_directory(
                            testy_dir, target_size=(img_height, 
                            img_width), color_mode = "grayscale", 
                            class_mode=None, batch_size=batch_size, 
                            seed=seed, shuffle=False)



#combine generators into one which yields image and masks
train_generator = zip(train_image_generator, train_mask_generator)
validate_generator = zip(validation_image_generator, 
                         validation_mask_generator)
test_generator = zip(test_image_generator, test_mask_generator)

def define_discriminator(image_shape):    
    init = RandomNormal(stddev=0.02) #As described in the original paper  
    in_src_image = Input(shape=image_shape)  #Image we want to convert to another image
    in_target_image = Input(shape=image_shape)  #Image we want to generate after training. 
    merged = concatenate([in_src_image, in_target_image])
    d = Conv2D(64, (4,4), strides=(2,2), padding='same', kernel_initializer=init)(merged)
    d = LeakyReLU(alpha=0.2)(d)
    d = Conv2D(128, (4,4), strides=(2,2), padding='same', kernel_initializer=init)(d)
    d = BatchNormalization()(d)
    d = LeakyReLU(alpha=0.2)(d)
    d = Conv2D(256, (4,4), strides=(2,2), padding='same', kernel_initializer=init)(d)
    d = BatchNormalization()(d)
    d = LeakyReLU(alpha=0.2)(d)
    d = Conv2D(512, (4,4), strides=(2,2), padding='same', kernel_initializer=init)(d)
    d = BatchNormalization()(d)
    d = LeakyReLU(alpha=0.2)(d)
    d = Conv2D(512, (4,4), padding='same', kernel_initializer=init)(d)
    d = BatchNormalization()(d)
    d = LeakyReLU(alpha=0.2)(d)
    d = Conv2D(1, (4,4), padding='same', kernel_initializer=init)(d)
    patch_out = Activation('sigmoid')(d)
    model = Model([in_src_image, in_target_image], patch_out)
    lr_schedule = tf.keras.optimizers.schedules.ExponentialDecay(
        initial_learning_rate=0.0002,
        decay_steps=40,
        decay_rate=0.7)
    opt = tf.keras.optimizers.Adam(learning_rate=lr_schedule, beta_1=0.5)
    model.compile(loss='binary_crossentropy', optimizer=opt, metrics=['accuracy'], loss_weights=[0.5])
    return model

# disc_model = define_discriminator((256,256,1))
# plot_model(disc_model, to_file='disc_model.png', show_shapes=True)

#############################
#kernel_regularizer=tf.keras.regularizers.l1_l2(l1=0.01, l2=0.01)
#kernel_regularizer=tf.keras.regularizers.l2(0.01)
#Now define the generator - in our case we will define a U-net
# define an encoder block to be used in generator
def define_encoder_block(layer_in, n_filters, batchnorm=True):
    init = RandomNormal(stddev=0.02)
    g = Conv2D(n_filters, (4,4), strides=(2,2), padding='same',
               kernel_initializer=init)(layer_in)
    if batchnorm:
            g = BatchNormalization()(g, training=True)
    g = LeakyReLU(alpha=0.2)(g)
    return g

# define a decoder block to be used in generator
def decoder_block(layer_in, skip_in, n_filters, dropout=0.5):
    init = RandomNormal(stddev=0.02)
    g = Conv2DTranspose(n_filters, (4,4), strides=(2,2), padding='same',
                        kernel_initializer=init)(layer_in)
    g = BatchNormalization()(g, training=True)
    if dropout:
            g = Dropout(dropout)(g, training=True)
    g = concatenate([g, skip_in])
    g = Activation('relu')(g)
    return g

# define the standalone generator model - U-net
def define_generator(image_shape=(256,256,1)):
    init = RandomNormal(stddev=0.02)
    in_image = Input(shape=image_shape)
    e1 = define_encoder_block(in_image, 64, batchnorm=False)
    e2 = define_encoder_block(e1, 128)
    e3 = define_encoder_block(e2, 256)
    e4 = define_encoder_block(e3, 512)
    e5 = define_encoder_block(e4, 512)
    e6 = define_encoder_block(e5, 512)
    e7 = define_encoder_block(e6, 512)
    b = Conv2D(512, (4,4), strides=(2,2), padding='same',
               kernel_initializer=init)(e7)
    b = Activation('relu')(b)
    d1 = decoder_block(b, e7, 512, dropout=0.8)
    d2 = decoder_block(d1, e6, 512, dropout=0.8)
    d3 = decoder_block(d2, e5, 512, dropout=0.9)
    d4 = decoder_block(d3, e4, 512, dropout=False)
    d5 = decoder_block(d4, e3, 256, dropout=False)
    d6 = decoder_block(d5, e2, 128, dropout=False)
    d7 = decoder_block(d6, e1, 64, dropout=False)
    g = Conv2DTranspose(image_shape[2], (4,4), strides=(2,2), padding='same',
                        kernel_initializer=init)(d7) #Modified
    out_image = Activation('tanh')(g)  #Generates images in the range -1 to 1. So change inputs also to -1 to 1
    model = Model(in_image, out_image)
    return model

# gen_model = define_generator((256,256,1))
# plot_model(gen_model, to_file='gen_model.png', show_shapes=True)

# Loss function
def ssim_loss(y_true, y_pred):
    return 1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, 1.0))

def smooth_L1_loss(y_true, y_pred):
    smooth = tf.keras.losses.Huber()
    return smooth(y_true, y_pred)

def ssim_L2_loss(y_true, y_pred):
    ssim_loss=1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, 1.0))
    L2_loss=tf.keras.losses.MeanSquaredError()
    return ssim_loss*0.84 + (1-0.84)*L2_loss(y_true, y_pred)

def ssim_L1_loss(y_true, y_pred):
    ssim_loss=1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, 1.0))
    L1_loss=tf.keras.losses.MeanAbsoluteError()
    return ssim_loss*0.84 + (1-0.84)*L1_loss(y_true, y_pred)

def ssim_L1_L2_loss(y_true, y_pred):
    ssim_loss=1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, 1.0))
    L1_loss=tf.keras.losses.MeanAbsoluteError()
    L2_loss=tf.keras.losses.MeanSquaredError()
    return ssim_loss*0.84 + 1/3*(1-0.84)*L1_loss(y_true, y_pred) + 2/3*(1-0.84)*L2_loss(y_true, y_pred)

# define the combined generator and discriminator model, for updating the generator
def define_gan(g_model, d_model, image_shape):
    for layer in d_model.layers:
            if not isinstance(layer, BatchNormalization):
                    layer.trainable = False
                    in_src = Input(shape=image_shape)
                    gen_out = g_model(in_src)
                    dis_out = d_model([in_src, gen_out])
                    model = Model(in_src, [dis_out, gen_out])
                    lr_schedule = tf.keras.optimizers.schedules.ExponentialDecay(
                        initial_learning_rate=0.002,
                        decay_steps=40,
                        decay_rate=0.7)
                    opt = tf.keras.optimizers.Adam(learning_rate=lr_schedule, beta_1=0.5)
                    model.compile(loss=['binary_crossentropy', ssim_L1_loss],
                                  optimizer=opt, loss_weights=[1,100])
    return model

# select a batch of random samples, returns images and target
def generate_real_samples(dataset, n_samples, patch_shape):
    trainA, trainB = dataset
    ix = randint(0, trainA.shape[0], n_samples)
    X1, X2 = trainA[ix], trainB[ix]
    y = ones((n_samples, patch_shape, patch_shape, 1))
    return [X1, X2], y


# generate a batch of images, returns images and targets
def generate_fake_samples(g_model, samples, patch_shape):
    X = g_model.predict(samples)
    y = zeros((len(X), patch_shape, patch_shape, 1))
    return X, y

# generate samples and save as a plot and save the model
#GAN models do not converge, we just want to find a good balance between
#the generator and the discriminator. Therefore, it makes sense to periodically
#save the generator model and check how good the generated image looks. 
def summarize_performance(step, g_model, dataset, n_samples=3):
    [X_realA, X_realB], _ = generate_real_samples(dataset, n_samples, 1)
    X_fakeB, _ = generate_fake_samples(g_model, X_realA, 1)
    X_realA = (X_realA + 1) / 2.0
    X_realB = (X_realB + 1) / 2.0
    X_fakeB = (X_fakeB + 1) / 2.0
    for i in range(n_samples):
            plt.subplot(3, n_samples, 1 + i)
            plt.axis('off')
            plt.imshow(X_realA[i])
    for i in range(n_samples):
            plt.subplot(3, n_samples, 1 + n_samples + i)
            plt.axis('off')
            plt.imshow(X_fakeB[i])
    for i in range(n_samples):
            plt.subplot(3, n_samples, 1 + n_samples*2 + i)
            plt.axis('off')
            plt.imshow(X_realB[i])
    path = r'E:\Zhang Xu\Data\02 Computer vision\pix2pixsummary\hMSC_transfer learning'
    filename1 = 'plot_%06d.png' % (step+1)
    plt.savefig(os.path.join(path, filename1))
    plt.close()
    filename2 = 'model_%06d.h5' % (step+1)
    g_model.save(filename2)
    print('>Saved: %s and %s' % (filename1, filename2))


# create a line plot of loss for the gan and save to file
def plot_history(d1_hist, d2_hist, g_hist, a1_hist, a2_hist):
    plt.subplot(2, 1, 1)
    plt.plot(d1_hist, label='d-real')
    plt.plot(d2_hist, label='d-fake')
    plt.plot(g_hist, label='gen')
    plt.legend()
    plt.subplot(2, 1, 2)
    plt.plot(a1_hist, label='acc-real')
    plt.plot(a2_hist, label='acc-fake')
    plt.legend()
    plt.show()


# train pix2pix models
def train(d_model, g_model, gan_model, dataset, n_epochs=100, n_batch=1):
    n_patch = d_model.output_shape[1]
    trainA, trainB = dataset
    bat_per_epo = int(len(trainA) / n_batch)
    n_steps = bat_per_epo * n_epochs
    d1_hist, d2_hist, g_hist, a1_hist, a2_hist = [], [], [], [], []
    for i in range(n_steps):
            [X_realA, X_realB], y_real = generate_real_samples(dataset, n_batch, n_patch)
            X_fakeB, y_fake = generate_fake_samples(g_model, X_realA, n_patch)
            d_loss1, d_acc1 = d_model.train_on_batch([X_realA, X_realB], y_real)
            d_loss2, d_acc2 = d_model.train_on_batch([X_realA, X_fakeB], y_fake)
            g_loss, _, _ = gan_model.train_on_batch(X_realA, [y_real, X_realB])
            print('>%d, d1[%.3f] d2[%.3f] g[%.3f], a1[%d], a2[%d]' % (i+1, d_loss1, d_loss2, g_loss, int(100*d_acc1), int(100*d_acc2)))
            d1_hist.append(d_loss1)
            d2_hist.append(d_loss2)
            g_hist.append(g_loss)
            a1_hist.append(d_acc1)
            a2_hist.append(d_acc2)
            if (i+1) % (bat_per_epo * 10) == 0:
                    summarize_performance(i, g_model, dataset)
    plot_history(d1_hist, d2_hist, g_hist, a1_hist, a2_hist)

    
# load dataset
src_images,tar_images=next(train_generator)
print('Loaded: ', src_images.shape, tar_images.shape)


#####################################


# define input shape based on the loaded dataset
image_shape = src_images.shape[1:]
# define the models
#d_model = define_discriminator(image_shape)
new_discriminator = define_discriminator(image_shape) # make a new discriminator


# g_model = define_generator(image_shape) #disabled by elmer as we will re-use old generator
trained_generator = load_model(r'E:\Zhang Xu\Data\02 Computer vision\Transfer learning\240104_Transfer_Learning\g_model_000960_nucl.h5') #load trained generator 
# define the composite model
#gan_model = define_gan(g_model, d_model, image_shape)
transferred_gan_model = define_gan(trained_generator, new_discriminator, image_shape)

#Define data
# load and prepare training images
data = [src_images, tar_images]

def preprocess_data(data):
    X1, X2 = data[0], data[1]
    X1 = (X1 - 32767.5) / 32767.5
    X2 = (X2 - 32767.5) / 32767.5
    return [X1, X2]

dataset = preprocess_data(data)
src_images, tar_images = dataset
src_images = (src_images + 1) / 2
tar_images = (tar_images + 1) / 2



import time
start_time=time.time()


#train_data = train(d_model, g_model, gan_model, dataset, n_epochs=30, n_batch=4)
train_data = train(new_discriminator, trained_generator, transferred_gan_model, dataset, n_epochs=30, n_batch=4)

end_time=time.time()
print(f'Time elapsed is {(end_time - start_time):g} seconds.')

model = load_model('model_001440.h5')#change the model name as system show
model.save(r'E:\Zhang Xu\Data\02 Computer vision\pix2pixsummary\hMSC_transfer learning\model.h5')

#training set prediction
src_images, tar_images = dataset
# generate image from source
gen_images = model.predict(src_images)
# plot all three images
src_images = (src_images + 1) / 2
#gen_image #tar_image
gen_images = (gen_images + 1) / 2
tar_images = (tar_images + 1) / 2

n_samples=4
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + i)
    plt.axis('off')
    plt.imshow(src_images[i],cmap='gray')

    
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples + i)
    plt.axis('off')
    plt.imshow(gen_images[i],cmap='gray')

    
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples*2 + i)
    plt.axis('off')
    plt.imshow(tar_images[i],cmap='gray')

plt.show()

import numpy as np
import matplotlib.pyplot as plt
from numpy import mean
#conda install scikit-image
from skimage import data, img_as_float
from skimage.metrics import structural_similarity as ssim
from skimage.metrics import mean_squared_error

mse_tar_list=list()
ssim_tar_ist=list()
mse_gen_list=list()
ssim_gen_ist=list()
for i in range(4):
    rows, cols = tar_images[i,:,:,0].shape
    fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(10, 4),
                         sharex=True, sharey=True)
    ax = axes.ravel()
    mse_tar = mean_squared_error(tar_images[i,:,:,0], tar_images[i,:,:,0])
    ssim_tar = ssim(tar_images[i,:,:,0], tar_images[i,:,:,0], data_range=tar_images[i,:,:,0].max() - tar_images[i,:,:,0].min())
    mse_tar_list.append(mse_tar)
    ssim_tar_ist.append(ssim_tar)
    mse_gen = mean_squared_error(tar_images[i,:,:,0], gen_images[i,:,:,0])
    ssim_gen = ssim(tar_images[i,:,:,0], gen_images[i,:,:,0],
                  data_range=gen_images[i,:,:,0].max() - gen_images[i,:,:,0].min())
    mse_gen_list.append(mse_gen)
    ssim_gen_ist.append(ssim_gen)
    ax[0].imshow(tar_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[0].set_xlabel(f'MSE: {mse_tar:.2f}, SSIM: {ssim_tar:.2f}')
    ax[0].set_title('Original image')
    ax[1].imshow(gen_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[1].set_xlabel(f'MSE: {mse_gen:.2f}, SSIM: {ssim_gen:.2f}')
    ax[1].set_title('Generated Image')
    plt.tight_layout()
    plt.show()

mse_gen_avg=mean(mse_gen_list)
ssim_gen_avg=mean(ssim_gen_ist)
mse_tar_avg=mean(mse_tar_list)
ssim_tar_avg=mean(ssim_tar_ist)

print('generator_average_ssim:', ssim_gen_avg)


for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\nucl\\' 
    filename='nucl_train_'+str(i+10)+'.tif'
    cv2.imwrite(path + filename, gen_images[i], ((int(cv2.IMWRITE_TIFF_RESUNIT), 2,
                                                                  int(cv2.IMWRITE_TIFF_COMPRESSION), 1,
                                                                  int(cv2.IMWRITE_TIFF_XDPI), 100,
                                                                  int(cv2.IMWRITE_TIFF_YDPI), 100)))

    
for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\src\\'
    filename='phase_train_'+str(i+10)+'.tif'
    cv2.imwrite(path + filename, src_images[i])


#validation set prediction
src_images,tar_images= next(validate_generator)
print('Loaded: ', src_images.shape, tar_images.shape)


data = [src_images, tar_images]
dataset = preprocess_data(data)
src_images, tar_images = dataset
gen_images = model.predict(src_images)
# plot all three images
src_images = (src_images + 1) / 2
#gen_image #tar_image
gen_images = (gen_images + 1) / 2
tar_images = (tar_images + 1) / 2


n_samples=4
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + i)
    plt.axis('off')
    plt.imshow(src_images[i],cmap='gray')

    
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples + i)
    plt.axis('off')
    plt.imshow(gen_images[i],cmap='gray')

    
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples*2 + i)
    plt.axis('off')
    plt.imshow(tar_images[i],cmap='gray')

plt.show()
   
import numpy as np
import matplotlib.pyplot as plt
from numpy import mean
from skimage import data, img_as_float
from skimage.metrics import structural_similarity as ssim
from skimage.metrics import mean_squared_error

mse_tar_list=list()
ssim_tar_ist=list()
mse_gen_list=list()
ssim_gen_ist=list()
for i in range(src_images.shape[0]):
    rows, cols = tar_images[i,:,:,0].shape
    fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(10, 4),
                         sharex=True, sharey=True)
    ax = axes.ravel()
    mse_tar = mean_squared_error(tar_images[i,:,:,0], tar_images[i,:,:,0])
    ssim_tar = ssim(tar_images[i,:,:,0], tar_images[i,:,:,0], data_range=tar_images[i,:,:,0].max() - tar_images[i,:,:,0].min())
    mse_tar_list.append(mse_tar)
    ssim_tar_ist.append(ssim_tar)
    mse_gen = mean_squared_error(tar_images[i,:,:,0], gen_images[i,:,:,0])
    ssim_gen = ssim(tar_images[i,:,:,0], gen_images[i,:,:,0],
                  data_range=gen_images[i,:,:,0].max() - gen_images[i,:,:,0].min())
    mse_gen_list.append(mse_gen)
    ssim_gen_ist.append(ssim_gen)
    ax[0].imshow(tar_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[0].set_xlabel(f'MSE: {mse_tar:.2f}, SSIM: {ssim_tar:.2f}')
    ax[0].set_title('Original image')
    ax[1].imshow(gen_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[1].set_xlabel(f'MSE: {mse_gen:.2f}, SSIM: {ssim_gen:.2f}')
    ax[1].set_title('Generated Image')
    plt.tight_layout()
    plt.show()

mse_gen_avg=mean(mse_gen_list)
ssim_gen_avg=mean(ssim_gen_ist)
mse_tar_avg=mean(mse_tar_list)
ssim_tar_avg=mean(ssim_tar_ist)

print('generator_average_ssim:', ssim_gen_avg)

for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\nucl_validation\\' 
    filename='grad_val_'+str(i+1)+'.tif'
    cv2.imwrite(path + filename, gen_images[i], ((int(cv2.IMWRITE_TIFF_RESUNIT), 2,
                                                 int(cv2.IMWRITE_TIFF_COMPRESSION), 1,
                                                 int(cv2.IMWRITE_TIFF_XDPI), 100,
                                                 int(cv2.IMWRITE_TIFF_YDPI), 100)))

for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\src_validation\\' 
    filename='phase_val_'+str(i+1)+'.tif'
    cv2.imwrite(path + filename, src_images[i])

#test set prediction
src_images,tar_images= next(test_generator)
print('Loaded: ', src_images.shape, tar_images.shape)

data = [src_images, tar_images]
dataset = preprocess_data(data)
src_images, tar_images = dataset
gen_images = model.predict(src_images)

# plot all three images (src_image, gen_image, tar_image)
src_images = (src_images + 1) / 2
gen_images = (gen_images + 1) / 2
tar_images = (tar_images + 1) / 2

n_samples=4
for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + i)
    plt.axis('off')
    plt.imshow(src_images[i],cmap='gray')

for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples + i)
    plt.axis('off')
    plt.imshow(gen_images[i],cmap='gray')

for i in range(n_samples):
    plt.subplot(3, n_samples, 1 + n_samples*2 + i)
    plt.axis('off')
    plt.imshow(tar_images[i],cmap='gray')

plt.show()

import numpy as np
import matplotlib.pyplot as plt
from numpy import mean
#conda install scikit-image
from skimage import data, img_as_float
from skimage.metrics import structural_similarity as ssim
from skimage.metrics import mean_squared_error

mse_tar_list=list()
ssim_tar_ist=list()
mse_gen_list=list()
ssim_gen_ist=list()
for i in range(4):
    rows, cols = tar_images[i,:,:,0].shape
    fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(10, 4),
                         sharex=True, sharey=True)
    ax = axes.ravel()
    mse_tar = mean_squared_error(tar_images[i,:,:,0], tar_images[i,:,:,0])
    ssim_tar = ssim(tar_images[i,:,:,0], tar_images[i,:,:,0], data_range=tar_images[i,:,:,0].max() - tar_images[i,:,:,0].min())
    mse_tar_list.append(mse_tar)
    ssim_tar_ist.append(ssim_tar)
    mse_gen = mean_squared_error(tar_images[i,:,:,0], gen_images[i,:,:,0])
    ssim_gen = ssim(tar_images[i,:,:,0], gen_images[i,:,:,0],
                  data_range=gen_images[i,:,:,0].max() - gen_images[i,:,:,0].min())
    mse_gen_list.append(mse_gen)
    ssim_gen_ist.append(ssim_gen)
    ax[0].imshow(tar_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[0].set_xlabel(f'MSE: {mse_tar:.2f}, SSIM: {ssim_tar:.2f}')
    ax[0].set_title('Original image')
    ax[1].imshow(gen_images[i,:,:,0], cmap=plt.cm.gray, vmin=0, vmax=1)
    ax[1].set_xlabel(f'MSE: {mse_gen:.2f}, SSIM: {ssim_gen:.2f}')
    ax[1].set_title('Generated Image')
    plt.tight_layout()
    plt.show()

mse_gen_avg=mean(mse_gen_list)
ssim_gen_avg=mean(ssim_gen_ist)
mse_tar_avg=mean(mse_tar_list)
ssim_tar_avg=mean(ssim_tar_ist)

print('generator_average_ssim:', ssim_gen_avg)

for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\nucl_test\\' 
    filename='grad_test_'+str(i+1)+'.tif'
    cv2.imwrite(path + filename, gen_images[i], ((int(cv2.IMWRITE_TIFF_RESUNIT), 2,
                                                 int(cv2.IMWRITE_TIFF_COMPRESSION), 1,
                                                 int(cv2.IMWRITE_TIFF_XDPI), 100,
                                                 int(cv2.IMWRITE_TIFF_YDPI), 100)))

for i in range(len(gen_images)):
    path='E:\\Zhang Xu\\Data\\02 Computer vision\\pix2pix\\G3_TL\\240207\\src_test\\' 
    filename='phase_test_'+str(i+1)+'.tif'
    cv2.imwrite(path + filename, src_images[i])
