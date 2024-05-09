## transfer nuclei to binary img
#read the file
setwd("C:/Data/Tingwei/ablation study for pix2pix/01dataset/008/hdr_output_new")
library(EBImage)
library(tiff)
img = channel(readImage(paste0(getwd(), "/", 
                               "Radiance_Map_Nuclei.tif")), "gray")
display(img)

# change it to binary
img = img > 0.128
display(img)
img[img == FALSE] = 0
img[img == TRUE] = 1

writeTIFF(EBImage::transpose(img),
          paste0(img, ".tif"),
          bits.per.sample = 16, compression = "none", 
          reduce = TRUE)



## transfer cytoplasm to binary img

#read the file
setwd("C:/Data/Tingwei/ablation study for pix2pix/01dataset/008/hdr_output_new")
library(EBImage)
library(tiff)
img = channel(readImage(paste0(getwd(), "/", 
                               "Radiance_Map_Calcein.tif")), "gray")
display(img)


# change it to binary
img = img > 0.52
display(img)
img[img == FALSE] = 0
img[img == TRUE] = 1

writeTIFF(EBImage::transpose(img),
          paste0(img, ".tif"),
          bits.per.sample = 16, compression = "none", 
          reduce = TRUE)



## transfer gradient to binary img
#read the file
setwd("C:/Data/Tingwei/ablation study for pix2pix/01dataset/008/hdr_output_new")
library(EBImage)
library(tiff)
img = channel(readImage(paste0(getwd(), "/", 
                               "gradient.tif")), "gray")
display(img)


# change it to binary
img = img > 0
display(img)
img[img == FALSE] = 0
img[img == TRUE] = 1

writeTIFF(EBImage::transpose(img),
          paste0(img, ".tif"),
          bits.per.sample = 16, compression = "none", 
          reduce = TRUE)




## crop and rotate

#setwd("D:/HE TINGWEI/GUHK-GBI/Elmer/UNet/001/Nuclei_binary")
setwd("Z:/01_Project01_AI/ablation study for pix2pix/01dataset/008/hdr_output_new")
library(EBImage)
library(tiff)
img = channel(readImage(paste0(getwd(), "/", 
                               "watershed.tif")), "gray")

x_dim = dim(img)[1]
y_dim = dim(img)[2]
x_increment = 256
y_increment = 256
x_start = seq(1, x_dim, x_increment)
x_end = seq(x_increment, x_dim, x_increment)
if (length(x_start) > length(x_end)) { 
  x_start[length(x_start)] = x_dim - x_increment + 1  

  x_end[(length(x_end)+1)] = x_dim 
}
y_start = seq(1, y_dim, y_increment)
y_end = seq(y_increment, y_dim, y_increment)
if (length(y_start) > length(y_end)) { 
  y_start[length(y_start)] = y_dim - y_increment + 1
  y_end[(length(y_end)+1)] = y_dim 
}
img_list = vector("list", length = length(x_start) * length(y_start))
index = 1
for (i in 1:length(x_start)) {
  for (j in 1:length(y_start)) {
    img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
    index = index + 1
  }
}
for (i in 1:length(img_list)) {
  writeTIFF(EBImage::transpose(img_list[[i]]),
            paste0(i+24, ".tif"),
            bits.per.sample = 16, compression = "none", 
            reduce = TRUE)
}