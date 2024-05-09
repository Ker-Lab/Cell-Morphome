#################################Phrase Contrast#####################################
#### crop and rotate heavy phase contrast


setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Phase.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}


#### crop and rotate light phase contrast

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Phase.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+300+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#################################Cytoplasm###################################



#### crop and rotate heavy cytoplasm

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Calcein.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#### crop and rotate light cytoplasm
setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Calcein.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+300+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}







#################################Nuclei#########################################


#### crop and rotate heavy nuclei

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Nuclei.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#### crop and rotate light nuclei
setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/Radiance_Map_Nuclei.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+300+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}



#################################Gradient#########################################


#### crop and rotate heavy gradient

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/gradient.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#### crop and rotate light gradient
setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/gradient.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+300+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}



#################################Instance#########################################


#### crop and rotate heavy instance

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/segmentedcells.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#### crop and rotate light instance
setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/segmentedcells.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 512
  y_increment = 512
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(6*(t-1)+300+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}


#################################Instance nuclei#########################################


#### crop and rotate heavy instance

setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Heavy_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/watershed.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 256
  y_increment = 256
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(24*(t-1)+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}




#### crop and rotate light instance
setwd("Z:/01_Project01_AI/Annotation/Whole_Dataset/Finalize/Light_Finalize")
library(EBImage)
library(tiff)

folders=list.dirs()
folders[[1]] <- NA
folders <- na.omit(folders)

for (t in 1:length(folders)){
  img = channel(readImage(paste0(folders[[t]], "/watershed.tif")), "gray")
  x_dim = dim(img)[1]
  y_dim = dim(img)[2]
  x_increment = 256
  y_increment = 256
  x_start = seq(1, x_dim, x_increment)
  x_end = seq(x_increment, x_dim, x_increment)
  if (length(x_start) > length(x_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    x_start[length(x_start)] = x_dim - x_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    x_end[(length(x_end)+1)] = x_dim #add the 'edge' of the image as the 
    #last element 
  }
  y_start = seq(1, y_dim, y_increment)
  y_end = seq(y_increment, y_dim, y_increment)
  if (length(y_start) > length(y_end)) { #this means that the image dimension 
    #is not divisible by the increment 
    y_start[length(y_start)] = y_dim - y_increment + 1 #replace the last 
    #element with a crop 
    #so that it terminates
    #at the 'edge' of the 
    #image
    y_end[(length(y_end)+1)] = y_dim #add the 'edge' of the image as the last 
    #element 
  }
  img_list = vector("list", length = length(x_start) * length(y_start))
  index = 1
  for (i in 1:length(x_start)) {
    for (j in 1:length(y_start)) {
      img_list[[index]] = img[x_start[i]:x_end[i], y_start[j]:y_end[j]]
      index = index + 1
    }
  }
  print(paste0("Now saving folder:", t))
  for (i in 1:length(img_list)) {
    writeTIFF(EBImage::transpose(img_list[[i]]),
              paste0(24*(t-1)+1200+i, ".tif"),
              bits.per.sample = 16, compression = "none", 
              reduce = TRUE)
  }
}