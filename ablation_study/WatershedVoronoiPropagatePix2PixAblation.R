###for pix2pix 16bit
install.packages ('gtools')
require ('gtools')
#set up libraries and working directory
library(EBImage)
library(tiff)


##load image
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023525/pix2pix cyto 4/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023525/pix2pix nucl 4/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023525/pix2pix grad 1/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

##load image 8 bit
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.6.20/ablation/8bit/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.6.20/ablation/8bit nucl/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.6.20/ablation/8bit grad/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

##load image 96 set
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.17 96-set/cyto-batch8/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.17 96-set/nucl/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.17 96-set/grad3/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

##load image 48 set
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.25 48-set/cyto batch4/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.25 48-set/nucl/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.25 48-set/grad/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

##load image heavy
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/heavy/pix2pix_heavy_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/heavy/pix2pix_heavy_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/heavy/pix2pix_heavy_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/heavy/pix2pix_heavy_phase"))

##load image light
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/light/pix2pix_light_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/light/pix2pix_light_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/light/pix2pix_light_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.26 heavy&light prediction/light/pix2pix_light_phase"))

##load image 600 set
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.27 600-set/cyto batch16/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.27 600-set/nucl/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.27 600-set/grad/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

##load image 1200 set
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.28 1200-set/cyto batch48/pix2pix_test_cyto"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.28 1200-set/nucl batch48/pix2pix_test_nucl"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.7.28 1200-set/grad batch48/pix2pix_test_grad"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/pix2pix batch60/pix2pix_test_phase"))

cyto_file_names <- mixedsort (list.files(cyto_folder))
nucl_file_names <- mixedsort(list.files(nucl_folder))
grad_file_names <- mixedsort(list.files(grad_folder))
phar_file_names <- mixedsort(list.files(phar_folder))

cyto_folder = file.path(cyto_folder, cyto_file_names)
nucl_folder = file.path (nucl_folder, nucl_file_names)
grad_folder = file.path (grad_folder, grad_file_names)
phar_folder = file.path (phar_folder, phar_file_names)

name_numbers <- numeric(length(cyto_file_names))
for (i in seq_along(cyto_file_names)) {
   name_numbers[i] <- as.numeric(gsub("[^0-9]+", "", cyto_file_names[i]))
 }


nuclei_tol = 0.01
nuclei_ext = 2
#nucl_threshold = 0.3
#cyto_threshold = 0.5
nucl_threshold = seq(from=0.18, to=0.24, by=0.01)
cyto_threshold = seq(from=0.15, to=0.5, by=0.05)
hdr_lambda_1 = 1e-4
hdr_lambda_2 = 1e-20
for (i in 1:length(cyto_folder)) {
  for (j in nucl_threshold){
    for (k in cyto_threshold){
      pix2pix_hoechst_filename = nucl_folder[[i]]
      pix2pix_calcein_filename = cyto_folder[[i]]
      pix2pix_gradient_filename = grad_folder[[i]]
      pix2pix_contrast_filename = phar_folder[[i]]
      ##commands to read pix2pix output images 
      pix2pix_hoechst = readImage(pix2pix_hoechst_filename)
      pix2pix_calcein = readImage(pix2pix_calcein_filename)
      pix2pix_gradient = readImage(pix2pix_gradient_filename)
      pix2pix_contrast = readImage(pix2pix_contrast_filename) #run in case colour
      pix2pix_contrast = Image(pix2pix_contrast, dim = c(256,256,3), colormode = Color)
      #pix2pix_gradient = Image(pix2pix_gradient, dim = c(256,256,1), colormode = Grayscale) #run in case colour
      
      ##commands to perform watershed
      pix2pix_nuclei = pix2pix_hoechst
      pix2pix_nuclei[pix2pix_nuclei < j] <- 0
      watershed_image = watershed(pix2pix_nuclei, tolerance = nuclei_tol, ext = nuclei_ext)
      display(colorLabels(watershed_image))
      
      ##commands to perform voronoi propagation
      cytoplasm_mask = pix2pix_calcein > k
      display(cytoplasm_mask)
      detected_cells = propagate(x = pix2pix_gradient, seeds = watershed_image, mask = cytoplasm_mask, lambda = hdr_lambda_1)
      display(colorLabels(detected_cells))
      
      detected_cells_norm = normalize(detected_cells)
      
      #savepath = "Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/wv val"
      #if(!file.exists(savepath)) {
      #  dir.create(savepath)
      #}  
      #dir.create(file.path(savepath, ))
      writeTIFF(EBImage::transpose(detected_cells_norm),
                paste0("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.11.29 bon predict/wv/nucl",j, "/cyto", k,"/mask_id/train/segmented_nucl",j,"_cyto",k,"_",name_numbers[[i]], ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      #store phrase contrast with colorlabel 
      display(0.4*colorLabels(detected_cells)+pix2pix_contrast)
      color_phase = 0.4*colorLabels(detected_cells)+pix2pix_contrast
      color_phase = normalize(color_phase) #have to normalize to write/ssave
      
      writeTIFF(EBImage::transpose(color_phase),
                paste0("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.11.29 bon predict/wv/nucl",j, "/cyto", k,"/color_label/color_phase_nucl",j,"_cyto",k,"_",name_numbers[[i]], ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
    }
  }
}



#binarized the data and then segment
nuclei_tol = 0.01
nuclei_ext = 2
#nucl_threshold = 0.3
#cyto_threshold = 0.5
nucl_threshold = seq(from=0.18, to=0.24, by=0.01)
cyto_threshold = seq(from=0.15, to=0.5, by=0.05)
hdr_lambda_1 = 1e-4
hdr_lambda_2 = 1e-20
for (i in 1:length(cyto_folder)) {
  for (j in nucl_threshold){
    for (k in cyto_threshold){
      pix2pix_hoechst_filename = nucl_folder[[i]]
      pix2pix_calcein_filename = cyto_folder[[i]]
      pix2pix_gradient_filename = grad_folder[[i]]
      pix2pix_contrast_filename = phar_folder[[i]]
      ##commands to read pix2pix output images 
      pix2pix_hoechst = readImage(pix2pix_hoechst_filename)
      pix2pix_calcein = readImage(pix2pix_calcein_filename)
      pix2pix_gradient = readImage(pix2pix_gradient_filename)
      pix2pix_contrast = readImage(pix2pix_contrast_filename) #run in case colour
      pix2pix_contrast = Image(pix2pix_contrast, dim = c(256,256,3), colormode = Color)
      #pix2pix_gradient = Image(pix2pix_gradient, dim = c(256,256,1), colormode = Grayscale) #run in case colour
      
      ##commands to perform watershed
      pix2pix_nuclei = pix2pix_hoechst
      pix2pix_nuclei = pix2pix_nuclei > j
      watershed_image = watershed(pix2pix_nuclei, tolerance = nuclei_tol, ext = nuclei_ext)
      display(colorLabels(watershed_image))
      
      ##commands to perform voronoi propagation
      cytoplasm_mask = pix2pix_calcein > k
      display(cytoplasm_mask)
      detected_cells = propagate(x = pix2pix_gradient, seeds = watershed_image, mask = cytoplasm_mask, lambda = hdr_lambda_1)
      display(colorLabels(detected_cells))
      
      detected_cells_norm = normalize(detected_cells)
      
      #savepath = "Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.5.19/wv val"
      #if(!file.exists(savepath)) {
      #  dir.create(savepath)
      #}  
      #dir.create(file.path(savepath, ))
      writeTIFF(EBImage::transpose(detected_cells_norm),
                paste0("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.11.29 bon predict/wv/nucl",j, "/cyto", k,"/mask_id/train/segmented_nucl",j,"_cyto",k,"_",name_numbers[[i]], ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      #store phrase contrast with colorlabel 
      display(0.4*colorLabels(detected_cells)+pix2pix_contrast)
      color_phase = 0.4*colorLabels(detected_cells)+pix2pix_contrast
      color_phase = normalize(color_phase) #have to normalize to write/ssave
      
      writeTIFF(EBImage::transpose(color_phase),
                paste0("Z:/01_Project01_AI/DataAndAnalysis/ResearchRecord/2023.11.29 bon predict/wv/nucl",j, "/cyto", k,"/color_label/color_phase_nucl",j,"_cyto",k,"_",name_numbers[[i]], ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
    }
  }
}




