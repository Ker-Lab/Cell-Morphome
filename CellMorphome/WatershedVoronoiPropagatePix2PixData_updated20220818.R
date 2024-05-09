#set up libraries and working directory
library(EBImage)
library(tiff)

##parameters
cyto_folder <- (Sys.glob("Z:/01_Project01_AI/ablation study for pix2pix/03SemanticResult/GAN_16bit/cytoplasm_predict_pix2pix_16bit/*.tif"))
nucl_folder <- (Sys.glob("Z:/01_Project01_AI/ablation study for pix2pix/03SemanticResult/GAN_16bit/nuclei_predict_pix2pix_16bit/*.tif"))
grad_folder <- (Sys.glob("Z:/01_Project01_AI/ablation study for pix2pix/03SemanticResult/GAN_16bit/gradient_predict_pix2pix_16bit/*.tif"))
phar_folder<- (Sys.glob("Z:/01_Project01_AI/ablation study for pix2pix/03SemanticResult/GAN_16bit/src/*.tif"))


nuclei_tol = 0.01
nuclei_ext = 1
#nucl_threshold = 0.3
#cyto_threshold = 0.5
nucl_threshold = seq(from=0.2, to=0.5, by=0.1)
cyto_threshold = seq(from=0.35, to=0.5, by=0.05)
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
      pix2pix_nuclei = pix2pix_hoechst > j
      watershed_image = watershed(pix2pix_nuclei, tolerance = nuclei_tol, ext = nuclei_ext)
      display(colorLabels(watershed_image))
      
      ##commands to perform voronoi propagation
      cytoplasm_mask = pix2pix_calcein > k
      display(cytoplasm_mask)
      detected_cells = propagate(x = pix2pix_gradient, seeds = watershed_image, mask = cytoplasm_mask, lambda = hdr_lambda_1)
      display(colorLabels(detected_cells))
      
      detected_cells_norm = normalize(detected_cells)
      writeTIFF(EBImage::transpose(detected_cells_norm),
                paste0("Z:/01_Project01_AI/ablation study for pix2pix/04WatershedVoronoi/GAN_16bit_binary/segmented_nucl",j,"_cyto",k,"_",i+9, ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      
      #store phrase contrast with colorlabel 
      display(0.4*colorLabels(detected_cells)+pix2pix_contrast)
      color_phase = 0.4*colorLabels(detected_cells)+pix2pix_contrast
      color_phase = normalize(color_phase) #have to normalize to write/ssave
      
      writeTIFF(EBImage::transpose(color_phase),
                paste0("Z:/01_Project01_AI/ablation study for pix2pix/04WatershedVoronoi/GAN_16bit_binary/color_phase_nucl",j,"_cyto",k,"_",i+9, ".tif"),
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
    }
  }
}





