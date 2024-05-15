#1. R Code: Load/Setup Libraries and Working Directory
library(spatstat) #to install, use install.packages("spatstat")
#to detach, use detach("package:spatstat", unload = TRUE)
#and detach("package:spatstat.data", unload = TRUE)
library(EBImage) #to install use: install.packages("BiocManager") 
#BiocManager::install("EBImage")
library(tiff) #to install use: install.packages("tiff")
library(MASS) #to install, use install.packages("MASS")
library(plotfunctions) #to install, use install.packages("plotfunctions")
library(shiny) #to install, use install.packages("shiny")
library(data.table) #to install, use install.packages("data.table")
library(DT) #to install, use install.packages("DT")
options(shiny.maxRequestSize = 20 * 1024^2) #Shiny can upload up to 20MB

#2. R Code: Functions and Code for Shiny Implementation (No Capability to Remove Oversegmented Nuclei Pixel)
#2i. R Code: User Interface
#Define UI
#UI Plan – Cycle through images or segments and followed by a tabbed 
#interface – where there are sliders for background normalization, 
#nuclei segmentation, followed by cell instance segmentation
if (interactive()) { #only run if interactive
  ui <- navbarPage("Cell Annotation",
                   tabPanel(value = "First_Step", title = "First Step",
                            h3(paste("Please input images exposure times, choose images,", 
                                     " and click ‘Compute HDR Radiance Maps")),
                            tags$head(tags$style('h5 {color:red;}')),
                            h5(HTML(
                              paste(
                                paste("*you can either start a new annotation or load a",
                                      " previously saved one. Please stay on this page ",  
                                      "until you see a notification asking you to proce",
                                      "ed to the second step. ", sep = ""),
                                paste("a) To start a new annotation, please upload imag", 
                                      "es, input exposure times, and click on the 'Comp", 
                                      "ute HDR Image & Radiance Maps' button.", sep =""),
                                paste("b) To load a previously saved annotation, please",  
                                      " upload images, previously saved parameter setti", 
                                      "ngs csv file, and click on the ‘Compute HDR Imag", 
                                      "e & Radiance Maps' button.", sep = ""), 
                                paste("Please wait for the computations to finish befor", 
                                      "e clicking elsewhere as this will protect agains",
                                      "t unexpected program crashes and errors. When co", 
                                      "mputations have finished, you will be notified a",
                                      "nd may proceed to the second step by clicking on",   
                                      " the ‘Second Step’ tab to optimize parameters fo",               
                                      "r cell segmentation.", sep = ""),
                                sep = "<br/>")
                            )),
                            textInput("p_exp", label = "Phase exposure time in seconds(s)",
                                      placeholder = "Separate each entry with a comma"),
                            fileInput("Phase", "Choose Phase File", 
                                      multiple = TRUE, 
                                      accept = c(".tif", ".tiff", "image/tiff")),
                            textInput("h_exp", label = "Hoechst exposure time in seconds(s)",
                                      placeholder = "Separate each entry with a comma"),
                            fileInput("Hoechst", "Choose Hoechst File",
                                      multiple = TRUE, 
                                      accept = c(".tif", ".tiff", "image/tiff")),
                            textInput("c_exp", label = "Calcein exposure time in seconds(s)",
                                      placeholder = "Separate each entry with a comma"),
                            fileInput("Calcein", "Choose Calcein File",
                                      multiple = TRUE, 
                                      accept = c(".tif", ".tiff", "image/tiff")),
                            fileInput("Load", "Load Previously Saved Parameters (optional)",
                                      multiple = FALSE, 
                                      accept = ".csv"),
                            actionButton("hdr_button", "Compute HDR Image & Radiance Maps")
                   ),
                   tabPanel(value = "Second_Step", title = "Second Step",
                            h3("Please optimize parameters for segmentation"),
                            h5(paste("*when first starting, please record the optimized ", 
                                     "parameters manually. This will protect your work from ",
                                     "unexpected program crash or other inadvertent errors.")), 
                            tabsetPanel(id = "maintabs_second", type = "tabs",
                                        tabPanel(value = "Back_ground", title = "Background",
                                                 h3("Threshold Background"),
                                                 fluidRow(
                                                   column(4, 
                                                          wellPanel(
                                                            sliderInput(inputId = "nthresh", 
                                                                        label = "Nuclei:", 
                                                                        min = 0, max = 1, 
                                                                        value = 0.100, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "nthresh_par"),
                                                            br(),
                                                            sliderInput(inputId = "cthresh", 
                                                                        label = "Cytoplasm:", 
                                                                        min = 0, max = 1, 
                                                                        value = 0.1000, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "cthresh_par"),
                                                            br(),
                                                            radioButtons(inputId = "pic_loc1", 
                                                                         label = "Location:", 
                                                                         choices = c("top left", 
                                                                                     "top center", 
                                                                                     "top right", 
                                                                                     "mid left", 
                                                                                     "mid center", 
                                                                                     "mid right",
                                                                                     "lower left", 
                                                                                     "lower center", 
                                                                                     "lower right"),
                                                                         selected = "top left"),     
                                                            radioButtons(inputId = "thresh_bdr", 
                                                                         label = paste("Borders",
                                                                                       " Visible:",
                                                                                       sep = ""), 
                                                                         choices = c("on", 
                                                                                     "off"),
                                                                         selected = "on"),
                                                            actionButton("thresh_button", 
                                                                         "Threshold Image"
                                                            )
                                                          )
                                                   ),
                                                   column(4, displayOutput("nuclei_bg",
                                                                           width = "100%")),
                                                   column(4, displayOutput("cytoplasm_bg",
                                                                           width = "100%"))
                                                 )
                                        ), 
                                        tabPanel(value = "Segment_Nuclei", title = "Nuclei",
                                                 h3("Segment Nuclei"),
                                                 fluidRow(
                                                   column(4, 
                                                          wellPanel(
                                                            #ws_pixel_rm_in and ws_pixel_rm_out is currently disabled because there 
                                                            #appears to be some kind of protect() error
                                                            #textInput("ws_pixel_rm_in", 
                                                            #          label = 
                                                            #          paste("Global Watershed P", 
                                                            #          "ixel Limit (Removes nucl", 
                                                            #          "ei whose size is less th", 
                                                            #          "an this number to avoid ", 
                                                            #          "oversegmentation. This p", 
                                                            #          "arameter applies to ALL ",  
                                                            #          "regions i.e. applies to ", 
                                                            #          "top left, mid center, lo", 
                                                            #          "wer right, etc. simultan",
                                                            #          "eously):", 
                                                            #          sep = ""),
                                                            #          placeholder = paste("Smal", 
                                                            #          "l values recommended: 1-",  
                                                            #          "5", sep = "")),
                                                            #textOutput(outputId = 
                                                            #          "ws_pixel_rm_out"),
                                                            #br(),
                                                            sliderInput(inputId = "n_ntol", 
                                                                        label = paste("Image ",
                                                                                      "Intensity Height Tole",
                                                                                      "rance For Separating ",
                                                                                      "Neighbouring Objects:",
                                                                                      sep = ""), 
                                                                        min = 0, max = 10, 
                                                                        value = 0.1, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "n_ntol_par"),
                                                            br(),
                                                            sliderInput(inputId = "n_next", 
                                                                        label = paste("Neigbour",
                                                                                      "hood Pixel Radius For ",
                                                                                      "Detecting Neighbouring",
                                                                                      " Objects:", sep = ""),  
                                                                        min = 1, max = 10, 
                                                                        value = 1, 
                                                                        step = 1),
                                                            textOutput(outputId = "n_next_par"),
                                                            br(),
                                                            radioButtons(inputId = "pic_loc2", 
                                                                         label = "Location:", 
                                                                         choices = c("top left", 
                                                                                     "top center", 
                                                                                     "top right", 
                                                                                     "mid left", 
                                                                                     "mid center", 
                                                                                     "mid right",
                                                                                     "lower left", 
                                                                                     "lower center", 
                                                                                     "lower right",
                                                                                     "border(s)"),
                                                                         selected = "top left"),  
                                                            radioButtons(inputId = "n_wshed_bdr", 
                                                                         label = paste("Borders",
                                                                                       " Visible:",
                                                                                       sep = ""), 
                                                                         choices = c("on", 
                                                                                     "off"),
                                                                         selected = "on"),
                                                            actionButton("nuclei_button", 
                                                                         "Segment Nuclei"
                                                            )
                                                          )
                                                   ),
                                                   column(8, displayOutput("nuclei_ws"))
                                                 )
                                        ),
                                        tabPanel(value = "Img_Gradient", title = "Gradient",
                                                 h3("Generate Image Gradient"),
                                                 fluidRow(
                                                   column(4, 
                                                          wellPanel(
                                                            sliderInput(inputId = "pninv_p", 
                                                                        label = paste("Phase & ",
                                                                                      "Inverse Nuclei Inters",
                                                                                      "ect: Phase Parameter",
                                                                                      sep = ""), 
                                                                        min = 0, max = 1, 
                                                                        value = 0.5, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "pninv_p_par"),
                                                            br(),
                                                            sliderInput(inputId = "pninv_n",
                                                                        label = paste("Phase & ",
                                                                                      "Inverse Nuclei Inters",
                                                                                      "ect: Nuclei Parameter",
                                                                                      sep = ""), 
                                                                        min = 0, max = 1, 
                                                                        value = 0.5, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "pninv_n_par"),
                                                            br(),
                                                            sliderInput(inputId = "pcinv_p", 
                                                                        label = paste("Phase & ",
                                                                                      "Inverse Calcein Inters",
                                                                                      "ect: Phase Parameter",
                                                                                      sep = ""), 
                                                                        min = 0, max = 1, 
                                                                        value = 0.5, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "pcinv_p_par"),
                                                            br(),
                                                            sliderInput(inputId = "pcinv_c", 
                                                                        label = paste("Phase & ",
                                                                                      "Inverse Calcein Inters",
                                                                                      "ect: Calcein Parameter",
                                                                                      sep = ""), 
                                                                        min = 0, max = 1, 
                                                                        value = 0.5, 
                                                                        step = 0.001),
                                                            textOutput(outputId = "pcinv_c_par"),
                                                            br(),
                                                            radioButtons(inputId = "pic_loc3", 
                                                                         label = "Location:", 
                                                                         choices = c("top left", 
                                                                                     "top center", 
                                                                                     "top right", 
                                                                                     "mid left", 
                                                                                     "mid center", 
                                                                                     "mid right",
                                                                                     "lower left", 
                                                                                     "lower center", 
                                                                                     "lower right",
                                                                                     "border(s)"),
                                                                         selected = "top left"),  
                                                            radioButtons(inputId = "instseg_bdr", 
                                                                         label = paste("Borders",
                                                                                       " Visible:",
                                                                                       sep = ""), 
                                                                         choices = c("on", 
                                                                                     "off"),
                                                                         selected = "on"),
                                                            actionButton("grad_button", paste( 
                                                              "Compute Gradient, ", 
                                                              "Segment Cells",
                                                              sep = "")
                                                            )
                                                          )
                                                   ),
                                                   column(4, displayOutput("gradient_adj", 
                                                                           width = "100%")),
                                                   column(4, displayOutput("phase_overlay",
                                                                           width = "100%"))
                                                 )
                                        ),
                                        tabPanel(value = "Custom_Annot", title = "Customize",
                                                 h3("Customize Annotations (by Cell Clusters)"),
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(
                                                            textInput("edit_x", 
                                                                      label = "X Coordinate:",
                                                                      placeholder = paste("Ente",
                                                                                          "r X Pixel ",
                                                                                          "Location", 
                                                                                          sep = "")
                                                            ),
                                                            textInput("edit_y", 
                                                                      label = "Y Coordinate:",
                                                                      placeholder = paste("Ente",
                                                                                          "r Y Pixel ",
                                                                                          "Location", 
                                                                                          sep = "")
                                                            ),
                                                            actionButton("edit_add_button",                    
                                                                         "Add Custom Annotation"
                                                            ),
                                                            dataTableOutput("annotations_table"),
                                                            tags$script("$(document).on('click',
                                            '#annotations_table button',   
                                            function () {
                                                Shiny.setInputValue(
                                                'lastClickId', this.id);
                                                Shiny.setInputValue(
                                                'lastClick',
                                                Math.random())
                                            }    
                                            );"
                                                            )               
                                                            
                                                          )                                      
                                                   ),     
                                                   column(8, displayOutput(
                                                     "mod_phase_overlay_all", width = 
                                                       "100%"))
                                                 )
                                        )   
                            )                  
                   ),
                   tabPanel(value = "Third_Step", title = "Third Step",
                            h3(paste("Please input folder path for this set of HDR images, ", 
                                     "click ‘Save’ to generate syntax for command line execution,",
                                     " HDR data as well as segmentation masks for further ", 
                                     "analysis.")),
                            h5(HTML(
                              paste(
                                paste("*for security reasons, users must input ",   
                                      "folder path manually. As of late 2019, ",                   
                                      "there is no way (that I'm aware of) to ", 
                                      "securely present a client side directory ",
                                      "selection dialog from a hosted web ", 
                                      "application."), 
                                paste("**Please make sure to input the desired, ", 
                                      "correct directory to avoid errors and loss",
                                      " of work."),
                                sep = "<br/>"
                              )
                            ) 
                            ), 
                            textInput("folderpath", label = "Local Data Folder Path:"),
                            actionButton("save_button", "Save")             
                   )
  ) 
  #2ii. R Code: Server
  server <- function(input, output, session) {
    #Define Variables for Use in Server Functions
    #Non-Reactive variables (Parameters)
    n_thresh_val_sr_dim = c(3,3)
    c_thresh_val_sr_dim = c(3,3)
    n_tol_sr_dim = c(3,3)
    n_ext_sr_dim = c(3,3)
    pninv_norm_thresh_p_sr_dim = c(3,3)
    pninv_norm_thresh_n_sr_dim = c(3,3)
    p_norm_thresh_val_sr_dim = c(3,3)
    c_inv_norm_thresh_sr_dim = c(3,3)
    #Non-Reactive variables (setup colors for heat map)
    jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF",     
                                    "cyan", "#7FFF7F", "yellow",
                                    "#FF7F00", "red", "#7F0000"))
    #Reactive variables (Parameters including starting parameters)
    p_names_sr = reactiveVal()
    h_names_sr = reactiveVal()
    c_names_sr = reactiveVal()
    p_text_sr = reactiveVal()
    h_text_sr = reactiveVal()
    c_text_sr = reactiveVal()
    n_thresh_val_sr = reactiveValues(topleft = 0.100, topcenter = 
                                       0.100, topright = 0.100, 
                                     midleft = 0.100, midcenter =
                                       0.100, midright = 0.100,
                                     lowerleft = 0.100, lowercenter =  
                                       0.100, lowerright = 0.100)
    c_thresh_val_sr = reactiveValues(topleft = 0.100, topcenter = 
                                       0.100, topright = 0.100, 
                                     midleft = 0.100, midcenter =
                                       0.100, midright = 0.100,
                                     lowerleft = 0.100, lowercenter =  
                                       0.100, lowerright = 0.100)
    n_tol_sr = reactiveValues(topleft = 0.275, topcenter = 0.275,
                              topright = 0.275, midleft = 0.275,  
                              midcenter = 0.275, midright = 0.275,
                              lowerleft = 0.275, lowercenter = 0.275, 
                              lowerright = 0.275)
    n_ext_sr = reactiveValues(topleft = 1.000, topcenter = 1.000,
                              topright = 1.000, midleft = 1.000,  
                              midcenter = 1.000, midright = 1.000,
                              lowerleft = 1.000, lowercenter = 1.000, 
                              lowerright = 1.000)
    b_n_tol_sr = reactiveVal(value = 0.275)
    b_n_ext_sr = reactiveVal(value = 1.000)
    #ws_pixel_rm_sr is currently disabled because there 
    #appears to be some kind of protect() error
    #ws_pixel_rm_sr = reactiveVal(value = 1) 
    pninv_norm_thresh_p_sr = reactiveValues(topleft = 0.500, 
                                            topcenter = 0.500, topright = 0.500,
                                            midleft = 0.500, midcenter = 0.500,
                                            midright = 0.500, lowerleft = 0.500,
                                            lowercenter = 0.500, lowerright = 0.500)
    pninv_norm_thresh_n_sr = reactiveValues(topleft = 0.500, 
                                            topcenter = 0.500, topright = 0.500,
                                            midleft = 0.500, midcenter = 0.500,
                                            midright = 0.500, lowerleft = 0.500,
                                            lowercenter = 0.500, lowerright = 0.500)
    b_pninv_norm_thresh_p_sr = reactiveVal(value = 0.500)
    b_pninv_norm_thresh_n_sr = reactiveVal(value = 0.500)
    p_norm_thresh_val_sr = reactiveValues(topleft = 0.500, 
                                          topcenter = 0.500, topright = 0.500,
                                          midleft = 0.500, midcenter = 0.500,
                                          midright = 0.500, lowerleft = 0.500,
                                          lowercenter = 0.500, lowerright = 0.500)
    c_inv_norm_thresh_sr = reactiveValues(topleft = 0.500, 
                                          topcenter = 0.500, topright = 0.500,
                                          midleft = 0.500, midcenter = 0.500,
                                          midright = 0.500, lowerleft = 0.500,
                                          lowercenter = 0.500, lowerright = 0.500)
    b_p_norm_thresh_val_sr = reactiveVal(value = 0.500)
    b_c_inv_norm_thresh_sr = reactiveVal(value = 0.500)
    segment_df_sr = reactiveVal()
    #Reactive variables (Radiance maps)
    pmask_radiancemap_sr = reactiveVal()             
    nmask_radiancemap_sr = reactiveVal()             
    cmask_radiancemap_sr = reactiveVal()             
    #Reactive variables (Thresholded images)
    pmask_thresh_sr = reactiveVal()
    nmask_thresh_sr = reactiveVal()
    nmask_thresh_inverse_sr = reactiveVal()
    cmask_thresh_sr = reactiveVal()
    cmask_thresh_inverse_sr = reactiveVal()
    img_bg_nuclei_sr = reactiveVal()
    img_bg_cytoplasm_sr = reactiveVal()
    #Reactive variables (Masks and Borders)
    #nmask_sr = reactiveVal() if want to save nuclei mask
    ctmask_sr = reactiveVal()
    connectedmask_sr = reactiveVal()
    connectedmask_list_sr = reactiveVal()
    borders_sr = reactiveVal() 
    #Reactive variables (Heatmaps)
    hm_phase_rm_sr = reactiveVal()
    hm_nuclei_rm_sr = reactiveVal()
    hm_nuclei_rm_inverse_sr = reactiveVal()
    hm_calcein_rm_sr = reactiveVal()
    hm_calcein_rm_inverse_sr = reactiveVal()
    #Reactive variables (Nuclei Watershed and Gradient)
    nuc_watershed_sr = reactiveVal()
    col_nuc_watershed_sr = reactiveVal()
    gradient_sr = reactiveVal()
    norm_gradient_sr = reactiveVal()
    img_nuclei_ws_sr = reactiveVal()
    img_phase_overlay_sr = reactiveVal()
    #Reactive variables (Segmented Masks)
    segmentedcells_sr = reactiveVal()
    col_segmentedcells_sr = reactiveVal()
    segmentedcells_overlay_sr = reactiveVal()
    #Reactive variables (Custom Annotations)
    custom_annot_sr <- reactiveValues()
    custom_annot_sr$Data <- data.table(x = NULL,
                                       y = NULL)
    corrections = reactiveVal(value = FALSE)
    state_ifaddisTRUE_ifmodifyisFALSE = reactiveVal()
    state_ifnucleiisTRUE_ifgradientisFALSE = reactiveVal(
      value = "none")
    state_ifnucleipressedisTRUE = reactiveVal(value = FALSE)
    state_ifgradpressedisTRUE = reactiveVal(value = FALSE)
    state_refresh_del = reactiveVal(value = 1)
    seg_pointx_s = reactiveVal()
    seg_pointy_s = reactiveVal()
    curr_seg_pointx_s = reactiveVal()
    curr_seg_pointy_s = reactiveVal()
    corr_ntol_sr = reactiveVal()
    corr_next_sr = reactiveVal()
    corr_pninv_normthresh_p_sr = reactiveVal()
    corr_pninv_normthresh_n_sr = reactiveVal()
    corr_p_normthresh_val_sr = reactiveVal()
    corr_cinv_normthresh_val_sr = reactiveVal()                                                
    corr_nuc_watershed_sr = reactiveVal()
    corr_img_phase_overlay_sr = reactiveVal()
    corr_curr_watershed_param_sr = reactiveVal()
    corr_curr_gradient_param_sr = reactiveVal()
    #Define Server Functions
    #Server Functions For Constructing High Dynamic Range Images as well 
    #as other supporting functions
    createSubDirectory <- function(path, dirname) {
      #check if subdirectory exists if it doesn’t, it will create one using
      #dirname as the name 
      if (file.exists(file.path(path, dirname))){
        print(paste("SubDirectory already exists: ", file.path(path, 
                                                               dirname)))
      } else {
        dir.create(file.path(path, dirname))
        print(paste("SubDirectory created: ", file.path(path, 
                                                        dirname)))
      }
    }
    #check to see if subdirectory called ‘name’ exists. If it 
    #doesn’t, it will create it. Delete directory manually if u wish 
    #to start afresh.
    processto8bit <- function(images) {
      #EBImage normalizes data between 0 and 1 when reading in images
      #this function alters pixel values between 0 and 255
      #Parameters
      #----------
      #images : A list containing a stack of single-channel (i.e., 
      #grayscale), layers of an HDR exposure stack
      #Returns: images: a new list of images with the image pixel 
      #intensity ranging from 0 to 255
      z_min = 0
      z_max = 255
      num_images = length(images)
      for (i in 1:num_images){ #change pixel intensities for greyscale 
        #image
        images[[i]] = round(EBImage::normalize(images[[i]], ft = 
                                                 c(z_min, z_max)))
      }
      return(images)
    }
    linearWeight <- function(pixel_value) {
      #Parameters
      #----------
      #pixel_value : A pixel intensity value from 0 to 255
      #Returns: weight : The weight corresponding to the input pixel 
      #intensity
      z_min = 0
      z_max = 255
      if (pixel_value <= (z_min + z_max) / 2) {
        return(pixel_value - z_min)
      } else {
        return(z_max - pixel_value)
      }
    }
    sampleIntensities <- function(images) {
      #Randomly sample pixel intensities from the exposure stack.
      #Parameters
      #----------
      #grayscale), layers of an HDR exposure stack
      #Returns
      #-------
      #intensity_values : An array containing a uniformly sampled 
      #intensity 
      #value from each exposure layer (shape = num_intensities x 
      #num_images)
      z_min = 0
      z_max = 255
      num_intensities = z_max - z_min + 1
      num_images = length(images)
      intensity_values = array(0, dim = c(num_intensities, 
                                          num_images))
      #Find the middle image to use as the source for pixel intensity 
      #locations
      mid_img = images[[ceiling(num_images / 2)]]
      for (i in z_min:z_max) {
        rows_cols = which(mid_img[,] == i, arr.ind = TRUE)
        rows = rows_cols[,1] #return indices of row where entry 
        #matches 
        cols = rows_cols[,2] #return indices of column where entry 
        #matches
        if (length(rows) != 0) {
          idx = sample(1:length(rows), 1, replace=FALSE)
          for (j in 1:num_images) {
            intensity_values[(i+1), j] = images[[j]][rows[[idx]],
                                                     cols[[idx]]]
          }
        }
      }
      return(intensity_values)
    }
    computeResponseCurve <- function(intensity_samples, log_exposures,  
                                     smoothing_lambda, 
                                     weighting_function) {
      #Find the camera response curve for a single color channel
      #Parameters
      #----------
      #intensity_samples : Stack of single channel input values 
      #(num_samples x num_images)
      #log_exposures : Log exposure times (size == num_images)
      #smoothing_lambda : float  
      #a constant value used to correct for scale differences between 
      #data and smoothing terms in the constraint matrix
      #source paper:
      #http://www.pauldebevec.com/Research/HDR/debevec-siggraph97.pdf
      #Paul Debevec's and Jitendra Malik's 1997 siggraph paper suggests         
      #a value of 100.
      #weighting_function : callable function that computes a weight 
      #from a pixel intensity
      #Returns
      #-------
      #Return a vector g(z) where the element at index i is the log 
      #exposure of a pixel with intensity value z = i
      #(e.g., g[0] is the log exposure of z=0, g[1] is the log exposure 
      #of z=1, etc.)
      z_min = 0
      z_max = 255
      intensity_range = 255 #difference between min and max possible                         
      #pixel value
      num_samples = nrow(intensity_samples)
      num_images = length(log_exposures)
      #NxP + [(Zmax-1) - (Zmin + 1)] + 1 constraints; N + 256 columns
      mat_A = array(data = 0, dim = c(num_images * num_samples + 
                                        intensity_range, num_samples + intensity_range + 
                                        1))
      mat_b = array(data = 0, dim = c(nrow(mat_A), 1))
      #1. Add data-fitting constraints:
      k = 1 
      for (i in 1:num_samples) {
        for (j in 1:num_images) {
          z_ij = intensity_samples[i, j]
          w_ij = weighting_function(z_ij)
          mat_A[k, z_ij] = w_ij
          mat_A[k, (intensity_range + 1 + i)] = -w_ij
          mat_b[k, 1] = w_ij * log_exposures[[j]]
          k = k + 1
        }    
      }
      #2. Add smoothing constraints:
      for (z_k in (z_min + 1):(z_max-1)) { #zmax-1 because unlike R, 
        #Python indexes from 0 and  
        #stops b4 zmax
        w_k = weighting_function(z_k)
        mat_A[k, z_k] = w_k * smoothing_lambda #not z_k minus one 
        #because unlike R,  
        #Python indexes from 
        #0 & stops before zmax
        mat_A[k, z_k + 1] = -2 * w_k * smoothing_lambda 
        #not z_k because unlike R, Python indexes 
        #indexes from 0 and stops before zmax
        mat_A[k, z_k + 2] = w_k * smoothing_lambda 
        #not z_k plus one because unlike R, 
        #Python indexes from 0 and stops before 
        #zmax
        k = k + 1
      }
      #3. Add color curve centering constraint:
      mat_A[k, ceiling((z_max - z_min) / 2)] = 1 
      
      #4. Solve the system using SVD
      inv_A = ginv(mat_A)
      x = inv_A %*% mat_b
      g = x[1:(intensity_range + 1)]
      return(g) 
    }
    computeRadianceMap <- function(images, log_exposure_times,         
                                   response_curve, 
                                   weighting_function) {
      #Calculate a radiance map for each pixel from the response curve.
      #Parameters
      #----------
      #images : list
      #Collection containing a single color layer (i.e., grayscale)
      #from each image in the exposure stack. (size == num_images)
      #log_exposure_times : Array containing the log exposure times for         
      #each image 
      #in the exposure stack (size == num_images)
      #response_curve : Array, least-squares fitted log exposure of 
      #each pixel value z
      #weighting_function : callable, function that computes the 
      #weights
      #Returns        
      #-------
      #Array, the image radiance map (in log space)
      img_shape = dim(images[[1]])
      img_rad_map = array(data = 0, dim = img_shape)
      num_images = length(images)
      log_exposure_times = as.numeric(log_exposure_times) #convert to 
      #numeric list
      for (i in 1:img_shape[1]) {
        #print(i) #print image row number/width that is being 
        #processed
        for (j in 1:img_shape[2]) {
          g = array(dim = num_images)
          w = array(dim = num_images)
          for (k in 1:num_images) {
            g[k] = response_curve[((images[[k]][i, j]) + 1)] 
            #need to add 1 because R arrays starts indexing at 
            #number 1 and not 0 unlike python
            w[k] = weighting_function(images[[k]][i, j])
          }
          SumW = sum(w)
          if (SumW > 0) {
            img_rad_map[i, j] = sum(w * (g-log_exposure_times) /
                                      SumW)
          } else {
            img_rad_map[i, j] = g[ceiling(num_images / 2)] - 
              log_exposure_times[ceiling(num_images / 2)]
          }
        }
      }  
      return(img_rad_map)
    }
    globalToneMapping <- function(image, gamma) {
      #Global tone mapping using gamma correction
      #----------
      #images : , array of image needed to be corrected
      #gamma : floating number, the number for gamma correction. Higher 
      #value for brighter result; lower for darker
      #Returns
      #-------
      #array, the resulting image after gamma correction
      image_corrected = (image/255)^(1.0/gamma)
      return(image_corrected)
    }
    intensityAdjustment <- function(image, template) {
      #Tune image intensity based on template
      #----------
      #image : array, image needed to be adjusted
      #template : array, 
      #Typically we use the middle image from image stack.
      #We want to match the image intensity for each channel to 
      #template's
      #Returns
      #-------
      #an array of the resulting image after intensity adjustment
      img_shape = dim(image)
      m = img_shape[1]
      n = img_shape[2]
      channel = img_shape[3]
      if (is.na(channel)) {
        channel = 1 #error checking for NA values
        image = array(data = image, dim = c(m, n, channel))
        template = array(data = template, dim = c(m, n, channel))
      }
      output = array(data = 0, dim = c(m, n, channel))
      for (ch in 1:channel) {
        image_avg = mean(image[, , ch])
        template_avg = mean(template[, , ch])
        output[, , ch] = image[, , ch] * (template_avg / image_avg)
      }
      return(output)
    }
    computeHDR <- function(images, log_exposure_times, smoothing_lambda = 
                             100, gamma = 0.6) {
      #Computational pipeline to produce the HDR images
      #----------
      #images : list of arrays, A list containing an exposure stack of                         
      #images
      #log_exposure_times : array of the log exposure times for each  
      #image in the exposure stack
      #smoothing_lambda : optional integer, a constant value to correct 
      #for scale differences between data and smoothing terms in the 
      #constraint matrix -- source paper suggests a value of 100.
      #Returns
      #-------
      #array of the resulting HDR with intensities scaled to fit 8 bit 
      #range
      img_shape = dim(images[[1]])
      num_images = length(images)
      if (length(img_shape) == 2) { #check if greyscale or colored 
        #image
        num_channels = 1
        for (i in 1:num_images) { #reshape image into x by y by 1 
          #colour dimension for easier 
          #processing
          images[[i]] = array(data = images[[i]], dim = 
                                c(dim(images[[i]])[1], 
                                  dim(images[[i]])[2], 1))  
        }
      } else {
        num_channels = img_shape[3] 
      }
      hdr_image = array(data = 0, dim = img_shape)
      rc = vector(mode = "list", length = num_channels) #save response 
      #curve for no. 
      #of channels
      img_rm = vector(mode = "list", length = num_channels) 
      #save img radiance map for no. of channels
      for (channel in 1:num_channels) {
        #Collect the current layer of each input image from the 
        #exposure stack
        layer_stack = vector("list", length = num_images)
        for (img in 1:num_images) {
          layer_stack[[img]] = images[[img]][, , channel]
        }
        #Sample image intensities
        intensity_samples = sampleIntensities(layer_stack)        
        #Compute Response Curve
        response_curve = computeResponseCurve(intensity_samples, 
                                              log_exposure_times, smoothing_lambda,
                                              linearWeight)
        rc[[channel]] <- response_curve #camera response curve
        print("Response curve computed")  
        #Build radiance map
        img_rad_map = computeRadianceMap(layer_stack, 
                                         log_exposure_times, response_curve, 
                                         linearWeight)
        img_rm[[channel]] <- img_rad_map #image radiance map
        print("Radiance map computed")      
        #Normalize hdr layer to (0, 255)
        if (num_channels == 1) {
          hdr_image[,] = normalize(img_rad_map, separate=TRUE, 
                                   ft = c(0, 255))  
        } else {
          hdr_image[, , channel] = normalize(img_rad_map, 
                                             separate=TRUE, ft = c(0, 255))
        }
      } 
      #Global tone mapping
      image_mapped = globalToneMapping(hdr_image, gamma)
      #Adjust image intensity based on the middle image from image 
      #stack
      template = images[[ceiling(length(images)/2)]]
      image_tuned = intensityAdjustment(image_mapped, template)
      #Output image
      hdr_output = normalize(image_tuned, separate=TRUE, ft=c(0, 255))
      #Return response curve, image radiance map, hdr image
      rc_img_rm_hdr_output_list = vector("list", length = 3) 
      rc_img_rm_hdr_output_list[[1]] = rc
      rc_img_rm_hdr_output_list[[2]] = img_rm
      rc_img_rm_hdr_output_list[[3]] = hdr_output
      names(rc_img_rm_hdr_output_list) = c("Response Curve(s)", 
                                           "Image Radiance Map(s)", 
                                           "HDR Output")
      return(rc_img_rm_hdr_output_list)
    }
    plotResponseCurve <- function (rc_img_rmhdr_output_list) {
      #code for plotting camera response
      #assumes either grayscale or RGB channel with Red being first 
      #channel, Green being second channel, and Blue being third channel
      rc = rc_img_rmhdr_output_list[["Response Curve(s)"]]
      if (length(rc) == 1) {
        channel = 1
      } else {
        channel = length(rc)
      }
      for (c in 1:channel) {
        plot(x = rc[[c]], y = 0:255, xlab = "Log exposure X", ylab = 
               "Pixel Value Z", main = paste("Camera Response Curve ", 
                                             c))
      }
    }
    #code for plotting radiance map in heat colours
    plotRadianceMap <- function (rc_img_rmhdr_output_list) {
      #code for plotting radiance map
      #assumes either grayscale or RGB channel with Red being first 
      #channel, Green being second channel, and Blue being third channel
      img_rm = rc_img_rmhdr_output_list[["Image Radiance Map(s)"]]
      jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", 
                                      "cyan", "#7FFF7F", "yellow", 
                                      "#FF7F00", "red","#7F0000"))
      if (length(img_rm) == 1) {
        channel = 1
      } else {
        channel = length(img_rm)
      }
      for (c in 1:channel) {
        radmap = img_rm[[c]]
        heatmap_img_rm <- colormap(normalize(radmap), palette = 
                                     jet.colors(256))
        display(heatmap_img_rm, method = "raster", margin =
                  c(0.15,0.15))
        gradientLegend(valRange=c(0,1), color = jet.colors(256), 
                       pos = c(1400,300,1430,800), side = 4, length = 
                         0.3, depth = 0.01, inside = TRUE, coords = 
                         TRUE) #may need to adjust depending on image 
        #and your computer 
      }
    }
    displayHDR <- function (rc_img_rmhdr_output_list) {
      #code for displaying HDR Image
      #assumes either grayscale or RGB channel with Red being first 
      #channel, Green being second channel, and Blue being third channel
      img_hdr = rc_img_rmhdr_output_list[["HDR Output"]]
      if (dim(img_hdr)[[3]] == 1) {
        display(normalize(img_hdr))
      } else {
        display(Image(data = normalize(img_hdr), colormode = Color))    
      }
    }
    place_centroids <- function(index, nucleicentroids, 
                                nucleicentroids_in_image) {
      xy <- nucleicentroids[index,]
      x <- xy[[1]]
      y <- xy[[2]]
      nucleicentroids_in_image[[x,y]] <- index
      return(nucleicentroids_in_image)
    }
    translate_centroids <- function(nucleilabelled_img, nuclei_rm) {
      nucleicentroids <- computeFeatures.moment(nucleilabelled_img,  
                                                nuclei_rm,
                                                "computeFeatures.moment")[,c("m.cx","m.cy")]
      nucleicentroids_in_image <- array(data = 0, dim = 
                                          c(dim(nucleilabelled_img)[[1]],
                                            dim(nucleilabelled_img)[[2]]))
      if (is.null(nrow(nucleicentroids))) {
        #do nothing
      } else {
        for (index in 1:nrow(nucleicentroids)) {
          nucleicentroids_in_image <- place_centroids(index, 
                                                      nucleicentroids, 
                                                      nucleicentroids_in_image) 
        }
      }
      return(nucleicentroids_in_image)
    }
    non_negative_matrix <- function(non_neg_matrix) {
      if (min(non_neg_matrix) < 0) {
        non_neg_matrix = non_neg_matrix + abs(min(non_neg_matrix))
      }
      return(non_neg_matrix)
    }
    calculate_segments <- function(img, parameter_array) {
      #calculates the start and stop of each individual segment in an 
      #image for localized image manipulation. Returns a dataframe with 
      #each row 
      #containing start and stop x and y coordinates 
      if (length(parameter_array) == 1) {
        numberofsegments_y = 1
        numberofsegments_x = 1
      } else {
        numberofsegments_y = dim(parameter_array)[[1]]
        numberofsegments_x = dim(parameter_array)[[2]]
      }
      start_end_df = data.frame(matrix(NA, nrow = numberofsegments_y * 
                                         numberofsegments_x, ncol = 4))
      names(start_end_df) = c("x_start", "y_start", "x_end", "y_end")
      img_dimx = dim(img)[[1]]
      img_dimy = dim(img)[[2]]
      listy_start = vector("list", length = numberofsegments_y)
      listy_start = vector("list", length = numberofsegments_y)
      listy_end = vector("list", length = numberofsegments_y)
      listx_end = vector("list", length = numberofsegments_y)
      listy = seq.int(1, img_dimy, length.out = (numberofsegments_y + 
                                                   1))
      listx = seq.int(1, img_dimx, length.out = (numberofsegments_x + 
                                                   1))
      listy_start = listy[1:(numberofsegments_y)]
      listx_start = listx[1:(numberofsegments_x)]
      listy_end = listy[2:(numberofsegments_y)] - 1
      listx_end = listx[2:(numberofsegments_x)] - 1
      listy_end[numberofsegments_y] = listy[(numberofsegments_y + 1)]
      listx_end[numberofsegments_x] = listx[(numberofsegments_x + 1)]
      row_index = 1
      for (i in 1:(numberofsegments_x)) {
        for (j in 1:(numberofsegments_x)) {
          start_end_df[row_index, "x_start"] = listx_start[[i]]
          start_end_df[row_index, "x_end"] = listx_end[[i]]
          start_end_df[row_index, "y_start"] = listy_start[[j]]
          start_end_df[row_index, "y_end"] = listy_end[[j]]
          row_index = row_index + 1
        }
      }
      return(round(start_end_df))
    }
    removebackground <- function(start_end_df, img, ref_map, thresh_val) 
    {
      #removes background for img. Background is removed based on local 
      #image regions denoted in start_end_df for image img according to 
      #threshold values specified in thresh_val against a reference                         
      #radiance map 
      checkparameters = identical(length(thresh_val), 
                                  nrow(start_end_df)) 
      try(if (!checkparameters) { 
        stop("the following parameters must ALL have the same number  
                      of parameters: start_end_df and thresh_val")
      })
      if (colorMode(img) == 0) { #grayscale image
        img_rm_bg = array(data = NA, dim = c(dim(img)[[1]], 
                                             dim(img)[[2]]))
        for (i in (1:nrow(start_end_df))) {
          x1 = start_end_df[[i, "x_start"]]
          x2 = start_end_df[[i, "x_end"]]
          y1 = start_end_df[[i, "y_start"]]
          y2 = start_end_df[[i, "y_end"]]
          crop_img = img[x1:x2,y1:y2]
          crop_refmap = ref_map[x1:x2,y1:y2]
          crop_img[!(crop_refmap > thresh_val[[i]])] = 0
          img_rm_bg[x1:x2, y1:y2] = crop_img
        }
      } else if (colorMode(img) == 2) {  #colour scale image
        img_rm_bg = array(data = NA, dim = c(dim(img)[[1]], 
                                             dim(img)[[2]], dim(img)[[3]]))
        for (i in (1:nrow(start_end_df))) {
          x1 = start_end_df[[i, "x_start"]]
          x2 = start_end_df[[i, "x_end"]]
          y1 = start_end_df[[i, "y_start"]]
          y2 = start_end_df[[i, "y_end"]]
          crop_img = img[x1:x2,y1:y2,]
          crop_refmap = ref_map[x1:x2,y1:y2]
          crop_img[!(crop_refmap > thresh_val[[i]])] = 0
          img_rm_bg[x1:x2, y1:y2,] = crop_img
        }
        img_rm_bg = Image(img_rm_bg, colormode = Color)
      }
      return(img_rm_bg)
    }
    compute_nuclei_or_cytoplasm_on_img_border_partitions <- function(img, 
                                                                     mask, start_end_df) {
      #need a function to account for nuclei and cytoplasm that overlap 
      #on image border segments. This is because calculate_segments 
      #function will partition image into smaller segments for local 
      #image computation. if a single nuclei happens to be positioned 
      #at the borders of this partitioned image, watershed may detect 
      #multiple nuclei incorrectly instead of a single one.
      #if a single cell (cytoplasm) happens to be positioned at the 
      #borders of this partitioned image, Voronoi propagation may 
      #detect cell borders incorrectly.
      #this function may be modified in future for fully confluent 
      #images as the current algorithm may result in the entire image 
      #being computed as opposed to the borders only. For example, 
      #potential cell borders may be introduced to introduce 
      #discontinuities into the computed connected regions 
      #this function is primarily use for DETECTING CYTOPLASM OVERLAP 
      #but is coded such that it can be used to detect overlapping 
      #nuclei if needed
      #this function only assumes that input is not fully confluent and 
      #GRAYSCALE ONLY
      
      #find borders of local image segments 
      x_borders = c(unique(start_end_df[["x_start"]]), 
                    unique(start_end_df[["x_end"]]))
      y_borders = c(unique(start_end_df[["y_start"]]), 
                    unique(start_end_df[["y_end"]]))
      #remove first and last borders 
      x_borders = x_borders[!x_borders %in% c(1, dim(img)[[1]])]
      y_borders = y_borders[!y_borders %in% c(1, dim(img)[[2]])] 
      #check for and compute border segments nuclei
      if ((length(x_borders) == 0) & (length(y_borders) == 0)) { 
        #no segments so return empty image
        n_or_c_on_img_borders = array(data = 0, dim = 
                                        c(dim(img)[[1]], dim(img)[[2]]))
      } else { #there are segments so compute connected nuclei or 
        #cytoplasm that touch or overlap with borders of image 
        #partition
        #sort in ascending order 
        x_borders = sort(x_borders)
        y_borders = sort(y_borders)
        #compute binary image and compute image for either connected 
        #nuclei or cytoplasm 
        connected_nc = connected(as.im(mask))
        connected_nc = array(data = connected_nc[["v"]], dim = 
                               c(connected_nc[["dim"]][1],
                                 connected_nc[["dim"]][2]))
        #reshape image according to
        #Rosenfeld, A. and Pfalz, J.L. (1966) 
        #Sequential operations in digital processing 
        #compute unique, connected nuclei or cytoplasm within         
        #x_borders and y_borders from connected_nc 
        x_num_elements = length(x_borders)
        y_num_elements = length(y_borders)
        xy_num_elements = x_num_elements + y_num_elements 
        borders_nc_list = vector("list", length = xy_num_elements)
        for (num in (1:x_num_elements)) {
          borders_nc_list[[num]] = connected_nc[x_borders[num], ]
        }
        index = x_num_elements + 1
        for (num in (1:y_num_elements)) {
          borders_nc_list[[index]] = connected_nc[, y_borders[num]]
          index = index + 1
        }
        borders_nc_list = unlist(borders_nc_list, recursive = FALSE)
        borders_nc_list = unique(borders_nc_list)
        wholeimg_nc_list = as.array(connected_nc)
        wholeimg_nc_list_list = unique(wholeimg_nc_list)
        not_borders_nc_list = setdiff(wholeimg_nc_list, 
                                      borders_nc_list)
        #compute radiance map containing only unique, connected         
        #nuclei or cytoplasm within x_borders and y_borders from img 
        for (num in (1:length(not_borders_nc_list))) {
          connected_nc[(connected_nc == 
                          not_borders_nc_list[[num]])] = 0
          #remove nuclei or cytoplasm that does not overlap with                                             
          #partitioned image borders
        }    
        n_or_c_on_img_borders = img
        n_or_c_on_img_borders[!(connected_nc > 0)] = 0
      }
      #return radiance map containing only nuclei or cytoplasm that 
      #overlap or touch partitioned image borders
      return(n_or_c_on_img_borders)
    }
    multi_segment_nuclei_shiny <- function(ms_nmask_thresh, ms_n_tol, 
                                           ms_n_ext, bdrs_n_tol, 
                                           bdrs_n_ext, 
                                           ms_cmask_thresh_inverse, 
                                           cytomask, start_end_df,
                                           corrections = FALSE,
                                           seg_pointx_s = NULL, 
                                           seg_pointy_s = NULL,
                                           corr_n_tol_s = NULL,
                                           corr_n_ext_s = NULL) {
      #divides image into multiple segments for localized computation 
      #of nuclei and gradient (for watershed-based Voronoi propagation
      #image is roughly divided equally according to the number of 
      #variable elements present in function arguments/parameters
      #this function only assumes that input is GRAYSCALE ONLY
      
      #error checking for non-error correction watershed
      checkparameters = all(sapply(list(length(ms_n_ext)), FUN = 
                                     identical, length(ms_n_tol)))     
      try(if (!checkparameters) { 
        stop("the following parameters must ALL have 
                      the same number of parameters: ms_n_tol, ms_n_ext")
      })
      #set parameters
      ms_nmask_centroids = vector("list", length = 2)
      no_bdrs_nmask_watershed = array(data = NA, dim = 
                                        c(dim(ms_nmask_thresh)[[1]], 
                                          dim(ms_nmask_thresh)[[2]]))
      no_bdrs_nmask_centroids = array(data = NA, dim = 
                                        c(dim(ms_nmask_thresh)[[1]], 
                                          dim(ms_nmask_thresh)[[2]]))
      #find cell cytoplasm that overlap or touch the borders of 
      #partitioned images followed by their accompanying nuclei
      cmaskrm_inverse_bdrs = 
        compute_nuclei_or_cytoplasm_on_img_border_partitions(img = 
                                                               ms_cmask_thresh_inverse, mask = cytomask, start_end_df =
                                                               start_end_df)
      nmaskrm_bdrs = ms_nmask_thresh
      nmaskrm_bdrs[!((cmaskrm_inverse_bdrs > 0) & (ms_nmask_thresh >
                                                     0))] = 0
      #relevant nuclei should be within connected cmasks that 
      #overlap or touch partitioned image border while 
      #non-relevant nuclei should be excluded
      #remove nuclei and cytoplasm that overlap or touch borders of         
      #partitioned image from radiance maps for local image computation 
      nmaskrm_no_bdrs = ms_nmask_thresh - nmaskrm_bdrs
      #perform nuclei watershed for nuclei that overlap or touch the   
      #borders of partitioned image using averaged parameter values
      nmaskrm_borders_watershed = watershed(nmaskrm_bdrs,
                                            tolerance = bdrs_n_tol,
                                            ext = bdrs_n_ext)
      bdrs_nmask_centroids =  
        translate_centroids(nmaskrm_borders_watershed, 
                            ms_nmask_thresh)
      #divide into multiple segments, compute watershed and centroids 
      #for a local region, and lastly re-merge back into a single image
      crop_count = 0 #each image partition/crop will have segmented 
      #cells that are labelled progressively from 1 
      #onwards. This variable ensures that each cell has 
      #a unique label number. 
      for (i in (1:nrow(start_end_df))) {
        #divide into multiple segments or partitions
        x1 = start_end_df[[i, "x_start"]]
        x2 = start_end_df[[i, "x_end"]]
        y1 = start_end_df[[i, "y_start"]]
        y2 = start_end_df[[i, "y_end"]]
        crop_n_thresh_img = nmaskrm_no_bdrs[x1:x2,y1:y2]
        #compute for local image segments or partitions
        ss_ng = single_segment_nuclei_shiny(ss_nmask_thresh = 
                                              crop_n_thresh_img,         
                                            ss_n_tol = ms_n_tol[[i]], 
                                            ss_n_ext = ms_n_ext[[i]])
        #re-merge data (for cells that do not overlay or touch   
        #borders of partitioned image). Due to local image 
        #partitioning, crop_count keeps track of nuclei/cell number 
        #from previous crops/partitions to ensure that each centroid  
        #is assigned a unique number label.
        crop_nmask_watershed = ss_ng[[1]] + crop_count  
        crop_nmask_watershed[(ss_ng[[1]] == 0)] = 0
        crop_nmask_centroid = ss_ng[[2]] + crop_count
        crop_nmask_centroid[(ss_ng[[2]] == 0)] = 0
        crop_count = crop_count + max(ss_ng[[1]]) #new unique labels 
        #number 
        no_bdrs_nmask_watershed[x1:x2, y1:y2] = crop_nmask_watershed
        no_bdrs_nmask_centroids[x1:x2, y1:y2] = crop_nmask_centroid
      }
      #compute and merge data for cells that overlay or touch borders 
      #of and partitioned image with those that do not 
      adj_nmaskrm_borders_watershed = nmaskrm_borders_watershed +
        max(no_bdrs_nmask_watershed)
      adj_nmaskrm_borders_watershed[(nmaskrm_borders_watershed == 0)] = 
        0
      adj_nmaskrm_borders_centroids = bdrs_nmask_centroids +
        max(no_bdrs_nmask_centroids)
      adj_nmaskrm_borders_centroids[(bdrs_nmask_centroids == 0)] = 0
      nmask_watershed = no_bdrs_nmask_watershed + 
        adj_nmaskrm_borders_watershed
      nmask_centroids = no_bdrs_nmask_centroids + 
        adj_nmaskrm_borders_centroids    
      #save computed watershed and centroids
      ms_nmask_centroids[[1]] = nmask_watershed
      ms_nmask_centroids[[2]] = nmask_centroids
      #include custom annotation/error correction for watershed if 
      #corrections == TRUE 
      if (corrections == TRUE) {
        #check for same length of parameters for seg_pointx, 
        #seg_pointy, corr_n_tol, corr_n_ext
        checkparameters1 = all(sapply(list(length(seg_pointx_s), 
                                           length(seg_pointy_s), 
                                           length(corr_n_tol_s)), 
                                      FUN = identical,
                                      length(corr_n_ext_s)))     
        checkparameters2 = !(is.null(seg_pointx_s) & 
                               is.null(seg_pointy_s) &
                               is.null(corr_n_tol_s) &         
                               is.null(corr_n_ext_s))
        try(
          if (!(checkparameters1 & checkparameters2)) { 
            stop("the following parameters must not be NULL and 
                              must ALL have the same number of parameters:         
                              seg_pointx_s, seg_pointy_s, corr_n_tol_s, 
                              corr_n_ext_s")
          } else {
            ms_nmask_centroids = add_seg_corr_nuc_shiny(
              seg_pointx_s = seg_pointx_s,         
              seg_pointy_s = seg_pointy_s,
              uncorr_seg_result_s = 
                ms_nmask_centroids,
              whole_nmask_thresh_s = 
                ms_nmask_thresh,
              corr_n_tol_s = corr_n_tol_s, 
              corr_n_ext_s = corr_n_ext_s, 
              cytomask_s = cytomask)  
          })
      } else {
        #corrections = FALSE, do nothing
      }
      #ws_pixel_rm_sr is currently disabled because there 
      #appears to be some kind of protect() error
      
      #remove watershed nuclei pixel size smaller than ws_pixel_rm_sr
      #features = Image(data = ms_nmask_centroids[[1]], dim = 
      #                 dim(ms_nmask_centroids[[1]]))
      #print("Computing nuclei attributes/features")
      #computed_shape_features = computeFeatures.shape(
      #                          features) #compute 
      #                                                  #nuclei ids 
      #                                                  #features 
      #                                                  #including 
      #                                                  #area 
      #area = as.array(computed_shape_features[,1])
      #print("Computing nuclei area")
      #remove = which(area < ws_pixel_rm_sr()) #compute nuclei ids 
      #                                        #that meet criteria 
      #                                        #for removal
      #remove_names = as.numeric(names(remove)) 
      #if (length(remove) > 0) { #remove and renumber nuclei ids
      #    print(paste("Removing nuclei with pixel size smaller ", 
      #                "than ", ws_pixel_rm_sr(), sep = ""))
      #    ws_remove_pixel_rm = features
      #    ws_remove_pixel_rm[(features %in% remove_names)] = 0
      #    renumbered_ws = ws_remove_pixel_rm
      #    renumber_cell_id = unique(as.vector(ws_remove_pixel_rm))
      #    renumber_cell_id = renumber_cell_id[renumber_cell_id != 0] 
      #        #not include 0 
      #    renumber_cell_id_num = length(renumber_cell_id)
      #    #find out the number of cells in the original watershed 
      #    #region (minus those that have been removed due to small 
      #    #pixel area (less than ws_pixel_rm). This corrects for an 
      #    #artefact of the watershed algorithm where there may be 
      #   #oversegmentation 
      #  for (new_cell_id in 1:renumber_cell_id_num) {
      #      renumbered_ws[
      #           ws_remove_pixel_rm == renumber_cell_id[new_cell_id] 
      #       ] = new_cell_id
      #   }
      #   renumbered_ws_centroids = translate_centroids(renumbered_ws, 
      #                             ms_nmask_thresh)
      #   ms_nmask_centroids[[1]] = renumbered_ws
      #   ms_nmask_centroids[[2]] = renumbered_ws_centroids
      #} else {
      #   #do nothing
      #}
      #return computed watershed and centroids
      return(ms_nmask_centroids)
    }
    single_segment_nuclei_shiny <- function(ss_nmask_thresh, ss_n_tol,
                                            ss_n_ext) {
      #set parameters
      ss_nmask_centroids = vector("list", length = 2)
      #calculate nuclei and nuclei centroids using watershed
      nmaskrm_thresh_watershed = watershed(ss_nmask_thresh,
                                           tolerance = ss_n_tol,
                                           ext = ss_n_ext)
      nmask_centroids = translate_centroids(nmaskrm_thresh_watershed, 
                                            ss_nmask_thresh)
      ss_nmask_centroids[[1]] = nmaskrm_thresh_watershed
      ss_nmask_centroids[[2]] = nmask_centroids 
      return(ss_nmask_centroids)
    }
    multi_segment_gradient_shiny <- function(ms_nmask_thresh, 
                                             ms_nmask_thresh_inverse, 
                                             ms_pninv_norm_thresh_p, 
                                             ms_pninv_norm_thresh_n,  
                                             bdrs_pninv_norm_thresh_p,
                                             bdrs_pninv_norm_thresh_n, 
                                             ms_pmask_thresh,
                                             ms_p_norm_thresh_val,   
                                             ms_cmask_thresh_inverse,
                                             bdrs_p_norm_thresh_val, 
                                             bdrs_c_inv_norm_thresh, 
                                             ms_c_inv_norm_thresh, 
                                             cytomask, start_end_df,
                                             corrections = FALSE,
                                             seg_pointx_s = NULL,
                                             seg_pointy_s = NULL,
                                             corr_pninv_norm_thresh_p_s = NULL, 
                                             corr_pninv_norm_thresh_n_s = NULL, 
                                             corr_p_norm_thresh_val_s = NULL,
                                             corr_c_inv_norm_thresh_val_s = NULL
    ) {
      #divides image into multiple segments for localized computation 
      #of nuclei and gradient (for watershed-based Voronoi propagation
      #image is roughly divided equally according to the number of 
      #variable elements present in function arguments/parameters
      #this function only assumes that input is GRAYSCALE ONLY
      
      #error checking
      checkparameters = all(sapply(list(length(ms_pninv_norm_thresh_p), 
                                        length(ms_pninv_norm_thresh_n), 
                                        length(ms_p_norm_thresh_val)), FUN = 
                                     identical, length(ms_c_inv_norm_thresh)))     
      try(if (!checkparameters) { 
        stop("the following parameters must ALL have 
                      the same number of parameters: ms_pninv_norm_thresh_p, 
                      ms_pninv_norm_thresh_n, ms_p_norm_thresh_val,
                      ms_c_inv_norm_thresh")
      })
      #set parameters
      no_bdrs_gradient = array(data = NA, dim = 
                                 c(dim(ms_nmask_thresh)[[1]],  
                                   dim(ms_nmask_thresh)[[2]]))
      #find cell cytoplasm that overlap or touch the borders of  
      #partitioned images followed by their accompanying nuclei
      cmaskrm_inverse_bdrs = 
        compute_nuclei_or_cytoplasm_on_img_border_partitions(img = 
                                                               ms_cmask_thresh_inverse, mask = cytomask, start_end_df =
                                                               start_end_df)
      nmaskrm_bdrs = ms_nmask_thresh
      nmaskrm_bdrs[!((cmaskrm_inverse_bdrs > 0) & (ms_nmask_thresh > 
                                                     0))] = 0
      #relevant nuclei should be within connected cmasks that 
      #overlap or touch partitioned image border while 
      #non-relevant nuclei should be excluded
      #remove nuclei and cytoplasm that overlap or touch borders of   
      #partitioned image from radiance maps for local image computation 
      pmaskrm_bdrs = ms_pmask_thresh
      pmaskrm_bdrs[!(cmaskrm_inverse_bdrs > 0)] = 0
      pmaskrm_no_bdrs = ms_pmask_thresh - pmaskrm_bdrs
      nmaskrm_no_bdrs = ms_nmask_thresh - nmaskrm_bdrs
      nmaskrm_inverse_no_bdrs = ms_nmask_thresh_inverse
      nmaskrm_inverse_no_bdrs[!(nmaskrm_no_bdrs > 0)] = 0
      cmaskrm_inverse_no_bdrs = ms_cmask_thresh_inverse - 
        cmaskrm_inverse_bdrs
      #calculate thresholds to compute the gradient that will be used 
      #for Voronoi propagation at partitioned image borders
      bdrs_n_inv_p_intersect_thresh = 
        ((normalize(nmaskrm_inverse_no_bdrs) > 
            bdrs_pninv_norm_thresh_n) & (normalize(pmaskrm_bdrs) > 
                                           bdrs_pninv_norm_thresh_p)) 
      bdrs_cp_intersect_thresh = ((normalize(pmaskrm_bdrs) >
                                     bdrs_p_norm_thresh_val) & (normalize(cmaskrm_inverse_bdrs) > 
                                                                  bdrs_c_inv_norm_thresh)) 
      #calculate cell borders between cells
      #this may be tuned for different images. need to select one that 
      #includes 
      #a) cell borders 
      #b) small round cells but excludes nuclei of large cells 
      bdrs_cmask_inverse_intersect_phase = pmaskrm_bdrs
      bdrs_cmask_inverse_intersect_phase[!(bdrs_cp_intersect_thresh > 
                                             0)] = 0
      #calculate cell borders for small-sized, round-shaped cells
      #this may be tuned for different images. need to select one that 
      #includes small round cells but excludes nuclei of large cells 
      #that typically do not have bright nuclei   
      bdrs_n_inverse_intersect_phase = pmaskrm_bdrs
      bdrs_n_inverse_intersect_phase[!(bdrs_n_inv_p_intersect_thresh > 
                                         0)] = 0
      #calculate gradient
      bdrs_gradient = bdrs_cmask_inverse_intersect_phase + 
        bdrs_n_inverse_intersect_phase
      #divide into multiple segments, compute gradient for a local 
      #region, and lastly re-merge back into a single image
      for (i in (1:nrow(start_end_df))) {
        #divide into multiple segments or partitions
        x1 = start_end_df[[i, "x_start"]]
        x2 = start_end_df[[i, "x_end"]]
        y1 = start_end_df[[i, "y_start"]]
        y2 = start_end_df[[i, "y_end"]]
        crop_n_thresh_img = nmaskrm_no_bdrs[x1:x2,y1:y2]
        crop_n_thresh_inverse_img = 
          nmaskrm_inverse_no_bdrs[x1:x2,y1:y2]
        crop_p_thresh_img = pmaskrm_no_bdrs[x1:x2,y1:y2]
        crop_c_thresh_inverse_img = 
          cmaskrm_inverse_no_bdrs[x1:x2,y1:y2]
        #compute for local image segments or partitions
        ss_ng = single_segment_gradient_shiny(
          ss_nmask_thresh_inverse = 
            crop_n_thresh_inverse_img, 
          ss_pninv_norm_thresh_p = 
            ms_pninv_norm_thresh_p[[i]],
          ss_pninv_norm_thresh_n = 
            ms_pninv_norm_thresh_n[[i]],
          ss_pmask_thresh = crop_p_thresh_img,
          ss_p_norm_thresh_val = ms_p_norm_thresh_val[[i]],
          ss_cmask_thresh_inverse = 
            crop_c_thresh_inverse_img,
          ss_c_inv_norm_thresh = ms_c_inv_norm_thresh[[i]])
        #re-merge data (for cells that do not overlay or touch 
        #borders of partitioned image).
        no_bdrs_gradient[x1:x2, y1:y2] = ss_ng
      }
      #compute and merge data for cells that overlay or touch borders 
      #of and partitioned image with those that do not 
      gradient = no_bdrs_gradient + bdrs_gradient
      #include custom annotation/error correction for gradient if 
      #corrections == TRUE 
      if (corrections == TRUE) {
        #check for same length of parameters for seg_pointx, 
        #seg_pointy, corr_pninv_norm_thresh_p_s, 
        #corr_pninv_norm_thresh_n_s, corr_p_norm_thresh_val_s, 
        #corr_c_inv_norm_thresh_val_s
        checkparameters1 = all(sapply(list(length(seg_pointx_s), 
                                           length(seg_pointy_s), 
                                           length(corr_pninv_norm_thresh_p_s), 
                                           length(corr_pninv_norm_thresh_n_s), 
                                           length(corr_p_norm_thresh_val_s)), 
                                      FUN = identical,
                                      length(corr_c_inv_norm_thresh_val_s))
        )     
        checkparameters2 = !(is.null(seg_pointx_s) & 
                               is.null(seg_pointy_s) &
                               is.null(corr_pninv_norm_thresh_p_s) &
                               is.null(corr_pninv_norm_thresh_n_s) &
                               is.null(corr_p_norm_thresh_val_s) &
                               is.null(corr_c_inv_norm_thresh_val_s))
        try(
          if (!(checkparameters1 & checkparameters2)) { 
            stop("the following parameters must not be NULL and 
                              must ALL have the same number of parameters: 
                              seg_pointx_s, seg_pointy_s, 
                              corr_pninv_norm_thresh_p_s, 
                              corr_pninv_norm_thresh_n_s,
                              corr_p_norm_thresh_val_s,  
                              corr_c_inv_norm_thresh_val_s")
          } else {
            gradient = add_seg_corr_gradient_shiny(
              seg_pointx_s = seg_pointx_s,         
              seg_pointy_s = seg_pointy_s,
              gradient = gradient,
              whole_nmask_thresh_s = ms_nmask_thresh,
              whole_nmask_thresh_inverse_s = 
                ms_nmask_thresh_inverse, 
              whole_pmask_thresh_s = ms_pmask_thresh,
              whole_cmask_thresh_inverse_s = 
                ms_cmask_thresh_inverse, 
              corr_pninv_norm_thresh_p_s =
                corr_pninv_norm_thresh_p_s, 
              corr_pninv_norm_thresh_n_s =
                corr_pninv_norm_thresh_n_s, 
              corr_p_norm_thresh_val_s = 
                corr_p_norm_thresh_val_s,
              corr_c_inv_norm_thresh_val_s =
                corr_c_inv_norm_thresh_val_s, 
              cytomask_s = cytomask)  
          })
      } else {
        #corrections = FALSE, do nothing
      }
      #return computed gradient
      return(gradient) 
    }
    single_segment_gradient_shiny <- function(ss_nmask_thresh_inverse, 
                                              ss_pninv_norm_thresh_p, 
                                              ss_pninv_norm_thresh_n,                                       
                                              ss_pmask_thresh,
                                              ss_p_norm_thresh_val,   
                                              ss_cmask_thresh_inverse,
                                              ss_c_inv_norm_thresh) {
      #calculate a gradient for nuclei-based voronoi propagation
      #=========================================================
      #calculate thresholds
      n_inverse_p_intersect_thresh = 
        ((normalize(ss_nmask_thresh_inverse) >
            ss_pninv_norm_thresh_n) & (normalize(ss_pmask_thresh) > 
                                         ss_pninv_norm_thresh_p)) 
      cp_intersect_thresh = ((normalize(ss_pmask_thresh) >
                                ss_p_norm_thresh_val) & (normalize(ss_cmask_thresh_inverse) > 
                                                           ss_c_inv_norm_thresh)) 
      #calculate cell borders between cells
      #this may be tuned for different images. need to select one that                                 
      #includes 
      #a) cell borders 
      #b) small round cells but excludes nuclei of large cells 
      cmask_inverse_intersect_phase = ss_pmask_thresh
      cmask_inverse_intersect_phase[!(cp_intersect_thresh > 0)] = 0
      #calculate cell borders for small-sized, round-shaped cells
      #this may be tuned for different images. need to select one that 
      #include small round cells but excludes nuclei of large cells 
      #that typically do not have bright nuclei   
      n_inverse_intersect_phase = ss_pmask_thresh
      n_inverse_intersect_phase[!(n_inverse_p_intersect_thresh > 0)] = 
        0
      #calculate gradient
      gradient = cmask_inverse_intersect_phase + 
        n_inverse_intersect_phase
      return(gradient)	
    }
    add_seg_corr_nuc_shiny <- function(seg_pointx_s, seg_pointy_s,
                                       uncorr_seg_result_s, 
                                       whole_nmask_thresh_s,  
                                       corr_n_tol_s, corr_n_ext_s, 
                                       cytomask_s) {
      #this function allows correction of watershed and cell centroids 
      # prior to propagate function. This function takes a point
      #as input and finds clusters of cells associated with 
      #seg_pointx_s and seg_pointy_s, and redoes watershed using the 
      #supplied correction parameters listed below -
      #For watershed: corr_n_tol_s and corr_n_ext_s
      #This function then renumbers segmentation results to correctly
      #ensure nuclei mask numbers are not duplicated (in case of more 
      #cells being detected) or skipped (in case of less cells being 
      #detected)
      
      #setup parameters
      uncorr_watershed = uncorr_seg_result_s[[1]]
      corr_nmask_centroids = vector("list", length = 2)
      #corr_cell_cluster_list = vector("list", length = 
      #                                length(seg_pointx_s))
      corr_cell_clusters_segpoint_xy = array(data = 0, dim = 
                                               dim(cytomask_s))
      combined_corr_watershed = array(data = 0, dim = dim(cytomask_s))
      numberofcells = 0 #for assigning unique cell ids
      #find connected cells and use connectedmask_sr variable to 
      #transform into an image and find connected cells that intersect 
      #only with seg_pointx_s and seg_pointy_s
      for (i in 1:length(seg_pointx_s)) {
        cell_clusters_segpoint_xy = connectedmask_list_sr()[[i]]
        corr_cell_clusters_segpoint_xy =  
          corr_cell_clusters_segpoint_xy + 
          cell_clusters_segpoint_xy #sum of all corrected areas 
        #compute watershed using corrected parameters (corr_n_tol_s 
        #and corr_n_ext_s)
        cell_clusters_nuclei_radmap = whole_nmask_thresh_s
        cell_clusters_nuclei_radmap[!(
          cell_clusters_segpoint_xy > 0)] = 0
        #only keep nuclei radiance map for connected cells region
        corr_watershed = watershed(cell_clusters_nuclei_radmap, 
                                   tolerance = corr_n_tol_s[i], ext = 
                                     corr_n_ext_s[i])
        renumber_watershed = corr_watershed + numberofcells 
        #no repeated cell ids by ensuring consecutive ids
        renumber_watershed[corr_watershed == 0] = 0 
        #reset background back to zero also, otherwise will have
        #errors      
        combined_corr_watershed = combined_corr_watershed + 
          renumber_watershed 
        numberofcells = max(combined_corr_watershed)
      }
      #renumber cell ids for uncorrected watershed region 
      uncorr_other_region = uncorr_watershed
      uncorr_other_region[(corr_cell_clusters_segpoint_xy > 0)] = 0
      #uncorrected areas, discard watershed results for connected 
      #cells region
      renumber_other_region = uncorr_other_region
      #for renumbering uncorr_other_region
      uncorr_other_region_cell_id = unique(as.vector(
        uncorr_other_region))
      uncorr_other_region_cell_id = uncorr_other_region_cell_id[
        uncorr_other_region_cell_id != 0]
      #find out the id number of cells in the original watershed 
      #region (minus the corrected cell cluster region) and remove 
      #0
      uncorr_other_region_cell_num = length(
        uncorr_other_region_cell_id)    
      #find out the number of cells in the original watershed 
      #region (minus the corrected cell cluster region) 
      for (new_cell_id in 1:uncorr_other_region_cell_num) {
        renumber_other_region[
          uncorr_other_region ==   
            uncorr_other_region_cell_id[new_cell_id]
        ] = (new_cell_id + numberofcells)
      }
      #renumber cell id in original watershed region 
      combined_corr_watershed = combined_corr_watershed + 
        renumber_other_region #combine them
      corr_translate_centroids = translate_centroids(
        combined_corr_watershed, 
        whole_nmask_thresh_s)
      corr_nmask_centroids[[1]] = combined_corr_watershed
      corr_nmask_centroids[[2]] = corr_translate_centroids
      #return corrected, computed result
      return(corr_nmask_centroids)
    }
    add_seg_corr_gradient_shiny <- function(seg_pointx_s, seg_pointy_s, 
                                            gradient, whole_nmask_thresh_s,  
                                            whole_nmask_thresh_inverse_s, 
                                            whole_pmask_thresh_s, 
                                            whole_cmask_thresh_inverse_s, 
                                            corr_pninv_norm_thresh_p_s, 
                                            corr_pninv_norm_thresh_n_s, 
                                            corr_p_norm_thresh_val_s,
                                            corr_c_inv_norm_thresh_val_s,
                                            cytomask_s) {
      #this function allows correction of the cell boundary-like 
      #gradient prior to propagate function. This function takes a 
      #point as input and finds clusters of cells associated with 
      #seg_pointx_s and seg_pointy_s, and redoes the cell boundary-like 
      #gradient using the supplied correction parameters listed below -
      #For gradient: corr_pninv_norm_thresh_p_s, 
      #corr_pninv_norm_thresh_n_s, corr_p_norm_thresh_val_s,
      #corr_c_inv_norm_thresh_val_s
      
      #setup parameters
      uncorr_gradient = gradient
      corr_gradient = array(data = 0, dim = dim(cytomask_s))
      corr_cell_clusters_segpoint_xy = array(data = 0, dim = 
                                               dim(cytomask_s))
      #find connected cells and use connectedmask_sr variable to 
      #transform into an image and find connected cells that intersect 
      #only with seg_pointx_s and seg_pointy_s
      for (i in 1:length(seg_pointx_s)) {
        cell_clusters_segpoint_xy = connectedmask_list_sr()[[i]]
        corr_cell_clusters_segpoint_xy = 
          corr_cell_clusters_segpoint_xy + 
          cell_clusters_segpoint_xy #sum of all corrected areas 
        #compute a gradient for nuclei-based voronoi propagation 
        #using corrected parameters (corr_pninv_norm_thresh_p_s, 
        #corr_pninv_norm_thresh_n_s,corr_p_norm_thresh_val_s, 
        #corr_c_inv_norm_thresh_val_s)                    
        #============================================================
        #calculate thresholds
        n_inverse_p_intersect_thresh = 
          ((normalize(whole_nmask_thresh_inverse_s) >
              corr_pninv_norm_thresh_n_s[i]) &         
             (normalize(whole_pmask_thresh_s) > 
                corr_pninv_norm_thresh_p_s[i])) 
        cp_intersect_thresh = ((normalize(whole_pmask_thresh_s) >
                                  corr_p_norm_thresh_val_s[i]) &
                                 (normalize(whole_cmask_thresh_inverse_s) > 
                                    corr_c_inv_norm_thresh_val_s[i])) 
        #calculate cell borders between cells
        #this may be tuned for different images. need to select one 
        #that includes 
        #a) cell borders 
        #b) small round cells but excludes nuclei of large cells 
        cell_clusters_phase_radmap = whole_pmask_thresh_s
        cell_clusters_phase_radmap[!(
          cell_clusters_segpoint_xy > 0)] = 0
        #only keep phase radiance map for connected cells region
        cmask_inverse_intersect_phase = cell_clusters_phase_radmap
        cmask_inverse_intersect_phase[!(cp_intersect_thresh > 0)] = 0
        #calculate cell borders for small-sized, round-shaped cells
        #this may be tuned for different images. need to select one 
        #that includes small round cells but excludes nuclei of large 
        #cells that typically do not have bright nuclei   
        n_inverse_intersect_phase = cell_clusters_phase_radmap
        n_inverse_intersect_phase[!(
          n_inverse_p_intersect_thresh > 0)] = 0
        #calculate gradient
        local_corr_gradient = cmask_inverse_intersect_phase + 
          n_inverse_intersect_phase
        corr_gradient = corr_gradient + local_corr_gradient
      }
      uncorr_gradient[(corr_cell_clusters_segpoint_xy > 0)] = 0 
      #delete gradient for connected cells region
      corr_gradient = corr_gradient + uncorr_gradient
      #return corrected, computed result
      return(corr_gradient)
    }
    
    #Step 1: Upload and Compute HDR Data
    computeHDRandRadMaps <- observeEvent(input$hdr_button, {
      #Define Variables and Default Parameters, Read in Image
      p_img = input$Phase[,4]
      p_img_names = input$Phase[,1]
      p_img_names = unlist(strsplit(p_img_names, ","))
      p_names_sr(p_img_names)
      h_img = input$Hoechst[,4]
      h_img_names = input$Hoechst[,1]
      h_img_names = unlist(strsplit(h_img_names, ","))
      h_names_sr(h_img_names)
      c_img = input$Calcein[,4]
      c_img_names = input$Calcein[,1]
      c_img_names = unlist(strsplit(c_img_names, ","))
      c_names_sr(c_img_names)
      if (is.null(input$Load)) {
        p_text = as.numeric(unlist(strsplit(input$p_exp, ",")))
        h_text = as.numeric(unlist(strsplit(input$h_exp, ",")))
        c_text = as.numeric(unlist(strsplit(input$c_exp, ",")))
        p_text_sr(p_text)
        h_text_sr(h_text)
        c_text_sr(c_text)
      } else {
        #set exposure time
        loadfile = input$Load[,4]
        loadfileparameters = read.csv(file = loadfile, header = 
                                        FALSE, sep = ",", fill = TRUE, 
                                      col.names = 1:max(count.fields(loadfile,  
                                                                     sep = ",")), stringsAsFactors = FALSE)
        p_text = as.list(loadfileparameters[5,])
        p_text = p_text[-1] #remove row name
        p_text = p_text[!is.na(p_text)] #remove NAs
        p_text = gsub(" ", "", p_text) #remove whitespaces
        p_text = as.numeric(p_text) #convert to numeric
        h_text = as.list(loadfileparameters[6,])
        h_text = h_text[-1] #remove row name
        h_text = h_text[!is.na(h_text)] #remove NAs
        h_text = gsub(" ", "", h_text) #remove whitespaces
        h_text = as.numeric(h_text) #convert to numeric
        c_text = as.list(loadfileparameters[7,])
        c_text = c_text[-1] #remove row name
        c_text = c_text[!is.na(c_text)] #remove NAs
        c_text = gsub(" ", "", c_text) #remove whitespaces
        c_text = as.numeric(c_text) #convert to numeric
        p_text_sr(p_text)
        h_text_sr(h_text)
        c_text_sr(c_text)
        #set non-reactive variables
        n_thresh_val_sr_dim = as.numeric(c(loadfileparameters[10,2], 
                                           loadfileparameters[10,3]))
        c_thresh_val_sr_dim = as.numeric(c(loadfileparameters[13,2],
                                           loadfileparameters[13,3]))
        n_tol_sr_dim = as.numeric(c(loadfileparameters[15,2], 
                                    loadfileparameters[15,3]))
        n_ext_sr_dim = as.numeric(c(loadfileparameters[17,2], 
                                    loadfileparameters[17,3]))
        pninv_norm_thresh_p_sr_dim = as.numeric(
          c(loadfileparameters[21,2],
            loadfileparameters[21,3]))
        pninv_norm_thresh_n_sr_dim = as.numeric(
          c(loadfileparameters[23,2],
            loadfileparameters[23,3]))
        p_norm_thresh_val_sr_dim = as.numeric(
          c(loadfileparameters[27,2],
            loadfileparameters[27,3]))
        c_inv_norm_thresh_sr_dim = as.numeric(
          c(loadfileparameters[29,2],
            loadfileparameters[29,3]))
        #set reactive variables (parameters including starting 
        #parameters)
        n_thresh_val_sr$topleft = as.numeric(loadfileparameters[8,2])  
        n_thresh_val_sr$topcenter = as.numeric(
          loadfileparameters[8,5]) 
        n_thresh_val_sr$topright = as.numeric(
          loadfileparameters[8,8]) 
        n_thresh_val_sr$midleft = as.numeric(loadfileparameters[8,3])
        n_thresh_val_sr$midcenter = as.numeric(
          loadfileparameters[8,6])
        n_thresh_val_sr$midright = as.numeric(
          loadfileparameters[8,9])
        n_thresh_val_sr$lowerleft = as.numeric(
          loadfileparameters[8,4]) 
        n_thresh_val_sr$lowercenter = as.numeric(
          loadfileparameters[8,7])
        n_thresh_val_sr$lowerright = as.numeric(
          loadfileparameters[8,10])
        c_thresh_val_sr$topleft = as.numeric(
          loadfileparameters[11,2])
        c_thresh_val_sr$topcenter = as.numeric(
          loadfileparameters[11,5]) 
        c_thresh_val_sr$topright = as.numeric(
          loadfileparameters[11,8])
        c_thresh_val_sr$midleft = as.numeric(
          loadfileparameters[11,3])
        c_thresh_val_sr$midcenter = as.numeric(
          loadfileparameters[11,6])
        c_thresh_val_sr$midright = as.numeric(
          loadfileparameters[11,9])
        c_thresh_val_sr$lowerleft = as.numeric(
          loadfileparameters[11,4]) 
        c_thresh_val_sr$lowercenter = as.numeric(
          loadfileparameters[11,7])
        c_thresh_val_sr$lowerright = as.numeric(
          loadfileparameters[11,10])
        n_tol_sr$topleft = as.numeric(loadfileparameters[14,2])  
        n_tol_sr$topcenter = as.numeric(loadfileparameters[14,5]) 
        n_tol_sr$topright = as.numeric(loadfileparameters[14,8])
        n_tol_sr$midleft = as.numeric(loadfileparameters[14,3])
        n_tol_sr$midcenter = as.numeric(loadfileparameters[14,6])
        n_tol_sr$midright = as.numeric(loadfileparameters[14,9])
        n_tol_sr$lowerleft = as.numeric(loadfileparameters[14,4]) 
        n_tol_sr$lowercenter = as.numeric(loadfileparameters[14,7])
        n_tol_sr$lowerright = as.numeric(loadfileparameters[14,10])
        n_ext_sr$topleft = as.numeric(loadfileparameters[16,2])
        n_ext_sr$topcenter = as.numeric(loadfileparameters[16,5]) 
        n_ext_sr$topright = as.numeric(loadfileparameters[16,8]) 
        n_ext_sr$midleft = as.numeric(loadfileparameters[16,3])
        n_ext_sr$midcenter = as.numeric(loadfileparameters[16,6])
        n_ext_sr$midright = as.numeric(loadfileparameters[16,9])
        n_ext_sr$lowerleft = as.numeric(loadfileparameters[16,4]) 
        n_ext_sr$lowercenter = as.numeric(loadfileparameters[16,7])
        n_ext_sr$lowerright = as.numeric(loadfileparameters[16,10])
        b_n_tol_sr(as.numeric(loadfileparameters[18,2]))
        b_n_ext_sr(as.numeric(loadfileparameters[19,2]))
        #ws_pixel_rm_sr is currently disabled because there 
        #appears to be some kind of protect() error
        #ws_pixel_rm_sr(as.numeric(loadfileparameters[41,2]))
        pninv_norm_thresh_p_sr$topleft = as.numeric(
          loadfileparameters[20,2])  
        pninv_norm_thresh_p_sr$topcenter = as.numeric(
          loadfileparameters[20,5])
        pninv_norm_thresh_p_sr$topright = as.numeric(
          loadfileparameters[20,8]) 
        pninv_norm_thresh_p_sr$midleft = as.numeric(
          loadfileparameters[20,3])
        pninv_norm_thresh_p_sr$midcenter = as.numeric(
          loadfileparameters[20,6])
        pninv_norm_thresh_p_sr$midright = as.numeric(
          loadfileparameters[20,9])
        pninv_norm_thresh_p_sr$lowerleft = as.numeric(
          loadfileparameters[20,4]) 
        pninv_norm_thresh_p_sr$lowercenter = as.numeric(
          loadfileparameters[20,7]
        )
        pninv_norm_thresh_p_sr$lowerright = as.numeric(
          loadfileparameters[20,10]
        )
        pninv_norm_thresh_n_sr$topleft = as.numeric(
          loadfileparameters[22,2])
        pninv_norm_thresh_n_sr$topcenter = as.numeric(
          loadfileparameters[22,5]) 
        pninv_norm_thresh_n_sr$topright = as.numeric(
          loadfileparameters[22,8]) 
        pninv_norm_thresh_n_sr$midleft = as.numeric(
          loadfileparameters[22,3])
        pninv_norm_thresh_n_sr$midcenter = as.numeric(
          loadfileparameters[22,6])
        pninv_norm_thresh_n_sr$midright = as.numeric(
          loadfileparameters[22,9])
        pninv_norm_thresh_n_sr$lowerleft = as.numeric(
          loadfileparameters[22,4]) 
        pninv_norm_thresh_n_sr$lowercenter = as.numeric(
          loadfileparameters[22,7]
        )
        pninv_norm_thresh_n_sr$lowerright = as.numeric(
          loadfileparameters[22,10]
        )
        b_pninv_norm_thresh_p_sr(as.numeric(
          loadfileparameters[24,2]))
        b_pninv_norm_thresh_n_sr(as.numeric(
          loadfileparameters[25,2]))
        p_norm_thresh_val_sr$topleft = as.numeric(
          loadfileparameters[26,2])  
        p_norm_thresh_val_sr$topcenter = as.numeric(
          loadfileparameters[26,5]) 
        p_norm_thresh_val_sr$topright = as.numeric(
          loadfileparameters[26,8]) 
        p_norm_thresh_val_sr$midleft = as.numeric(
          loadfileparameters[26,3])
        p_norm_thresh_val_sr$midcenter = as.numeric(
          loadfileparameters[26,6])
        p_norm_thresh_val_sr$midright = as.numeric(
          loadfileparameters[26,9])
        p_norm_thresh_val_sr$lowerleft = as.numeric(
          loadfileparameters[26,4]) 
        p_norm_thresh_val_sr$lowercenter = as.numeric(
          loadfileparameters[26,7])
        p_norm_thresh_val_sr$lowerright = as.numeric(
          loadfileparameters[26,10])
        c_inv_norm_thresh_sr$topleft = as.numeric(
          loadfileparameters[28,2])  
        c_inv_norm_thresh_sr$topcenter = as.numeric(
          loadfileparameters[28,5]) 
        c_inv_norm_thresh_sr$topright = as.numeric(
          loadfileparameters[28,8]) 
        c_inv_norm_thresh_sr$midleft = as.numeric(
          loadfileparameters[28,3])
        c_inv_norm_thresh_sr$midcenter = as.numeric(
          loadfileparameters[28,6])
        c_inv_norm_thresh_sr$midright = as.numeric(
          loadfileparameters[28,9])
        c_inv_norm_thresh_sr$lowerleft = as.numeric(
          loadfileparameters[28,4]) 
        c_inv_norm_thresh_sr$lowercenter = as.numeric(
          loadfileparameters[28,7])
        c_inv_norm_thresh_sr$lowerright = as.numeric(
          loadfileparameters[28,10])
        b_p_norm_thresh_val_sr(as.numeric(loadfileparameters[30,2]))
        b_c_inv_norm_thresh_sr(as.numeric(loadfileparameters[31,2]))
        #compute/read in reactive variables (for custom annotations)
        any_corrections = loadfileparameters[32,2]
        if (any_corrections == "TRUE") {
          print("Have corrections")                         
          x_seg = as.numeric(as.list(loadfileparameters[33,])[-1])
          x_seg = x_seg[!is.na(x_seg)] #remove NAs
          y_seg = as.numeric(as.list(loadfileparameters[34,])[-1])
          y_seg = y_seg[!is.na(y_seg)] #remove NAs
          corr_n_tol = as.numeric(as.list(
            loadfileparameters[35,])[-1])
          corr_n_tol = corr_n_tol[!is.na(corr_n_tol)] #remove NAs
          corr_n_ext = as.numeric(as.list(
            loadfileparameters[36,])[-1])
          corr_n_ext = corr_n_ext[!is.na(corr_n_ext)] #remove NAs
          corr_pninv_norm_thresh_p = as.numeric(as.list(
            loadfileparameters[37,])[-1])
          corr_pninv_norm_thresh_p = corr_pninv_norm_thresh_p[
            !is.na(
              corr_pninv_norm_thresh_p)]
          #remove NAs               
          corr_pninv_norm_thresh_n = as.numeric(as.list(
            loadfileparameters[38,])[-1])
          corr_pninv_norm_thresh_n = corr_pninv_norm_thresh_n[
            !is.na(
              corr_pninv_norm_thresh_n)]
          #remove NAs
          corr_p_norm_thresh_val = as.numeric(as.list(
            loadfileparameters[39,])[-1])
          corr_p_norm_thresh_val = corr_p_norm_thresh_val[!is.na(
            corr_p_norm_thresh_val)]
          #remove NAs
          corr_c_inv_norm_thresh_val = as.numeric(as.list(
            loadfileparameters[40,])[-1] 
          )
          corr_c_inv_norm_thresh_val = corr_c_inv_norm_thresh_val[
            !is.na(
              corr_c_inv_norm_thresh_val)]
          #remove NAs
          #set reactive variables (custom annotations)
          new_row = data.frame(x = x_seg, y = y_seg)
          custom_annot_sr$Data <- rbind(custom_annot_sr$Data, 
                                        new_row)
          corrections(TRUE)
          state_ifaddisTRUE_ifmodifyisFALSE(NULL)
          state_ifnucleiisTRUE_ifgradientisFALSE("none")
          state_ifnucleipressedisTRUE(NULL)
          state_ifgradpressedisTRUE(NULL)
          seg_pointx_s(x_seg)
          seg_pointy_s(y_seg)
          curr_seg_pointx_s(NULL)
          curr_seg_pointy_s(NULL)
          corr_ntol_sr(corr_n_tol)
          corr_next_sr(corr_n_ext)
          corr_pninv_normthresh_p_sr(corr_pninv_norm_thresh_p)
          corr_pninv_normthresh_n_sr(corr_pninv_norm_thresh_n)
          corr_p_normthresh_val_sr(corr_p_norm_thresh_val)
          corr_cinv_normthresh_val_sr(corr_c_inv_norm_thresh_val)
        }
      }
      #Compute HDR and Radiance Maps
      withProgress(message = "Computing HDR Image & Radiance Maps",
                   style = "notification", 
                   value = 0, {
                     #Phase HDR Image and Radiance Map Computation 
                     withProgress(message = " Generating Phase HDR data:", 
                                  style = "notification", 
                                  detail = "Computing Phase Radiance Map",
                                  value = 0.1, {
                                    img_phase_shiny = vector("list", length = 
                                                               length(p_img))
                                    for (i in (1:length(p_img))) {
                                      img_phase_shiny[[i]] = readImage(p_img[[i]]) 
                                    }
                                    incProgress(0.1, detail = "Reading in images")
                                    img_phase_shiny = processto8bit(img_phase_shiny)
                                    exposure_phase_shiny = vector("list", length = 
                                                                    length(p_text))
                                    for (i in (1:length(p_text))) {
                                      exposure_phase_shiny[[i]] = log(p_text[[i]])         
                                    }
                                    incProgress(0.3, detail = "Processing images")
                                    hdr_phase_shiny = computeHDR(img_phase_shiny, 
                                                                 exposure_phase_shiny,
                                                                 smoothing_lambda = 100, 
                                                                 gamma = 1.0)
                                    #Increment the bottom-level progress indicator
                                    setProgress(1.0, detail = "Phase Radiance Map Computed")
                                  })
                     #Increment the top-level progress indicator
                     incProgress(0.25)
                     #Hoechst HDR Image and Radiance Map Computation 
                     withProgress(message = "Generating Hoechst HDR data:", 
                                  style = "notification", 
                                  detail = "Computing Hoechst Radiance Map",
                                  value = 0.1, {
                                    img_hoechst_shiny = vector("list", length = 
                                                                 length(h_img))
                                    for (i in (1:length(h_img))) {
                                      img_hoechst_shiny[[i]] = readImage(h_img[[i]])         
                                    }
                                    incProgress(0.1, detail = "Reading in images")
                                    img_hoechst_shiny = processto8bit(img_hoechst_shiny)
                                    exposure_hoechst_shiny = vector("list", length = 
                                                                      length(h_text))
                                    for (i in (1:length(h_text))) {
                                      exposure_hoechst_shiny[[i]] = log(h_text[[i]])         
                                    }
                                    incProgress(0.3, detail = "Processing images")
                                    hdr_hoechst_shiny = computeHDR(img_hoechst_shiny, 
                                                                   exposure_hoechst_shiny,
                                                                   smoothing_lambda = 100, 
                                                                   gamma = 1.0)
                                    #Increment the bottom-level progress indicator
                                    setProgress(1.0, detail = paste("Hoechst Radiance Map",
                                                                    " Computed", sep = ""))
                                  })
                     #Increment the top-level progress indicator
                     incProgress(0.25)
                     #Calcein HDR Image and Radiance Map Computation 
                     withProgress(message = "Generating Calcein HDR data:", 
                                  style = "notification", 
                                  detail = "Computing Calcein Radiance Map",
                                  value = 0.1, {
                                    img_calcein_shiny = vector("list", length = 
                                                                 length(c_img))
                                    for (i in (1:length(c_img))) {
                                      img_calcein_shiny[[i]] = readImage(c_img[[i]])         
                                    }
                                    incProgress(0.1, detail = "Reading in images")
                                    img_calcein_shiny = processto8bit(img_calcein_shiny)
                                    exposure_calcein_shiny = vector("list", length = 
                                                                      length(c_text))
                                    for (i in (1:length(c_text))) {
                                      exposure_calcein_shiny[[i]] = log(c_text[[i]])         
                                    }
                                    incProgress(0.3, detail = "Processing images")
                                    hdr_calcein_shiny = computeHDR(img_calcein_shiny, 
                                                                   exposure_calcein_shiny,
                                                                   smoothing_lambda = 100, 
                                                                   gamma = 1.0)
                                    #Increment the bottom-level progress indicator
                                    setProgress(1.0, detail = paste("Calcein Radiance Map",
                                                                    " Computed", sep = ""))
                                  })
                     #Extract and Prepare Radiance Maps for Subsequent Computation
                     
                     #phase contrast HDR radiance map
                     pmask_radiancemap_shiny = hdr_phase_shiny[[2]][[1]]
                     pmask_thresh_shiny = pmask_radiancemap_shiny
                     pmask_thresh_shiny = non_negative_matrix(pmask_thresh_shiny)
                     #nuclei HDR radiance map
                     nmask_radiancemap_shiny = hdr_hoechst_shiny[[2]][[1]]
                     nmask_thresh_shiny = nmask_radiancemap_shiny
                     nmask_thresh_shiny = non_negative_matrix(nmask_thresh_shiny)
                     #nuclei (Inverse) HDR radiance map
                     nmask_thresh_inverse_shiny = normalize(nmask_thresh_shiny,
                                                            ft = c(max(nmask_thresh_shiny), 
                                                                   min(nmask_thresh_shiny)))
                     #calcein HDR radiance map
                     cmask_radiancemap_shiny = hdr_calcein_shiny[[2]][[1]]
                     cmask_thresh_shiny = cmask_radiancemap_shiny
                     cmask_thresh_shiny = non_negative_matrix(cmask_thresh_shiny)
                     #calcein (Inverse) HDR radiance map
                     cmask_thresh_inverse_shiny = normalize(cmask_thresh_shiny,
                                                            ft = c(max(cmask_thresh_shiny), 
                                                                   min(cmask_thresh_shiny)))
                     #Increment the top-level progress indicator
                     incProgress(0.25, detail = paste0("Completed HDR Calculatio", 
                                                       "ns. Initializing Paramet",
                                                       "ers"))
                     print("Completed HDR Calculations. Initializing Parameters.")
                     #Set Reactive Variables
                     pmask_radiancemap_sr(pmask_radiancemap_shiny)             
                     pmask_thresh_sr(pmask_thresh_shiny)
                     nmask_radiancemap_sr(nmask_radiancemap_shiny) 
                     nmask_thresh_sr(nmask_thresh_shiny)
                     nmask_thresh_inverse_sr(nmask_thresh_inverse_shiny)
                     cmask_radiancemap_sr(cmask_radiancemap_shiny)             
                     cmask_thresh_sr(cmask_thresh_shiny)
                     cmask_thresh_inverse_sr(cmask_thresh_inverse_shiny)
                     n_thresh_val_shiny = array(data = c(n_thresh_val_sr$topleft, 
                                                         n_thresh_val_sr$midleft,
                                                         n_thresh_val_sr$lowerleft, 
                                                         n_thresh_val_sr$topcenter, 
                                                         n_thresh_val_sr$midcenter, 
                                                         n_thresh_val_sr$lowercenter,
                                                         n_thresh_val_sr$topright, 
                                                         n_thresh_val_sr$midright, 
                                                         n_thresh_val_sr$lowerright), dim =
                                                  n_thresh_val_sr_dim)
                     segment_df_shiny = calculate_segments(nmask_thresh_shiny, 
                                                           n_thresh_val_shiny)
                     segment_df_sr(segment_df_shiny)
                     #compute Image Partitions/Borders
                     border_lines = array(data = 0, dim = c(
                       dim(pmask_thresh_shiny)[[1]],
                       dim(pmask_thresh_shiny)[[2]], 3))
                     #find borders of local image segments         
                     x_borders = c(unique(segment_df_shiny[["x_start"]]), 
                                   unique(segment_df_shiny[["x_end"]]))
                     y_borders = c(unique(segment_df_shiny[["y_start"]]), 
                                   unique(segment_df_shiny[["y_end"]]))
                     #remove first and last borders 
                     x_borders = x_borders[!x_borders %in% c(1, 
                                                             dim(pmask_thresh_shiny)[[1]])]
                     y_borders = y_borders[!y_borders %in% c(1,                
                                                             dim(pmask_thresh_shiny)[[2]])] 
                     for (i in (1:length(x_borders))) {
                       border_lines[x_borders[[i]],,] = 1
                     }
                     for (i in (1:length(y_borders))) {
                       border_lines[,y_borders[[i]],] = 1
                     }
                     border_lines = Image(data = border_lines, dim = c(
                       dim(border_lines)[1], 
                       dim(border_lines)[2],
                       dim(border_lines)[3]), 
                       colormode="Color")
                     borders_sr(border_lines)
                     #initialize nuclei (Hoechst) and cytoplasm (calcein) images 
                     #or load previously saved parameters
                     if (is.null(input$Load)) {
                       print("No parameter file was loaded")
                       nmask_shiny = nmask_thresh_shiny > (max(
                         nmask_thresh_shiny) * 
                           0.1) 
                       cmask_shiny = cmask_thresh_shiny > (max(
                         cmask_thresh_shiny) * 
                           0.1)
                     } else {
                       print("A parameter file was loaded")
                       n_thresh_val_shiny = n_thresh_val_shiny * 
                         max(non_negative_matrix(
                           nmask_radiancemap_sr()))
                       # use un-normalized values
                       nmask_thresh_shiny = removebackground(start_end_df = 
                                                               segment_df_sr(),
                                                             img = nmask_thresh_shiny,
                                                             ref_map = non_negative_matrix(
                                                               nmask_radiancemap_sr()),
                                                             thresh_val = n_thresh_val_shiny)
                       nmask_thresh_inverse_shiny = removebackground(
                         start_end_df = 
                           segment_df_sr(), img = 
                           nmask_thresh_inverse_shiny, 
                         ref_map = 
                           non_negative_matrix(
                             nmask_radiancemap_sr()), 
                         thresh_val = 
                           n_thresh_val_shiny)
                       nmask_thresh_sr(nmask_thresh_shiny)
                       nmask_thresh_inverse_sr(nmask_thresh_inverse_shiny)
                       nmask_shiny = nmask_thresh_shiny > 0
                       c_thresh_val_shiny = array(data = 
                                                    c(c_thresh_val_sr$topleft, 
                                                      c_thresh_val_sr$midleft,
                                                      c_thresh_val_sr$lowerleft, 
                                                      c_thresh_val_sr$topcenter, 
                                                      c_thresh_val_sr$midcenter, 
                                                      c_thresh_val_sr$lowercenter,
                                                      c_thresh_val_sr$topright, 
                                                      c_thresh_val_sr$midright, 
                                                      c_thresh_val_sr$lowerright), dim =
                                                    c_thresh_val_sr_dim)
                       c_thresh_val_shiny = c_thresh_val_shiny * 
                         max(non_negative_matrix(
                           cmask_radiancemap_sr()))
                       # use un-normalized values 
                       cmask_thresh_shiny = removebackground(start_end_df = 
                                                               segment_df_sr(),img = 
                                                               cmask_thresh_shiny,
                                                             ref_map = non_negative_matrix(
                                                               cmask_radiancemap_sr()),
                                                             thresh_val = c_thresh_val_shiny)
                       cmask_thresh_inverse_shiny = removebackground(
                         start_end_df = 
                           segment_df_sr(), img = 
                           cmask_thresh_inverse_shiny, 
                         ref_map = 
                           non_negative_matrix(
                             cmask_radiancemap_sr()), 
                         thresh_val = 
                           c_thresh_val_shiny)
                       cmask_thresh_sr(cmask_thresh_shiny)
                       cmask_thresh_inverse_sr(cmask_thresh_inverse_shiny)
                       cmask_shiny = cmask_thresh_shiny > 0
                       ctmask_sr(cmask_shiny)
                       spatstatmask = connected(as.im(cmask_shiny))
                       data = as.numeric(droplevels(spatstatmask[[1]])) #remove 
                       #factors 
                       #&
                       #levels    
                       spatstatmask = array(data = data, dim = 
                                              spatstatmask[["dim"]])
                       spatstatmask[is.na(spatstatmask)] = 0
                       connectedmask_sr(spatstatmask)
                       if (corrections() == TRUE) { 
                         connectedmask_list = vector("list", length = 
                                                       length(x_seg))
                         for (i in 1:length(x_seg)) {
                           cluster_id = spatstatmask[x_seg[i], y_seg[i]]  
                           connected = spatstatmask
                           connected[spatstatmask != cluster_id] = 0
                           connectedmask_list[[i]] = connected
                         }
                         connectedmask_list_sr(connectedmask_list)
                       }
                       compute_wholeimg_watershed()
                       compute_wholeimg_gradient_and_segment()
                       state_refresh_del(state_refresh_del()+1) 
                     }
                     phase_overlay = normalize(pmask_radiancemap_sr()) 
                     combined_img_nuc = Image(data = c(nmask_shiny, nmask_shiny,
                                                       nmask_shiny, 
                                                       (nmask_shiny + phase_overlay), 
                                                       phase_overlay, phase_overlay),
                                              dim = c(dim(nmask_shiny)[1], 
                                                      dim(nmask_shiny)[2], 3, 2), 
                                              colormode = "Color")
                     combined_img_cyto = Image(data = c(cmask_shiny, cmask_shiny, 
                                                        cmask_shiny, phase_overlay,
                                                        (cmask_shiny + phase_overlay), 
                                                        phase_overlay), dim = 
                                                 c(dim(cmask_shiny)[1], 
                                                   dim(cmask_shiny)[2], 3, 2), 
                                               colormode = "Color")
                     img_bg_nuclei_sr(combined_img_nuc)
                     img_bg_cytoplasm_sr(combined_img_cyto)
                     #Increment the top-level progress indicator
                     setProgress(1.00, detail = "Completed First Step")
                     print("Please proceed to the Second Step")
                     showNotification(ui = "Please proceed to the Second Step",
                                      duration = NULL, type = "message")
                   })
    })
    #Step 2: Optimize Parameters
    #Step 2a: Display Current Parameters to User
    output$nthresh_par <- renderText({         
      switch(input$pic_loc1,
             "top left" = {curr_value = n_thresh_val_sr$topleft},
             "top center" = {curr_value = n_thresh_val_sr$topcenter},
             "top right" = {curr_value = n_thresh_val_sr$topright},
             "mid left" = {curr_value = n_thresh_val_sr$midleft},
             "mid center" = {curr_value = n_thresh_val_sr$midcenter},
             "mid right" = {curr_value = n_thresh_val_sr$midright},
             "lower left" = {curr_value = n_thresh_val_sr$lowerleft},
             "lower center" = {curr_value = n_thresh_val_sr$lowercenter},
             "lower right" = {curr_value = n_thresh_val_sr$lowerright}  
      )
      paste("Current: ", curr_value)
    })
    output$cthresh_par <- renderText({         
      switch(input$pic_loc1,
             "top left" = {curr_value = c_thresh_val_sr$topleft},
             "top center" = {curr_value = c_thresh_val_sr$topcenter},
             "top right" = {curr_value = c_thresh_val_sr$topright},
             "mid left" = {curr_value = c_thresh_val_sr$midleft},
             "mid center" = {curr_value = c_thresh_val_sr$midcenter},
             "mid right" = {curr_value = c_thresh_val_sr$midright},
             "lower left" = {curr_value = c_thresh_val_sr$lowerleft},
             "lower center" = {curr_value = c_thresh_val_sr$lowercenter},
             "lower right" = {curr_value = c_thresh_val_sr$lowerright}  
      )
      paste("Current: ", curr_value)
    })
    #ws_pixel_rm_sr is currently disabled because there 
    #appears to be some kind of protect() error
    #output$ws_pixel_rm_out <- renderText({
    #    paste("Current: ", ws_pixel_rm_sr())
    #})    
    output$n_ntol_par <- renderText({
      switch(input$pic_loc2,
             "top left" = {curr_value = n_tol_sr$topleft},
             "top center" = {curr_value = n_tol_sr$topcenter},
             "top right" = {curr_value = n_tol_sr$topright},
             "mid left" = {curr_value = n_tol_sr$midleft},
             "mid center" = {curr_value = n_tol_sr$midcenter},
             "mid right" = {curr_value = n_tol_sr$midright},
             "lower left" = {curr_value = n_tol_sr$lowerleft},
             "lower center" = {curr_value = n_tol_sr$lowercenter},
             "lower right" = {curr_value = n_tol_sr$lowerright},  
             "border(s)" = {curr_value = b_n_tol_sr()}
      )
      paste("Current: ", curr_value)
    })
    output$n_next_par <- renderText({
      switch(input$pic_loc2,
             "top left" = {curr_value = n_ext_sr$topleft},
             "top center" = {curr_value = n_ext_sr$topcenter},
             "top right" = {curr_value = n_ext_sr$topright},
             "mid left" = {curr_value = n_ext_sr$midleft},
             "mid center" = {curr_value = n_ext_sr$midcenter},
             "mid right" = {curr_value = n_ext_sr$midright},
             "lower left" = {curr_value = n_ext_sr$lowerleft},
             "lower center" = {curr_value = n_ext_sr$lowercenter},
             "lower right" = {curr_value = n_ext_sr$lowerright},
             "border(s)" = {curr_value = b_n_ext_sr()} 
      )
      paste("Current: ", curr_value)
    })
    output$pninv_p_par <- renderText({
      switch(input$pic_loc3,
             "top left" = {curr_value = pninv_norm_thresh_p_sr$topleft},
             "top center" = {curr_value = 
               pninv_norm_thresh_p_sr$topcenter},
             "top right" = {curr_value = 
               pninv_norm_thresh_p_sr$topright},
             "mid left" = {curr_value = pninv_norm_thresh_p_sr$midleft},
             "mid center" = {curr_value = 
               pninv_norm_thresh_p_sr$midcenter},
             "mid right" = {curr_value = 
               pninv_norm_thresh_p_sr$midright},
             "lower left" = {curr_value = 
               pninv_norm_thresh_p_sr$lowerleft},
             "lower center" = {curr_value = 
               pninv_norm_thresh_p_sr$lowercenter},
             "lower right" = {curr_value = 
               pninv_norm_thresh_p_sr$lowerright},
             "border(s)" = {curr_value = b_pninv_norm_thresh_p_sr()} 
      )
      paste("Current: ", curr_value)
    })
    output$pninv_n_par <- renderText({
      switch(input$pic_loc3,
             "top left" = {curr_value = pninv_norm_thresh_n_sr$topleft},
             "top center" = {curr_value = 
               pninv_norm_thresh_n_sr$topcenter},
             "top right" = {curr_value = 
               pninv_norm_thresh_n_sr$topright},
             "mid left" = {curr_value = pninv_norm_thresh_n_sr$midleft},
             "mid center" = {curr_value = 
               pninv_norm_thresh_n_sr$midcenter},
             "mid right" = {curr_value = 
               pninv_norm_thresh_n_sr$midright},
             "lower left" = {curr_value = 
               pninv_norm_thresh_n_sr$lowerleft},
             "lower center" = {curr_value = 
               pninv_norm_thresh_n_sr$lowercenter},
             "lower right" = {curr_value = 
               pninv_norm_thresh_n_sr$lowerright},
             "border(s)" = {curr_value = b_pninv_norm_thresh_n_sr()} 
      )
      paste("Current: ", curr_value)
    })
    output$pcinv_p_par <- renderText({
      switch(input$pic_loc3,
             "top left" = {curr_value = p_norm_thresh_val_sr$topleft},
             "top center" = {curr_value = 
               p_norm_thresh_val_sr$topcenter},
             "top right" = {curr_value = 
               p_norm_thresh_val_sr$topright},
             "mid left" = {curr_value = p_norm_thresh_val_sr$midleft},
             "mid center" = {curr_value = 
               p_norm_thresh_val_sr$midcenter},
             "mid right" = {curr_value = 
               p_norm_thresh_val_sr$midright},
             "lower left" = {curr_value = 
               p_norm_thresh_val_sr$lowerleft},
             "lower center" = {curr_value = 
               p_norm_thresh_val_sr$lowercenter},
             "lower right" = {curr_value = 
               p_norm_thresh_val_sr$lowerright},
             "border(s)" = {curr_value = b_p_norm_thresh_val_sr()} 
      )
      paste("Current: ", curr_value)
    })
    output$pcinv_c_par <- renderText({
      switch(input$pic_loc3,
             "top left" = {curr_value = c_inv_norm_thresh_sr$topleft},
             "top center" = {curr_value = 
               c_inv_norm_thresh_sr$topcenter},
             "top right" = {curr_value = 
               c_inv_norm_thresh_sr$topright},
             "mid left" = {curr_value = c_inv_norm_thresh_sr$midleft},
             "mid center" = {curr_value = 
               c_inv_norm_thresh_sr$midcenter},
             "mid right" = {curr_value = 
               c_inv_norm_thresh_sr$midright},
             "lower left" = {curr_value = 
               c_inv_norm_thresh_sr$lowerleft},
             "lower center" = {curr_value = 
               c_inv_norm_thresh_sr$lowercenter},
             "lower right" = {curr_value = 
               c_inv_norm_thresh_sr$lowerright},
             "border(s)" = {curr_value = b_c_inv_norm_thresh_sr()} 
      )
      paste("Current: ", curr_value)
    })
    calculate_which_region <- function(x_point, y_point, parameter_array,
                                       cytomask, ms_cmask_thresh_inverse)  
    {
      #calculates where a point lies in which particular (local) region  
      #of an image 
      
      #find cell cytoplasm that overlap or touch the borders of 
      #partitioned images followed by their accompanying nuclei
      bdrs = compute_nuclei_or_cytoplasm_on_img_border_partitions(img = 
                                                                    ms_cmask_thresh_inverse, mask = cytomask, start_end_df =
                                                                    parameter_array)
      bdrs = bdrs > 0 #make it binary – 1 or 0
      #match based on region 
      matchrow = 11
      if ((bdrs[x_point, y_point] == TRUE)) { #match based on whether 
        #it is on the image 
        #partition borders
        #more than zero = TRUE 
        matchrow = 10 #the x,y point is on an image partition border 
      } else { #the x,y point is NOT on an image partition border
        for (i in 1:nrow(parameter_array)) { 
          #match based on top left, top center, etc. regions
          if ((x_point >= parameter_array[i,1]) & 
              (x_point <= parameter_array[i,3]) & 
              (y_point >= parameter_array[i,2]) & 
              (y_point <= parameter_array[i,4])) {
            matchrow = i
            break 
          }      
        }
      }
      switch(matchrow,
             "1" = {curr_value = "top left"},
             "2" = {curr_value = "mid left"},
             "3" = {curr_value = "lower left"},
             "4" = {curr_value = "top center"},
             "5" = {curr_value = "mid center"},
             "6" = {curr_value = "lower center"},
             "7" = {curr_value = "top right"},
             "8" = {curr_value = "mid right"},
             "9" = {curr_value = "lower right"},
             "10" = {curr_value = "border(s)"},
             "11" = {curr_value = "error"},
      )
      return(curr_value)      
    }    
    lookup_watershed_parameters <- function() {
      #look up watershed parameters for local custom annotation 
      req((state_ifaddisTRUE_ifmodifyisFALSE() == FALSE | 
             state_ifaddisTRUE_ifmodifyisFALSE() == TRUE),
          segment_df_sr(), ctmask_sr(), cmask_thresh_inverse_sr(),
          curr_seg_pointx_s(), curr_seg_pointy_s(),
          seg_pointx_s(), seg_pointy_s()
      )
      #set up variables for computation
      segment_df_s = segment_df_sr()
      cytomask = ctmask_sr()
      ms_cmask_thresh_inverse = cmask_thresh_inverse_sr()
      curr_seg_pointx_s = curr_seg_pointx_s()
      curr_seg_pointy_s = curr_seg_pointy_s()
      seg_pointx_s = seg_pointx_s()
      seg_pointy_s = seg_pointy_s()
      corr_ntol_s = corr_ntol_sr()
      corr_next_s = corr_next_sr()
      if ((state_ifaddisTRUE_ifmodifyisFALSE() == FALSE) |
          ((state_ifaddisTRUE_ifmodifyisFALSE() == TRUE)  & 
           (state_ifnucleipressedisTRUE() == TRUE))) {
        #modifying existing custom annotation or adding annotation
        #after update button has been pressed so look up previous
        #settings from local custom annotation reactive variables 
        condition_xy = which((seg_pointx_s == curr_seg_pointx_s) &   
                               (seg_pointy_s == curr_seg_pointy_s))
        corr_ntol = corr_ntol_s[condition_xy] 
        corr_next = corr_next_s[condition_xy] 
      } else if ((state_ifaddisTRUE_ifmodifyisFALSE() == TRUE) &
                 (state_ifnucleipressedisTRUE() == FALSE)) {
        #adding new custom annotation so look up previous settings
        #from local image (e.g. topleft, topcenter, etc) reactive 
        #variables 
        region = calculate_which_region(x_point = curr_seg_pointx_s, 
                                        y_point = curr_seg_pointy_s, 
                                        parameter_array =  
                                          segment_df_s,
                                        cytomask = cytomask, 
                                        ms_cmask_thresh_inverse = 
                                          ms_cmask_thresh_inverse)
        switch(region,
               "top left" = {corr_ntol = n_tol_sr$topleft
               corr_next = n_ext_sr$topleft
               },
               "mid left" = {corr_ntol = n_tol_sr$midleft
               corr_next = n_ext_sr$midleft
               },
               "lower left" = {corr_ntol = n_tol_sr$lowerleft
               corr_next = n_ext_sr$lowerleft
               },
               "top center" = {corr_ntol = n_tol_sr$topcenter
               corr_next = n_ext_sr$topcenter
               },
               "mid center" = {corr_ntol = n_tol_sr$midcenter
               corr_next = n_ext_sr$midcenter
               },
               "lower center" = {corr_ntol = n_tol_sr$lowercenter
               corr_next = n_ext_sr$lowercenter
               },
               "top right" = {corr_ntol = n_tol_sr$topright
               corr_next = n_ext_sr$topright
               },
               "mid right" = {corr_ntol = n_tol_sr$midright
               corr_next = n_ext_sr$midright
               },
               "lower right" = {corr_ntol = n_tol_sr$lowerright
               corr_next = n_ext_sr$lowerright
               },
               "border(s)" = {corr_ntol = b_n_tol_sr()
               corr_next = b_n_ext_sr()                                  
               },
               "error" = {corr_ntol = "error"
               corr_next = "error"
               }
        )
      }
      #create a list to return for use with renderText outputs
      curr_value <- vector("list", length = 2)
      curr_value[1] = corr_ntol
      curr_value[2] = corr_next
      corr_curr_watershed_param_sr(curr_value)
    }
    lookup_gradient_parameters <- function() {
      #look up gradient parameters for local custom annotation 
      req((state_ifaddisTRUE_ifmodifyisFALSE() == FALSE | 
             state_ifaddisTRUE_ifmodifyisFALSE() == TRUE),
          segment_df_sr(), ctmask_sr(), cmask_thresh_inverse_sr(),
          curr_seg_pointx_s(), curr_seg_pointy_s(),
          seg_pointx_s(), seg_pointy_s()
      )
      #set up variables for computation
      segment_df_s = segment_df_sr()
      cytomask = ctmask_sr()
      ms_cmask_thresh_inverse = cmask_thresh_inverse_sr()
      curr_seg_pointx_s = curr_seg_pointx_s()
      curr_seg_pointy_s = curr_seg_pointy_s()
      seg_pointx_s = seg_pointx_s()
      seg_pointy_s = seg_pointy_s()
      corr_pninv_normthresh_p_s = corr_pninv_normthresh_p_sr()
      corr_pninv_normthresh_n_s = corr_pninv_normthresh_n_sr()
      corr_p_normthresh_val_s = corr_p_normthresh_val_sr()
      corr_cinv_normthresh_val_s = corr_cinv_normthresh_val_sr()
      if ((state_ifaddisTRUE_ifmodifyisFALSE() == FALSE) |
          ((state_ifaddisTRUE_ifmodifyisFALSE() == TRUE)  & 
           (state_ifgradpressedisTRUE() == TRUE))) {
        #modifying existing custom annotation or adding annotation
        #after update button has been pressed so look up previous
        #settings from local custom annotation reactive variables 
        condition_xy = which((seg_pointx_s == curr_seg_pointx_s) &   
                               (seg_pointy_s == curr_seg_pointy_s))
        pninv_normthresh_p = corr_pninv_normthresh_p_s[condition_xy] 
        pninv_normthresh_n = corr_pninv_normthresh_n_s[condition_xy] 
        p_normthresh_val = corr_p_normthresh_val_s[condition_xy] 
        cinv_normthresh_val = 
          corr_cinv_normthresh_val_s[condition_xy]
      } else if ((state_ifaddisTRUE_ifmodifyisFALSE() == TRUE) &
                 (state_ifgradpressedisTRUE() == FALSE)) {
        #adding new custom annotation so look up previous settings
        #from local image (e.g. topleft, topcenter, etc) reactive 
        #variables
        region = calculate_which_region(x_point = curr_seg_pointx_s, 
                                        y_point = curr_seg_pointy_s, 
                                        parameter_array =  
                                          segment_df_s,
                                        cytomask = cytomask, 
                                        ms_cmask_thresh_inverse = 
                                          ms_cmask_thresh_inverse)
        switch(region,
               "top left" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$topleft
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$topleft
               p_normthresh_val = 
                 p_norm_thresh_val_sr$topleft  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$topleft 
               },
               "mid left" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$midleft
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$midleft
               p_normthresh_val = 
                 p_norm_thresh_val_sr$midleft  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$midleft 
               },
               "lower left" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$lowerleft
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$lowerleft
               p_normthresh_val = 
                 p_norm_thresh_val_sr$lowerleft  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$lowerleft 
               },
               "top center" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$topcenter
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$topcenter
               p_normthresh_val = 
                 p_norm_thresh_val_sr$topcenter                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$topcenter
               },
               "mid center" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$midcenter
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$midcenter
               p_normthresh_val = 
                 p_norm_thresh_val_sr$midcenter                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$midcenter
               },
               "lower center" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$lowercenter
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$lowercenter
               p_normthresh_val = 
                 p_norm_thresh_val_sr$lowercenter                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$lowercenter
               },
               "top right" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$topright
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$topright
               p_normthresh_val = 
                 p_norm_thresh_val_sr$topright                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$topright
               },
               "mid right" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$midright
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$midright
               p_normthresh_val = 
                 p_norm_thresh_val_sr$midright                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$midright
               },
               "lower right" = {pninv_normthresh_p = 
                 pninv_norm_thresh_p_sr$lowerright
               pninv_normthresh_n = 
                 pninv_norm_thresh_n_sr$lowerright
               p_normthresh_val = 
                 p_norm_thresh_val_sr$lowerright                                  
               cinv_normthresh_val = 
                 c_inv_norm_thresh_sr$lowerright
               },
               "border(s)" = {pninv_normthresh_p = 
                 b_pninv_norm_thresh_p_sr()
               pninv_normthresh_n = 
                 b_pninv_norm_thresh_n_sr()
               p_normthresh_val = 
                 b_p_norm_thresh_val_sr()
               cinv_normthresh_val = 
                 b_c_inv_norm_thresh_sr()
               },
               "error" = {pninv_normthresh_p = "error"
               pninv_normthresh_n = "error"
               p_normthresh_val = "error"  
               cinv_normthresh_val = "error" 
               }
        )
      }
      #create a list to return for use with renderText outputs
      curr_value <- vector("list", length = 4)
      curr_value[1] = pninv_normthresh_p 
      curr_value[2] = pninv_normthresh_n 
      curr_value[3] = p_normthresh_val 
      curr_value[4] = cinv_normthresh_val 
      corr_curr_gradient_param_sr(curr_value)
    }
    output$n_ntol_lc_par <- renderText({
      req(corr_curr_watershed_param_sr())
      paste("Current:", corr_curr_watershed_param_sr()[1])
    })
    output$n_next_lc_par <- renderText({
      req(corr_curr_watershed_param_sr())
      paste("Current:", corr_curr_watershed_param_sr()[2])
    })
    output$pninv_p_lc_par <- renderText({
      req(corr_curr_gradient_param_sr())
      paste("Current:", corr_curr_gradient_param_sr()[1])
    })
    output$pninv_n_lc_par <- renderText({
      req(corr_curr_gradient_param_sr())
      paste("Current:", corr_curr_gradient_param_sr()[2])
    })
    output$pcinv_p_lc_par <- renderText({
      req(corr_curr_gradient_param_sr())
      paste("Current:", corr_curr_gradient_param_sr()[3])
    })
    output$pcinv_c_lc_par <- renderText({
      req(corr_curr_gradient_param_sr())
      paste("Current:", corr_curr_gradient_param_sr()[4])
    })
    #Step 2b: Compute Parameters and Display Segmentation to User
    img_bg_nuclei <- observeEvent(input$thresh_button, {
      req(nmask_radiancemap_sr(), nmask_thresh_sr(), 
          nmask_thresh_inverse_sr(), segment_df_sr())
      #remove background, make nuclei mask, and relevant heatmaps
      switch(input$pic_loc1,
             "top left" = {n_thresh_val_sr$topleft = input$nthresh},
             "top center" = {n_thresh_val_sr$topcenter = input$nthresh},
             "top right" = {n_thresh_val_sr$topright = input$nthresh},
             "mid left" = {n_thresh_val_sr$midleft = input$nthresh},
             "mid center" = {n_thresh_val_sr$midcenter = input$nthresh},
             "mid right" = {n_thresh_val_sr$midright = input$nthresh},
             "lower left" = {n_thresh_val_sr$lowerleft = input$nthresh},
             "lower center" = {n_thresh_val_sr$lowercenter = 
               input$nthresh},
             "lower right" = {n_thresh_val_sr$lowerright = input$nthresh}  
      )
      n_thresh_val_shiny = array(data = c(n_thresh_val_sr$topleft, 
                                          n_thresh_val_sr$midleft,
                                          n_thresh_val_sr$lowerleft, 
                                          n_thresh_val_sr$topcenter, 
                                          n_thresh_val_sr$midcenter, 
                                          n_thresh_val_sr$lowercenter,
                                          n_thresh_val_sr$topright, 
                                          n_thresh_val_sr$midright, 
                                          n_thresh_val_sr$lowerright), dim =
                                   n_thresh_val_sr_dim)
      nmask_thresh_shiny = nmask_radiancemap_sr()
      nmask_thresh_shiny = non_negative_matrix(nmask_thresh_shiny)
      n_thresh_val_shiny = n_thresh_val_shiny * 
        max(nmask_thresh_shiny)
      # use un-normalized values
      nmask_thresh_inverse_shiny = normalize(nmask_thresh_shiny,
                                             ft = c(max(nmask_thresh_shiny), 
                                                    min(nmask_thresh_shiny)))
      nmask_thresh_shiny = removebackground(start_end_df = 
                                              segment_df_sr(),
                                            img = nmask_thresh_shiny,
                                            ref_map = 
                                              non_negative_matrix(
                                                nmask_radiancemap_sr()),
                                            thresh_val = 
                                              n_thresh_val_shiny)
      nmask_thresh_inverse_shiny = removebackground(start_end_df = 
                                                      segment_df_sr(), img = 
                                                      nmask_thresh_inverse_shiny, 
                                                    ref_map = non_negative_matrix(
                                                      nmask_radiancemap_sr()), 
                                                    thresh_val = n_thresh_val_shiny)
      nmask_thresh_sr(nmask_thresh_shiny)
      nmask_thresh_inverse_sr(nmask_thresh_inverse_shiny)
      nmask_shiny = nmask_thresh_shiny > 0
      #nmask_sr(nmask_shiny) if want to save nuclei mask
      phase_overlay = normalize(pmask_radiancemap_sr()) 
      combined_img = Image(data = c(nmask_shiny, nmask_shiny, 
                                    nmask_shiny, phase_overlay, phase_overlay, 
                                    (nmask_shiny + phase_overlay)), dim = 
                             c(dim(nmask_shiny)[1], dim(nmask_shiny)[2], 
                               3, 2), colormode = "Color")
      img_bg_nuclei_sr(combined_img)
    })
    output$nuclei_bg <- renderDisplay({
      req(img_bg_nuclei_sr())
      if (input$thresh_bdr == "on") {
        display(img_bg_nuclei_sr() + Image(data = c(borders_sr(),
                                                    borders_sr()), 
                                           dim = 
                                             c(dim(borders_sr())[1], 
                                               dim(borders_sr())[2], 
                                               dim(borders_sr())[3], 2),
                                           colormode = "Color"))
      } else if (input$thresh_bdr == "off") {
        display(img_bg_nuclei_sr())
      } 
    })
    #Define Cytoplasm Background Threshold
    img_bg_cytoplasm <- observeEvent(input$thresh_button, {
      req(pmask_radiancemap_sr(), cmask_radiancemap_sr(),
          cmask_thresh_sr(), cmask_thresh_inverse_sr(),
          segment_df_sr())
      #remove background, make cytoplasm mask, and relevant heatmaps
      switch(input$pic_loc1,
             "top left" = {c_thresh_val_sr$topleft = input$cthresh},
             "top center" = {c_thresh_val_sr$topcenter = input$cthresh},
             "top right" = {c_thresh_val_sr$topright = input$cthresh},
             "mid left" = {c_thresh_val_sr$midleft = input$cthresh},
             "mid center" = {c_thresh_val_sr$midcenter = input$cthresh},
             "mid right" = {c_thresh_val_sr$midright = input$cthresh},
             "lower left" = {c_thresh_val_sr$lowerleft = input$cthresh},
             "lower center" = {c_thresh_val_sr$lowercenter = 
               input$cthresh},
             "lower right" = {c_thresh_val_sr$lowerright = input$cthresh}  
      )
      c_thresh_val_shiny = array(data = c(c_thresh_val_sr$topleft, 
                                          c_thresh_val_sr$midleft,
                                          c_thresh_val_sr$lowerleft, 
                                          c_thresh_val_sr$topcenter, 
                                          c_thresh_val_sr$midcenter, 
                                          c_thresh_val_sr$lowercenter,
                                          c_thresh_val_sr$topright, 
                                          c_thresh_val_sr$midright, 
                                          c_thresh_val_sr$lowerright), dim =
                                   c_thresh_val_sr_dim)
      cmask_thresh_shiny = cmask_radiancemap_sr()
      cmask_thresh_shiny = non_negative_matrix(cmask_thresh_shiny)
      c_thresh_val_shiny = c_thresh_val_shiny * 
        max(cmask_thresh_shiny)
      # use un-normalized values 
      cmask_thresh_inverse_shiny = normalize(cmask_thresh_shiny,
                                             ft = c(max(cmask_thresh_shiny), 
                                                    min(cmask_thresh_shiny)))
      cmask_thresh_shiny = removebackground(start_end_df = 
                                              segment_df_sr(),img = cmask_thresh_shiny,
                                            ref_map = non_negative_matrix(
                                              cmask_radiancemap_sr()),
                                            thresh_val = c_thresh_val_shiny)
      cmask_thresh_inverse_shiny = removebackground(start_end_df = 
                                                      segment_df_sr(), img = 
                                                      cmask_thresh_inverse_shiny, 
                                                    ref_map = non_negative_matrix(
                                                      cmask_radiancemap_sr()), 
                                                    thresh_val = c_thresh_val_shiny)
      cmask_thresh_sr(cmask_thresh_shiny)
      cmask_thresh_inverse_sr(cmask_thresh_inverse_shiny)
      cmask_shiny = cmask_thresh_shiny > 0
      ctmask_sr(cmask_shiny)
      phase_overlay = normalize(pmask_radiancemap_sr()) 
      combined_img = Image(data = c(cmask_shiny, cmask_shiny, 
                                    cmask_shiny, phase_overlay,
                                    (cmask_shiny + phase_overlay), 
                                    phase_overlay), dim = c(dim(cmask_shiny)[1], 
                                                            dim(cmask_shiny)[2], 3, 2), 
                           colormode = "Color")
      img_bg_cytoplasm_sr(combined_img)
      spatstatmask = connected(as.im(cmask_shiny))
      data = as.numeric(droplevels(spatstatmask[[1]])) #remove 
      #factors & 
      #levels 
      spatstatmask = array(data = data, dim = spatstatmask[["dim"]])
      spatstatmask[is.na(spatstatmask)] = 0
      connectedmask_sr(spatstatmask)
      if (corrections() == TRUE) { 
        connectedmask_list = vector("list", length = 
                                      length(seg_pointx_s()))
        for (i in 1:length(seg_pointx_s())) {
          cluster_id = spatstatmask[seg_pointx_s()[i], 
                                    seg_pointy_s()[i]]  
          connected = spatstatmask
          connected[spatstatmask != cluster_id] = 0
          connectedmask_list[[i]] = connected
        }
        connectedmask_list_sr(connectedmask_list)
      }
    })
    output$cytoplasm_bg <- renderDisplay({
      req(img_bg_cytoplasm_sr())
      if (input$thresh_bdr == "on") {
        display(img_bg_cytoplasm_sr() + Image(data = c(borders_sr(),
                                                       borders_sr()), dim = 
                                                c(dim(borders_sr())[1], 
                                                  dim(borders_sr())[2], 
                                                  dim(borders_sr())[3], 
                                                  2), colormode = 
                                                "Color"))
      } else if (input$thresh_bdr == "off") {
        display(img_bg_cytoplasm_sr())
      } 
    })
    compute_wholeimg_watershed <- function() { #Compute watershed results 
      #for whole image
      #Recompute most up-to-date values for watershed
      ms_n_tol = array(data = c(n_tol_sr$topleft, n_tol_sr$midleft,
                                n_tol_sr$lowerleft, 
                                n_tol_sr$topcenter,
                                n_tol_sr$midcenter, 
                                n_tol_sr$lowercenter,
                                n_tol_sr$topright, n_tol_sr$midright,
                                n_tol_sr$lowerright), dim =
                         n_tol_sr_dim)
      ms_n_ext = array(data = c(n_ext_sr$topleft, n_ext_sr$midleft,
                                n_ext_sr$lowerleft, 
                                n_ext_sr$topcenter,
                                n_ext_sr$midcenter, 
                                n_ext_sr$lowercenter,
                                n_ext_sr$topright, n_ext_sr$midright,
                                n_ext_sr$lowerright), dim =
                         n_ext_sr_dim)
      #Recompute watershed result for whole image
      ms_nuc_s = multi_segment_nuclei_shiny(ms_nmask_thresh = 
                                              nmask_thresh_sr(), 
                                            ms_n_tol = ms_n_tol, 
                                            ms_n_ext = ms_n_ext,
                                            bdrs_n_tol = b_n_tol_sr(),
                                            bdrs_n_ext = b_n_ext_sr(),
                                            ms_cmask_thresh_inverse = 
                                              cmask_thresh_inverse_sr(), 
                                            cytomask = ctmask_sr(),
                                            start_end_df = 
                                              segment_df_sr(),   
                                            corrections = 
                                              corrections(),
                                            seg_pointx_s = 
                                              seg_pointx_s(), 
                                            seg_pointy_s = 
                                              seg_pointy_s(),
                                            corr_n_tol_s = 
                                              corr_ntol_sr(),
                                            corr_n_ext_s = 
                                              corr_next_sr())
      nuc_watershed_sr(ms_nuc_s)
      col_ms_nuc_s = colorLabels(ms_nuc_s[[1]])
      col_nuc_watershed_sr(col_ms_nuc_s)
      col_nmask_overlay = col_ms_nuc_s * 0.35 + toRGB(normalize(
        pmask_radiancemap_sr()))
      combined_img = Image(data = c(col_ms_nuc_s, col_nmask_overlay),
                           dim = c(dim(col_ms_nuc_s)[1], 
                                   dim(col_ms_nuc_s)[2], dim(col_ms_nuc_s)[3],
                                   2), colormode="Color")
      img_nuclei_ws_sr(combined_img)
    }
    update_localcluster_watershed <- function() {
      #Display computed watershed results only for custom annotation
      #area
      spatstatmask = connectedmask_sr()
      cluster_id = spatstatmask[curr_seg_pointx_s(), 
                                curr_seg_pointy_s()]
      cell_clusters_segpoint_xy = ctmask_sr()
      cell_clusters_segpoint_xy[spatstatmask != cluster_id ] = 0
      kern = makeBrush(3, shape='box') #spatstat library::connected() 
      #functions sometimes misses out
      #some edges. Using a dilated 
      #cell clusters should account
      #for these discrepancies
      cell_clusters_segpoint_xy = dilate(cell_clusters_segpoint_xy,
                                         kern)
      col_ms_nuc_s = col_nuc_watershed_sr()
      col_ms_nuc_s[!(cell_clusters_segpoint_xy)] = 0
      col_nmask_overlay = col_ms_nuc_s * 0.35 + toRGB(normalize(
        pmask_radiancemap_sr()))
      combined_img = Image(data = c(col_ms_nuc_s, col_nmask_overlay),
                           dim = c(dim(col_ms_nuc_s)[1], 
                                   dim(col_ms_nuc_s)[2], dim(col_ms_nuc_s)[3],
                                   2), colormode="Color")
      corr_nuc_watershed_sr(combined_img)
    }
    #Define Nuclei Watershed-Based Segementation
    img_nuclei_ws <- observeEvent(input$nuclei_button, {
      req(pmask_radiancemap_sr(), nmask_thresh_sr(), 
          cmask_thresh_inverse_sr(), ctmask_sr(), segment_df_sr())
      #compute nuclei watershed nuclei
      switch(input$pic_loc2,
             "top left" = {n_tol_sr$topleft = input$n_ntol},
             "top center" = {n_tol_sr$topcenter = input$n_ntol},
             "top right" = {n_tol_sr$topright = input$n_ntol},
             "mid left" = {n_tol_sr$midleft = input$n_ntol},
             "mid center" = {n_tol_sr$midcenter = input$n_ntol},
             "mid right" = {n_tol_sr$midright = input$n_ntol},
             "lower left" = {n_tol_sr$lowerleft = input$n_ntol},
             "lower center" = {n_tol_sr$lowercenter = input$n_ntol},
             "lower right" = {n_tol_sr$lowerright = input$n_ntol},
             "border(s)" = {b_n_tol_sr(input$n_ntol)}   
      )
      switch(input$pic_loc2,
             "top left" = {n_ext_sr$topleft = input$n_next},
             "top center" = {n_ext_sr$topcenter = input$n_next},
             "top right" = {n_ext_sr$topright = input$n_next},
             "mid left" = {n_ext_sr$midleft = input$n_next},
             "mid center" = {n_ext_sr$midcenter = input$n_next},
             "mid right" = {n_ext_sr$midright = input$n_next},
             "lower left" = {n_ext_sr$lowerleft = input$n_next},
             "lower center" = {n_ext_sr$lowercenter = input$n_next},
             "lower right" = {n_ext_sr$lowerright = input$n_next},  
             "border(s)" = {b_n_ext_sr(input$n_next)}
      )
      #ws_pixel_rm_sr is currently disabled because there 
      #appears to be some kind of protect() error
      #if (input$ws_pixel_rm_in == "") {
      #    ws_pixel_rm_sr(1)
      #    print(paste("No value entered for ws_pixel_rm_sr. ",
      #                " ws_pixel_rm_sr updated to: 1", sep = ""))
      #    showNotification(ui = paste("No value entered for ",  
      #                               "ws_pixel_rm_sr. ws_pixel_rm_sr",  
      #                               " updated to: 1", sep = ""), 
      #                     duration = 10, type = "warning")
      #} else {
      #    ws_pixel_rm_sr(as.numeric(input$ws_pixel_rm_in))
      #    print(paste("ws_pixel_rm_sr updated to: ", ws_pixel_rm_sr(), 
      #          sep = ""))
      #}
      compute_wholeimg_watershed()
    })
    output$nuclei_ws <- renderDisplay({
      req(img_nuclei_ws_sr())
      if (input$n_wshed_bdr == "on") {
        display(img_nuclei_ws_sr() + Image(data = c(borders_sr(), 
                                                    borders_sr()), dim = c(
                                                      dim(borders_sr())[1],
                                                      dim(borders_sr())[2],
                                                      dim(borders_sr())[3],
                                                      2
                                                    ),
                                           colormode="Color")
        ) 
      } else if (input$n_wshed_bdr == "off") {
        display(img_nuclei_ws_sr())
      } 
    })
    compute_wholeimg_gradient_and_segment <- function() { 
      #Compute gradient results for whole image as well as 
      #segmentation results
      
      #Recompute most up-to-date values for gradient
      ms_pninv_norm_thresh_p = array(data = c(
        pninv_norm_thresh_p_sr$topleft, 
        pninv_norm_thresh_p_sr$midleft,
        pninv_norm_thresh_p_sr$lowerleft, 
        pninv_norm_thresh_p_sr$topcenter,
        pninv_norm_thresh_p_sr$midcenter, 
        pninv_norm_thresh_p_sr$lowercenter,
        pninv_norm_thresh_p_sr$topright,
        pninv_norm_thresh_p_sr$midright,
        pninv_norm_thresh_p_sr$lowerright), 
        dim = pninv_norm_thresh_p_sr_dim)
      ms_pninv_norm_thresh_n = array(data = c(
        pninv_norm_thresh_n_sr$topleft, 
        pninv_norm_thresh_n_sr$midleft,
        pninv_norm_thresh_n_sr$lowerleft, 
        pninv_norm_thresh_n_sr$topcenter,
        pninv_norm_thresh_n_sr$midcenter, 
        pninv_norm_thresh_n_sr$lowercenter,
        pninv_norm_thresh_n_sr$topright,
        pninv_norm_thresh_n_sr$midright,
        pninv_norm_thresh_n_sr$lowerright), 
        dim = pninv_norm_thresh_n_sr_dim)
      ms_p_norm_thresh_val = array(data = c(
        p_norm_thresh_val_sr$topleft, 
        p_norm_thresh_val_sr$midleft,
        p_norm_thresh_val_sr$lowerleft, 
        p_norm_thresh_val_sr$topcenter,
        p_norm_thresh_val_sr$midcenter, 
        p_norm_thresh_val_sr$lowercenter,
        p_norm_thresh_val_sr$topright,
        p_norm_thresh_val_sr$midright,
        p_norm_thresh_val_sr$lowerright), 
        dim = p_norm_thresh_val_sr_dim)
      ms_c_inv_norm_thresh = array(data = c(
        c_inv_norm_thresh_sr$topleft, 
        c_inv_norm_thresh_sr$midleft,
        c_inv_norm_thresh_sr$lowerleft, 
        c_inv_norm_thresh_sr$topcenter,
        c_inv_norm_thresh_sr$midcenter, 
        c_inv_norm_thresh_sr$lowercenter,
        c_inv_norm_thresh_sr$topright,
        c_inv_norm_thresh_sr$midright,
        c_inv_norm_thresh_sr$lowerright), 
        dim = c_inv_norm_thresh_sr_dim)
      #Recompute gradient result for whole image
      ms_grad_s = multi_segment_gradient_shiny(
        ms_nmask_thresh = nmask_thresh_sr(), 
        ms_nmask_thresh_inverse = nmask_thresh_inverse_sr(), 
        ms_pninv_norm_thresh_p = ms_pninv_norm_thresh_p, 
        ms_pninv_norm_thresh_n = ms_pninv_norm_thresh_n,  
        bdrs_pninv_norm_thresh_p = 
          b_pninv_norm_thresh_p_sr(),
        bdrs_pninv_norm_thresh_n = 
          b_pninv_norm_thresh_n_sr(), 
        ms_pmask_thresh = pmask_thresh_sr(),
        ms_p_norm_thresh_val = ms_p_norm_thresh_val,   
        ms_cmask_thresh_inverse = cmask_thresh_inverse_sr(),
        bdrs_p_norm_thresh_val = b_p_norm_thresh_val_sr(), 
        bdrs_c_inv_norm_thresh = b_c_inv_norm_thresh_sr(), 
        ms_c_inv_norm_thresh = ms_c_inv_norm_thresh,
        cytomask = ctmask_sr(),
        start_end_df = segment_df_sr(),
        corrections = corrections(),
        seg_pointx_s = seg_pointx_s(),
        seg_pointy_s = seg_pointy_s(),
        corr_pninv_norm_thresh_p_s = 
          corr_pninv_normthresh_p_sr(), 
        corr_pninv_norm_thresh_n_s =
          corr_pninv_normthresh_n_sr(), 
        corr_p_norm_thresh_val_s = 
          corr_p_normthresh_val_sr(),
        corr_c_inv_norm_thresh_val_s = 
          corr_cinv_normthresh_val_sr())
      gradient_sr(ms_grad_s)
      norm_gradient_sr(normalize(ms_grad_s))
      #Compute cell segmentation
      segment_mask = propagate(gradient_sr(), seeds = 
                                 nuc_watershed_sr()[[2]], mask = ctmask_sr(),
                               lambda = 1e-10)
      col_segment_mask = colorLabels(segment_mask)
      col_segment_mask_overlay = col_segment_mask * 0.35 + 
        toRGB(normalize(
          pmask_radiancemap_sr())) 
      segmentedcells_sr(segment_mask)
      col_segmentedcells_sr(col_segment_mask)
      segmentedcells_overlay_sr(col_segment_mask_overlay)
    }
    update_localcluster_gradient_and_segment <- function() {
      #Display computed gradient and segmented results only for custom 
      #annotation area
      spatstatmask = connectedmask_sr()
      cluster_id = spatstatmask[curr_seg_pointx_s(), 
                                curr_seg_pointy_s()]
      cell_clusters_segpoint_xy = ctmask_sr()
      cell_clusters_segpoint_xy[spatstatmask != cluster_id ] = 0
      #kern = makeBrush(3, shape='box') #spatstat library::connected() 
      #                                 #functions sometimes misses out
      #                                 #some edges. Using a dilated 
      #                                 #cell clusters should account
      #                                 #for these discrepancies
      #                                 #This is not critical for 
      #                                 #visualizing gradients compared 
      #                                 #to watershed and this has been
      #                                 #left 'commented out'
      #cell_clusters_segpoint_xy = dilate(cell_clusters_segpoint_xy,
      #                                   kern)
      col_ms_segment_s = col_segmentedcells_sr()
      col_ms_segment_s[!(cell_clusters_segpoint_xy)] = 0
      ms_segment_overlay_s = col_ms_segment_s * 0.35 + toRGB(normalize(
        pmask_radiancemap_sr()))
      hm_phase_rm = hm_phase_rm_sr()
      hm_nuclei_rm_inverse = hm_nuclei_rm_inverse_sr()
      hm_calcein_rm_inverse = hm_calcein_rm_inverse_sr()
      hm_phase_rm[!(cell_clusters_segpoint_xy)] = 0
      hm_nuclei_rm_inverse[!(cell_clusters_segpoint_xy)] = 0
      hm_calcein_rm_inverse[!(cell_clusters_segpoint_xy)] = 0
      combined_img = Image(data = c(ms_segment_overlay_s, 
                                    col_ms_segment_s, 
                                    toRGB(normalize(
                                      pmask_radiancemap_sr())),
                                    hm_phase_rm,
                                    hm_nuclei_rm_inverse,
                                    hm_calcein_rm_inverse), 
                           dim = c(
                             dim(col_ms_segment_s)[1], 
                             dim(col_ms_segment_s)[2], 
                             dim(col_ms_segment_s)[3], 6), 
                           colormode="Color")
      corr_img_phase_overlay_sr(combined_img)
    }
    img_gradient_adj <- observeEvent(input$grad_button, {
      req(pmask_thresh_sr(), nmask_thresh_sr(), 
          nmask_thresh_inverse_sr(), cmask_thresh_inverse_sr(),
          ctmask_sr(), segment_df_sr())
      #compute gradient
      switch(input$pic_loc3,
             "top left" = {pninv_norm_thresh_p_sr$topleft =
               input$pninv_p},
             "top center" = {pninv_norm_thresh_p_sr$topcenter =
               input$pninv_p},
             "top right" = {pninv_norm_thresh_p_sr$topright =
               input$pninv_p},
             "mid left" = {pninv_norm_thresh_p_sr$midleft =
               input$pninv_p},
             "mid center" = {pninv_norm_thresh_p_sr$midcenter = 
               input$pninv_p},
             "mid right" = {pninv_norm_thresh_p_sr$midright = 
               input$pninv_p},
             "lower left" = {pninv_norm_thresh_p_sr$lowerleft = 
               input$pninv_p},
             "lower center" = {pninv_norm_thresh_p_sr$lowercenter = 
               input$pninv_p},
             "lower right" = {pninv_norm_thresh_p_sr$lowerright = 
               input$pninv_p},
             "border(s)" = {b_pninv_norm_thresh_p_sr(input$pninv_p)}   
      )
      switch(input$pic_loc3,
             "top left" = {pninv_norm_thresh_n_sr$topleft = 
               input$pninv_n},
             "top center" = {pninv_norm_thresh_n_sr$topcenter = 
               input$pninv_n},
             "top right" = {pninv_norm_thresh_n_sr$topright = 
               input$pninv_n},
             "mid left" = {pninv_norm_thresh_n_sr$midleft = 
               input$pninv_n},
             "mid center" = {pninv_norm_thresh_n_sr$midcenter = 
               input$pninv_n},
             "mid right" = {pninv_norm_thresh_n_sr$midright = 
               input$pninv_n},
             "lower left" = {pninv_norm_thresh_n_sr$lowerleft = 
               input$pninv_n},
             "lower center" = {pninv_norm_thresh_n_sr$lowercenter = 
               input$pninv_n},
             "lower right" = {pninv_norm_thresh_n_sr$lowerright = 
               input$pninv_n},  
             "border(s)" = {b_pninv_norm_thresh_n_sr(input$pninv_n)}
      )
      switch(input$pic_loc3,
             "top left" = {p_norm_thresh_val_sr$topleft = input$pcinv_p},
             "top center" = {p_norm_thresh_val_sr$topcenter = 
               input$pcinv_p},
             "top right" = {p_norm_thresh_val_sr$topright = 
               input$pcinv_p},
             "mid left" = {p_norm_thresh_val_sr$midleft = input$pcinv_p},
             "mid center" = {p_norm_thresh_val_sr$midcenter = 
               input$pcinv_p},
             "mid right" = {p_norm_thresh_val_sr$midright = 
               input$pcinv_p},
             "lower left" = {p_norm_thresh_val_sr$lowerleft = 
               input$pcinv_p},
             "lower center" = {p_norm_thresh_val_sr$lowercenter = 
               input$pcinv_p},
             "lower right" = {p_norm_thresh_val_sr$lowerright = 
               input$pcinv_p},
             "border(s)" = {b_p_norm_thresh_val_sr(input$pcinv_p)}   
      )
      switch(input$pic_loc3,
             "top left" = {c_inv_norm_thresh_sr$topleft = input$pcinv_c},
             "top center" = {c_inv_norm_thresh_sr$topcenter = 
               input$pcinv_c},
             "top right" = {c_inv_norm_thresh_sr$topright = 
               input$pcinv_c},
             "mid left" = {c_inv_norm_thresh_sr$midleft = input$pcinv_c},
             "mid center" = {c_inv_norm_thresh_sr$midcenter = 
               input$pcinv_c},
             "mid right" = {c_inv_norm_thresh_sr$midright = 
               input$pcinv_c},
             "lower left" = {c_inv_norm_thresh_sr$lowerleft = 
               input$pcinv_c},
             "lower center" = {c_inv_norm_thresh_sr$lowercenter = 
               input$pcinv_c},
             "lower right" = {c_inv_norm_thresh_sr$lowerright = 
               input$pcinv_c},  
             "border(s)" = {b_c_inv_norm_thresh_sr(input$pcinv_c)}
      )
      compute_wholeimg_gradient_and_segment()
    })
    output$gradient_adj <- renderDisplay({
      req(norm_gradient_sr())
      
      if (input$instseg_bdr == "on") {
        display(norm_gradient_sr() + borders_sr()[,,1])
      } else if (input$instseg_bdr == "off") {
        display(norm_gradient_sr())
      } 
    })
    pnc_heatmap <- function(rad_map1, rad_map2, thresh_val) {
      hm_rm_shiny = colormap(normalize(rad_map1), palette =  
                               jet.colors(256))
      hm_rm_shiny = removebackground(start_end_df = segment_df_sr(),
                                     img = hm_rm_shiny, ref_map =
                                       non_negative_matrix(rad_map2),
                                     thresh_val = thresh_val)
      return(hm_rm_shiny)   
    }  
    img_phase_overlay <- observeEvent({input$grad_button |
        input$dismiss |
        state_refresh_del()}, {
          
          req(segment_df_sr(),ctmask_sr(), pmask_radiancemap_sr(), 
              nmask_radiancemap_sr(), cmask_radiancemap_sr())
          #Compute Hoechst and Calcein Thresholds
          n_thresh_val = array(data = c(n_thresh_val_sr$topleft, 
                                        n_thresh_val_sr$midleft,
                                        n_thresh_val_sr$lowerleft,
                                        n_thresh_val_sr$topcenter, 
                                        n_thresh_val_sr$midcenter,
                                        n_thresh_val_sr$lowercenter,
                                        n_thresh_val_sr$topright, 
                                        n_thresh_val_sr$midright,
                                        n_thresh_val_sr$lowerright), 
                               dim = n_thresh_val_sr_dim)
          c_thresh_val = array(data = c(c_thresh_val_sr$topleft, 
                                        c_thresh_val_sr$midleft,
                                        c_thresh_val_sr$lowerleft,
                                        c_thresh_val_sr$topcenter, 
                                        c_thresh_val_sr$midcenter,
                                        c_thresh_val_sr$lowercenter,
                                        c_thresh_val_sr$topright, 
                                        c_thresh_val_sr$midright,
                                        c_thresh_val_sr$lowerright),
                               dim = c_thresh_val_sr_dim)
          n_thresh_val = n_thresh_val * max(non_negative_matrix(
            nmask_radiancemap_sr()))
          # use un-normalized values 
          c_thresh_val = c_thresh_val * max(non_negative_matrix(
            cmask_radiancemap_sr()))
          # use un-normalized values  
          #Compute phase radiance heatmaps
          hm_phase_rm_shiny = pnc_heatmap(rad_map1 = non_negative_matrix(
            pmask_radiancemap_sr()),
            rad_map2 = non_negative_matrix(
              cmask_radiancemap_sr()), 
            thresh_val = c_thresh_val)
          hm_phase_rm_sr(hm_phase_rm_shiny)
          #Compute nuclei radiance heatmaps
          hm_nuclei_rm_inverse_shiny = pnc_heatmap(rad_map1 =
                                                     nmask_thresh_inverse_sr(), 
                                                   rad_map2 = non_negative_matrix(
                                                     nmask_radiancemap_sr()),
                                                   thresh_val = n_thresh_val)
          hm_nuclei_rm_inverse_sr(hm_nuclei_rm_inverse_shiny) 
          #Compute calcein radiance heatmaps
          hm_calcein_rm_inverse_shiny = pnc_heatmap(rad_map1 =
                                                      cmask_thresh_inverse_sr(), 
                                                    rad_map2 = non_negative_matrix(
                                                      cmask_radiancemap_sr()),
                                                    thresh_val = c_thresh_val)
          hm_calcein_rm_inverse_sr(hm_calcein_rm_inverse_shiny)
          #create a combined image of segmentation results with phase 
          #overlay, phase, and various heatmaps
          combined_img = Image(data = c(segmentedcells_overlay_sr(), 
                                        col_segmentedcells_sr(), 
                                        toRGB(normalize(
                                          pmask_radiancemap_sr())),
                                        hm_phase_rm_sr(),
                                        hm_nuclei_rm_inverse_sr(),
                                        hm_calcein_rm_inverse_sr()), 
                               dim = c(
                                 dim(col_segmentedcells_sr())[1], 
                                 dim(col_segmentedcells_sr())[2], 
                                 dim(col_segmentedcells_sr())[3], 6), 
                               colormode="Color")
          img_phase_overlay_sr(combined_img)
        }, ignoreInit = TRUE)
    output$phase_overlay <- renderDisplay({
      req(img_phase_overlay_sr())
      if (input$instseg_bdr == "on") {
        display(img_phase_overlay_sr() + Image(data = c(borders_sr(), 
                                                        borders_sr(), 
                                                        borders_sr(),
                                                        borders_sr(), 
                                                        borders_sr(),
                                                        borders_sr()
        ), dim = c( 
          dim(borders_sr())[1],
          dim(borders_sr())[2],
          dim(borders_sr())[3],
          6), colormode="Color") 
        )
      } else if (input$instseg_bdr == "off") {
        display(img_phase_overlay_sr())
      } 
    })
    output$mod_phase_overlay_all <- renderDisplay({
      req(img_phase_overlay_sr())
      display(img_phase_overlay_sr()) 
    })
    output$annotations_table <- renderDataTable({
      req(nrow(custom_annot_sr$Data) > 0) 
      DT = custom_annot_sr$Data
      DT[["Actions"]] <- paste0('<div class = "btn-group" role = 
                                      "group" aria-label = 
                                      "Basic example">
                                          <button type = "button"
                                               class = 
                                               "btn btn-secondary delete" 
                                               id=delete_', 1:nrow( 
                                                 custom_annot_sr$Data), 
                                '>Delete</button>
                                          <button type="button" 
                                               class = "btn btn-secondary 
                                               modify"id = modify_',
                                1:nrow(
                                  custom_annot_sr$Data),
                                '>Modify</button>
                                      </div>'
      )
      datatable(DT, escape = F)
    })
    #Code for managing in row addition, deletion and, modification
    modal_add_or_modify <- modalDialog(
      fluidPage(
        h3(strong("Annotation Modification"), align = "center"),
        hr(),
        tabsetPanel(id = "modaltabs", type = "tabs",
                    tabPanel(value = "Cust_Nuclei", title = "Nuclei",
                             h3("Segment Nuclei"),
                             h5(paste("*please remember to press Update ",  
                                      "when done to save changes.", sep = "")
                             ),
                             tags$head(tags$style('h6 {color:green;}')),
                             h6(paste("**when adding a new annotation, what",
                                      " is shown on the display is based on",
                                      " prior parameter settings. If you fo",
                                      "rget to press the update button, def",
                                      "ault nuclei parameters will be appli",    
                                      "ed. Please note that this may differ",
                                      " from what is shown on the screen.",  
                                      sep = "")
                             ),
                             column(4, 
                                    wellPanel(
                                      sliderInput(inputId = "n_ntol_lc", 
                                                  label = paste("Image Intensity Hei",
                                                                "ght Tolerance For ",                   
                                                                "Separating Neighb",
                                                                "ouring Objects:",
                                                                sep = ""),
                                                  min = 0, max = 10, value = 0.1, 
                                                  step = 0.001),
                                      textOutput(outputId = "n_ntol_lc_par"),
                                      br(),
                                      sliderInput(inputId = "n_next_lc", 
                                                  label = paste("Neigbourhood Pixel ",
                                                                "Radius For Detectin",
                                                                "g Neighbouring Obje",  
                                                                "cts:", sep = ""), 
                                                  min = 1, max = 10, value =  1,
                                                  step = 1),
                                      textOutput(outputId = "n_next_lc_par"),
                                      br(),
                                      actionButton("update_changes_nuclei",
                                                   "Update")
                                    )
                             ),
                             column(8, displayOutput(
                               "mod_phase_overlay_localcluster_nuc", 
                               width = "100%")
                             )
                    ), 
                    tabPanel(value = "Cust_Gradient", title = "Gradient",
                             h3("Generate Image Gradient"),
                             h5(paste("*please remember to press Update ",  
                                      "when done to save changes.", sep = "")
                             ),
                             h6(paste("**when adding a new annotation, what",
                                      " is shown on the display is based on",
                                      " prior parameter settings. If you fo",
                                      "rget to press the update button, def",
                                      "ault gradient parameters will be app",    
                                      "lied. Please note that this may diff",
                                      "er from what is shown on the screen.",  
                                      sep = "")
                             ),
                             column(4, 
                                    wellPanel(
                                      sliderInput(inputId = "pninv_p_lc", 
                                                  label = paste("Phase & Inverse ",
                                                                "Nuclei Intersect: ",                   
                                                                " Phase Parameter",
                                                                sep = ""),
                                                  min = 0, max = 1, value = 0.5, 
                                                  step = 0.001),
                                      textOutput(outputId = "pninv_p_lc_par"),
                                      br(),
                                      sliderInput(inputId = "pninv_n_lc", 
                                                  label = paste("Phase & Inverse ",
                                                                "Nuclei Intersect: ",                   
                                                                "Nuclei Parameter",
                                                                sep = ""), 
                                                  min = 0, max = 1, value =  0.5,
                                                  step = 0.001),
                                      textOutput(outputId = "pninv_n_lc_par"),
                                      br(),
                                      sliderInput(inputId = "pcinv_p_lc", 
                                                  label = paste("Phase & Inverse ",
                                                                "Calcein Intersect: ",                   
                                                                "Phase Parameter",
                                                                sep = ""),
                                                  min = 0, max = 1, value = 0.5, 
                                                  step = 0.001),
                                      textOutput(outputId = "pcinv_p_lc_par"),
                                      br(),
                                      sliderInput(inputId = "pcinv_c_lc", 
                                                  label = paste("Phase & Inverse ",
                                                                "Calcein Intersect: ",                   
                                                                "Calcein Parameter",
                                                                sep = ""), 
                                                  min = 0, max = 1, value =  0.5,
                                                  step = 0.001),
                                      textOutput(outputId = "pcinv_c_lc_par"),
                                      br(),
                                      actionButton("update_changes_gradient",
                                                   "Update")
                                    )
                             ),
                             column(8, displayOutput(
                               "mod_phase_overlay_localcluster_grad", 
                               width = "100%")
                             )
                    )
        )
      ),
      footer = actionButton("dismiss", "Dismiss"),
      size="l"
    )
    observeEvent(input$modaltabs, { 
      #to refresh screen after custom annotations added or
      #modified (in case user forgets to press the update
      #button) as well as toggling in between "Cust_Nuclei"
      #and "Cust_Gradient" tabs
      if (input$modaltabs == "Cust_Gradient") {
        #if gradient tab is clicked, it will update 
        #corr_img_phase_overlay_sr() by running 
        #update_localcluster_gradient_and_segment
        update_localcluster_gradient_and_segment()
        lookup_gradient_parameters()
      }
    })
    observeEvent(input$dismiss, { #reset Shiny reactive variables
      #after modal dismiss button is pressed 
      #check what happens if add annotation but don’t click update
      #for nuclei and/or gradient parameters
      if (is.null(corr_ntol_sr()) | is.null(corr_next_sr()) |
          is.null(corr_pninv_normthresh_p_sr()) |
          is.null(corr_pninv_normthresh_n_sr()) |
          is.null(corr_p_normthresh_val_sr()) | 
          is.null(corr_cinv_normthresh_val_sr()) | 
          length(corr_ntol_sr()) != length(seg_pointx_s()) |
          length(corr_ntol_sr()) != length(seg_pointy_s()) |
          length(corr_ntol_sr()) != length(seg_pointx_s()) |
          length(corr_next_sr()) != length(seg_pointy_s()) |
          length(corr_pninv_normthresh_p_sr()) != length(
            seg_pointx_s()) |
          length(corr_pninv_normthresh_p_sr()) != length(
            seg_pointy_s()) |
          length(corr_pninv_normthresh_n_sr()) != length(
            seg_pointx_s()) |
          length(corr_pninv_normthresh_n_sr()) != length(
            seg_pointy_s()) |
          length(corr_p_normthresh_val_sr()) != length(
            seg_pointx_s()) |
          length(corr_p_normthresh_val_sr()) != length(
            seg_pointy_s()) |
          length(corr_cinv_normthresh_val_sr()) != length(
            seg_pointx_s()) |
          length(corr_cinv_normthresh_val_sr()) != length(
            seg_pointy_s()) |
          length(corr_next_sr()) != nrow(custom_annot_sr$Data) |
          length(corr_next_sr()) != nrow(custom_annot_sr$Data) |
          length(corr_pninv_normthresh_p_sr()) != nrow(
            custom_annot_sr$Data
          ) |
          length(corr_pninv_normthresh_n_sr()) != nrow(
            custom_annot_sr$Data
          ) |
          length(corr_p_normthresh_val_sr()) != nrow(
            custom_annot_sr$Data
          ) |
          length(corr_cinv_normthresh_val_sr()) != nrow(
            custom_annot_sr$Data
          ) &
          (state_ifaddisTRUE_ifmodifyisFALSE() == TRUE) 
      ) {
        #Update button was not pressed so custom annotation was not 
        #saved. Error checking is performed to identify the relevant 
        #entry row/position and it is then deleted from 
        #custom_annot_sr$Data, seg_pointx_s(), seg_pointy_s(), 
        #corr_pninv_normthresh_p_sr(), corr_pninv_normthresh_n_sr(), 
        #corr_p_normthresh_val_sr(), and   
        #corr_cinv_normthresh_val_sr() 
        curr_seg_pointx_s = curr_seg_pointx_s()
        curr_seg_pointy_s = curr_seg_pointy_s()
        seg_pointx_s = seg_pointx_s()
        seg_pointy_s = seg_pointy_s()
        corr_n_tol_s = corr_ntol_sr()
        corr_n_ext_s = corr_next_sr()
        corr_pninv_normthresh_p_s = corr_pninv_normthresh_p_sr()
        corr_pninv_normthresh_n_s = corr_pninv_normthresh_n_sr()
        corr_p_normthresh_val_s = corr_p_normthresh_val_sr()
        corr_cinv_normthresh_val_s = corr_cinv_normthresh_val_sr()
        i = which((seg_pointx_s == curr_seg_pointx_s) &   
                    (seg_pointy_s == curr_seg_pointy_s))
        if (length(i) != 0) {
          if (length(corr_n_tol_s) > length(
            corr_pninv_normthresh_p_s))  
          { 
            #display warning message
            print("Only nuclei update button was pressed.")  
            print(paste("Update button for gradient paramet",  
                        "ers was not pressed so segmentatio", 
                        "n will be recomputed using default", 
                        " gradient parameters. As such, thi", 
                        "s window may take slightly longer ", 
                        "to close. Please be patient. Pleas",
                        "e also verify whether this is suit", 
                        "able or not.", sep = "")
            )
            showNotification(ui = paste("Update button for ",  
                                        "gradient parameter",  
                                        "s was not pressed ", 
                                        "so segmentation wi", 
                                        "ll be recomputed u",  
                                        "sing default gradi",  
                                        "ent parameters to ", 
                                        "avoid errors. As s", 
                                        "uch, this window m", 
                                        "ay take slightly l", 
                                        "onger to close. Pl", 
                                        "ease be patient. P", 
                                        "lease also verify ", 
                                        "whether this is su", 
                                        "itable or not.",  
                                        sep = ""), duration =
                               10, type = "warning")
            #nuclei update button was pressed but the 
            #gradient update button was not so add 0.5 as 
            #default gradient parameters
            corr_pninv_normthresh_p_s = append(
              corr_pninv_normthresh_p_s, 0.5, after = i-1)
            corr_pninv_normthresh_n_s = append(
              corr_pninv_normthresh_n_s, 0.5, after = i-1)
            corr_p_normthresh_val_s = append(
              corr_p_normthresh_val_s, 0.5, after = i-1)
            corr_cinv_normthresh_val_s = append(
              corr_cinv_normthresh_val_s, 0.5, after = i-1)
            #update Shiny reactive variables for custom 
            #annotations and recompute
            corr_pninv_normthresh_p_sr(corr_pninv_normthresh_p_s)
            corr_pninv_normthresh_n_sr(corr_pninv_normthresh_n_s)
            corr_p_normthresh_val_sr(corr_p_normthresh_val_s)
            corr_cinv_normthresh_val_sr(
              corr_cinv_normthresh_val_s) 
            compute_wholeimg_gradient_and_segment() 
          } else if (length(corr_n_tol_s) < length(
            corr_pninv_normthresh_p_s)) 
          { 
            #display warning message
            print("Only gradient update button was pressed.")
            print(paste("Update button for nuclei parameter",  
                        "s was not pressed so segmentation ", 
                        "will be recomputed using default ", 
                        "nuclei parameters. As such, this ", 
                        "window may take slightly longer to", 
                        " close. Please be patient. Please ",
                        "also verify whether this is suitable", 
                        " or not.", sep =  "")
            )
            showNotification(ui = paste("Update button for ",  
                                        "nuclei parameters ",  
                                        "was not pressed so", 
                                        " segmentation will", 
                                        " be recomputed usi",  
                                        "ng default nuclei ",  
                                        "parameters to avoi", 
                                        "d errors. As such,", 
                                        " this window may t", 
                                        "ake slightly longe", 
                                        "r to close. Please", 
                                        " be patient. Pleas",
                                        "e also verify whet", 
                                        "her this is suitab", 
                                        "le or not.",  sep = 
                                          ""), duration = 10,
                             type = "warning")
            #gradient update button was pressed but the 
            #nuclei update button was not so add 0.1 and 1.0 
            #as default nuclei parameters 
            corr_n_tol_s = append(corr_n_tol_s, 0.1, after = 
                                    i-1)
            corr_n_ext_s = append(corr_n_ext_s, 1, after = 
                                    i-1)
            #update Shiny reactive variables for custom 
            #annotations and recompute
            corr_ntol_sr(corr_n_tol_s)
            corr_next_sr(corr_n_ext_s)
            compute_wholeimg_watershed()
            compute_wholeimg_gradient_and_segment()
          } else if (length(corr_n_tol_s) == length(
            corr_pninv_normthresh_p_s))  
          {
            #display warning message
            print("Both update buttons were not pressed.") 
            print(paste("Update button(s) for nuclei and/or",  
                        " gradient parameters were not pres", 
                        "sed so custom annotation was not a", 
                        "dded.", sep = ""))
            showNotification(ui = paste("Update button(s) f",  
                                        "or nuclei and grad",  
                                        "ient parameters we", 
                                        "re not pressed so ", 
                                        "custom annotation ",  
                                        "was not added.", 
                                        sep = ""), duration = 
                               10, type = "warning")
            #gradient update button and nuclei update button
            #were not pressed
            seg_pointx_s = seg_pointx_s[-i] #remove x entry
            seg_pointy_s = seg_pointy_s[-i] #remove y entry
            custom_annot_sr$Data = custom_annot_sr$Data[-i]
            connectedmask_list = connectedmask_list_sr()
            connectedmask_list = connectedmask_list[-i]
            #remove table entry
            #update Shiny reactive variables for custom 
            #annotations
            seg_pointx_s(seg_pointx_s) 
            seg_pointy_s(seg_pointy_s) 
            connectedmask_list_sr(connectedmask_list)
            #check if all custom annotations deleted or not
            if (length(seg_pointx_s()) == 0 & 
                length(seg_pointy_s()) == 0 &
                length(corr_ntol_sr()) == 0 & 
                length(corr_next_sr()) == 0 &
                length(corr_pninv_normthresh_p_sr()) == 0 &
                length(corr_pninv_normthresh_n_sr()) == 0 &
                length(corr_p_normthresh_val_sr()) == 0 &
                length(corr_cinv_normthresh_val_sr()) == 0 &
                length(connectedmask_list_sr()) == 0
            ) { #set corrections to false to indicate no 
              #custom annotations
              corrections(FALSE)
              print("Set corrections() variable to FALSE")   
            }
            
          }  
        }  
      }
      #Re-run segmentation in case a custom annotation was modified 
      #and nuclei update button was the most recently pressed button.  
      #This is will ensure that gradient-based segmentation is not 
      #‘out-of-date’ and avoid errors 
      if (state_ifaddisTRUE_ifmodifyisFALSE() == FALSE & 
          state_ifnucleiisTRUE_ifgradientisFALSE() == TRUE) {
        print(paste("Most recently pressed ‘update’ button was for ", 
                    "saving nuclei/watershed parameters so segmentation ", 
                    "will be re-computed using previously saved gradient ", 
                    "parameters to avoid errors. As such, this window may", 
                    " take slightly longer to close. Please be patient.",
                    sep = ""))
        showNotification(ui = paste("Most recently pressed ‘upda",  
                                    "te’ button was for saving nuclei/water", 
                                    "shed parameters so segmentation will be r",
                                    "e-computed using previously saved gradien",
                                    "t parameters to avoid errors. As such, th",
                                    "is window may take slightly longer to clo",
                                    "se. Please be patient.", sep = ""),
                         duration = 10, type = "message")
        compute_wholeimg_gradient_and_segment()
      }  
      #reset variables
      curr_seg_pointx_s(NULL) 
      curr_seg_pointy_s(NULL) 
      state_ifaddisTRUE_ifmodifyisFALSE(NULL)
      state_ifnucleiisTRUE_ifgradientisFALSE("none")
      state_ifnucleipressedisTRUE(NULL)
      state_ifgradpressedisTRUE(NULL)
      removeModal()
    })
    add_cust_annotation <- observeEvent(input$edit_add_button, {
      req(ctmask_sr(), input$edit_x, input$edit_y) 
      cytomask = ctmask_sr()
      if (((as.numeric(input$edit_x) < 1) == TRUE) | 
          ((as.numeric(input$edit_y) < 1) == TRUE) | 
          ((as.numeric(input$edit_x) > dim(cytomask)[1]) == TRUE) |
          ((as.numeric(input$edit_y) > dim(cytomask)[2]) == TRUE)) {
        print(paste("Indicated position is out of image range so cu", 
                    "stom annotation was not added.", sep = ""))
        showNotification(ui = paste("Indicated position is out of ",  
                                    "image range so custom annotation was not ", 
                                    "added.", sep = ""), duration = 10, type =                                 
                           "warning")
      } else {
        spatstatmask = connectedmask_sr()
        cluster_id = spatstatmask[as.numeric(input$edit_x),
                                  as.numeric(input$edit_y)]
        if (cluster_id == 0) {
          print(paste("No cells present at indicated position bas", 
                      "ed on thresholded fluorescence image so custom annot", 
                      "ation was not added.", sep = ""))
          showNotification(ui = paste("No cells present at indica",  
                                      "ted position based on thresholded fluores", 
                                      "cence image so custom annotation was not ", 
                                      "added.", sep = ""), duration = 10, type =
                             "warning")
        } else {
          #check if a custom annotation was previously added for     
          #the newly indicated position. If so, no new annotation 
          #is added. Otherwise, a new custom annotation is added.
          x_annot = seg_pointx_s()
          y_annot = seg_pointy_s()
          corrections = corrections() 
          already_added = NULL   
          if (corrections == FALSE) {
            already_added = FALSE #set already_added to FALSE as 
            #there are no custom 
            #annotations to perform a 
            #cross-check with 
          } else if (corrections == TRUE) { #perform a cross-check 
            #for an existing custom
            #annotation for the 
            #same region
            xy_cluster_pts = which(spatstatmask == cluster_id,
                                   arr.ind = TRUE) #find out x,y points 
            #associated with 
            #cluster_id
            x_cluster_pts = xy_cluster_pts[, 1] #find out x 
            #points                                                            
            #associated with 
            #cluster_id
            y_cluster_pts = xy_cluster_pts[, 2] #find out y 
            #points                                                            
            #associated with 
            #cluster_id
            i = which((x_annot %in% x_cluster_pts) &   
                        (y_annot %in% y_cluster_pts))
            if (length(i) != 0) {
              already_added = TRUE                               
              #show warning about not adding new annotation 
              print(paste("A custom annotation has previo", 
                          "usly been added for the indica", 
                          "ted position so no custom anno",
                          "tation was added. Please modif",  
                          "y this point instead: ", 
                          x_annot[i], ",", y_annot[i], 
                          sep = ""))
              showNotification(ui = paste("A custom anno",
                                          "tation has previously been ad",   
                                          "ded for the indicated positio",  
                                          "n so no custom annotation was", 
                                          " added. Please modify this po", 
                                          "int instead: ", x_annot[i], 
                                          ",", y_annot[i], sep = ""),
                               duration = 10, type = "warning")
            } else {
              already_added = FALSE
            }
          }
          if (!already_added) { 
            new_row = data.frame(x = as.numeric(input$edit_x),
                                 y = as.numeric(input$edit_y))
            custom_annot_sr$Data <- rbind(custom_annot_sr$Data, 
                                          new_row)
            #update seg_pointx_s and seg_pointy_s
            seg_pointx_s = seg_pointx_s()
            seg_pointy_s = seg_pointy_s()
            connectedmask_list = connectedmask_list_sr()
            seg_pointx_s[length(seg_pointx_s)+1] = as.numeric(
              input$edit_x)
            seg_pointy_s[length(seg_pointy_s)+1] = as.numeric(
              input$edit_y)
            connected = spatstatmask
            connected[spatstatmask != cluster_id] = 0
            connectedmask_list[[length(connectedmask_list)
                                +1]] = connected
            connectedmask_list_sr(connectedmask_list)
            seg_pointx_s(seg_pointx_s)
            seg_pointy_s(seg_pointy_s)
            corrections(TRUE) 
            curr_seg_pointx_s(as.numeric(input$edit_x)) 
            #set as current x point 
            curr_seg_pointy_s(as.numeric(input$edit_y)) 
            #set as current y point
            state_ifaddisTRUE_ifmodifyisFALSE(TRUE)
            state_ifnucleiisTRUE_ifgradientisFALSE("none")
            state_ifnucleipressedisTRUE(FALSE)
            state_ifgradpressedisTRUE(FALSE)
            #display custom annotation area
            update_localcluster_watershed()
            lookup_watershed_parameters()
            #do not need the command(s) below as now using 
            #observeEvent for Cust_Gradient tab 
            #update_localcluster_gradient_and_segment()
            #lookup_gradient_parameters()
            showModal(modal_add_or_modify)
          }
        } 
      }                         
    })
    img_corr_nuc_overlay <- observeEvent(input$update_changes_nuclei, {
      req(curr_seg_pointx_s(), curr_seg_pointy_s(),
          seg_pointx_s(), seg_pointy_s(),
          !(is.null(state_ifaddisTRUE_ifmodifyisFALSE()))
      )
      #Setup variables
      curr_seg_pointx_s = curr_seg_pointx_s()
      curr_seg_pointy_s = curr_seg_pointy_s()
      seg_pointx_s = seg_pointx_s()
      seg_pointy_s = seg_pointy_s()
      corr_n_tol_s = corr_ntol_sr()
      corr_n_ext_s = corr_next_sr()
      state_ifnucleiisTRUE_ifgradientisFALSE(TRUE)
      #Read in local cell cluster nuclei input & update Shiny variables
      n_tol = input$n_ntol_lc
      n_ext = input$n_next_lc 
      i = which((seg_pointx_s == curr_seg_pointx_s) &   
                  (seg_pointy_s == curr_seg_pointy_s))
      if (state_ifaddisTRUE_ifmodifyisFALSE() == FALSE) {
        #modifying existing custom annotation 
        if (length(i) != 0) {
          corr_n_tol_s[i] = n_tol
          corr_n_ext_s[i] = n_ext
        }  
      } else if (state_ifaddisTRUE_ifmodifyisFALSE() == TRUE) {
        #adding custom annotation 
        if (length(seg_pointx_s) == 1) { #this is the first custom 
          #annotation. No cross-
          #checking is needed.
          corr_n_tol_s = n_tol
          corr_n_ext_s = n_ext
          state_ifnucleipressedisTRUE(TRUE)
        } else { #cross-check to match position of seg_pointx_s and  
          #seg_pointy_s with corr_n_tol_s and corr_n_ext_s
          if ((length(i) != 0) & (state_ifnucleipressedisTRUE() == 
                                  FALSE)) {                     
            corr_n_tol_s = append(corr_n_tol_s, n_tol, 
                                  after = i-1)
            corr_n_ext_s = append(corr_n_ext_s, n_ext, 
                                  after = i-1)
            state_ifnucleipressedisTRUE(TRUE)                         
          } else if ((length(i) != 0) &
                     (state_ifnucleipressedisTRUE() == TRUE))
          {                        
            corr_n_tol_s[i] = n_tol
            corr_n_ext_s[i] = n_ext                       
          } 
        }
      } else {
        #unexpected error 
        print(paste("Error: state_ifaddisTRUE_ifmodifyisFALSE", 
                    "is undefined.", sep ="")) 
        showNotification(ui = paste("Error: state_ifaddisTRUE_ifmod",                                   
                                    "ifyisFALSE is undefined.", sep = 
                                      ""), 
                         duration = 10, type = "error")
      }
      #Update Shiny reactive variables and recalculate variables for 
      #computing watershed
      corr_ntol_sr(corr_n_tol_s)
      corr_next_sr(corr_n_ext_s)
      compute_wholeimg_watershed()
      #Display computed results only for custom annotation area
      update_localcluster_watershed()
      lookup_watershed_parameters()
      #do not need the command(s) below as now using observeEvent
      #for Cust_Nuclei and Cust_Gradient tabs 
      #update_localcluster_gradient_and_segment()
      #lookup_gradient_parameters()
    })
    output$mod_phase_overlay_localcluster_nuc <- renderDisplay({
      req(corr_nuc_watershed_sr())
      display(corr_nuc_watershed_sr()) 
    })
    img_corr_phase_overlay_localcluster <- observeEvent(
      input$update_changes_gradient, 
      {
        req(curr_seg_pointx_s(), curr_seg_pointy_s(),
            seg_pointx_s(), seg_pointy_s(),
            !(is.null(state_ifaddisTRUE_ifmodifyisFALSE()))
        )
        #Setup variables
        curr_seg_pointx_s = curr_seg_pointx_s()
        curr_seg_pointy_s = curr_seg_pointy_s()
        seg_pointx_s = seg_pointx_s()
        seg_pointy_s = seg_pointy_s()
        corr_pninv_normthresh_p_s = corr_pninv_normthresh_p_sr()
        corr_pninv_normthresh_n_s = corr_pninv_normthresh_n_sr()
        corr_p_normthresh_val_s = corr_p_normthresh_val_sr()
        corr_cinv_normthresh_val_s = corr_cinv_normthresh_val_sr()                                                
        state_ifnucleiisTRUE_ifgradientisFALSE(FALSE)
        #Read in local cell cluster nuclei input & update Shiny variables
        pninv_normthresh_p = input$pninv_p_lc
        pninv_normthresh_n = input$pninv_n_lc
        p_normthresh_val = input$pcinv_p_lc
        cinv_normthresh_val = input$pcinv_c_lc
        i = which((seg_pointx_s == curr_seg_pointx_s) &   
                    (seg_pointy_s == curr_seg_pointy_s))
        if (state_ifaddisTRUE_ifmodifyisFALSE() == FALSE) {
          #modifying existing custom annotation 
          if (length(i) != 0) {
            corr_pninv_normthresh_p_s[i] = pninv_normthresh_p
            corr_pninv_normthresh_n_s[i] = pninv_normthresh_n
            corr_p_normthresh_val_s[i] = p_normthresh_val
            corr_cinv_normthresh_val_s[i] = cinv_normthresh_val
          }
        } else if (state_ifaddisTRUE_ifmodifyisFALSE() == TRUE) {
          #adding custom annotation 
          if (length(seg_pointx_s) == 1) { #this is the first custom 
            #annotation. No cross-
            #checking is needed.
            corr_pninv_normthresh_p_s = pninv_normthresh_p
            corr_pninv_normthresh_n_s = pninv_normthresh_n
            corr_p_normthresh_val_s = p_normthresh_val
            corr_cinv_normthresh_val_s = cinv_normthresh_val
            state_ifgradpressedisTRUE(TRUE)
          } else { #cross-check to match position of seg_pointx_s and  
            #seg_pointy_s with corr_pninv_normthresh_p_s,
            #corr_pninv_normthresh_n_s, corr_p_normthresh_val_s, 
            #and corr_cinv_normthresh_val_s
            if ((length(i) != 0) & (state_ifgradpressedisTRUE() == 
                                    FALSE)) {
              corr_pninv_normthresh_p_s = append(
                corr_pninv_normthresh_p_s,
                pninv_normthresh_p, 
                after = i-1)
              corr_pninv_normthresh_n_s = append(
                corr_pninv_normthresh_n_s,  
                pninv_normthresh_n, 
                after = i-1)
              corr_p_normthresh_val_s = append(
                corr_p_normthresh_val_s, p_normthresh_val, 
                after = i-1)
              corr_cinv_normthresh_val_s = append(
                corr_cinv_normthresh_val_s, 
                cinv_normthresh_val, after = i-1)
              state_ifgradpressedisTRUE(TRUE)
            } else if ((length(i) != 0) & 
                       (state_ifgradpressedisTRUE() == TRUE)) 
            {                        
              corr_pninv_normthresh_p_s[i] = pninv_normthresh_p
              corr_pninv_normthresh_n_s[i] = pninv_normthresh_n
              corr_p_normthresh_val_s[i] = p_normthresh_val
              corr_cinv_normthresh_val_s[i] = cinv_normthresh_val
            }
          }
        } else {
          #unexpected error 
          print(paste("Error: state_ifaddisTRUE_ifmodifyisFALSE", 
                      "is undefined.", sep ="")) 
          showNotification(ui = paste("Error: state_ifaddisTRUE_ifmod",                                   
                                      "ifyisFALSE is undefined.", sep = 
                                        ""), 
                           duration = 10, type = "error")
        }
        #Update Shiny reactive variables and recalculate variables for 
        #computing watershed
        corr_pninv_normthresh_p_sr(corr_pninv_normthresh_p_s)
        corr_pninv_normthresh_n_sr(corr_pninv_normthresh_n_s)
        corr_p_normthresh_val_sr(corr_p_normthresh_val_s)
        corr_cinv_normthresh_val_sr(corr_cinv_normthresh_val_s)                                                
        compute_wholeimg_gradient_and_segment()                   
        #Display computed results only for custom annotation area
        update_localcluster_gradient_and_segment()
        lookup_gradient_parameters()
      })
    output$mod_phase_overlay_localcluster_grad <- renderDisplay({
      req(corr_img_phase_overlay_sr())
      display(corr_img_phase_overlay_sr()) 
    })
    observeEvent(input$lastClick, {
      if (input$lastClickId%like%"delete") {
        #notification that may take long time
        print("Delete button was pressed.")  
        print(paste("Correction was deleted and segmentation will ",  
                    "be recomputed using remaining parameters. As ", 
                    "such, this may take slightly longer. Please be", 
                    " patient.", sep = "")
        )
        showNotification(ui = paste("Correction was deleted and seg",  
                                    "mentation will be recomputed u",  
                                    "sing remaining parameters. As ", 
                                    "such, this may take slightly l", 
                                    "onger. Please be patient.",
                                    sep = ""), duration = 10, type =
                           "warning")
        row_to_del=as.numeric(gsub("delete_", "", input$lastClickId))
        dt_entry = custom_annot_sr$Data[row_to_del] 
        dt_entry_x = dt_entry[[1,1]] #find x coordinate to delete
        dt_entry_y = dt_entry[[1,2]] #find y coordinate to delete
        seg_pointx_s = seg_pointx_s()
        seg_pointy_s = seg_pointy_s()
        corr_n_tol_s = corr_ntol_sr()
        corr_n_ext_s = corr_next_sr()
        corr_pninv_normthresh_p_s = corr_pninv_normthresh_p_sr()
        corr_pninv_normthresh_n_s = corr_pninv_normthresh_n_sr()
        corr_p_normthresh_val_s = corr_p_normthresh_val_sr()
        corr_cinv_normthresh_val_s = corr_cinv_normthresh_val_sr()
        connectedmask_list = connectedmask_list_sr()
        i = which((seg_pointx_s == dt_entry_x) &   
                    (seg_pointy_s == dt_entry_y))
        if (length(i) != 0) { #remove x & y coordinates
          seg_pointx_s = seg_pointx_s[-i]
          seg_pointy_s = seg_pointy_s[-i]
          corr_n_tol_s = corr_n_tol_s[-i]
          corr_n_ext_s = corr_n_ext_s[-i]
          corr_pninv_normthresh_p_s = corr_pninv_normthresh_p_s[-i]
          corr_pninv_normthresh_n_s = corr_pninv_normthresh_n_s[-i]
          corr_p_normthresh_val_s = corr_p_normthresh_val_s[-i]
          corr_cinv_normthresh_val_s =
            corr_cinv_normthresh_val_s[-i]
          connectedmask_list = connectedmask_list[-i]
          state_refresh_del(state_refresh_del()+1) 
        }
        #update Shiny reactive variables for custom annotationse
        seg_pointx_s(seg_pointx_s) 
        seg_pointy_s(seg_pointy_s) 
        corr_ntol_sr(corr_n_tol_s)
        corr_next_sr(corr_n_ext_s)
        corr_pninv_normthresh_p_sr(corr_pninv_normthresh_p_s)
        corr_pninv_normthresh_n_sr(corr_pninv_normthresh_n_s)
        corr_p_normthresh_val_sr(corr_p_normthresh_val_s)
        corr_cinv_normthresh_val_sr(corr_cinv_normthresh_val_s)                                                
        connectedmask_list_sr(connectedmask_list)
        if (length(seg_pointx_s()) == 0 & 
            length(seg_pointy_s()) == 0 &
            length(corr_ntol_sr()) == 0 & 
            length(corr_next_sr()) == 0 &
            length(corr_pninv_normthresh_p_sr()) == 0 &
            length(corr_pninv_normthresh_n_sr()) == 0 &
            length(corr_p_normthresh_val_sr()) == 0 &
            length(corr_cinv_normthresh_val_sr()) == 0 &
            length(connectedmask_list_sr()) == 0
        ) { #set corrections to false to indicate no custom 
          #annotations
          corrections(FALSE)
          print("Set corrections() variable to FALSE")   
        }
        custom_annot_sr$Data = custom_annot_sr$Data[-row_to_del]
        compute_wholeimg_watershed()
        compute_wholeimg_gradient_and_segment()                   
      }
      else if (input$lastClickId%like%"modify") {
        print("Modify button was pressed.")  
        selected_row = as.numeric(gsub("modify_", "",
                                       input$lastClickId))
        mod_row = custom_annot_sr$Data[selected_row]
        mod_x  = mod_row[[1]] 
        mod_y = mod_row[[2]]
        curr_seg_pointx_s(mod_x) #set current x point to modify 
        #annotation
        curr_seg_pointy_s(mod_y) #set current y point to modify 
        #annotation
        state_ifaddisTRUE_ifmodifyisFALSE(FALSE)
        state_ifnucleiisTRUE_ifgradientisFALSE("none")
        state_ifnucleipressedisTRUE(TRUE)
        state_ifgradpressedisTRUE(TRUE)
        #display custom annotation area
        update_localcluster_watershed()
        lookup_watershed_parameters()
        #do not need the command(s) below as now using 
        #observeEvent for Cust_Gradient tab 
        #update_localcluster_gradient_and_segment()
        #lookup_gradient_parameters()
        showModal(modal_add_or_modify)
      } 
    })
    #End of code for managing in row addition, deletion, and modification 
    #Step 3: Save Data
    saveAnnotation <- observeEvent(input$save_button, {
      req(nmask_thresh_inverse_sr(), cmask_thresh_inverse_sr(),
          pmask_radiancemap_sr(), nmask_radiancemap_sr(),
          cmask_radiancemap_sr(), nuc_watershed_sr(), gradient_sr(),
          col_segmentedcells_sr(), segmentedcells_overlay_sr(),
          segment_df_sr())
      #Recompute most up-to-date values
      p_text = p_text_sr()
      h_text = h_text_sr()
      c_text = c_text_sr()
      norm_n_thresh_val = array(data = c(n_thresh_val_sr$topleft, 
                                         n_thresh_val_sr$midleft,
                                         n_thresh_val_sr$lowerleft,
                                         n_thresh_val_sr$topcenter, 
                                         n_thresh_val_sr$midcenter,
                                         n_thresh_val_sr$lowercenter,
                                         n_thresh_val_sr$topright, 
                                         n_thresh_val_sr$midright,
                                         n_thresh_val_sr$lowerright), 
                                dim = n_thresh_val_sr_dim)
      norm_c_thresh_val = array(data = c(c_thresh_val_sr$topleft, 
                                         c_thresh_val_sr$midleft,
                                         c_thresh_val_sr$lowerleft,
                                         c_thresh_val_sr$topcenter, 
                                         c_thresh_val_sr$midcenter,
                                         c_thresh_val_sr$lowercenter,
                                         c_thresh_val_sr$topright, 
                                         c_thresh_val_sr$midright,
                                         c_thresh_val_sr$lowerright),
                                dim = c_thresh_val_sr_dim)
      n_thresh_val = norm_n_thresh_val * max(non_negative_matrix(
        nmask_radiancemap_sr()))
      # use un-normalized values 
      c_thresh_val = norm_c_thresh_val * max(non_negative_matrix(
        cmask_radiancemap_sr()))
      # use un-normalized values  
      n_tol = array(data = c(n_tol_sr$topleft, n_tol_sr$midleft,
                             n_tol_sr$lowerleft, n_tol_sr$topcenter, 
                             n_tol_sr$midcenter, n_tol_sr$lowercenter,
                             n_tol_sr$topright, n_tol_sr$midright,
                             n_tol_sr$lowerright), dim = n_tol_sr_dim)
      n_ext = array(data = c(n_ext_sr$topleft, n_ext_sr$midleft,
                             n_ext_sr$lowerleft, n_ext_sr$topcenter, 
                             n_ext_sr$midcenter, n_ext_sr$lowercenter,
                             n_ext_sr$topright, n_ext_sr$midright,
                             n_ext_sr$lowerright), dim = n_ext_sr_dim)
      b_n_tol = b_n_tol_sr()
      b_n_ext = b_n_ext_sr()
      pninv_norm_thresh_p = array(data = c(
        pninv_norm_thresh_p_sr$topleft, 
        pninv_norm_thresh_p_sr$midleft,
        pninv_norm_thresh_p_sr$lowerleft,
        pninv_norm_thresh_p_sr$topcenter, 
        pninv_norm_thresh_p_sr$midcenter,
        pninv_norm_thresh_p_sr$lowercenter,
        pninv_norm_thresh_p_sr$topright, 
        pninv_norm_thresh_p_sr$midright,
        pninv_norm_thresh_p_sr$lowerright), 
        dim = pninv_norm_thresh_p_sr_dim)
      pninv_norm_thresh_n = array(data = c(
        pninv_norm_thresh_n_sr$topleft, 
        pninv_norm_thresh_n_sr$midleft,
        pninv_norm_thresh_n_sr$lowerleft,
        pninv_norm_thresh_n_sr$topcenter, 
        pninv_norm_thresh_n_sr$midcenter,
        pninv_norm_thresh_n_sr$lowercenter,
        pninv_norm_thresh_n_sr$topright, 
        pninv_norm_thresh_n_sr$midright,
        pninv_norm_thresh_n_sr$lowerright), 
        dim = pninv_norm_thresh_n_sr_dim)
      b_pninv_norm_thresh_p = b_pninv_norm_thresh_p_sr()
      b_pninv_norm_thresh_n = b_pninv_norm_thresh_n_sr()
      p_norm_thresh_val = array(data = c(p_norm_thresh_val_sr$topleft,
                                         p_norm_thresh_val_sr$midleft,
                                         p_norm_thresh_val_sr$lowerleft,
                                         p_norm_thresh_val_sr$topcenter, 
                                         p_norm_thresh_val_sr$midcenter,
                                         p_norm_thresh_val_sr$lowercenter,
                                         p_norm_thresh_val_sr$topright, 
                                         p_norm_thresh_val_sr$midright,
                                         p_norm_thresh_val_sr$lowerright), 
                                dim = p_norm_thresh_val_sr_dim)
      c_inv_norm_thresh = array(data = c(c_inv_norm_thresh_sr$topleft,
                                         c_inv_norm_thresh_sr$midleft,
                                         c_inv_norm_thresh_sr$lowerleft,
                                         c_inv_norm_thresh_sr$topcenter, 
                                         c_inv_norm_thresh_sr$midcenter,
                                         c_inv_norm_thresh_sr$lowercenter,
                                         c_inv_norm_thresh_sr$topright, 
                                         c_inv_norm_thresh_sr$midright,
                                         c_inv_norm_thresh_sr$lowerright), 
                                dim = c_inv_norm_thresh_sr_dim)
      b_p_norm_thresh_val = b_p_norm_thresh_val_sr()
      b_c_inv_norm_thresh = b_c_inv_norm_thresh_sr()
      corrections = corrections()  
      seg_pointx_s = seg_pointx_s()
      seg_pointy_s = seg_pointy_s()
      corr_ntol = corr_ntol_sr()
      corr_next = corr_next_sr()
      corr_pninv_normthresh_p = corr_pninv_normthresh_p_sr()
      corr_pninv_normthresh_n = corr_pninv_normthresh_n_sr()
      corr_p_normthresh_val = corr_p_normthresh_val_sr()
      corr_cinv_normthresh_val = corr_cinv_normthresh_val_sr()                                                
      #Re-compute radiance heatmaps
      #Note: n_thresh_val and c_thresh_val use un-normalized values
      #i.e.: 
      #n_thresh_val = norm_n_thresh_val * max(nmask_radiancemap_sr())
      #c_thresh_val = norm_c_thresh_val * max(cmask_radiancemap_sr())
      #Compute phase radiance heatmaps
      hm_phase_rm_shiny = pnc_heatmap(rad_map1 = non_negative_matrix(
        pmask_radiancemap_sr()),
        rad_map2 = non_negative_matrix(
          cmask_radiancemap_sr()), 
        thresh_val = c_thresh_val)
      hm_phase_rm_sr(hm_phase_rm_shiny)
      #Compute nuclei radiance heatmaps
      hm_nuclei_rm_shiny = pnc_heatmap(rad_map1 = non_negative_matrix(
        nmask_radiancemap_sr()),
        rad_map2 = non_negative_matrix(
          nmask_radiancemap_sr()), 
        thresh_val = n_thresh_val)
      hm_nuclei_rm_sr(hm_nuclei_rm_shiny)
      hm_nuclei_rm_inverse_shiny = pnc_heatmap(rad_map1 =
                                                 nmask_thresh_inverse_sr(), 
                                               rad_map2 = non_negative_matrix(
                                                 nmask_radiancemap_sr()),
                                               thresh_val = n_thresh_val)
      hm_nuclei_rm_inverse_sr(hm_nuclei_rm_inverse_shiny) 
      #Compute calcein radiance heatmaps
      hm_calcein_rm_shiny = pnc_heatmap(rad_map1 = non_negative_matrix(
        cmask_radiancemap_sr()),
        rad_map2 = non_negative_matrix(
          cmask_radiancemap_sr()), 
        thresh_val = c_thresh_val)
      hm_calcein_rm_sr(hm_calcein_rm_shiny)
      hm_calcein_rm_inverse_shiny = pnc_heatmap(rad_map1 =
                                                  cmask_thresh_inverse_sr(), 
                                                rad_map2 = non_negative_matrix(
                                                  cmask_radiancemap_sr()),
                                                thresh_val = c_thresh_val)
      hm_calcein_rm_inverse_sr(hm_calcein_rm_inverse_shiny)
      #Allow user to set data folder path for command line syntax
      folderpath = input$folderpath
      folderpath = gsub("\\\\", "/", folderpath) #replace backslash 
      #with forward slash
      #save data
      setwd(folderpath)
      print(paste("Working Directory is: ", folderpath, "/hdr_output",
                  sep = ""))
      showNotification(paste("Working Directory is: ", folderpath, 
                             "/hdr_output", sep = ""), duration = 10)
      createSubDirectory(folderpath, "hdr_output")  
      setwd(paste(folderpath, "/hdr_output", sep = ""))
      writeTIFF(EBImage::transpose(hm_phase_rm_sr()), 
                "hm_phase_rm_shiny.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(hm_nuclei_rm_sr()),
                "hm_nuclei_rm_shiny.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(hm_nuclei_rm_inverse_sr()),
                "hm_nuclei_rm_inverse_shiny.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(hm_calcein_rm_sr()), 
                "hm_calcein_rm_shiny.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(hm_calcein_rm_inverse_sr()),
                "hm_calcein_rm_inverse_shiny.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(normalize(nuc_watershed_sr()[[1]])),
                "watershed.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(nuc_watershed_sr()[[1]]), 
                  file = "watershed.csv", 
                  sep = ",", row.names = F, col.names = F)
      writeTIFF(EBImage::transpose(col_nuc_watershed_sr()),
                "watershed_col.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(normalize(gradient_sr())),
                "gradient.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(gradient_sr()), 
                  file = "gradient.csv", 
                  sep = ",", row.names = F, col.names = F)
      writeTIFF(EBImage::transpose(normalize(segmentedcells_sr())),
                "segmentedcells.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(segmentedcells_sr()), 
                  file = "segmentedcells.csv", 
                  sep = ",", row.names = F, col.names = F)
      writeTIFF(EBImage::transpose(col_segmentedcells_sr()),
                "segmentedcells_col.tif",
                bits.per.sample = 8, compression = "none", 
                reduce = TRUE)
      writeTIFF(EBImage::transpose(normalize(
        segmentedcells_overlay_sr())),
        "segmentedcells_overlay.tif",
        bits.per.sample = 8, compression = "none", 
        reduce = TRUE)
      writeTIFF(EBImage::transpose(normalize(pmask_radiancemap_sr())),
                "Radiance_Map_Phase.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(pmask_radiancemap_sr()), 
                  file = "Radiance_Map_Phase.csv", 
                  sep = ",", row.names = F, col.names = F)
      writeTIFF(EBImage::transpose(normalize(nmask_radiancemap_sr())),
                "Radiance_Map_Nuclei.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(nmask_radiancemap_sr()), 
                  file = "Radiance_Map_Nuclei.csv", 
                  sep = ",", row.names = F, col.names = F)
      writeTIFF(EBImage::transpose(normalize(cmask_radiancemap_sr())),
                "Radiance_Map_Calcein.tif",
                bits.per.sample = 16, compression = "none", 
                reduce = TRUE)
      write.table(EBImage::transpose(cmask_radiancemap_sr()), 
                  file = "Radiance_Map_Calcein.csv", 
                  sep = ",", row.names = F, col.names = F)
      write_file1 = file("parameters.csv")
      csv_file <- paste(paste("Folder path,", folderpath, sep = ""), 
                        paste("Phase filenames,", 
                              toString(p_names_sr()), sep = ""),
                        paste("Hoechst filenames,", 
                              toString(h_names_sr()), sep = ""),
                        paste("Calcein filenames,", 
                              toString(c_names_sr()), sep = ""),
                        paste("Phase exposure,", toString(p_text), 
                              sep = ""),
                        paste("Hoechst exposure,", toString(h_text),
                              sep = ""),
                        paste("Calcein exposure,", toString(c_text),
                              sep = ""),
                        paste("norm_n_thresh_val,", 
                              toString(norm_n_thresh_val),
                              sep = ""), 
                        paste("n_thresh_val,", toString(n_thresh_val),
                              sep = ""), 
                        paste("n_thresh_val_sr_dim,",
                              toString(n_thresh_val_sr_dim), sep = ""),
                        paste("norm_c_thresh_val,",  
                              toString(norm_c_thresh_val),
                              sep = ""), 
                        paste("c_thresh_val,", toString(c_thresh_val),
                              sep = ""),
                        paste("c_thresh_val_sr_dim,", 
                              toString(c_thresh_val_sr_dim), sep = ""),
                        paste("n_tol,", toString(n_tol), sep = ""), 
                        paste("n_tol_sr_dim,", toString(n_tol_sr_dim),
                              sep = ""),
                        paste("n_ext,", toString(n_ext), sep = ""), 
                        paste("n_ext_sr_dim,", toString(n_ext_sr_dim),
                              sep = ""),
                        paste("b_n_tol,", toString(b_n_tol), sep = ""), 
                        paste("b_n_ext,", toString(b_n_ext), sep = ""),
                        paste("pninv_norm_thresh_p,", 
                              toString(pninv_norm_thresh_p), sep = ""), 
                        paste("pninv_norm_thresh_p_sr_dim,",   
                              toString(pninv_norm_thresh_p_sr_dim),
                              sep = ""),
                        paste("pninv_norm_thresh_n,",  
                              toString(pninv_norm_thresh_n), sep = ""), 
                        paste("pninv_norm_thresh_n_sr_dim,", 
                              toString(pninv_norm_thresh_n_sr_dim), 
                              sep = ""),
                        paste("b_pninv_norm_thresh_p,",   
                              toString(b_pninv_norm_thresh_p), 
                              sep = ""),
                        paste("b_pninv_norm_thresh_n,",   
                              toString(b_pninv_norm_thresh_n), 
                              sep = ""),
                        paste("p_norm_thresh_val,",   
                              toString(p_norm_thresh_val), sep = ""), 
                        paste("p_norm_thresh_val_sr_dim,",   
                              toString(p_norm_thresh_val_sr_dim), sep = ""), 
                        paste("c_inv_norm_thresh,",   
                              toString(c_inv_norm_thresh), sep = ""), 
                        paste("c_inv_norm_thresh_sr_dim,",   
                              toString(c_inv_norm_thresh_sr_dim), sep = ""),
                        paste("b_p_norm_thresh_val,",   
                              toString(b_p_norm_thresh_val), sep = ""), 
                        paste("b_c_inv_norm_thresh,",
                              toString(b_c_inv_norm_thresh), sep = ""),
                        paste("corrections,",
                              toString(corrections), sep = ""),
                        paste("seg_pointx,",
                              toString(seg_pointx_s), sep = ""),
                        paste("seg_pointy,",
                              toString(seg_pointy_s), sep = ""),
                        paste("corr_n_tol,",
                              toString(corr_ntol), sep = ""),
                        paste("corr_n_ext,",
                              toString(corr_next), sep = ""),
                        paste("corr_pninv_norm_thresh_p,",
                              toString(corr_pninv_normthresh_p), sep = ""),
                        paste("corr_pninv_norm_thresh_n,",
                              toString(corr_pninv_normthresh_n), sep = ""),
                        paste("corr_p_norm_thresh_val,",
                              toString(corr_p_normthresh_val), sep = ""),
                        paste("corr_c_inv_norm_thresh_val,",
                              toString(corr_cinv_normthresh_val), sep = ""),
                        #ws_pixel_rm_sr is currently disabled because 
                        #there appears to be some kind of protect() 
                        #error
                        #paste("ws_pixel_rm,", 
                        #toString(ws_pixel_rm_sr()), sep = ""),
                        sep = "\n") 
      writeLines(csv_file, write_file1)
      close(write_file1)
      write_file2 = file("command_line_code.txt")
      if (corrections == TRUE) {
        print("Have corrections")
        cmd_syntax = paste("hdr_img = segmentation_pipeline_using_h",
                           "dr('",
                           folderpath,
                           "',",
                           "f_p = c(",
                           paste("'", gsub(", ", "','", toString(
                             p_names_sr())), "'", sep = ""), 
                           "), f_h = c(",
                           paste("'", gsub(", ", "','", toString(
                             h_names_sr())), "'", sep = ""),
                           "), f_c = c(",
                           paste("'", gsub(", ", "','", toString(
                             c_names_sr())), "'", sep = ""),
                           "), e_p = c(",
                           paste(toString(p_text), sep = ""), 
                           "), e_h = c(",
                           paste(toString(h_text), sep = ""), 
                           "), e_c = c(",
                           paste(toString(c_text), sep = ""), 
                           "), n_thresh_val = array(data = c(",
                           toString(n_thresh_val),
                           "), dim = c(",
                           toString(n_thresh_val_sr_dim), 
                           ")), c_thresh_val = array(data = c(",
                           toString(c_thresh_val),
                           "), dim = c(",
                           toString(c_thresh_val_sr_dim), 
                           ")), n_tol = array(data = c(",
                           toString(n_tol),
                           "), dim = c(",
                           toString(n_tol_sr_dim), 
                           ")), n_ext = array(data = c(",
                           toString(n_ext),
                           "), dim = c(",
                           toString(n_ext_sr_dim), 
                           ")), b_n_tol = ",
                           b_n_tol,                             
                           ", b_n_ext = ",
                           b_n_ext,
                           #ws_pixel_rm_sr is currently disabled 
                           #because there appears to be some kind of 
                           #protect() error
                           #", ws_pixel_rm = ", 
                           #toString(ws_pixel_rm_sr()),
                           ", pninv_norm_thresh_p = array(data = c(",
                           toString(pninv_norm_thresh_p),  
                           "), dim = c(",
                           toString(pninv_norm_thresh_p_sr_dim), 
                           ")), pninv_norm_thresh_n = array(data =",
                           " c(",
                           toString(pninv_norm_thresh_n),
                           "), dim = c(",
                           toString(pninv_norm_thresh_n_sr_dim), 
                           ")), b_pninv_norm_thresh_p = ",
                           b_pninv_norm_thresh_p,
                           ", b_pninv_norm_thresh_n = ",
                           b_pninv_norm_thresh_n, 
                           ", p_norm_thresh_val = array(data = c(",
                           toString(p_norm_thresh_val),
                           "), dim = c(",
                           toString(p_norm_thresh_val_sr_dim), 
                           ")), c_inv_norm_thresh = array(data = c(",
                           toString(c_inv_norm_thresh),
                           "), dim = c(",
                           toString(c_inv_norm_thresh_sr_dim),                        
                           ")), b_p_norm_thresh_val = ",
                           b_p_norm_thresh_val, 
                           ", b_c_inv_norm_thresh = ",
                           b_c_inv_norm_thresh,
                           ", corrections = ",
                           corrections,
                           ", seg_pointx = c(",
                           toString(seg_pointx_s),
                           "), seg_pointy = c(",
                           toString(seg_pointy_s),
                           "), corr_n_tol = c(",
                           toString(corr_ntol),
                           "), corr_n_ext = c(",
                           toString(corr_next),
                           "), corr_pninv_norm_thresh_p = c(",                  
                           toString(corr_pninv_normthresh_p),
                           "), corr_pninv_norm_thresh_n = c(", 
                           toString(corr_pninv_normthresh_n),
                           "), corr_p_norm_thresh_val = c(",
                           toString(corr_p_normthresh_val),
                           "), corr_c_inv_norm_thresh_val = c(",  
                           toString(corr_cinv_normthresh_val),
                           ")",
                           ");",
                           "display_segmentation_pipeline_using_hdr", 
                           "(hdr_img)",
                           sep = "")
      } else {
        print("No corrections")
        cmd_syntax = paste("hdr_img = segmentation_pipeline_using_h",
                           "dr('",
                           folderpath,
                           "',",
                           "f_p = c(",
                           paste("'", gsub(", ", "','", toString(
                             p_names_sr())), "'", sep = ""), 
                           "), f_h = c(",
                           paste("'", gsub(", ", "','", toString(
                             h_names_sr())), "'", sep = ""),
                           "), f_c = c(",
                           paste("'", gsub(", ", "','", toString(
                             c_names_sr())), "'", sep = ""),
                           "), e_p = c(",
                           paste(toString(p_text), sep = ""), 
                           "), e_h = c(",
                           paste(toString(h_text), sep = ""), 
                           "), e_c = c(",
                           paste(toString(c_text), sep = ""), 
                           "), n_thresh_val = array(data = c(",
                           toString(n_thresh_val),
                           "), dim = c(",
                           toString(n_thresh_val_sr_dim), 
                           ")), c_thresh_val = array(data = c(",
                           toString(c_thresh_val),
                           "), dim = c(",
                           toString(c_thresh_val_sr_dim), 
                           ")), n_tol = array(data = c(",
                           toString(n_tol),
                           "), dim = c(",
                           toString(n_tol_sr_dim), 
                           ")), n_ext = array(data = c(",
                           toString(n_ext),
                           "), dim = c(",
                           toString(n_ext_sr_dim), 
                           ")), b_n_tol = ",
                           b_n_tol,                             
                           ", b_n_ext = ",
                           b_n_ext,
                           #ws_pixel_rm_sr is currently disabled 
                           #because there appears to be some kind of 
                           #protect() error
                           #", ws_pixel_rm = ", 
                           #toString(ws_pixel_rm_sr()),
                           ", pninv_norm_thresh_p = array(data = c(",
                           toString(pninv_norm_thresh_p),  
                           "), dim = c(",
                           toString(pninv_norm_thresh_p_sr_dim), 
                           ")), pninv_norm_thresh_n = array(data =",
                           " c(",
                           toString(pninv_norm_thresh_n),
                           "), dim = c(",
                           toString(pninv_norm_thresh_n_sr_dim), 
                           ")), b_pninv_norm_thresh_p = ",
                           b_pninv_norm_thresh_p,
                           ", b_pninv_norm_thresh_n = ",
                           b_pninv_norm_thresh_n, 
                           ", p_norm_thresh_val = array(data = c(",
                           toString(p_norm_thresh_val),
                           "), dim = c(",
                           toString(p_norm_thresh_val_sr_dim), 
                           ")), c_inv_norm_thresh = array(data = c(",
                           toString(c_inv_norm_thresh),
                           "), dim = c(",
                           toString(c_inv_norm_thresh_sr_dim),                        
                           ")), b_p_norm_thresh_val = ",
                           b_p_norm_thresh_val, 
                           ", b_c_inv_norm_thresh = ",
                           b_c_inv_norm_thresh,
                           ");",
                           "display_segmentation_pipeline_using_hdr", 
                           "(hdr_img)",
                           sep = "")
      } 
      writeLines(cmd_syntax, write_file2)
      close(write_file2)
      showNotification("Files saved.", duration = 10) 
    })
  }
  #2iii. R Code: Launch Shiny App
  #Create Shiny Object
  shinyApp(ui = ui, server = server)
}
