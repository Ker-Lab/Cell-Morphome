from numpy.random import randint 
from keras.preprocessing.image import img_to_array 
from keras.preprocessing.image import load_img 


import os 
import matplotlib.pyplot as plt 
import numpy as np
import cv2
import re


def load_images(path):
    img_list = list()
    filename = os.listdir(path)
    sorted_filename = sorted(filename, key=lambda x: [int(s) if s.isdigit() else s.lower() for s in re.split(r'(\d+)', x)])
    for filename in sorted_filename:
        #print(filename)
        file_path=path +'/' +filename
        pixels = cv2.imread(file_path, -1)
        #pixels = cv2.flip(pixels, 1)
        #pixels = cv2.rotate(pixels, cv2.ROTATE_90_COUNTERCLOCKWISE)
        img_list.append(pixels)
    return img_list


# load ground truth images 
#path1 = r'Z:\01_Project01_AI\ablation study for pix2pix\01dataset\Whole_Dataset_cropped_16bit\validation_id\val'
#path1 = r'Z:\01_Project01_AI\ablation study for pix2pix\01dataset\Whole_Dataset_cropped_16bit\training_id'
path1 = r'Z:\01_Project01_AI\ablation study for pix2pix\01dataset\Whole_Dataset_cropped_16bit\test_id'
gt_img = load_images(path1)


# convert (256,256) into (256,256,id_num)
gt_dict=dict()
for r in range(len(gt_img)):
    img = gt_img[r]
    id_list=list(np.unique(img))
    id_list.remove(0)
    num_gt=len(id_list)
    gt_array=[[[0]*num_gt]*256]*256
    gt_array=np.array(gt_array)
    for i in range(num_gt):
        current_id=id_list[i]
        current_mask=np.where(img==current_id, 1, 0)
        gt_array[:,:,i]=current_mask
    gt_dict[r+1]=gt_array

    

#20 sets pd image to load in one dictionary
pd_dict=dict()
for k in [0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24]:
    for j in [0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5]:
        #path = "Z:\\01_Project01_AI\\DataAndAnalysis\\ResearchRecord\\2023525\\wv train\\nucl" + str(k) + "\\cyto" + str(j) + "\\mask_id\\train"
        path = "Z:\\01_Project01_AI\\DataAndAnalysis\\ResearchRecord\\2023525\\wv test\\nucl" + str(k) + "\\cyto" + str(j) + "\\mask_id\\train"
        print("Now loading images in nucl:", k, ',cyto:', j)
        pd_img=load_images(path)
        for r in range(len(pd_img)):
            img = pd_img[r]
            #img=img.astype('int')
            id_list=list(np.unique(img))
            id_list.remove(0)
            num_pd=len(id_list)
            pd_array=[[[0]*num_pd]*256]*256
            pd_array=np.array(pd_array)
            for i in range(num_pd):
                current_id=id_list[i]
                current_mask=np.where(img==current_id, 1, 0)
                pd_array[:,:,i]=current_mask
            if r+1 in pd_dict.keys():
                pd_dict[r+1].append(pd_array)
            else:
                pd_dict[r+1]=[pd_array]





##plot to show the images
n_samples=4
for i in range(n_samples):
    plt.subplot(2, n_samples, 1 + i)
    plt.axis('off')
    plt.imshow(gt_img[i],cmap='gray')

    
for i in range(n_samples):
    plt.subplot(2, n_samples, 1 + n_samples + i)
    plt.axis('off')
    plt.imshow(pd_img[i],cmap='gray')

plt.show()





##calculate IOU
                
def IoU(mask1, mask2):
    mask1_area = np.count_nonzero(mask1 == 1)       # I assume this is faster as mask1 == 1 is a bool array
    mask2_area = np.count_nonzero(mask2 == 1)
    intersection = np.count_nonzero( np.logical_and( mask1, mask2) )
    if mask1_area+mask2_area-intersection != 0:
        iou = intersection/(mask1_area+mask2_area-intersection)
    else:
        iou=0
    return iou
                

###for threshold screening
iou_dict=dict()
for key,val in pd_dict.items():
    pd_array_list=val
    for k in range(len(pd_array_list)):
        print('dealing with predict images: ', key)
        iou_list=[]
        pd_array=pd_array_list[k]
        gt_array=gt_dict[key].copy()
        for i in range(gt_array.shape[2]):  ##set base
            gt_cell=gt_array[:,:,i]
            highest_iou=0
            for t in range(pd_array.shape[2]):
                pd_cell=pd_array[:,:,t]
                IoU_score=IoU(gt_cell, pd_cell)
                if IoU_score>highest_iou:
                    highest_iou=IoU_score
                    pd_array[:,:,t]=[0]
            iou_list.append(highest_iou)
        if key in iou_dict.keys():
            iou_dict[key].append(iou_list)
        else:
            iou_dict[key]=[iou_list]



####calculate precision and recall by using TP,TN,FN
def precision_recall_curve(pred_scores, thresholds, num_pd):
    precisions = []
    recalls = []
    for threshold in thresholds:
        TP=0
        for iou in pred_scores:
            if iou>=threshold:
                TP=TP+1
        FN=len(pred_scores)- TP
        FP=num_pd - TP
        if num_pd and len(pred_scores) !=0:
            precision=TP/(TP+FP)
            recall=TP/(TP+FN)
        elif num_pd and len(pred_scores) == 0:
            precision = 1.0
            recall= 1.0
        else:
            precision = 0
            recall= 0
        precisions.append(precision)
        recalls.append(recall)
    return precisions, recalls


#set thresholds f IOU
thresholds=np.arange(start=0.50, stop=1.00, step=0.05)



# mean average precison for different group of threshold
mAP_dict=dict()
P_dict=dict()
for key, val in iou_dict.items():
    pd_thresholds=pd_dict[key]
    for i in range(len(val)):
        pred_scores=val[i]
        pd_list=pd_thresholds[i]
        num_pd=pd_list.shape[2]
        precisions, recalls = precision_recall_curve(pred_scores=pred_scores, thresholds=thresholds, num_pd= num_pd)
        precisions = np.array(precisions)
        recalls = np.array(recalls)
        mAP = np.sum(precisions)/len(thresholds)
        if key in mAP_dict.keys():
            mAP_dict[key].append(mAP)
            P_dict[key].append(precisions)
        else:
             mAP_dict[key]=[mAP]
             P_dict[key]= [precisions]

    


#find the best mAP
best_mAP_dict=dict()
best_P_dict=dict()
max_sum=0
for key, val in mAP_dict.items():
    max_mAP=0
    max_group_num=0
    P=P_dict[key]
    for i in range(len(val)):
        if val[i]>max_mAP:
            max_mAP = val[i]
            max_group_num=i+1
    max_sum=max_sum+max_mAP
    best_mAP_dict[key]='Group '+ str(max_group_num) +': ' + str(max_mAP)
    best_P_dict[key]=P[max_group_num-1]

best_mAP_dict['Average']= 'Average: ' + str(max_sum/len(mAP_dict))




# write as file  
with open("pix2pix_iou_val.txt", 'w') as f: 
    for key, value in iou_dict.items():
        f.write('Image '+str(key)+':{\n\n')
        for i in range(len(value)):        
            f.write('Group '+str(i+1)+':'+str(value[i])+'}\n\n')
        f.write('\n\n')


#average     
with open("pix2pix_mAP_val_0.55-0.95.txt", 'w') as f: 
    for key, value in mAP_dict.items():
        f.write('Image '+str(key)+':\n\n')
        for i in range(len(value)):        
            f.write('Group '+str(i+1)+':'+str(value[i])+'\n\n')
        f.write('\n\n')


with open("pix2pix_best_mAP_val_0.55-0.95.txt", 'w') as f: 
    for key, value in best_mAP_dict.items():
        f.write('Image '+str(key)+':\n')
        f.write(str(value) +'\n\n')





#average precision
def precision_recall(pred_scores, threshold, num_pd):
    TP=0
    for iou in pred_scores:
        if iou>=threshold:
            TP=TP+1
    FN=len(pred_scores)- TP
    FP=num_pd - TP
    if num_pd and len(pred_scores) !=0:
            precision=TP/(TP+FP)
            recall=TP/(TP+FN)
    elif num_pd and len(pred_scores) == 0:
        precision = 1.0
        recall= 1.0
    else:
        precision = 0
        recall= 0
    return precision, recall


#set threshold
threshold=0.75
precisions_dict = dict()
recalls_dict = dict()
f1_dict = dict()
for key, val in iou_dict.items():
    pd_thresholds=pd_dict[key]
    for i in range(len(val)):
        pred_scores=val[i]
        pd_list=pd_thresholds[i]
        num_pd=pd_list.shape[2]
        precision, recall = precision_recall(pred_scores=pred_scores, threshold=threshold, num_pd= num_pd)
        if precision+recall != 0:
            f1=2*precision*recall/(precision+recall)
        else:
            f1=0
        if key in precisions_dict.keys():
            precisions_dict[key].append(precision)
            recalls_dict[key].append(recall)
            f1_dict[key].append(f1)
        else:
            precisions_dict[key]=[precision]
            recalls_dict[key]=[recall]
            f1_dict[key]=[f1]





#####find the best precisions, recall and f1
#best_precisions_dict=dict()
best_recalls_dict=dict()
best_f1_dict=dict()
best_precision_dict=dict()
f1_sum=0
recall_sum=0
precision_sum=0
for key, val in recalls_dict.items():
    f1_val=f1_dict[key]
    precision_val=precisions_dict[key]
    max_recall=0
    max_f1=0
    max_precision=0
    max_group_num_recall=0
    max_group_num_f1=0
    max_group_num_precision=0
    for i in range(len(val)):
        if val[i]>max_recall:
            max_recall = val[i]
            max_group_num_recall=i+1
        if f1_val[i]>max_f1:
            max_f1=f1_val[i]
            max_group_num_f1=i+1
        if precision_val[i]>max_precision:
            max_precision=precision_val[i]
            max_group_num_precision=i+1
    f1_sum=f1_sum+max_f1
    recall_sum=recall_sum+max_recall
    precision_sum=precision_sum+max_precision
    best_recalls_dict[key]='Group '+ str(max_group_num_recall) +': ' + str(max_recall)
    best_f1_dict[key]='Group '+ str(max_group_num_f1) +': ' + str(max_f1)
    best_precision_dict[key]='Group '+ str(max_group_num_f1) +': ' + str(max_precision)

    
best_recalls_dict['Average']= 'Average: ' + str(recall_sum/len(recalls_dict))
best_f1_dict['Average']= 'Average: ' + str(f1_sum/len(recalls_dict))
best_precision_dict['Average']= 'Average: ' + str(precision_sum/len(precisions_dict))

    
## store as file
with open("pix2pix_precision_val_0.75.txt", 'w') as f: 
    for key, value in precisions_dict.items():
        f.write('Image '+str(key)+':\n\n')
        for i in range(len(value)):        
            f.write('Group '+str(i+1)+':'+str(value[i])+'\n\n')
        f.write('\n\n')


with open("pix2pix_recall_val_0.75.txt", 'w') as f: 
    for key, value in recalls_dict.items():
        f.write('Image '+str(key)+':\n\n')
        for i in range(len(value)):        
            f.write('Group '+str(i+1)+':'+str(value[i])+'\n\n')
        f.write('\n\n')


with open("pix2pix_F1_val_0.75.txt", 'w') as f: 
    for key, value in f1_dict.items():
        f.write('Image '+str(key)+':\n\n')
        for i in range(len(value)):        
            f.write('Group '+str(i+1)+':'+str(value[i])+'\n\n')
        f.write('\n\n')        


with open("pix2pix_best_recall_val_0.75.txt", 'w') as f: 
    for key, value in best_recalls_dict.items():
        f.write('Image '+str(key)+':\n')
        f.write(str(value) +'\n\n')


with open("pix2pix_best_F1_val_0.75.txt", 'w') as f: 
    for key, value in best_f1_dict.items():
        f.write('Image '+str(key)+':\n')
        f.write(str(value) +'\n\n')


with open("pix2pix_best_precision_val_0.75.txt", 'w') as f: 
    for key, value in best_precision_dict.items():
        f.write('Image '+str(key)+':\n')
        f.write(str(value) +'\n\n')


