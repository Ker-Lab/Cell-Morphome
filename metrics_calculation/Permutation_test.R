# Set working directory
setwd("please input your file location here")

# Load the dataset (csv file)
d <- read.csv("file name.csv", header = T, sep = ",")

# check the names 
names(d) 
levels(d$group) 

# how many observations in each group? 
table(d$group) 

# generate a boxplot of area by those 2 groups
boxplot(d$parameter~d$group, las = 1,  
        ylab = "parameter",  
        xlab = "group", 
        main = "parameter") 

# calculate the difference in sample MEANS 
mean(d$parameter[d$group == "group name for groundtruths"]) # mean for Groundtruths
mean(d$parameter[d$group == "group name for predictions"]) # mean for Predictions

# calculate the absolute difference in means 
test.stat <- abs(mean(d$parameter[d$group == "group name for groundtruths"]) -  
                   mean(d$parameter[d$group == "group name for predictions"]))  
test.stat 

# Permutation Test 

# for reproducibility of results 
set.seed(123)   

# the number of observations to sample 
n <- length(d$group) 

# the number of permutation samples to take 
P <- 999  

# the variable we will resample from  
variable <- d$parameter

# initialize a matrix to store the permutation data 
PermSamples <- matrix(0, nrow = n, ncol = P) 

# each column is a permutation sample of data 
# now, get those permutation samples, using a loop 
for(i in 1:P) 
{ 
  PermSamples[, i] <- sample(variable,  
                             size = n,  
                             replace = FALSE) 
} 

# initialize vectors to store all of the Test-stats 
Perm.test.stat <- rep(0, P) 

# loop thru, and calculate the test-stats 
for (i in 1:P) 
{ 
  # calculate the perm-test-stat and save it 
  Perm.test.stat[i] <- abs(mean(PermSamples[d$group == "group name for groundtruths",i]) -  
                             mean(PermSamples[d$group == "group name for predictions",i])) 
} 

# Calculate the p-value, for all P = 999
mean(Perm.test.stat >= test.stat)

# Correction of the p-value
p_value <- (sum(Perm.test.stat >= test.stat) + 1) / (length(Perm.test.stat) + 1)
print(p_value)
