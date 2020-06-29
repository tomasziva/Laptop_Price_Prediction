rm(list = ls())

packages <- c("dplyr",
              "pls",
              "stringr",
              "tidyverse",
              "ggplot2",
              "Matrix",
              "leaps", #variable selection
              "car", #partial residual plots
              "psych", #pairs.panels
              "blm",
              "betareg",
              "visdat", # graph missing values
              "naniar",
              "VIM",
              "caTools", # to split train data
              "DMwR", # KNN imputation of missing values
              "mice", # Impute missing values both continuous and factor var
              "rpart", # Impute missing values both continuous and factor var
              "aod",
              "randomForest",
              "bst",
              "xgboost",
              "gbm",
              "import",
              "caret",
              "PerformanceAnalytics",
              "forcats",
              "e1071",
              "MLmetrics",
              "pROC",
              "gtools")



package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = T)) install.packages(x)
  if (! (x %in% (.packages() ))) library(x, character.only = T)
})

stepAIC <- MASS::stepAIC
combinations <- gtools::combinations

# load data
# train <- read.csv("train.csv", na.strings = "")
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
GPU <- read.csv("GPU_details.csv", stringsAsFactors = FALSE)

# load full laptops data
laptops <- read.csv("laptops.csv", stringsAsFactors = FALSE)

# find out which of laptops belong to test dataset 
test.label <- test[, c("ï..id", "base_name")]
test.label$dataset <- "test"
laptops <- left_join(laptops, test.label, by = "ï..id")
test <- laptops[(laptops$dataset %in% "test"), ]
test$dataset <- NULL
test$base_name.y <- NULL
test <- test %>% rename(base_name = base_name.x)
#############################################################################################
####################################    Data Exploration    #################################
#############################################################################################

# Extract dimensions
dim(train)
dim(test)

# Descriptive statistics
summary(train)
str(train)


# Print out factor variable values for each observtion
# for (i in colnames(train)){
#   if (!is.numeric(train[,i])){
#     print(i)
#     print(factor(train[,i]))
#   }
# }

# Missingness
vis_miss(train,cluster= TRUE) # VIM package for missing values
gg_miss_var(train)
gg_miss_case(train)

# Correlation
#dev.off()
library(corrgram)
nums <- unlist(lapply(train, is.numeric))
tr22 <- train
colnames(tr22)[12] <- "keyboard"
corrgram(tr22[ ,nums], order=TRUE, upper.panel=panel.cor, font.labels=2, cex.labels=1)



#############################################################################################
############################    Preprocessing train and test data    ########################
#############################################################################################

#------------------- adding additional variables ---------------------------#

# remove whitespaces from GPU and train dataset
train$gpu <- str_trim(train$gpu, "both")
train$gpu <- gsub("  ", " ", train$gpu)
train$gpu <- gsub("[[:space:]]", " ", train$gpu)
test$gpu <- str_trim(test$gpu, "both")
test$gpu <- gsub("  ", " ", test$gpu)
test$gpu <- gsub("[[:space:]]", " ", test$gpu)
GPU$gpu <- paste0(GPU$Brand, " ", GPU$Product.Name)
GPU$gpu <- gsub("  ", " ", GPU$gpu)
GPU$gpu <- gsub("   ", " ", GPU$gpu)
GPU$gpu <- gsub("[[:space:]]", " ", GPU$gpu)
GPU$gpu <- str_trim(GPU$gpu, "both")

#add videocards without word 'Graphics' in the name (since it is not present in train dataset's GPU column)
fix <- gsub("Graphics", "", GPU$gpu)
fix <- gsub("  ", " ", fix)
fix <- gsub("   ", " ", fix)
fix <- gsub("[[:space:]]", " ", fix)
fix <- str_trim(fix, "both")
# fix <- str_replace_all(fix, fixed(" "), "")
GPU$fix <- fix
GPU_t <- unique(rbind(GPU %>% select(-fix), GPU %>% select(-gpu) %>% rename(gpu = fix)))
GPU_t <- GPU_t[!duplicated(GPU_t$gpu), ]

#join GPU details with train and test datasets
train_2 <- unique(left_join(train, GPU_t[, -(2:5)], by = "gpu"))
test_2 <- unique(left_join(test, GPU_t[, -(2:5)], by = "gpu"))
# View(train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), ])

#replace unknown video cards with most popular ones for AMD, NVIDIA, Intel
#select top 1 most popular gpu per brand
vids <- train_2 %>% group_by(Brand, gpu) %>% summarise(N = n()) %>% top_n(1)
cnames <- colnames(GPU_t[, -c(2:5, 10)])

#replace unknown AMD GPUs with most popular AMD gpu in dataset
Amd <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("AMD", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Amd2 <- Amd[rep(row.names(Amd), dim(train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), cnames])[1]),]

#replace unknown Intel GPUs with most popular Intel gpu in dataset
Intel <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("Intel", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Intel <- Intel[rep(row.names(Intel), dim(train_2[is.na(train_2$Memory) & grepl("Intel", train_2$gpu), cnames])[1]),]

#replace unknown NVIDIA GPUs with most popular NVIDIA gpu in dataset
Nvidia <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("NVIDIA", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Nvidia <- Nvidia[rep(row.names(Nvidia), dim(train_2[is.na(train_2$Memory) & grepl("NVIDIA", train_2$gpu), cnames])[1]),]

train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), cnames] <- Amd2
train_2[is.na(train_2$Memory) & grepl("Intel", train_2$gpu), cnames] <- Intel
train_2[is.na(train_2$Memory) & grepl("NVIDIA", train_2$gpu), cnames] <- Nvidia

#replace Other GPUs or blank GPUs with most popular AMD gpu (due to AMD being most popular dedicated GPU)
train_2$gpu[train_2$gpu == ""] <- "Unknown"
Other <- Amd[rep(row.names(Amd), dim(train_2[is.na(train_2$Memory), cnames])[1]),]
#replace brand with brands (1st name in GPU name) from our original dataset
Other$Brand <- unlist(lapply(strsplit(train_2$gpu[is.na(train_2$Memory)], " "), `[[`, 1))
train_2[is.na(train_2$Memory), cnames] <- Other


# Reduce(setdiff, strsplit(c(GPU$fix[1243], test$gpu[234]), split = ""))
# Reduce(setdiff, strsplit(c(GPU$fix[1244], test$gpu[1329]), split = ""))

# SSD is possible substitute for HDD(storage), so these features should not be analyzed independently but in relation with each oth

# Split the string into individual words, find GHz and extract it
splitString <- strsplit(train_2$cpu_details[train_2$cpu_details != ""], " ")
cpu_ghz <- c()
for (i in 1:length(splitString)) {
  loc <- grep("GHz", splitString[[i]])
  if (length(loc) != 0) {
    cpu_ghz[[i]] <- as.numeric(splitString[[i]][loc-1])
  }
}

# add GHz column to train dataset
train_2$ghz <- NA
train_2$ghz[train_2$cpu_details != ""] <- cpu_ghz

# add threading variable, remove excess parenthesis
train_2$thread <- sapply(strsplit(train_2$cpu_details, ' ', fixed = TRUE), function(i) 
  paste(grep('Thread', i, value = TRUE, fixed = TRUE), collapse = ' '))
train_2$thread <- gsub(")", "", train_2$thread)
#blank values replaced with no-threading
train_2$thread[train_2$thread == ""] <- "No-Threading"

# Split the string into individual words, find GPU memory and extract it
splitString_gpu <- strsplit(train_2$Memory, " ")
gpu_mb <- c()
for (i in 1:length(splitString_gpu)) {
  loc_gb <- grep("GB", splitString_gpu[[i]])
  loc_mb <- grep("MB", splitString_gpu[[i]])
  if (length(loc_gb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_gb-1]) * 1024
  }
  else if (length(loc_mb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_mb-1])
  } else {
    gpu_mb[[i]] <- 0
  }
}
# View(cpu_ghz)
# add memory variable to train dataset
train_2$gpu_memory <- gpu_mb
train_2$Memory <- NULL

#add Shaders, TMU, ROP for gpu
t <- str_split_fixed((train_2$`Shaders...TMUs...ROPs`)," / ", 3) %>% as.data.frame()
train_2$gpu_shaders <- as.numeric(t$V1)
train_2$gpu_tmu <- as.numeric(t$V2)
train_2$gpu_rop <- as.numeric(t$V3)
train_2$Shaders...TMUs...ROPs <- NULL

#add cores variable to train dataset (extracted from cpu_details)
train_2$cores <- NA
train_2$cores[grep("Dual-Core", train_2$cpu_details)] <- 2  
train_2$cores[grep("Quad-Core", train_2$cpu_details)] <- 4
train_2$cores[grep("Hexa-Core", train_2$cpu_details)] <- 6
train_2$cores[grep("Octa-Core", train_2$cpu_details)] <- 8 

#create apple brand premium as dummy variable
train_2$brand_premium <- ifelse(train_2$brand == "Apple", 1, 0)

#change GPU.clock to numeric variable
train_2$GPU.clock <- as.numeric(gsub(" MHz", "", train_2$GPU.clock))

# change Memory clock to numeric
train_2$Memory.clock[grepl("System Shared", train_2$Memory.clock)] <- "0"
train_2$Memory.clock <- as.numeric(gsub(" MHz", "", train_2$Memory.clock))


# screen_surface - to lowercase (previously Matte and matte as separate classes)
train_2$screen_surface <- tolower(train_2$screen_surface)

# 1 if bluetooth present
train_2$bluetooth <- as.numeric(str_detect(tolower(train_2$name), "bluetooth"))

# 1 if webcam present
train_2$webcam <- as.numeric(str_detect(tolower(train_2$name), "webcam"))

# 1 if usb present
train_2$usb <- as.numeric(str_detect(tolower(train_2$name), "usb"))

# 1 if wifi present
train_2$wifi <- as.numeric(str_detect(tolower(train_2$name), "wifi"))

# 1 if dvd present
train_2$dvd <- as.numeric(str_detect(tolower(train_2$name), "dvd"))

# 1 if hdmi present
train_2$hdmi <- as.numeric(str_detect(tolower(train_2$name), "hdmi"))

# 1 if fingerprint reader present
train_2$fingerprint <- as.numeric(str_detect(tolower(train_2$name), "fingerprint"))

# 1 if gold colour reader present
train_2$gold <- as.numeric(str_detect(tolower(train_2$name), "gold"))

# number of perks (dvd, hdmi, fingerprint, gold, etc) as a percentage
train_2$perks <- (train_2$bluetooth + train_2$webcam + train_2$usb +
                    train_2$wifi + train_2$dvd + train_2$hdmi + train_2$fingerprint + 
                    train_2$gold)/8

# delete unneeded columns
train_2$bluetooth <- NULL
train_2$webcam <- NULL
train_2$usb <- NULL
train_2$wifi <- NULL
train_2$dvd <- NULL
train_2$hdmi <- NULL
train_2$fingerprint <- NULL
train_2$gold <- NULL

# standardize column names
train_2 <- train_2 %>%  rename(gpu_brand=Brand, 
                               gpu_clock=GPU.clock,
                               gpu_memory_clock=Memory.clock)


#factorize categorical vars
#train_2$screen_surface[train_2$screen_surface == ""] <- "glossy"
#train_2$thread <- as.factor(train_2$thread)
#train_2$screen_surface <- as.factor(train_2$screen_surface)
#train_2$os <- as.factor(train_2$os)


############################ the same procedure with test data #################################


#replace unknown AMD GPUs with most popular AMD gpu in dataset
Amd <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("AMD", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Amd2 <- Amd[rep(row.names(Amd), dim(test_2[is.na(test_2$Memory) & grepl("AMD", test_2$gpu), cnames])[1]),]

#replace unknown Intel GPUs with most popular Intel gpu in dataset
Intel <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("Intel", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Intel <- Intel[rep(row.names(Intel), dim(test_2[is.na(test_2$Memory) & grepl("Intel", test_2$gpu), cnames])[1]),]

#replace unknown NVIDIA GPUs with most popular NVIDIA gpu in dataset
Nvidia <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("NVIDIA", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Nvidia <- Nvidia[rep(row.names(Nvidia), dim(test_2[is.na(test_2$Memory) & grepl("NVIDIA", test_2$gpu), cnames])[1]),]

test_2[is.na(test_2$Memory) & grepl("AMD", test_2$gpu), cnames] <- Amd2
test_2[is.na(test_2$Memory) & grepl("Intel", test_2$gpu), cnames] <- Intel
test_2[is.na(test_2$Memory) & grepl("NVIDIA", test_2$gpu), cnames] <- Nvidia

#replace Other GPUs or blank GPUs with most popular AMD gpu (due to AMD being most popular dedicated GPU)
test_2$gpu[test_2$gpu == ""] <- "Unknown"
Other <- Amd[rep(row.names(Amd), dim(test_2[is.na(test_2$Memory), cnames])[1]),]
#replace brand with brands (1st name in GPU name) from our original dataset
Other$Brand <- unlist(lapply(strsplit(test_2$gpu[is.na(test_2$Memory)], " "), `[[`, 1))
test_2[is.na(test_2$Memory), cnames] <- Other

# Split the string into individual words, find GHz and extract it
test_2$cpu_details <- as.character(test_2$cpu_details)
splitString <- strsplit(test_2$cpu_details[test_2$cpu_details != ""], " ")
cpu_ghz <- c()
for (i in 1:length(splitString)) {
  loc <- grep("GHz", splitString[[i]])
  if (length(loc) != 0) {
    cpu_ghz[[i]] <- as.numeric(splitString[[i]][loc-1])
  }
}

#add GHz column to test dataset
test_2$ghz <- NA
test_2$ghz[test_2$cpu_details != ""] <- cpu_ghz

#add threading variable, remove excess parenthesis
test_2$thread <- sapply(strsplit(test_2$cpu_details, ' ', fixed = TRUE), function(i) 
  paste(grep('Thread', i, value = TRUE, fixed = TRUE), collapse = ' '))
test_2$thread <- gsub(")", "", test_2$thread)
#blank values replaced with no-threading
test_2$thread[test_2$thread == ""] <- "No-Threading"

# Split the string into individual words, find GPU memory and extract it
splitString_gpu <- strsplit(test_2$Memory, " ")
gpu_mb <- c()
for (i in 1:length(splitString_gpu)) {
  loc_gb <- grep("GB", splitString_gpu[[i]])
  loc_mb <- grep("MB", splitString_gpu[[i]])
  if (length(loc_gb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_gb-1]) * 1024
  }
  else if (length(loc_mb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_mb-1])
  } else {
    gpu_mb[[i]] <- 0
  }
}
# View(cpu_ghz)
#add memory variable to test dataset
test_2$gpu_memory <- gpu_mb
test_2$Memory <- NULL

#add Shaders, TMU, ROP for gpu
t <- str_split_fixed((test_2$`Shaders...TMUs...ROPs`)," / ", 3) %>% as.data.frame()
test_2$gpu_shaders <- as.numeric(t$V1)
test_2$gpu_tmu <- as.numeric(t$V2)
test_2$gpu_rop <- as.numeric(t$V3)
test_2$Shaders...TMUs...ROPs <- NULL

#add cores variable to test dataset (extracted from cpu_details)
test_2$cores <- NA
test_2$cores[grep("Dual-Core", test_2$cpu_details)] <- 2  
test_2$cores[grep("Quad-Core", test_2$cpu_details)] <- 4
test_2$cores[grep("Hexa-Core", test_2$cpu_details)] <- 6
test_2$cores[grep("Octa-Core", test_2$cpu_details)] <- 8 

#create apple brand premium as dummy variable
test_2$brand_premium <- ifelse(test_2$brand == "Apple", 1, 0)

#change GPU.clock to numeric variable
test_2$GPU.clock <- as.numeric(gsub(" MHz", "", test_2$GPU.clock))

# change Memory clock to numeric
test_2$Memory.clock[grepl("System Shared", test_2$Memory.clock)] <- "0"
test_2$Memory.clock <- as.numeric(gsub(" MHz", "", test_2$Memory.clock))

# screen_surface - to lowercase (previously Matte and matte as separate classes)
test_2$screen_surface <- tolower(test_2$screen_surface)

# 1 if bluetooth present
test_2$bluetooth <- as.numeric(str_detect(tolower(test_2$name), "bluetooth"))

# 1 if webcam present
test_2$webcam <- as.numeric(str_detect(tolower(test_2$name), "webcam"))

# 1 if usb present
test_2$usb <- as.numeric(str_detect(tolower(test_2$name), "usb"))

# 1 if wifi present
test_2$wifi <- as.numeric(str_detect(tolower(test_2$name), "wifi"))

# 1 if dvd present
test_2$dvd <- as.numeric(str_detect(tolower(test_2$name), "dvd"))

# 1 if hdmi present
test_2$hdmi <- as.numeric(str_detect(tolower(test_2$name), "hdmi"))

# 1 if fingerprint reader present
test_2$fingerprint <- as.numeric(str_detect(tolower(test_2$name), "fingerprint"))

# 1 if gold colour reader present
test_2$gold <- as.numeric(str_detect(tolower(test_2$name), "gold"))

# number of perks (dvd, hdmi, fingerprint, gold, etc) as a percentage
test_2$perks <- (test_2$bluetooth + test_2$webcam + test_2$usb +
                   test_2$wifi + test_2$dvd + test_2$hdmi + test_2$fingerprint + 
                   test_2$gold)/8

test_2$bluetooth <- NULL
test_2$webcam <- NULL
test_2$usb <- NULL
test_2$wifi <- NULL
test_2$dvd <- NULL
test_2$hdmi <- NULL
test_2$fingerprint <- NULL
test_2$gold <- NULL




#rename
test_2 <- test_2 %>%  rename(gpu_brand=Brand, 
                             gpu_clock=GPU.clock,
                             gpu_memory_clock=Memory.clock)

#factorize categorical vars
#test_2$screen_surface[test_2$screen_surface == ""] <- "glossy"
#test_2$thread <- as.factor(test_2$thread)
#test_2$screen_surface <- as.factor(test_2$screen_surface)
#test_2$os <- as.factor(test_2$os)




#rename
colnames(train_2)[1] <- "id" # renames column
colnames(test_2)[1] <- "id" # renames column



# change all empty cells to NA
train_2 <- train_2 %>% mutate_all(na_if,"")
test_2 <- test_2 %>% mutate_all(na_if,"")



#--------View missing valuesVIM package
vis_miss(train_2,cluster= TRUE) # VIM package for missing values
gg_miss_var(train_2) + theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))
gg_miss_case(train_2) + theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))


#-------- Imputation of NA values with kNN on train dataset

# remove unneeded columns
train_clean1 <- train_2
train_clean1$name <- NULL
train_clean1$base_name <- NULL
train_clean1$cpu_details <- NULL
train_clean1$os_details <- NULL
train_clean1$gpu <- NULL

# factorize categorical variables
train_clean1$brand <- as.factor(train_clean1$brand)
train_clean1$cpu <- as.factor(train_clean1$cpu)
train_clean1$gpu_brand <- as.factor(train_clean1$gpu_brand)
train_clean1$screen_surface <- as.factor(train_clean1$screen_surface)
train_clean1$os <- as.factor(train_clean1$os)
train_clean1$thread <- as.factor(train_clean1$thread)


aggr(x = train_clean1)
glimpse(train_clean1)

# kNN imputation
set.seed(123)
clean2_knn <- knnImputation(train_clean1)

aggr(x=clean2_knn)
clean2_knn %>%
  summarise_if(is.factor,nlevels) # too many levels in cpu -- do not use
glimpse(clean2_knn)



#-------- Check for multicollinearity

cor(clean2_knn$pixels_x, clean2_knn$pixels_y) #-- very high correlation -- use only pixels_y


vars3 <- colnames(clean2_knn)
VIF <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF) # gpu_memory_clock VIF 41.62 - delete variable

#no gpu_memory_clock
VIF2 <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("gpu_memory_clock", "id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF2) # discrete_gpu VIF 14.32 - delete variable

#no gpu_memory_clock and discrete_gpu
VIF3 <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("discrete_gpu", "gpu_memory_clock", "id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF3) # All VIFs < 10 no more multicollinearity

mean(diag(VIF3)) # not much larger than 1 -- good


clean2_knn$discrete_gpu <- NULL
clean2_knn$gpu_memory_clock <- NULL

nums <- unlist(lapply(clean2_knn, is.numeric))
tr22 <- clean2_knn
colnames(tr22)[9] <- "keyboard"
corrgram(tr22[ ,nums], order=TRUE, upper.panel=panel.cor, font.labels=2, cex.labels=1.5, cex = 2) 



#-------- Imputation of NA values with kNN on test dataset

vis_miss(test_2,cluster= TRUE) # VIM package for missing values
gg_miss_var(test_2)
gg_miss_case(test_2)


test_clean1 <- test_2
test_clean1$name <- NULL
test_clean1$base_name <- NULL
test_clean1$cpu_details <- NULL
test_clean1$os_details <- NULL
test_clean1$gpu <- NULL


test_clean1$brand <- as.factor(test_clean1$brand)
test_clean1$cpu <- as.factor(test_clean1$cpu)
test_clean1$gpu_brand <- as.factor(test_clean1$gpu_brand)
test_clean1$screen_surface <- as.factor(test_clean1$screen_surface)
test_clean1$os <- as.factor(test_clean1$os)
test_clean1$thread <- as.factor(test_clean1$thread)


aggr(x = test_clean1)
glimpse(test_clean1)
dist_data <- train_clean1
dist_data$min_price <- NULL
dist_data$max_price <- NULL
set.seed(123)
clean2_knn_test <- knnImputation(test_clean1 %>% select(-c(min_price, max_price)), distData = dist_data)
aggr(x=clean2_knn_test)

clean2_knn_test %>%
  summarise_if(is.factor,nlevels)

clean2_knn_test$discrete_gpu <- NULL
clean2_knn_test$gpu_memory_clock <- NULL
clean2_knn_test$min_price <- test$min_price
clean2_knn_test$max_price <- test$max_price





#############################################################################################
#########################    Cross-validation    ############################################
#############################################################################################

## DO NOT USE THIS, BECAUSE WE USE CROSS-VALIDATION AND TEST MODELS IN THE LEADERBOARD
# set.seed(741)
# train_index <- sample(1:nrow(clean2_knn), 0.8 * nrow(clean2_knn))
# test_index <- setdiff(1:nrow(clean2_knn), train_index)

# data.training <- clean2_knn[train_index,]
# data.test <- clean2_knn[test_index,]



#-------------- 3 different methods for repeated cv:


###### 1 default

# Training control definition
# set.seed(123)
# train.control <- trainControl(method = "repeatedcv",
#                              number = 5, repeats = 3)


# Used this cv to tune parameter ntrees for random forest. Results from the leaderboard:

# ntrees = 50   error: 333.71915917
# ntrees = 100  error: 329.98914464
# ntrees = 150  error: 329.22929872
# ntrees = 200  error: 330.05413688
# ntrees = 250  error: 328.63883385
# ntrees = 300  error: 328.48849774
# ntrees = 350  error: 329.42086368
# ntrees = 500  error: 331.73181822
# ntrees = 1000 error: 333.76290307
# ntrees = 1500 error: 335.35766153

### so choose ntrees = 300


## However, default repeated cv only tries 3 different values for mtry (another parameter of random forest)
## so tried grid search and random search with more checked values for mtry


###### 2 grid search -- better than random, a bit worse than default (error 334.64918312)
## (however, larger grid of mtry values tried, so better generalization)

# set model controls for cross validation
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                                number = 5, repeats = 3, search = "grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:25)) #(AND INSIDE TRAIN FUNCTION NEAR NTREES WRITE tuneGrid=tunegrid)


###### 3 random search -- produced worse results (error 342.93214978)

#set.seed(123)
#train.control <- trainControl(method = "repeatedcv",
#                              number = 5, repeats = 3, search = "random")
# (AND INSIDE TRAIN FUNCTION NEAR NTREES WRITE tuneLength=25)


#############################################################################################
################################    Modelling    ############################################
#############################################################################################



#------------ Training with MAX_PRICE and MIN_PRICE ------------#

#### Selecting features to use (and creating dummies)

#maxPrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, max_price)
#maxPrice_Clean_Training <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev))

minPrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, min_price)
#minPrice_Clean_Training <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev))


#------------ Train for MAX_PRICE

##### Train the model 5 eXtreme Gradient Boosting
#set.seed(123)
#model5_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
#set.seed(123)
#model6_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 7 Stochastic Gradient Boosting
#set.seed(123)
#model7_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                    method = "gbm", trControl = train.control, metric = "MAE")

#------------ Train for MIN_PRICE

##### Train the model 5 eXtreme Gradient Boosting
#set.seed(123)
#model5_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
set.seed(123)
model6_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE", ntree = 300, mtry = 2,
                    tuneGrid=tunegrid, importance = T)
# imp <- importance(model6_min, type=1, scale = F) # permutation importances
varImp(model6_min)
##### Train the model 7 Stochastic Gradient Boosting
#set.seed(123)
#model7_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                    method = "gbm", trControl = train.control, metric = "MAE")


#------- Summarize the results ----------------

#print(min(model5_max$results$MAE+model5_min$results$MAE))
#print(min(model6_max$results$MAE+model6_min$results$MAE))
#print(min(model7_max$results$MAE+model7_min$results$MAE))




# -------- Prediction of test data 

# Test data not normalized
#Price_Test <- clean2_knn_test %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks)

# ----------- Results ------------------

#id_test <- clean2_knn_test %>% select(id)


#Model 5
# bothModels <- list(model5_min ,model5_max)
# pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
# names(pred) <- c("MIN","MAX")
# 
# results <- cbind(id_test,pred)
# 
# write.csv(results, file = "Model 5 (Extreme Gradient Boosting) - min max price.csv", row.names = F)


#Model 6
# bothModels <- list(model6_min ,model6_max)
# pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
# names(pred) <- c("MIN","MAX")
# 
# results <- cbind(id_test,pred)
# 
# write.csv(results, file = "Model 6 (Parallel Random Forest) - min max price.csv", row.names = F)


#Model 7
# bothModels <- list(model7_min ,model7_max)
# pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
# names(pred) <- c("MIN","MAX")
# 
# results <- cbind(id_test,pred)
# results
# 
# write.csv(results, file = "Model 7 (Stochastic Gradient Boosting) - min max price.csv", row.names = F)


#------------ Training with RANGE and MIN_PRICE ------------#


# Selecting features to use (and creating dummies)
clean2_knn$price_range <- clean2_knn$max_price - clean2_knn$min_price
rangePrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, price_range)
#rangePrice_Clean_Training <- data.frame(model.matrix(~., data=rangePrice_Clean_Training_prev))

##### Train the model 5 eXtreme Gradient Boosting
# set.seed(123)
# model5_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
#                     method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
set.seed(123)
model6_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE", ntree = 300, mtry = 2, tuneGrid=tunegrid,
                    importance = T)
# imp <- importance(model6_range, type=1, scale = F) # permutation importances
varImp(model6_range)

##### Train the model 7 Stochastic Gradient Boosting
# set.seed(123)
# model7_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
#                     method = "gbm", trControl = train.control, metric = "MAE")
# 

model7_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "RMSE", ntree = 300, mtry = 2,
                    tuneGrid=tunegrid, importance = T)

model7_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
                      method = "parRF", trControl = train.control, metric = "RMSE", ntree = 300, mtry = 2, tuneGrid=tunegrid,
                      importance = T)


# -------- Prediction of test data

# Test data not normalized
Price_Test <- clean2_knn_test %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks)

# ----------- Results ------------------

id_test <- clean2_knn_test %>% select(id)


#Model 5
# bothModels <- list(model5_min ,model5_range)
# pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
# names(pred) <- c("MIN","DIFFERENCE")
# pred$MAX <- pred$MIN + pred$DIFFERENCE
# pred$DIFFERENCE <- NULL
# 
# results <- cbind(id_test,pred)
# results
# 
# write.csv(results, file = "Model 5 (Extreme Gradient Boosting) - min range price.csv", row.names = F)
# 
# 
#Model 6
bothModels <- list(model6_min ,model6_range)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","DIFFERENCE")

bothModels2 <- list(model7_min ,model7_range)
pred2 <- data.frame(predict(bothModels2, Price_Test, type = "raw"))
names(pred2) <- c("MIN_RMSE","DIFFERENCE_RMSE")

pred$MAX <- pred$MIN + pred$DIFFERENCE
# pred$DIFFERENCE <- NULL

pred2$MAX_RMSE <- pred2$MIN_RMSE + pred2$DIFFERENCE_RMSE
# pred2$DIFFERENCE_RMSE <- NULL

results <- cbind(id_test,pred)
results2 <- cbind(id_test,pred2)

results_all <- left_join(results, test_2[, c("min_price", "max_price", "id", "name")], by = "id")
results_all <- left_join(results_all, results2, by = "id")
results_all$residual_max <- results_all$max_price - results_all$MAX
results_all$residual_min <- results_all$min_price - results_all$MIN
results_all$residual_diff <- (results_all$max_price - results_all$max_price)  - results_all$DIFFERENCE
results_all$residual_max_rmse <- results_all$max_price - results_all$MAX_RMSE
results_all$residual_min_rmse <- results_all$min_price - results_all$MIN_RMSE
results_all$residual_diff_rmse <- (results_all$max_price - results_all$max_price)  - results_all$DIFFERENCE_RMSE

plot(1:222, results_all$residual_max, type = "l")
line(1:222, results_all$residual_max_rmse)
plot(1:222, results_all$residual_min, type = "l")

mean(abs(results_all$residual_max)) #max
mean(abs(results_all$residual_min)) #min

mean(abs(results_all$residual_max_rmse)) #max
mean(abs(results_all$residual_min_rmse)) #min

ggplot(results_all, aes(x=1:222)) + 
  geom_line(aes(y = residual_max), color = "darkred") + 
  geom_line(aes(y = residual_max_rmse), color="steelblue", linetype="twodash") 

ggplot(results_all, aes(x=1:222)) + 
  geom_line(aes(y = residual_min), color = "darkred") + 
  geom_line(aes(y = residual_min_rmse), color="steelblue", linetype="twodash") 

ggplot(results_all, aes(x=1:222)) + 
  geom_line(aes(y = residual_diff), color = "darkred", size = 0.75) + 
  geom_line(aes(y = residual_diff_rmse), color="steelblue", linetype="twodash", size = 0.75)  +
  labs(y= "Residual", x = "Index", size = 16)

# write.csv(results, file = "Model 6 (Parallel Random Forest) - best model so far 300 trees grid search with tunegrid and cv number 8 repeats 4.csv", row.names = F)

print(model6_min)
print(model6_range)

print(model6_min$finalModel)
print(model6_range$finalModel)

#Model 7
# bothModels <- list(model7_min ,model7_range)
# pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
# names(pred) <- c("MIN","DIFFERENCE")
# pred$MAX <- pred$MIN + pred$DIFFERENCE
# pred$DIFFERENCE <- NULL
# 
# results <- cbind(id_test,pred)
# results
# 
# write.csv(results, file = "Model 7 (Stochastic Gradient Boosting) - min range price.csv", row.names = F)

### Feature importance using Boruta package ###

library(Boruta)
set.seed(123)
str(clean2_knn)
x1<-clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard,
                          os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu,
                          gpu_rop, cores, brand_premium, perks, max_price)
str(x1)
x1_train <- Boruta(x1$max_price~., data = x1, doTrace = 2, ntree = 300, mtry = 2)
print(x1_train)
boruta.x1 <- TentativeRoughFix(x1_train)
print(boruta.x1)
boruta.x1[["finalDecision"]]
par(mar=c(10,4,1,1))  
plot(boruta.x1,  las=2, cex.lab=1, cex.axis=1, font=1,col.axis="black", xlab = "", ylab = "Importance")
mtext("Attributes", side=1, line=8)
cat("\n\nAttribute Importance Details:\n")
options(width=125)
data<-arrange(cbind(attr=rownames(attStats(boruta.x1)), attStats(boruta.x1)),desc(medianImp))

set.seed(123)
str(clean2_knn)
x1<-clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard,
                          os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu,
                          gpu_rop, cores, brand_premium, perks, min_price)
str(x1)
x1_train <- Boruta(x1$min_price~., data = x1, doTrace = 2, ntree = 300, mtry = 2)
print(x1_train)
boruta.x1 <- TentativeRoughFix(x1_train)
print(boruta.x1)
boruta.x1[["finalDecision"]]
par(mar=c(10,4,1,1))  
plot(boruta.x1,  las=2, cex.lab=1, cex.axis=1, font=1,col.axis="black", xlab = "", ylab = "Importance")
mtext("Attributes", side=1, line=8)
cat("\n\nAttribute Importance Details:\n")
options(width=125)
data2<-arrange(cbind(attr=rownames(attStats(boruta.x1)), attStats(boruta.x1)),desc(medianImp))
# write.csv(data2,"importance ranking2.csv")


#############################################################################################
#################################    Data Exploration    ####################################
#############################################################################################

data.training <- clean2_knn
p <- dim(data.training)[2]

# Histograms for numeric variables
for (i in 1:p){
  if (is.numeric(data.training[,i])){
    hist(data.training[,i], xlab = colnames(data.training)[i])
  }
}


# Boxplots for numeric variables
for (i in 1:p){
  if (is.numeric(data.training[,i])){
    boxplot(data.training[,i], xlab = colnames(data.training)[i])
  }
}


# Boxplot of max_price by brand
par(mfrow = c(1,1))
boxplot(max_price~brand, data = train_2)

# Scatter plots for numeric variables
par(mfrow = c(3,3))
for (i in 1:p){
  #  for (j in 1:p){
  for (j in c("min_price")){
    if (i != j &  is.numeric(data.training[,i]) & is.numeric(data.training[,j])){
      plot(data.training[,i], data.training[,j], xlab = colnames(data.training)[i], ylab = colnames(data.training)[j])
      if (i!=12 & i!=20 &i!=26&i!=32&i!=42) {lines(lowess(data.training[,i], data.training[,j]), col="red")}
      
    }
  }
}


glimpse(data.training)
# res<-cor(data.training[,-c(1,2,6,8,10,17,20 )])
# corrplot(res, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)

#MAE evaluation
tree.frame <- data.frame(ntrees = c(50,100,150,200,250,300,350,500,1000,1500), 
                         error = c(333.71915917, 329.98914464, 329.22929872,330.05413688,
                                   328.63883385, 328.48849774, 329.42086368, 331.73181822, 333.76290307, 335.35766153))

ggplot(data=tree.frame, aes(x=ntrees, y=error)) +
  geom_line(color="black") +
  geom_point(color="red") + 
  labs(y= "MAE", x = "Number of Trees", size = 16) +
  scale_x_continuous(breaks = seq(100, 1500, by = 100)) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=24))


