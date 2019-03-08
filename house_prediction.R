library(tidyverse)
library(data.table)
library(dplyr)
library(plyr)
library(gbm)
library(randomForest)

train.data <- data.frame(fread(file = '../input/train.csv'))
test.data <- data.frame(fread(file = '../input/test.csv'))

# Indicators to determine whether a given row when combined will be train/test data
train.data$isTrain <- TRUE
test.data$isTrain <- FALSE

# Columns with excessive NA, > 70%
drop_cols <- c("PoolQC", "Fence", "MiscFeature", "Alley")

# Use rfImpute to remove NA and run RF
train.data.imputed <- mutate_if(train.data, is.character, as.factor)
train.data.imputed <- train.data.imputed[, !names(train.data.imputed) %in% drop_cols]
train.data.imputed$isTrain <- NULL
train.data.imputed <- rfImpute(x = SalePrice ~ ., data = train.data.imputed)

test_rf <- randomForest(formula = SalePrice ~., data = train.data.imputed, ntree = 400)
importance(test_rf)


et SalePrice for test data to NA in order to bind the two tables
test.data$SalePrice <- NA

# Bind two tables to data clean both simultaneously
full.data <- rbind(train.data, test.data)

# Change all character columns to factors
full.data <- mutate_if(full.data, is.character, as.factor)

# Columns that contain very large amounts of missing values,
# easier to just omit them (~1200+/>70% of dataset)
full.data <- full.data[,!names(full.data) %in% drop_cols] 

na_count <- data.frame(col_name = colnames(full.data),
                       na_count = sapply(full.data, function(y) sum(length(which(is.na(y))))))
na_count <- filter(na_count, na_count > 0)
                                          
# Basement Data Cleaning
# Replace NA with mode, very small size of NAs being replaced, so this is acceptible
# NA = 81, NA = 82, NA = 82, NA = 79, NA = 80
full.data[is.na(full.data$BsmtQual), "BsmtQual"] <- 'TA'
full.data[is.na(full.data$BsmtCond), "BsmtCond"] <- 'TA'                     
full.data[is.na(full.data$BsmtExposure), "BsmtExposure"] <- 'No'        
full.data[is.na(full.data$BsmtFinType1), "BsmtFinType1"] <- 'Unf' 
full.data[is.na(full.data$BsmtFinType2), "BsmtFinType2"] <- 'Unf'    
full.data[is.na(full.data$BsmtFinSF2), "BsmtFinSF2"] <- 0                                         
full.data[is.na(full.data$BsmtUnfSF), "BsmtUnfSF"] <- 0                                                                                
full.data[is.na(full.data$TotalBsmtSF), "TotalBsmtSF"] <- 0
                
# MasVnr Data Cleaning
# Replace NA with mode, very small size of NAs being replaced, so this is acceptible
# NA = 24, NA = 23                                       
full.data[is.na(full.data$MasVnrType), "MasVnrType"] <- 'None'
full.data[is.na(full.data$MasVnrArea), "MasVnrArea"] <- median(na.omit(full.data$MasVnrArea))


                                                                   
# Garage Data Cleaning
# Replace NA with mode, very small size of NAs being replaced, so this is acceptible
# NA = 157, NA = 159, NA = 159, NA = 159, NA = 159                                                 
full.data[is.na(full.data$GarageType), "GarageType"] <- 'Attchd'  

full.data[,"GarageYrBlt"] <- full.data$GarageYrBlt                              
# Make randomizer based off discrete probability distribution using sample()
GarageYrBlt.map <- count(full.data, "GarageYrBlt")
GarageYrBlt.map <- GarageYrBlt.map[-nrow(GarageYrBlt.map),] # Remove number of NAs - last row of DF
cum_sum.freq <- sum(GarageYrBlt.map$freq) 
GarageYrBlt.probs <- mapply(function(x) {return (x / cum_sum.freq)}, GarageYrBlt.map$freq) # Probabilities of all GarageBltYrs
full.data[is.na(full.data$GarageYrBlt), "GarageYrBlt"] <- as.vector(sample(x = GarageYrBlt.map$GarageYrBlt, 
                                                                           size = 159, 
                                                                           replace = TRUE, 
                                                                           prob = GarageYrBlt.probs))


# Make randomizer based off discrete probability distribution using sample()
GarageFinish.map <- count(full.data, "GarageFinish")
GarageFinish.map  <- GarageFinish.map [-nrow(GarageFinish.map ),] # Remove number of NAs - last row of DF
cum_sum.freq <- sum(GarageFinish.map$freq) 
GarageFinish.probs <- mapply(function(x) {return (x / cum_sum.freq)}, GarageFinish.map$freq) # Probabilities of all GarageFinish
full.data[is.na(full.data$GarageFinish), "GarageFinish"] <- as.vector(sample(x = GarageFinish.map$GarageFinish, 
                                                                           size = 159, 
                                                                           replace = TRUE, 
                                                                           prob = GarageFinish.probs))
full.data[is.na(full.data$GarageQual), "GarageQual"] <- 'TA'
full.data[is.na(full.data$GarageCond), "GarageCond"] <- 'TA'
# Lot Frontage Cleaning
# Make randomizer based off discrete probability distribution using sample()
LotFrontage.map <- count(full.data, "LotFrontage")
LotFrontage.map  <- LotFrontage.map [-nrow(LotFrontage.map),] # Remove number of NAs - last row of DF
cum_sum.freq <- sum(LotFrontage.map$freq) 
LotFrontage.probs <- mapply(function(x) {return (x / cum_sum.freq)}, LotFrontage.map$freq) # Probabilities of all LotFrontage
full.data[is.na(full.data$LotFrontage), "LotFrontage"] <- as.vector(sample(x = LotFrontage.map$LotFrontage, 
                                                                           size = 486, 
                                                                           replace = TRUE, 
                                                                           prob = LotFrontage.probs))
                                         

# FireplaceQu Cleaning
# Make randomizer based off discrete probability distribution using sample()
FireplaceQu.map <- count(full.data, "FireplaceQu")
FireplaceQu.map <- FireplaceQu.map[-nrow(FireplaceQu.map),] # Remove number of NAs - last row of DF
cum_sum.freq <- sum(FireplaceQu.map$freq)
FireplaceQu.probs <- mapply(function(x) {return (x / cum_sum.freq)}, FireplaceQu.map$freq) # Probabilities of all FireplaceQu
full.data[is.na(full.data$FireplaceQu), "FireplaceQu"] <- as.vector(sample(x = FireplaceQu.map$FireplaceQu, 
                                                                           size = 1420, 
                                                                           replace = TRUE, 
                                                                           prob = FireplaceQu.probs))


# MS Zoning Cleaning
full.data[is.na(full.data$MSZoning), "MSZoning"] <- 'RL'
                                         
# Utilities Cleaning
full.data[is.na(full.data$Utilities), "Utilities"] <- 'AllPub'
                                         
# Exteriors Cleaning
full.data[is.na(full.data$Exterior1st), "Exterior1st"] <- 'VinylSd'  
full.data[is.na(full.data$Exterior2nd), "Exterior2nd"] <- 'VinylSd'
                                         
# Misc Cleaning
full.data[is.na(full.data$Electrical), "Electrical"] <- 'SBrkr'
full.data[is.na(full.data$BsmtFullBath), "BsmtFullBath"] <- 0
full.data[is.na(full.data$BsmtHalfBath), "BsmtHalfBath"] <- 0
full.data[is.na(full.data$KitchenQual), "KitchenQual"] <- 'TA'
full.data[is.na(full.data$Functional), "Functional"] <- 'Typ'   
full.data[is.na(full.data$GarageCars), "GarageCars"] <- 2
full.data[is.na(full.data$GarageArea), "GarageArea"] <- 0
full.data[is.na(full.data$SaleType), "SaleType"] <- 'WD'  



                                         
# Split back into train and test data
train.data <- as.data.frame(full.data[full.data$isTrain == TRUE, 
                                      !names(full.data) %in% c("isTrain", "Id")],stringsAsFactors=TRUE)
test.data <- as.data.frame(full.data[full.data$isTrain == FALSE, 
                                     !names(full.data) %in% c("isTrain", "SalePrice")])

# Train Gradient Boosted Machine
gbm_model_1 <- gbm(formula = SalePrice ~ ., data = train.data, distribution = "gaussian", n.trees  = 250)

gbm_model_2 <- gbm(formula = SalePrice ~ ., data = train.data.imputed, distribution = "gaussian", n.trees = 510)
sale_predictions <- predict(gbm_model_2, newdata = test.data, n.trees = 300, interaction.depth = 2)
output <- data.frame("Id" = test.data$Id, "SalePrice" = sale_predictions)
                                         
fwrite(x = output, file = "submission.csv")           

