
library(dplyr)
library(ggplot2)
library(Matrix)
library(xgboost)

setwd("E:\\Google Drive\\R Codes\\Hackathon\\BigMart Sales Prediction_Practice")

train = read.csv("Train.csv", header = TRUE)
test = read.csv("Test.csv", header = TRUE)


data <- rbind(data, test)

factor_1 <- rep("train", nrow(train))
factor_1 <- append(factor_1, after= nrow(train), rep("test", nrow(test)))

dsales <- NULL
dsales <- append(dsales,train$Item_Outlet_Sales)
dsales <- append(dsales, after = nrow(train), rep(0, nrow(test)))
data$Item_Outlet_Sales <- dsales 
data$datsource <- as.factor(factor_1)

apply(data[data$datsource == "train",c(2,4,6,12)], 2, summary)

dat_df <- tbl_df(data)
dr_id <<- grepl("DR",data$Item_Identifier)
nc_id <<- grepl("NC", data$Item_Identifier)
fd_id <<- grepl("FD", data$Item_Identifier)

#filter FD*
fdw <- data$Item_Weight[fd_id]
meanfdw <- mean(fdw, na.rm = T)
meanfdw
#REPLACE NA'S IN FD* WEIGHTS
data$Item_Weight[fd_id & is.na(data$Item_Weight)] <- meanfdw

#filter NC*
ncw <- data$Item_Weight[nc_id]
meanfcw <- mean(ncw, na.rm = T)
meanfcw
#replace na values
data$Item_Weight[nc_id & is.na(data$Item_Weight)] <-meanfcw

#filter NC*
drw <- data$Item_Weight[dr_id]
meandrw <- mean(drw, na.rm = T)
#Replace NA values
data$Item_Weight[dr_id & is.na(data$Item_Weight)] <-meandrw


#FInd which is the maximum mode in each outlet type and ensure you don't select "" column 
mn_os_ot <- dat_df %>% group_by(Outlet_Type) %>%  summarise(modsize = names(which.max(summary(Outlet_Size)[c(2,3,4)])))

str(mn_os_ot)

data$Outlet_Size[grepl("Grocery", data$Outlet_Type) & (data$Outlet_Size == "")] <- mn_os_ot$modsize[grepl("Grocery",mn_os_ot$Outlet_Type)]

data$Outlet_Size[grepl("Supermarket Type1", data$Outlet_Type) & (data$Outlet_Size == "")] <- mn_os_ot$modsize[grepl("Supermarket Type1",mn_os_ot$Outlet_Type)]

data$Outlet_Size[grepl("Supermarket Type2", data$Outlet_Type) & (data$Outlet_Size == "")] <- mn_os_ot$modsize[grepl("Supermarket Type2",mn_os_ot$Outlet_Type)]

data$Outlet_Size[grepl("Supermarket Type3", data$Outlet_Type) & (data$Outlet_Size == "")] <- mn_os_ot$modsize[grepl("Supermarket Type3",mn_os_ot$Outlet_Type)]

#Check for missing values
table(data$Outlet_Size)

str(data$Item_Visibility)
mn_vs_dr <- dat_df %>% filter(dr_id & Item_Visibility >0 ) %>% summarise(mean(Item_Visibility))

mn_vs_nc <- dat_df %>% filter(nc_id & Item_Visibility >0 ) %>% summarise(mean(Item_Visibility))

mn_vs_fd <- dat_df %>% filter(fd_id & Item_Visibility >0 ) %>% summarise(mean(Item_Visibility))

data$Item_Visibility[dr_id & data$Item_Visibility ==0] <- as.numeric(mn_vs_dr)
data$Item_Visibility[nc_id & data$Item_Visibility ==0] <- as.numeric(mn_vs_nc)

data$Item_Visibility[fd_id & data$Item_Visibility ==0] <- as.numeric(mn_vs_fd)


str(data)

#Create new groups 
data$Item_Group[fd_id] <- "Food"
data$Item_Group[nc_id] <- "Non-consumable"
data$Item_Group[dr_id] <- "Drinks"
data$Item_Group <- factor(data$Item_Group)

data$Outlet_Years <- 2013 - data$Outlet_Establishment_Year


data$Item_Fat_Content[grepl("LF",data$Item_Fat_Content)] <- "Low Fat"
data$Item_Fat_Content[grepl("low fat",data$Item_Fat_Content)] <- "Low Fat"
data$Item_Fat_Content[grepl("reg",data$Item_Fat_Content)] <- "Regular"
#Adding a new factor level
levels(data$Item_Fat_Content) <- c(levels(data$Item_Fat_Content),"Non-Edible")
#Now add the factor to the data frame
data$Item_Fat_Content[data$Item_Group == "Non-consumable"] <- "Non-Edible"

data$Item_Fat_Content <- factor(data$Item_Fat_Content)

vars <- c("Item_Fat_Content","Outlet_Location_Type","Outlet_Size","Outlet_Type","Item_Group")

library(plyr)
data$Outlet <- as.numeric(revalue(data$Outlet_Identifier, c("OUT010"="1", "OUT013"="2", "OUT017"="3", "OUT018"="4", "OUT019"="5", "OUT027"="6", "OUT035"="7", "OUT045"="8", "OUT046"="9", "OUT049" ="10")))


ohe <- model.matrix(~ . + 0, data=data[vars], contrasts.arg = lapply(data[vars], contrasts, contrasts=FALSE))

colnames(as.data.frame(ohe))
data <- cbind(data,as.data.frame(ohe))
str(data)
colnames(data)


#Filter train data


vars_trn <- c("Item_Weight","Item_Visibility","Item_MRP","Outlet_Years","Outlet_TypeGrocery Store","Outlet_TypeSupermarket Type1",
              "Outlet_TypeSupermarket Type2","Outlet_TypeSupermarket Type3", "Item_Fat_ContentLow Fat","Item_Fat_ContentRegular","Item_Fat_ContentNon-Edible","Outlet_SizeHigh","Outlet_SizeMedium",
              "Outlet_SizeSmall","Outlet_Location_TypeTier 1","Outlet_Location_TypeTier 2","Outlet_Location_TypeTier 3","Item_GroupDrinks","Item_GroupFood", 
              "Item_GroupNon-consumable","Outlet")
vars_tst <- c(vars_trn,"Item_Identifier","Outlet_Identifier")
dat_train <- data[data$datsource == "train",vars_trn]
dat_test <- data[data$datsource == "test",vars_tst]
output_train <- data[data$datsource == "train","Item_Outlet_Sales"]

dat_train$Item_Visibility <- as.numeric(unlist(dat_train$Item_Visibility))
str(dat_train)
str(dat_test)
str(output_train)

dat_test$Item_Visibility <- as.numeric(unlist(dat_test$Item_Visibility))

str(dat_test)

#Baseline Model

if(FALSE){
  set.seed(1234)
  xgb <- xgboost(data = data.matrix(dat_train), 
                 label = data.matrix(output_train), 
                 eta = 0.05,
                 max_depth = 20, 
                 nround=50, 
                 seed = 123,
                 objective = "reg:linear",
                 #num_class = 12,
                 nthread = 3
  )
  
  
  dat_xgb <- dat_test[,-c(grep("Item_Identifier",colnames(dat_test)),grep("Outlet_Identifier",colnames(dat_test)) )]
  
  head(dat_xgb)
  write.csv(dat_xgb,"xgb_test.csv",col.names = TRUE)
  
  y_pred <- predict(xgb, data.matrix(dat_xgb))
  
  head(y_pred)
  writedata <- data.frame(Item_Identifier=dat_test$Item_Identifier,Outlet_Identifier =   dat_test$Outlet_Identifier, Item_Outlet_Sales = y_pred)
  str(writedata)
}

if(TRUE){
  
  library(MASS)
  fit <- lm(output_train ~ `Item_Visibility`+`Item_Weight` + `Item_Visibility` + `Item_MRP`+`Outlet_Years` + `Outlet_TypeGrocery Store` + `Outlet_TypeSupermarket Type1` + `Outlet_TypeSupermarket Type2` + `Outlet_TypeSupermarket Type3` + `Item_Fat_ContentLow Fat`+ `Item_Fat_ContentRegular` + `Outlet_SizeHigh` + `Outlet_SizeMedium` + `Outlet_SizeSmall` + `Outlet_Location_TypeTier 1` + `Outlet_Location_TypeTier 2` + `Outlet_Location_TypeTier 3` + `Item_GroupDrinks` + `Item_GroupDrinks` + `Item_GroupFood` + `Item_GroupNon-consumable` + `Outlet`, 
            data=dat_train)
  
  step <- stepAIC(fit, direction="both")
  step$anova # display results
  #Revised based on step regression results
  
  fit <- lm(output_train ~ `Item_MRP`+`Outlet_Years` + `Outlet_TypeGrocery Store` 
            + `Outlet_TypeSupermarket Type1` + `Outlet_TypeSupermarket Type2` + `Item_Fat_ContentLow Fat`
            + `Outlet_SizeHigh` + `Outlet_SizeMedium` + `Outlet_Location_TypeTier 1` 
            + `Outlet_Location_TypeTier 2` + `Outlet`, 
            data=dat_train)
  
  y_pred <- predict(fit, dat_test)
  
  
}

if(TRUE){
  
  library(randomForest)
  # Fitting model
  fit <- randomForest(y=data.matrix(output_train), x=data.matrix(dat_train),ntree=50)
  summary(fit)
  y_pred2 = predict(fit,dat_test)
}

if(TRUE)
{
  library(e1071)
  #Support vector machines
  #svm_fit<-svm(y=data.matrix(output_train), x=data.matrix(dat_train))
  svm_fit<-svm(output_train~.,data= dat_train)
  
  y_pred3 <- predict(svm_fit,newdata=dat_test)
  print(y_pred3[1:10])
}
y_pred <- (y_pred + y_pred2 + y_pred3 )/3
#writedata$Item_Outlet_Sales <- y_pred
writedata <- cbind(as.character(dat_test[,"Item_Identifier"]),as.character(dat_test[,"Outlet_Identifier"]),y_pred)
colnames(writedata) = c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
#writedata[1:10,]
#writedata[1:10,]
write.csv(writedata,"solution_ln.csv",row.names = FALSE)


