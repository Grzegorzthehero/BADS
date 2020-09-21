
install.packages("gridExtra", repos = "http://cran.univ-lyon1.fr")

# ipak function: install and load multiple R packages.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
options(scipen=10)

# usage
packages <- c('pastecs',"klaR", "Information", "data.table", 
              "xgboost", "magrittr", "dplyr", "Matrix", "tidyverse",
              "spatstat", "mlr", "onehot", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr",
              "reticulate", "rsample", "recipe", "readr", "stringr", "caret", "car",
              ,"ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid", "devtools")
ipak(packages)





# print all of the loaded and installed packages

my_packages <- library()$results[,1]
head(my_packages, 1000)


#set the working directory for the project

setwd("/Users/grzegorzantkiewicz/BADS/bads1920/")

# Read datasets
known <- read.csv("BADS_WS1920_known.csv", stringsAsFactors = FALSE)
unknown <- read.csv("BADS_WS1920_unknown.csv", stringsAsFactors = FALSE)


#delete all of rows with brand IDs that are not in unknown from the known set 
unique_brand <- unique(unknown$brand_id)

known1 <- known$brand_id %in% unique_brand

known <- known[known1,]

#delete all of rows with item colors that are not in unknown

unique_color <- unique(unknown$item_color)

known1 <- known$item_color %in% unique_color

known <- known[known1,]

#delete all of rows with item sizes that are not in unknown

unique_size <- unique(unknown$item_size)

known1 <- known$item_size %in% unique_size

known <- known[known1,]

#The following code up until the datasplit for the model should be run twice: once for the train set and once for the validation set


#Create age column for every dataset 


today <- as.Date(Sys.Date(), "%Y-%m-%d")

known$user_dob = as.Date(known$user_dob)

known$user_age = as.Date(today) - as.Date(known$user_dob)

known$user_age = round(known$user_age/365.25)

#Unknown

unknown$user_dob = as.Date(unknown$user_dob)

unknown$user_age = as.Date(today) - as.Date(unknown$user_dob)

unknown$user_age = round(unknown$user_age/365.25)


### MERGE 2 dataframes so that we can perform every operation only once 

unknown$return <- 1000

full <- rbind(known, unknown)





train <- full

nrow(train)




#stops inbewteen for saving the dataframe
train_5 <- train 


# create a categorical veriable from user title 

for (i in seq(1, nrow(train), by = 1)){
  if((train[i, "user_title"] == "Company" | train[i, "user_title"] == "Family" | train[i, "user_title"] == "not reported")){
    train$unknown_gender = 1  } else {
    train$unknown_gender = 0
    
  }
}




# count the time for the delivery 

train$order <- as.Date(as.character(train$order_date),"%Y-%m-%d")
train$delivery<- as.Date(as.character(train$delivery_date),"%Y-%m-%d")


# dealing with nans and false data in delivery dates

train$countDown <- ifelse(is.na(train$delivery_date) | train$delivery_date=="1994-12-31", today - train$order, today - train$delivery)




train$deliverytime <- ifelse( is.na(train$delivery_date)|
                                train$delivery_date=="1994-12-31" , 0, 
                              train$delivery - train$order)

train$delivery_dummy<-ifelse(train$delivery_date=="1994-12-31" | is.na(train$delivery_date) ,1, 0)

table(train$delivery_dummy)

train$deliverytime <- ifelse(train$delivery_dummy==1 | is.na(train$delivery_date), round(mean(train$deliverytime), digits = 0),  train$deliverytime)

train$delivery <- NULL
nrow(train)

#create a dummy which tells whether a user ordered a similar item

for (i in seq(2, nrow(train), by = 1)) {
  
  if(train[i, "user_id"] == train[i-1, "user_id"] & ((train[i, "item_color"]== train[i-1, "item_color"] & train[i, "item_size"]== train[i-1, "item_size"])|
                                                     (train[i, "item_id"]== train[i-1, "item_id"] & train[i, "item_color"]== train[i-1, "item_color"])|
                                                     (train[i, "item_id"] == train[i-1, "item_id"] & train[i, "item_size"]== train[i-1, "item_size"]))) {
    train[i, "ordered_similar"] = 1 } else {
      train[i, "ordered_similar"] = 0
    }
}



for (i in seq(2, nrow(train), by = 1)) {
  
  if(train[i, "user_id"] == train[i-1, "user_id"] & ((train[i, "item_color"]== train[i-1, "item_color"] & train[i, "item_size"]== train[i-1, "item_size"])|
     (train[i, "item_id"]== train[i-1, "item_id"] & train[i, "item_color"]== train[i-1, "item_color"])|
     (train[i, "item_id"] == train[i-1, "item_id"] & train[i, "item_size"]== train[i-1, "item_size"])) | 
     train[i, "user_id"] == train[i+1, "user_id"] & ((train[i, "item_color"]== train[i+1, "item_color"] & train[i, "item_size"]== train[i+1, "item_size"])|
                                                      (train[i, "item_id"]== train[i+1, "item_id"] & train[i, "item_color"]== train[i+1, "item_color"])|
                                                      (train[i, "item_id"] == train[i+1, "item_id"] & train[i, "item_size"]== train[i+1, "item_size"]))) {
    train[i, "ordered_similar"] = 1 } else {
    train[i, "ordered_similar"] = 0
    }
}




#Fill in the dummy value for the first row

train[1, "ordered_similar"] <- 0
train_1 <-train   

nrow(train)

# inspect the column ordered_similar

table(train$ordered_similar)


# inspect the column delivery_dummy

table(train$delivery_dummy)


print(train[2, 3])




#check the insight of every column

sum(complete.cases(train[,3]))

#fill the N/A of delivery_date with the delivery_date of products in the same order 

train$order_date <- NULL
train$delivery_date <- NULL







#calculate mean/meadian and plot a histogram of column age



#change user_age to a numeric value 
train$user_age <- as.numeric(train$user_age, units="days")





table(train$user_title)





#assign the median age to na in age 

  
train$user_age[is.na(train$user_age)] <- median(train$user_age, na.rm = TRUE)


###

hist(train$user_age)


#delete user date of birth column


train$user_dob <- NULL


#create west and east germany division (Berlin is assigned to West Germany)

table(train$user_state)

#One hot encoding of user State and Brand_Id 





for (row in 1:nrow(train)) {
  if ((train[row, "user_state"] == "Mecklenburg-Western Pomerania" | train[row, "user_state"] == "Brandenburg" | train[row, "user_state"] == "Saxony-Anhalt" | train[row, "user_state"] == "Saxony" | train[row, "user_state"] == "Thuringia")) {
    train[row, "germany_east"] = 1 } else {
      train[row, "germany_east"] = 0
    }
}


#create a new column years since registering 

train$user_years_registered = as.Date(today) - as.Date(train$user_reg_date)

train$user_years_registered = round(train$user_years_registered/365.25)

train$user_reg_date <- NULL

#check the column 

table(train$user_years_registered)



str(train)

#transform the other difftime values to numeric 

train$user_years_registered <- as.numeric(train$user_years_registered, units="days")

train$deliverytime <- as.numeric(train$deliverytime, units="days")

#create a new dummy variable "is_female"

for (row in 1:nrow(train)) {
  if(train[row, "user_title"] == "Mrs") {
    train[row, "is_female"] = 1} else {
      train[row, "is_female"] = 0
    }
}

table(train$user_title)

#transform in to factor variable


train$brand_id <- as.factor(train$brand_id)


train$item_color <- as.factor(train$item_color)
train$item_size <- as.factor(train$item_size)
train$user_state <- as.factor(train$user_state)


order <- train$order

train$order <- NULL



str(train)

#drop user_title

train$user_title <- NULL


#XGB boost and Random Forest 

train_complete <- train[train$order_item_id<100001,]


validation_complete<-train[train$order_item_id>100000,]

#Set the return column validation back to NULL


validation_complete$return <- NULL

#transform to a datatable
setDT(train_complete)
setDT(validation_complete)



 

train <- train_complete



#convert user state to a factor variable


validate_model <- validation_complete

#NEW APPROACH - GML

set.seed(1000)

train_test_split <- initial_split(train, prop = 0.95)

train_test_split

train_tbl <- training(train_test_split)
unique(train_tbl$brand_id)
test_tbl  <- testing(train_test_split)


#Create a vector with target variable 

y_train_vec <- train_tbl$return
y_test_vec  <- test_tbl$return

train_tbl <- dplyr::select(train_tbl, return, everything())
test_tbl <- dplyr::select(test_tbl, return, everything())




#XGBOOST RANDOM FOREST 


#delete unneeded Columns if needed, if they are already deleted nothing bad will happen


train_tbl$order_item_id <- NULL
train_tbl$user_id <- NULL

validation_complete$order_item_id <- NULL
validation_complete$user_id <- NULL

test_tbl$order_item_id <- NULL
test_tbl$user_id <- NULL




#convert the data to a sparse matrix model

trainm <- sparse.model.matrix(return ~ .-1, data = train_tbl)

str(trainm)
trainm
head(trainm)



train_label <- train_tbl[, "return"]

train_label
nrow(trainm)

validate_model <- validation_complete

head(validate_model)
validatem <- sparse.model.matrix(~ .-1, data=validate_model)
head(validatem)

train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = as.matrix(train_label))

validate_matrix <- xgb.DMatrix(data = as.matrix(validatem))

str(validate_model)
head(validate_matrix)


testm <- sparse.model.matrix(return~.-1, data=test_tbl)

test_label <- test_tbl[, "return"]


# prepate the test matrix for xgb

test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = as.matrix(test_label))

nrow(test_matrix)
nrow(train_matrix)

nrow(validate_matrix)

#Parameters

nc <- length(unique(as.matrix(train_label)))

xgb_params <- list("objective" = "binary:logistic", 
                   "eval_metric" = "auc")

watchlist <- list(train = train_matrix, test = test_matrix)

head(test_matrix)

#train the model with set parameters

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 535,
                       watchlist = watchlist,
                       eta = 0.3,
                       max.depth = 3)








bst_model

#get the importance of the columns

imp <- xgb.importance(colnames(train_matrix), model = bst_model)

print(train_matrix)
print(validate_matrix)




print(imp)

#plot the importance

xgb.plot.importance(imp)

#Predict the values for new daa

p <- predict(bst_model, newdata = validate_matrix)


#Save the result to a dataframe 

return <- as.list(as.numeric(p))
id <- as.list(as.numeric(seq (100001, 150000, 1)))

return <- unlist(return, recursive = FALSE)
id <- unlist(id, recursive = FALSE)

my_list <- list(order_item_id = id, return = return)
typeof(my_list)
res_df <- as.data.frame(do.call(cbind, my_list))

res_df

#write the predictions to csv

write.csv(res_df, file = 'BADS10_result.csv', row.names = FALSE, sep = ",")



