library(readr)
library(tidyverse)
library(leaflet)
library(shiny)
library(rsconnect)
#warming up the model 
library(shinydashboard)
require(caTools)
library(googleway)
library(Momocs)
library(DT)
library(shinyjs)
library(reticulate)
library(geosphere)

# options(rsconnect.max.bundle.size=3145728000)
# options(rsconnect.max.bundle.files = 100000, rsconnect.max.bundle.files = 3145728000)

# ?rsconnectOptions
# py_config()
use_virtualenv("/env3")
use_python("/env3/bin/python3.6")
py_config()
#install.packages("shinyjs")
# setAccountInfo(name='timsyang',
#   token='F8456F2914890E207D4C989044EA08D3',
#   secret='MiWRsIhRx2duk6eRUDiWBL/ZV7NM0lycx+++vCMa')
# setwd("/Users/zakariaelhjouji/Dropbox (MIT)/Valeasy/Mexico/Imputation_and_API/")

#in this script, we have 3 main functions: 
#normalize
#get_price
#get_knn 

d_radius_r <- read_csv("d_radius_MEX.csv")
#Util functions 
MAPE <- function(Y_hat, Y_test){
  return(mean(abs((Y_hat - Y_test)/Y_test)))
}

normalize <- function(x){
  return((x - means)/stds)
}

#load the data 
data <- read_csv('data_ui2.csv')
data <- data[, c(2:20, 1, 21:ncol(data))]
# data = data[, 2:ncol(data)]

#normalize
means <- read_csv("means.csv")
#skip first column
means <- means[, 2:ncol(means)]
stds <- read_csv("stds.csv")
stds <- stds[, 2:ncol(stds)]

#functions we need to build 
#normalize()
#knn(k) where k is the number of neighbours
#RF takes as input normalized data and outputs a model 


#Modeling Data 
cols_norm = colnames(data)[grepl('norm', colnames(data))]
cols = c("latitude",        "longitude" ,      "Bathrooms" ,      "Bedrooms"   ,     "Builded.surface" ,"Total.area"    , 
         "food"         ,   "hospital"      ,  "lodging"       ,  "school"        ,  "university",
         "shopping_mall"  , "park")
X_norm <- data[cols_norm]
Y <- data$Price_MN

#train 
split = round(0.99* nrow(X_norm))
X_train <- X_norm[1:split,]
X_test <- X_norm[split:nrow(X_norm),]

Y_train <- Y[1:split]
Y_test <- Y[split:length(Y)]


# conda_create("r-reticulate")
# conda_install("r-reticulate", "scikit-learn")
#conda_install("r-reticulate", "cloudpickle")
#conda_install("r-reticulate", "numpy")
# conda_install("r-reticulate", "pandas")

# ?use_virtualenv

pd = import("pandas")
np = import("numpy")
sklearn <- import("sklearn")

# py_config()
#loading model
model = py_load_object('rf')

#testing
X_test = np$array(as.matrix(X_test))
Y_test = np$array(as.matrix(Y_test))
Y_hat_test = model$predict(X_test)
mape = MAPE(Y_hat_test, Y_test[,1])
print(paste("MAPE testing: ", mape))


get_price = function(x){
  x_norm <- normalize(x)
  x_norm = np$array(as.matrix(x_norm))
  y_hat = model$predict(x_norm)
  return(y_hat)
}


#Example 
cols_norm
colnames(data)
y_cols_norm = c("mape", "mape_test", "Price_MN", "url", "city", "address")
cols_norm = append(cols_norm, y_cols_norm)

i = np$random$randint(1000)
x = data[i, !(names(data) %in% cols_norm)]
y = data$Price_MN[i]
paste('True Price: ', y)
paste('Prediction: ', get_price(x))


#KNN

#load knn
knn = py_load_object("knn2")


get_knn = function(x){
  #return indices of 100 nearest nieghbours and the distances
  n = as.integer(2)
  p = as.integer(1)
  x = np$reshape(np$array(as.matrix(x)), c(p,n))
  a =  knn$kneighbors(x)
  dist = unlist(a[1])
  ind = unlist(a[2]) + 1
  return(list(dist, ind))
}

#example
x = X_train[102,1:2]
a = get_knn(x)
dist = unlist(a[1])
ind = unlist(a[2])
#####################

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}