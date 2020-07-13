house <- read.csv("train-data.csv")
min <- read.csv("min.csv")
max <- read.csv("max.csv")
median <- read.csv("median.csv")
low <- read.csv("low.csv")
mid <- read.csv("mid.csv")
high <- read.csv("high.csv")
test <- read.csv("test-data.csv")

rows <- nrow(test)

#############################################################################3
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + view + condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + zipcode + lat + long + sqft_living15 + sqft_lot15, data = house)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}
write.csv(TA, "houes-full.csv")

#############################################################################3
#############################################################################3
TA <- NULL
dataSelect_new.lm <- lm(price ~ bathrooms + sqft_living + view + condition + grade + yr_built + yr_renovated + lat, data = house)
for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "houes-elemination.csv")

#############################################################################
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm(price ~ bathrooms + sqft_living + sqft_lot  + condition + yr_built + lat, data = min)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "Zip-low.csv")
#############################################################################3
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm(price ~ bedrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + zipcode + lat + sqft_living15, data = median)
  
for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "Zip-median.csv")

#############################################################################3
#############################################################################

TA <- NULL
dataSelect_new.lm <- lm(price ~ bathrooms + sqft_living + sqft_lot + condition + yr_built + lat, data = max)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "Zip-max.csv")


#############################################################################3
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm (price ~ bathrooms + sqft_living + view + condition
                         + grade + lat, data = low)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "10-low.csv")

#############################################################################3
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                          floors + view + condition + grade + yr_built + zipcode + 
                          lat + sqft_living15, data = mid)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "80-mid.csv")
#############################################################################3
#############################################################################
TA <- NULL
dataSelect_new.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + view + grade + sqft_above + yr_built + zipcode + lat + long , data = high)

for (i in 1:rows){
  predicted.dat <- data.frame(predict(dataSelect_new.lm, newdata=test[i,]))
  TA <- rbind(TA, data.frame(test[i,]$id, predicted.dat$predict.dataSelect_new.lm..newdata...test.i....))
}

write.csv(TA, "10-high.csv")
