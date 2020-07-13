avocado <- read.csv("avocado.csv")

avocado.lm <- lm(AveragePrice ~
                   Total.Volume +
                   X4770 +
                   X4046 +
                   X4225 +
                   type +

                   Date +
                   region
                 , data = avocado)

x = summary(avocado.lm)
print(x)


#mean = cbind( AveragePrice = mean(avocado$AveragePrice), Total.Volume = mean(avocado$Total.Volume), X4046 = mean(avocado$X4046), X4225 = mean(avocado$X4225), X4770 = mean(avocado$X4770), Total.Bags = mean(avocado$Total.Bags), Small.Bags = mean(avocado$Small.Bags), Large.Bags = mean(avocado$Large.Bags), XLarge.Bags = mean(avocado$XLarge.Bags))
#minimum = cbind( AveragePrice = min(avocado$AveragePrice), Total.Volume = min(avocado$Total.Volume), X4046 = min(avocado$X4046), X4225 = min(avocado$X4225), X4770 = min(avocado$X4770), Total.Bags = min(avocado$Total.Bags), Small.Bags = min(avocado$Small.Bags), Large.Bags = min(avocado$Large.Bags), XLarge.Bags = min(avocado$XLarge.Bags))
#maximum = cbind( AveragePrice = max(avocado$AveragePrice), Total.Volume = max(avocado$Total.Volume), X4046 = max(avocado$X4046), X4225 = max(avocado$X4225), X4770 = max(avocado$X4770), Total.Bags = max(avocado$Total.Bags), Small.Bags = max(avocado$Small.Bags), Large.Bags = max(avocado$Large.Bags), XLarge.Bags = max(avocado$XLarge.Bags))
#variance = cbind( AveragePrice = var(avocado$AveragePrice), Total.Volume = var(avocado$Total.Volume), X4046 = var(avocado$X4046), X4225 = var(avocado$X4225), X4770 = var(avocado$X4770), Total.Bags = var(avocado$Total.Bags), Small.Bags = var(avocado$Small.Bags), Large.Bags = var(avocado$Large.Bags), XLarge.Bags = var(avocado$XLarge.Bags))




new <- NULL

for (i in 1:1){
  dataSelect.dat <- avocado
  rows <- nrow(dataSelect.dat)
  f <- 0.01
  upper_bound <- floor(f * rows)
  permuted_dataSelect.dat <- dataSelect.dat[sample(rows), ]
  train.dat <- permuted_dataSelect.dat[1:upper_bound, ]
  test.dat <- permuted_dataSelect.dat[(upper_bound+1):rows, ]
  dataSelect_new.lm <- lm(AveragePrice ~
                            Total.Volume +
                            X4770 +
                            X4046 +
                            X4225 +
                            type +
                            
                            Date +
                            region
                          , data = avocado)
  
  predicted.dat <- predict(dataSelect_new.lm, newdata=test.dat)
  delta <- predicted.dat - test.dat$AveragePrice
  new <- append(new, delta)
}
x = t.test(new, conf.level = 0.95)


print(x)
plot(delta)
