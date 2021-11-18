#library imports

library(readr)
library(ggplot2)
library(dplyr)
library(e1071)
library(outliers)
library(leaflet)
#leaflet only for visual purposes of m
library(data.table)
library(psych)
library(glmnet)

#to reproduce a *particular* sequence of random numbers - we want consistency
set.seed(123)

solar_dataset <- readRDS("/Users/andrea/Documents/MBD/R Programming/Group Project Nov 6/solar_dataset.RData")
solar_dataset$Date <- as.Date(solar_dataset[["Date"]], "%Y%m%d")
additional_variables <- readRDS("/Users/andrea/Documents/MBD/R Programming/Group Project Nov 6/additional_variables.RData")
station_info <- read_csv("/Users/andrea/Documents/MBD/R Programming/Group Project Nov 6/station_info.csv")
colnames(solar_dataset)
solar_dataset_subset <- solar_dataset[1:5113, 1:99]

summary(solar_dataset_subset[ , -1])

date_average <- data.frame(solar_dataset_subset[ , 1],rowMeans(solar_dataset_subset[ , -1]))
colnames(date_average) <- c("Date", "Average") 

#aes = aesthetic mappings
ggplot(data = date_average, aes(x = Date, y = Average))+
  geom_line(color = "#00AFBB", size = 0.01) + ggtitle("average solar energy production across stations (1994-2007)")+ theme(plot.title = element_text(hjust = 0.5))


#column averages
avg <- data.frame(colMeans(solar_dataset_subset[ , -1]))

#setDT converts lists and data frames to data
setDT(avg, keep.rownames = TRUE)[]
colnames(avg) <- c("stid", "Average_Power")


station_avg_power <- merge(x = station_info, y = avg, by = "stid", all = TRUE)


station_avg_power$level <- cut(station_avg_power$Average_Power, c(14000000,15000000,16000000, 17000000, 18000000, 19000000), labels = c("<15mn", "15-16mn", "16-17mn", "17-18mn", ">18mn"))

powerCol <- colorFactor(palette = 'RdYlGn', station_avg_power$level)


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(data = station_avg_power , lng=~elon, lat=~nlat, color = ~powerCol(level), popup = ~stid) %>%
  addLegend('bottomright', pal = powerCol, values = station_avg_power$level,
            title = 'Average Solar <br> power production',
            opacity = 1)

m

correlation_matrix <- cor(solar_dataset_subset[ , -1])
correlation_matrix

heatmap(cor(solar_dataset_subset[ , -1]))
heatmap

skew(solar_dataset_subset[, -1], na.rm = TRUE)

#normalize the dataset
solar_dataset_subset_normalized <- scale(solar_dataset_subset[ , -1])

x <- data.frame(solar_dataset_subset_normalized >= 3 | solar_dataset_subset_normalized <=-3)

outlier_list <- c()

for(i in 1: length(x)){
  new_value <- sum(x[ ,i]==TRUE)
  outlier_list <- c(outlier_list, new_value)
  #print(i)
}

outlier_list

outlier_index <- match(c(1),outlier_list)

colnames(x)[outlier_index]
#IDAB 

#acme
p <- ggplot(solar_dataset_subset, aes(x=Date, y=ACME)) +
  geom_line() + 
  xlab("")
p+scale_x_date(date_labels = "%Y %b %d")

#idab
p <- ggplot(solar_dataset_subset, aes(x=Date, y=IDAB)) +
  geom_line() + 
  xlab("")
p+scale_x_date(date_labels = "%Y %b %d")

which.max(solar_dataset_subset$IDAB)
abc <- grubbs.test(solar_dataset_subset$IDAB)
#grubbs test for one outlier
abc



df <- solar_dataset[ 1:5113, ]
df$day <- format(df$Date, "%d")
idx <- sample(seq(1, 3), size = nrow(df), replace = TRUE, prob = c(.8, .2, .2))
train <- df[idx == 1,]
test <- df[idx == 2,]
val <- df[idx == 3,]
solar_dataset$day <- format(solar_dataset$Date, "%d")
final_pred <- data.frame(solar_dataset[ 5114:6909, 1:99])

#for loop first it creates the ML using training data. Second it validate the 
#model using test data and optimizing hyperparameter lambda. Lastly, it 
#predicts the power production at each station using remaining data

train.x <-  data.matrix(train[ , 100:457])

for (i in 2:99){
  train.y <-  data.matrix(train[ , ..i])
  mlm <- glmnet(train.x, train.y,family="gaussian", lambda = cv.glmnet(train.x, train.y)$lambda.1se)
  ypred_val <- predict.glmnet(mlm, data.matrix(val[ , 100:457]), type = c("response"))
  ypred_test <- predict.glmnet(mlm, data.matrix(solar_dataset[ 5114: 6909, 100:457]), type = c("response"))
  final_pred[ , i] <- ypred_test
  
  
}

x <- data.frame(final_pred$Date) 
x$y <- anytime::yyyymmdd(x$Date)

final_pred$Date <- x$y
write.csv(x=final_pred, file="Final Predictions Group B.csv", row.names = FALSE)
#to save the file













