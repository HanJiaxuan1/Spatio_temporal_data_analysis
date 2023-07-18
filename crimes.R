# Load the required library
library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(ggplot2)
library(GGally)
library(OpenStreetMap)
library(raster)
library(tidyverse)
library(ggmap)
library(sp)
library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(knitr)
library(rgdal)
library(forecast)
library(nnet) 
library(rgdal)
# install the package if the package is missing
# install.packages("forecast")

#Reading Chicago Crime Data
#Please set the path settings to the folder on this machine
setwd("D:/Term2/STDM") 

#Read csv data
crime_data <- read.csv("crimes.csv")

#Read spatial unit data
# chicago_wards <- readOGR(dsn = "chicagoWards/chicagoWards.shp", layer = "chicagoWards", 
#                          p4s = CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs")@projargs,
#                          stringsAsFactors = FALSE)
chicago_wards <- st_read("chicagoWards/chicagoWards.shp")

source("Data/starima_package.R")

par(mar=c(5,5,2,2)+0.1) # Set the margin
Sys.setlocale("LC_ALL", "en_US.UTF-8") # Set language

# 0.Data processing
# Delete null and outliers
crime_data_clean <- crime_data %>%
  filter( !is.na(Longitude) & !is.na(Latitude) & !is.na(Year) &
           between(Longitude, -90, -87) & between(Latitude, 41, 43) &
           between(Year, 2015, 2022))

# Extract time information: year, month, and day of week
crime_data_clean$Date <- lubridate::mdy_hms(crime_data_clean$Date)
crime_data_clean$Year <- as.numeric(format(crime_data_clean$Date, "%Y"))
crime_data_clean$Month <- as.factor(format(crime_data_clean$Date, "%B"))
crime_data_clean$Month <- factor(crime_data_clean$Month, levels = month.name)
crime_data_clean$Hour <- hour(crime_data_clean$Date)

# 1. feature analysis 
# Create a bar chart by crime type
crime_data_clean %>% 
  ggplot(aes(x = Primary.Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Crimes by Type", x = "Crime Type", y = "Count")

# Create a bar chart by year
crime_data_clean %>% 
  ggplot(aes(x = Year)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
  labs(title = "Number of Crimes by Year", x = "Year", y = "Count")

# Create a bar chart by Month
crime_data_clean %>%
  ggplot(aes(x = Month)) +
  geom_bar(fill = "seagreen", color = "black") +
  labs(title = "Number of Crimes by Month", x = "Month", y = "Count") +
  scale_x_discrete(labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Draw QQ diagram
# qq_plot <- ggplot(crime_data_clean, aes(sample = Latitude)) +
#   stat_qq() +
#   stat_qq_line() +
#   ggtitle("QQ Plot of Crime Data (Latitude)") +
#   xlab("Theoretical Quantiles") +
#   ylab("Sample Quantiles")
# qq_plot

# Calculate the number of crimes by year and draw a line chart
crime_data_year <- crime_data_clean %>%
  group_by(Year) %>%
  summarise(crime_count = n())

ggplot(crime_data_year, aes(x = Year, y = crime_count)) +
  geom_line() +
  labs(title = "Number of Crimes in Chicago by Year", x = "Year", y = "Crime Count")

# Calculate the number of crimes by month and draw a line chart
# Group by 'Month' and summarize the crime count
monthly_crime_data <- crime_data_clean %>%
  group_by(Month) %>%
  summarise(crime_count = sum(n()))

# Plot the line chart
ggplot(monthly_crime_data, aes(x = as.integer(Month), y = crime_count)) +
  geom_line(group = 1) +
  labs(title = "Number of Crimes in Chicago by Month", x = "Month", y = "Crime Count") +
  scale_x_continuous(breaks = 1:12)

# 2. Spatiotemporal correlation and autocorrelation
# 2.1 Data format processing
# Total monthly crime count
crime_data_sum <- crime_data_clean %>%
  group_by(Ward, Year, Month) %>%
  summarize(total_crime = n()) %>%
  ungroup()

#Convert data to wide format
crime_data_wide <- pivot_wider(crime_data_sum, 
                               names_from = c("Year", "Month"), 
                               values_from = "total_crime",
                               names_prefix = "total_crime_")
names(crime_data_wide)[1] <- "ward"

crime_data_wide$ward <- as.character(crime_data_wide$ward)

# Crime_ data_ Wide and Chicago_ Wards connection
crime_data_spatial <- chicago_wards %>%
  left_join(crime_data_wide, by = "ward") %>%
  st_as_sf()

# 2.2 Mapping Data
tm_shape(crime_data_spatial) + 
  tm_fill("total_crime_2018_January", style="jenks", palette="Purples") +
  tm_borders("white") +
  tm_compass(position=c("right","top"), size = 0.5) + # Resize the compass
  tm_scale_bar(size = 0.5) + # Resize the scale bar
  tm_layout(title = "Chicago Crime in January 2018",legend.width = 0.5, title.size = 1.5) 

tm_shape(crime_data_spatial) + 
  tm_fill("total_crime_2018_August", style="jenks", palette="Purples") +
  tm_borders("white") +
  tm_compass(position=c("right","top"), size = 0.5) + # Resize the compass
  tm_scale_bar(size = 0.5) + # Resize the scale bar
  tm_layout(title = "Chicago Crime in August 2018",legend.width = 0.5, title.size = 1.5) 

# In this figure, the palette is defined according to the whole space-time sequence
brks <- quantile(crime_data_spatial$total_crime_2018_January, seq(0, 1, 1/5))
tm_shape(crime_data_spatial)+ 
  tm_fill("total_crime_2018_January", style="fixed", palette="-Spectral", breaks=brks)+
  tm_borders("white")+
  tm_compass(position=c("right","top"), size = 0.5)+
  tm_scale_bar(size = 0.5) +
  tm_layout(title = "Chicago Crime in January 2018", title.size = 1.5)

tm_shape(crime_data_spatial)+ 
  tm_fill("total_crime_2018_August", style="fixed", palette="-Spectral", breaks=brks)+
  tm_borders("white")+
  tm_compass(position=c("right","top"), size = 0.5)+
  tm_scale_bar(size = 0.5) +
  tm_layout(title = "Chicago Crime in August 2018", title.size = 1.5)

# 2.3 The concept of lagging variables 
# PMCC to calculate the correlation between variables and themselves
# Total number of crimes in the year
crime_data_sum_year <- crime_data_clean %>%
  group_by(Ward, Year) %>%
  summarize(total_crime = n()) %>%
  ungroup()

# Convert data to wide format
crime_data_wide_year <- pivot_wider(crime_data_sum_year, 
                               names_from = c("Year"), 
                               values_from = "total_crime",
                               names_prefix = "total_crime_")
names(crime_data_wide_year)[1] <- "ward"

Meancrime <- colMeans(crime_data_wide_year[,2:(ncol(crime_data_wide_year))])

CLagged <- data.frame(year = 2015:2021, t=Meancrime[2:(length(Meancrime))], t_minus_1=Meancrime[1:(length(Meancrime)-1)])
p1 <- ggplot(CLagged, aes(x=year, y=t)) + geom_line()
p2 <- ggplot(CLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  ggplot2::annotate("text", 4000, 6000, label=paste("r =", round(cor(CLagged$t, CLagged$t_minus_1), 3)),hjust=-1, vjust=1) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)

# 2.4 Calculating Time Autocorrelation
par(mar = c(5, 4, 4, 2) + 0.1)  # 默认边距，顶部边距稍微增加一点
crime_data_df <- st_drop_geometry(crime_data_spatial)
crime_data_df_matrix <- data.matrix(crime_data_df[, -c(1:3)])
acf(crime_data_df_matrix["42",], lag.max=50, main="ACF, Ward 42 Mag Mile")
acf(crime_data_df_matrix["42",], main="ACF, Ward 42 Mag Mile Annual Average")
pacf(crime_data_df_matrix["42",], lag.max=50, main="ACF, Ward 42 Mag Mile")

#2.5 Spatial autocorrelation
#2.5.1 Spatial adjacency and spatial weight matrix
W <- nb2listw(poly2nb(crime_data_spatial))
W
kable(listw2mat(W))
crime_avg <- rowMeans(crime_data_df_matrix)

# 2.5.2 Global spatial autocorrelation metric
moran.test(x=crime_avg, listw=W)
# Moran I test under randomisation
# data:  crime_avg  
# weights: W    
# Moran I statistic standard deviate = 4.9934, p-value = 2.966e-07
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.415634557      -0.020408163       0.007625342 

moran.mc(x=crime_avg, listw=W, nsim=9999)
# Monte-Carlo simulation of Moran I
# data:  crime_avg 
# weights: W  
# number of simulations + 1: 10000 
# statistic = 0.41563, observed rank = 9999, p-value = 1e-04
# alternative hypothesis: greater

#2.5.3 Local autocorrelation measurement
lm <- localmoran(x=rowMeans(crime_data_df_matrix), listw=W)
lm

#2.6 Spatiotemporal autocorrelation
#2.6.1 Spatiotemporal autocorrelation function
Wmat <- listw2mat(W)
#STACF
stacf(t(crime_data_df_matrix), Wmat, 48)
#STPACF
stpacf(t(crime_data_df_matrix), Wmat, 4)

# 3.Model 1
# Arima prediction
rownames(crime_data_df_matrix) <- crime_data_df[,"ward"]
# 3.1 data trends and patterns 
# Monthly
plot(crime_data_df_matrix["42",], 
     ylab="Monthly total crime", 
     xlab="Time (in months)", 
     main="Time series plot for Mag Mile based on month", 
     type="l",
     cex.axis=1, # Resize Label
     xpd=TRUE # Allow labels to exceed drawing boundaries
)

# Yearly
crime_year_matrix<-data.matrix(crime_data_wide_year[,2:ncol(crime_data_wide_year)])
rownames(crime_year_matrix) <- paste("sta",1:nrow(crime_year_matrix),sep = "")
plot(crime_year_matrix["sta42",],
     ylab="Annual total crime",
     xlab="Time (in years)",
     main="Time series plot for Mag Mile based on year", 
     type="l")

# 3.2	Experimental Design
# The first three lag charts of data
lag.plot(crime_data_df_matrix["42",], lags=12, do.lines=FALSE)
# autocorrelation
par(mar = c(5, 4, 4, 2) + 0.1)  # 默认边距，顶部边距稍微增加一点

acf(crime_data_df_matrix["42",], lag.max=72, xlab="Lag", ylab="ACF", main="Autocorrelation plot of monthly total crime")
ward.1.diff <- diff(crime_data_df_matrix["42",], lag=40, differences=1)
acf(ward.1.diff, lag.max=72, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot")
# Partial autocorrelation
pacf(crime_data_df_matrix["42",], lag.max=36,xlab="Lag",ylab="PACF",main="Partial Autocorrelation plot of monthly total crime")
pacf(ward.1.diff, lag.max=36, xlab="Lag", ylab="ACF",main="Partial Autocorrelation plot of monthly total crime")

# 3.3	Data Preparation 
# Parameter fitting
fit.ar <- arima(crime_data_df_matrix["42",1:96],order=c(1,0,2),seasonal=list(order=c(1,1,1),period=12))
fit.ar
# Coefficients:
#   ar1      ma1      ma2     sar1     sar2     sma1
# 0.9714  -0.7253  -0.0203  -0.0867  -0.1766  -0.7216
# s.e.  0.0376   0.1199   0.1141   0.2760   0.2082   0.3678
# sigma^2 estimated as 824.2:  log likelihood = -407.42,  aic = 828.84
NRMSE_fit <- NRMSE(res=fit.ar$residuals, obs=crime_data_df_matrix["42",1:72])
tsdiag(fit.ar)
# The training set (from 2015-2020 for the first 6 years) is used to fit the model, and the test set (from 2021-2022 for the next 2 years) is used to test its predictive ability.
pre.ar<-predict(fit.ar, n.ahead=12)
matplot(1:12,cbind(crime_data_df_matrix["42", 73:84],pre.ar$pred),type="l",main="ARIMA crime for Mag Mile",  xlab="Month", ylab="Total crime")

fit.Ar <- Arima(crime_data_df_matrix["42", 1:72], order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
pre.Ar <- Arima(crime_data_df_matrix["42", 73:(ncol(crime_data_df_matrix))], model=fit.ar)
matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l",main="ARIMA crime for Mag Mile",  xlab="Month", ylab="Total crime")

# Add a MAPE function to observe accuracy
MAPE <- function(pred, obs) {
  return(mean(abs((obs - pred) / obs)) * 100)
}

# Calculating MAPE for 2021 crime data
predictions_2021 <- pre.ar$pred[1:12]
actual_2021 <- crime_data_df_matrix["42", 73:84]
MAPE_2021 <- MAPE(predictions_2021, actual_2021)
print(paste("MAPE of the ARIMA model on the 2021 test data:", MAPE_2021))

#Automatic parameter adjustment Arima
fit.auto.ar <- auto.arima(crime_data_df_matrix["42",1:72])
fit.auto.ar

# Coefficients:
#   ar1     ar2      ma1
# 0.5006  0.1553  -0.9446
# s.e.  0.1309  0.1290   0.0613
# 
# sigma^2 = 858.6:  log likelihood = -339.44
# AIC=686.87   AICc=687.48   BIC=695.92

# First, make predictions using the auto.arima model
predictions_auto_ar <- forecast(fit.auto.ar, h=12)$mean
# Get the actual values from the test set
actual_values <- crime_data_df_matrix["42", 73:84]
# Calculate the MAPE for the auto.arima model
MAPE_auto_ar <- MAPE(predictions_auto_ar, actual_values)
print(paste("MAPE of the auto.arima model on the test data:", MAPE_auto_ar))

# 4. Model 2
# STarima
W_fit<-list(w1=Wmat)
# 4.1 Experimental Design
#stacf plot
pst1<- stacf(t(crime_data_df_matrix), Wmat,48)
pst2<- stpacf(t(crime_data_df_matrix),Wmat,48)
diffstaccf <- diff(t(crime_data_df_matrix),lag = 4, differences=1)
stacf(diffstaccf,Wmat,48)
stpacf(diffstaccf,Wmat,36)
fit.star <- starima_fit(t(crime_data_df_matrix)[1:72,],W_fit,p=3,d=12,q=1)

residuals <- fit.star$RES[, "42"]
sigma_squared_est <- sum(residuals^2) / length(residuals)
print(paste("Sigma^2 (estimated):", sigma_squared_est))

stacf(fit.star$RES,Wmat,24)
hist(fit.star$RES[,'42'],main='Residual for Mag Mile')

# 4.2 Data Preparation 
# ward42
pre.star <- starima_pre(t(crime_data_df_matrix)[57:84,], model=fit.star)
pre.star$RES[,'42']


matplot(1:12,cbind(t(crime_data_df_matrix)[72:83,"42"],pre.star$PRE[,"42"]),type="l",main='STARIMA crime for Mag Mile',  xlab="Month", ylab="Total crime")
 
ST_mse<- mean((t(crime_data_df_matrix)[72:83,"42"] - pre.star$PRE[,"42"])^2)
ST_mse

# Calculate accuracy by MAPE
observed_values <- t(crime_data_df_matrix)[72:83, "42"]
predicted_values <- pre.star$PRE[, "42"]
MAPE_star <- mean(abs((observed_values - predicted_values) / observed_values)) * 100
MAPE_star


# Model 3
# Artificial neural network
# X <- t(as.matrix(crime_data_df_matrix))
# y <- as.matrix(X[-1,])
# temp.nnet <- nnet(X[1:95, 1:50], y[1:95, 1:50], decay=5e-6, linout = TRUE, size=8)
# temp.pred<-predict(temp.nnet, y[72:95, 1:50])
# temp.pred[,1]
# matplot(cbind(y[72:83,1], temp.pred[,1]),ylab="Monthly crime", xlab="Time (in months)", main="ward 42 ", type="l")
# # Calculate MAPE for the Artificial Neural Network
# observed_values_ann <- y[72:83, 1]
# predicted_values_ann <- temp.pred[1:12, 1]
# MAPE_ann <- mean(abs((observed_values_ann - predicted_values_ann) / observed_values_ann)) * 100
# MAPE_ann

