# Value-Retention-for-Vehicles-in-India
# Introduction

Our research question is what factors lead to value retention in motor vehicles. We are interested in this topic because motor vehicles are a big investment and it would be useful to know how one can best retain the value of a motor vehicle, so as not to face too big of a financial loss when purchasing one. Additionally, this information could benefit those looking to get a motor vehicle for a discounted price as they could look at motor vehicles with characteristics that cause them to lose their value and therefore get a good deal on a motor vehicle.

## The Data

Our dataset had 301 observations of motor vehicle sales which were scrapped from websites in India ([Vehicle Dataset](https://www.kaggle.com/datasets/nehalbirla/vehicle-dataset-from-cardekho/?select=car+data.csv) on Kaggle). Each observation contained 9 variables.

-   Car_Name
-   Year
-   Selling_Price
-   Present_Price
-   Kms_Driven
-   Fuel_Type
-   Seller_Type
-   Transmission
-   Owner

However, in order to extract more meaningful data, we converted Selling_Price and Present_Price from its original units which were Lakhs (100,000) of Indian Rupees into US dollars. We also converted Kms_Driven into miles driven, so it is easier for us to interpret. Lastly, in order to quantify the response variable we wished to obtain we created a new variable to store the Relative Value Retention Percentage (RVRP) of each motor vehicle, using the formula:

$$RVRP = 1 - \frac{Present\_Price - Selling\_Price}{Present\_Price}$$

<img width="1000" alt="image" src="https://github.com/ssant096/Value-Retention-for-Vehicles-in-India/assets/102336530/e961e052-0c08-4479-a072-28c53bca38d9">


## EDA

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

```{r, echo=FALSE}
library(readxl)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(GGally)

carData = read_excel(file.choose())

# Conversion Indian Lakh to USD
carData$Present_Price = carData$Present_Price*1203.06 
carData$Selling_Price = carData$Selling_Price*1203.06

# Convert KM to Miles
miles = carData$Kms_Driven*0.6214
carData$Mileage = miles

# Create Variable ValueRetentionPercent
Depreciation = carData$Present_Price - carData$Selling_Price
ValueRetentionPercent =  1 - (Depreciation / carData$Present_Price)
carData$RVRP = ValueRetentionPercent
```

```{r fig.alighn='center', fig.cap = "Histogram of Response Variable", echo=FALSE}
# Plot Histogram of Response Variable
ggplot(carData, aes(x=RVRP)) + geom_histogram(binwidth=0.01)
```

<img width="750" alt="image" src="https://github.com/ssant096/Value-Retention-for-Vehicles-in-India/assets/102336530/a46761a6-0dd7-4335-9634-059da9cd0694">


```{r fig.alighn='center', fig.cap = "Histogram of Exploratory Variables", echo=FALSE}
# Plot eda histograms
h1<-ggplot(carData, aes(x=Year)) + geom_histogram(binwidth=1)
h2<-ggplot(carData, aes(x=Mileage)) + geom_histogram(binwidth=10000)
h3<-ggplot(carData, aes(x=Fuel_Type)) + geom_histogram(stat="count")
h4<-ggplot(carData, aes(x=Seller_Type)) + geom_histogram(stat="count")
h5<-ggplot(carData, aes(x=Transmission)) + geom_histogram(stat="count")
h6<-ggplot(carData, aes(x=Owner)) + geom_histogram(stat="count")
grid.arrange(h1,h2,h3,h4,h5,h6, ncol = 3)
```
<img width="750" alt="image" src="https://github.com/ssant096/Value-Retention-for-Vehicles-in-India/assets/102336530/c09b9816-4e4f-4088-9901-496c739de8b2">


```{r fig.alighn='center', fig.cap = "Predictor vs Response Scatterplot/Boxplot", echo=FALSE}
# Plot eda predictor vs response
s1<-ggplot(carData, aes(x=Mileage, y=RVRP)) + geom_point()
s2<-ggplot(carData, aes(x=Year, y=RVRP)) + geom_point()
s3<-ggplot(carData, aes(x=Owner, y=RVRP)) + geom_point()
box1<-ggplot(carData, aes(x=Fuel_Type, y=RVRP)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
box2<-ggplot(carData, aes(x=Seller_Type, y=RVRP)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
box3<-ggplot(carData, aes(x=Transmission, y=RVRP)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
grid.arrange(s1,s2,s3,box1,box2,box3, ncol = 3)  
```
<img width="750" alt="image" src="https://github.com/ssant096/Value-Retention-for-Vehicles-in-India/assets/102336530/5944fc79-8593-427a-8e4f-cb3f3d778951">


## Summary Statistics

From our summary statistics (see Table 1), we discovered that the vehicle that sold for the lowest price was a Bajaj Pulsar 150, a motorcycle sold in 2006 for only \$120.31. While the vehicle that sold for the most is a Toyota Land Cruiser, an SUV which was sold in 2010 for \$42,107.10. We have year of purchase ranging from 2003 to 2018, number of owners ranging from 0 to 3, and original price of the vehicle ranging from \$384.98 up to \$111,403.36. Our variable of interest, RVRP, also has quite the range with some vehicles retaining as little as 10.54% of its original value and others retaining up to 98.93% of its original value. From this summary of our data, we get an idea of what range our data covers. When making models and conclusions using this data we can avoid extrapolation by keeping our scope within the bounds of this particular dataset.

# Regression Analysis

## Final Model

```{r, echo=FALSE}
car.final.mlr = lm(RVRP ~ Year + I(Year^2) + Owner + Mileage + Year*Mileage, data = carData)
summary(car.final.mlr)
```
<img width="750" alt="image" src="https://github.com/ssant096/Value-Retention-for-Vehicles-in-India/assets/102336530/15f32c82-f407-441c-a8c5-58b6d03ab06b">

Our final model is:
$RVRP = 7386 - 7.402*Year + 0.001854*Year^2 - 0.06773*Owner + 0.0004307*Mileage - 2.147*10^{-7}*Year*Mileage$

The residuals for our final model satisfies the linearity, constant variance, and normality assumptions, however it does not satisfy the independence assumption (see "Checking Assumptions of Final Model" in Appendix in Final Report PDF for details). Our model has an R\^2 value of 0.7562, meaning 75.62% of the variance in RVRP can be explained by our model. However, since our data does not satisfy the independence assumption, we cannot make conclusions outside of our sample.

## Processs of Obtaining the Final Model

When obtaining our final model, we began with our stepwise regression model (see "Building a Stepwise Regression Model" in Appendix in Final Report PDF for details). We took the variables discovered to be of significant contribution to our response variable, RVRP, from our stepwise model and looked to see if the addition of second order and interaction terms would improve the residuals further and create an even more accurate model. These variables were Year, Mileage, Owner, and Fuel_Type. In order to determine which interaction and second order term would be needed, if any at all, we looked at the residuals for each individual variable one at a time and found their functional forms by trial and error and moved from one variable to the next. We knew we found a variable's proper functional form when its residuals no longer showed a pattern. After going through this process for each variable from our stepwise regression model, we arrived at our final model.

*Note: We did not consider any transformations on our response variable (RVRP) as it was already normally distributed (see figure 1).*

## Alternate Model Considerations

Models we excluded include the initial step wise regression model we made as well as any models with interaction terms with the categorical variable, Fuel_Type, as we ultimately found Fuel_Type to not have a significant relationship with our response variable (see "Recovering Function Form" in Appendix in Final Report PDF for details). We also tried models removing some of the variables from the final model we ended up with, however all of the variables in our final model contributed a significant amount to our Y variable (see "Alternate Model Considerations" in Appendix in Final Report PDF for details).

# Conclusion

Based on our final model, we would suggest that for those that want a vehicle to retain as much of its value as possible, to make sure that the vehicle has as few previous owners as possible, has driven the least possible number of miles possible, and is as new as possible. Those that want to find a good deal on a car should do the opposite, find a car with as many previous owner as possible, with the most miles driven as possible, and have the car be as old as possible. However, it is important to keep in mind that this recommendation is only applicable to this sample, and may not be applicable to data outside of this sample.

We came to this conclusion based on the fact that the coefficient of the Year\^2 term in our model is positive, which would indicate an upward facing curve between Year and RVRP, therefore newer cars will keep more of their original value. Since Owner has a negative coefficient, more owners decreases RVRP, and since Mileage and the Interaction between Year and Mileage is negative, then as Mileage increases RVRP is expected to decrease.

# Limitations

Our model failed to meet the independence of errors assumption, therefore it cannot be said to be representative of data outside of our sample. In order for us to meet this assumption, we would either have to collect additional data or clean up our current data until it satisfies our independence assumption.

Additionally, we only had 301 observations originating from India, and the data collection method was described vaguely. The only information we were given about how the data was collected was that it was scrapped from the web, therefore we were unable to determine an accurate population for this data.

Lastly, our final model assumes that our stepwise regression model selected all of the variables relevant for explaining RVRP. It should be noted that the other two categorical variables not included in the stepwise model, Seller_Type and Transmission, which we did not consider interaction terms for in our final model, may have been able to improve our model. The other variable we did not account for as we felt it would be challenging to incorporate into our model given our time constraints, was the name of the vehicle. The name of a vehicle, and the associated brand recognition that comes with a vehicle's name could very well have an effect on that vehicle's value retention.
