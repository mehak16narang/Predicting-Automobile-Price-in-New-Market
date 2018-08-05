# Required libraries
library(car)
library(MASS)
library(Hmisc)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggplot2)

# ------------------------------- Load Source Data ----------------------------------
# Load dataset into a data frame
df <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

# Understanding structure of data
str(df)

# checking duplicate rows
length(unique(df$car_ID)) == nrow(df)
# car_ID is the primary key with no duplicate values

# Checking missing values
sum(is.na(df))
# No missing values

# Summary of Data
summary(df)
describe(df)

# ---------------------------- Data Cleaning ----------------------------------------

# Analysing variable 'symboling'
summary(factor(df$symboling))
typeof(df$symboling)

# Splitting 'CarName' into 'CarCompany' and 'CarModel' based on business understanding
df$CarName <- as.character(df$CarName)
df <- separate(df,CarName, into = c('CarCompany', 'CarModel'), sep ="[ ]")
sum(is.na(df$CarModel))
# 2 rows have CarModel as NA (Warning message)
# As per business understanding, CarModel is not required for further analysis
df$CarModel <- NULL

# Checking distict CarCompany
summary(factor(df$CarCompany))

# Few CarCompany names have spelling mistakes
df$CarCompany <- tolower(df$CarCompany)

# Renaming 'maxda' to 'mazda'
df$CarCompany[which(df$CarCompany == 'maxda')] <- 'mazda'

# Renaming 'porcshce' to 'porsche'
df$CarCompany[which(df$CarCompany == 'porcshce')] <- 'porsche'

# Renaming 'toyouta' to 'toyota'
df$CarCompany[which(df$CarCompany == 'toyouta')] <- 'toyota'

# Renaming 'vokswagen' and 'vw' to 'volkswagen'
df$CarCompany[which(df$CarCompany == 'vokswagen')] <- 'volkswagen'
df$CarCompany[which(df$CarCompany == 'vw')] <- 'volkswagen'

summary(factor(df$CarCompany))

# Converting variable 'CarCompany' into factor
df$CarCompany <- as.factor(df$CarCompany)

# Analysing variable 'fueltype'
summary(factor(df$fueltype))
df$fueltype <- as.factor(df$fueltype)

# Analysing variable 'aspiration'
summary(factor(df$aspiration))
df$aspiration <- as.factor(df$aspiration)

# Analysing variable 'doornumber'
summary(factor(df$doornumber))

# We can convert 'doornumber' into numerical variable
df$doornumber[which(df$doornumber == 'two')] <- 2
df$doornumber[which(df$doornumber == 'four')] <- 4
df$doornumber <- as.numeric(df$doornumber)

# Analysing variable 'carbody'
summary(factor(df$carbody))
df$carbody <- as.factor(df$carbody)

# Analysing variable 'driverwheel'
summary(factor(df$drivewheel))

# Renaming '4wd' to 'fwd'
df$drivewheel[which(df$drivewheel == '4wd')] <- 'fwd'
summary(factor(df$drivewheel))
df$drivewheel <- as.factor(df$drivewheel)

# Analysing variable 'enginelocation'
summary(factor(df$enginelocation))
df$enginelocation <- as.factor(df$enginelocation)

# Analysing variable 'wheelbase'
# values of wheelbase at the gap 1 pecentile
summary(df$wheelbase)
quantile(df$wheelbase, seq(0,1,0.01))
# We can see some outliers in upper range

# Outlier Treatment
q1 <- quantile(df$wheelbase, c(0.25))
q3 <- quantile(df$wheelbase, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
nrow(df[df$wheelbase > upper_range,])
# 3 outliers in upper range. But, we can not change car specifications.

# Analysing variable 'carlength'
summary(df$carlength)
quantile(df$carlength, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$carlength, c(0.25))
q3 <- quantile(df$carlength, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$carlength > upper_range,])
nrow(df[df$carlength < lower_range,])
# 1 outlier in lower range. But, we can not chnage car specifications. Therefore, ignoring outliers.

# Analysing variable 'carwidth'
summary(df$carwidth)
quantile(df$carwidth, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$carwidth, c(0.25))
q3 <- quantile(df$carwidth, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$carwidth > upper_range,])
# 8 outliers in upper range of 'carwidth'
nrow(df[df$carwidth < lower_range,])
# 0 outliers in lower range.
# We can't change car specifications. Therefore, not removing outliers.


# Analysing variable 'carheight'
summary(df$carheight)
quantile(df$carheight, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$carheight, c(0.25))
q3 <- quantile(df$carheight, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
df[df$carheight > upper_range,]
df[df$carheight < lower_range,]
# No Outliers

# Analysing variable 'curbweight'
summary(df$curbweight)
quantile(df$curbweight, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$curbweight, c(0.25))
q3 <- quantile(df$curbweight, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
df[df$curbweight > upper_range,]
df[df$curbweight < lower_range,]
# No Outliers

# Analysing variable 'enginetype'
summary(factor(df$enginetype))

# After checking engine types on internet, I couldn't find any engine of type 'dohcv'.
# Replacing 'dohcv' with 'dohc'.
df$enginetype[which(df$enginetype == 'dohcv')] <- 'dohc'
summary(factor(df$enginetype))

df$enginetype <- as.factor(df$enginetype)

# Analysing variable 'enginetype'
summary(factor(df$cylindernumber))

# Converting 'enginetype' into numerical variable
df$cylindernumber[which(df$cylindernumber == 'two')] <- 2
df$cylindernumber[which(df$cylindernumber == 'three')] <- 3
df$cylindernumber[which(df$cylindernumber == 'four')] <- 4
df$cylindernumber[which(df$cylindernumber == 'five')] <- 5
df$cylindernumber[which(df$cylindernumber == 'six')] <- 6
df$cylindernumber[which(df$cylindernumber == 'twelve')] <- 12
df$cylindernumber[which(df$cylindernumber == 'eight')] <- 8

df$cylindernumber <- as.numeric(df$cylindernumber)

# Analysing variable 'enginesize'
summary(df$enginesize)
quantile(df$enginesize, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$enginesize, c(0.25))
q3 <- quantile(df$enginesize, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$enginesize > upper_range,])
nrow(df[df$enginesize < lower_range,])
# 10 outliers in upper range. 
# We can not remove them because we should not change car specifications.


# Analysing variable 'fuelsystem'
summary(factor(df$fuelsystem))
df$fuelsystem <- as.factor(df$fuelsystem)

# Analysing variable 'boreratio'
summary(df$boreratio)
quantile(df$boreratio, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$boreratio, c(0.25))
q3 <- quantile(df$boreratio, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
df[df$boreratio > upper_range,]
df[df$boreratio < lower_range,]
# No outliers

# Analysing variable 'stroke'
summary(df$stroke)
quantile(df$stroke, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$stroke, c(0.25))
q3 <- quantile(df$stroke, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$stroke > upper_range,])
# 5 outliers in upper range
nrow(df[df$stroke < lower_range,])
# 15 outliers in lower range
# We can not change engine specifications. Therefore, not removing outliers.

# Analysing variable 'enginesize'
summary(df$compressionratio)
quantile(df$compressionratio, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$compressionratio, c(0.25))
q3 <- quantile(df$compressionratio, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$compressionratio > upper_range,])
nrow(df[df$compressionratio < lower_range,])
# 21 outliers in upper range.
# 7 outliers in lower range

# We can not change engine specifications. Therefore, not removing outliers.

# Analysing variable 'horsepower'
summary(df$horsepower)
quantile(df$horsepower, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$horsepower, c(0.25))
q3 <- quantile(df$horsepower, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$horsepower > upper_range,])
nrow(df[df$horsepower < lower_range,])
# 6 outliers in upper range
# We can not change engine specifications. Therefore, not removing outliers.

# Analysing variable 'peakrpm'
summary(df$peakrpm)
quantile(df$peakrpm, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$peakrpm, c(0.25))
q3 <- quantile(df$peakrpm, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$peakrpm > upper_range,])
nrow(df[df$peakrpm < lower_range,])
# 2 outliers in upper range
# We can not change engine specifications. Therefore, not removing outliers.

# Analysing variable 'citympg'
summary(df$citympg)
quantile(df$citympg, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$citympg, c(0.25))
q3 <- quantile(df$citympg, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$citympg > upper_range,])
nrow(df[df$citympg < lower_range,])
# 2 outliers in upper range

# Treating outliers
df$citympg[which(df$citympg > upper_range)] <- upper_range

# Analysing variable 'highwaympg'
summary(df$highwaympg)
quantile(df$highwaympg, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$highwaympg, c(0.25))
q3 <- quantile(df$highwaympg, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$highwaympg > upper_range,])
nrow(df[df$highwaympg < lower_range,])
# 3 outliers in upper range

# Treating outliers
df$highwaympg[which(df$highwaympg > upper_range)] <- upper_range

# Analysing variable 'price'
summary(df$price)
quantile(df$price, seq(0,1,0.01))

# Checking outliers
q1 <- quantile(df$price, c(0.25))
q3 <- quantile(df$price, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(df[df$price > upper_range,])
a <- df[df$price > upper_range,]
nrow(df[df$price < lower_range,])
# 15 outliers in upper range.
# If we have to  target rich customer segment as well, then we can not remove these outliers.


# ------------------------ Exploratory Data Analysis ---------------------------------

# Selecting all Numeric Variables
df_Numeric_Variable <- select_if(df, is.numeric)
df_Numeric_Variable

# Correlation of Numeric variables with price
corr <- cor(df_Numeric_Variable, use ='pairwise.complete.obs')
corrplot(corr,  type = "upper",tl.col = "black", tl.cex = 0.7)
# wheelbase, carlength, carwidth, curbweight, cylindernumber, enginesize, boreratio
# and horsepower are positively correlated with price.

# citympg and highwaympg are negatively correlated with price.

# Analysing Categorical variables wrt price

# CarCompany 
ggplot(df, aes(x=CarCompany, y=price)) + geom_boxplot(outlier.colour = "red") +theme(text = element_text(size=10), axis.text.x = element_text(angle=60, hjust=1))

# fueltype
ggplot(df, aes(x=fueltype, y=price)) + geom_boxplot(outlier.colour = "red")

# aspiration
ggplot(df, aes(x=aspiration, y=price)) + geom_boxplot(outlier.colour = "red")

# carbody
ggplot(df, aes(x=carbody, y=price)) + geom_boxplot(outlier.colour = "red")

# drivewheel
ggplot(df, aes(x=drivewheel, y=price)) + geom_boxplot(outlier.colour = "red")

# enginelocation
ggplot(df, aes(x=enginelocation, y=price)) + geom_boxplot(outlier.colour = "red")

# enginetype
ggplot(df, aes(x=enginetype, y=price)) + geom_boxplot(outlier.colour = "red")

# fuelsystem
ggplot(df, aes(x=fuelsystem, y=price)) + geom_boxplot(outlier.colour = "red")


# -------------------------- Dummy Variables ---------------------------------------

# CarCompany
length(levels(df$CarCompany))
# Total 22 unique CarCompanies

# Creating dummy variable
dummy_carcompany <- data.frame(model.matrix( ~CarCompany, data = df))
dummy_carcompany <- dummy_carcompany[,-1]
length(dummy_carcompany)
# 21 columns (n-1) in dummy variable

# Add dummy variables to dataframe
df_1 <- cbind(select(df, -'CarCompany'), dummy_carcompany)
ncol(df_1)
# 46 variables after adding dummy variables for CarCompany

# fueltype
length(levels(df_1$fueltype))
# Total 2 unique fueltypes

# Converting categorical variable into numerical variable
levels(df_1$fueltype)<-c(1,0)
df_1$fueltype<- as.numeric(levels(df_1$fueltype))[df_1$fueltype]

# aspiration
length(levels(df_1$aspiration))
# Total 2 unique aspiration

# Converting categorical variable into numerical variable
levels(df_1$aspiration)<-c(1,0)
df_1$aspiration<- as.numeric(levels(df_1$aspiration))[df_1$aspiration]

sum(is.na(df_1$aspiration))

# carbody
length(levels(df$carbody))
# Total 5 unique CarCompanies

# Creating dummy variable
dummy_carbody <- data.frame(model.matrix( ~carbody, data = df_1))
dummy_carbody <- dummy_carbody[,-1]
length(dummy_carbody)
# 4 columns (n-1) in dummy variable

# Add dummy variables to dataframe
df_2 <- cbind(select(df_1, -'carbody'), dummy_carbody)
ncol(df_2)
# 49 variables after adding dummy variables for carbody

# drivewheel
length(levels(df_2$drivewheel))
# Total 2 unique drivewheel

# Converting categorical variable into numerical variable
levels(df_2$drivewheel)<-c(1,0)
df_2$drivewheel<- as.numeric(levels(df_2$drivewheel))[df_2$drivewheel]
View(df_2)

sum(is.na(df_2$drivewheel))

# enginelocation
length(levels(df_2$enginelocation))
# Total 2 unique drivewheel

# Converting categorical variable into numerical variable
levels(df_2$enginelocation)<-c(1,0)
df_2$enginelocation<- as.numeric(levels(df_2$enginelocation))[df_2$enginelocation]


sum(is.na(df_2$enginelocation))

# enginetype
length(levels(df_2$enginetype))
# Total 6 unique CarCompanies

# Creating dummy variable
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = df_2))
dummy_enginetype <- dummy_enginetype[,-1]
length(dummy_enginetype)
# 5 columns (n-1) in dummy variable

# Add dummy variables to dataframe
df_3 <- cbind(select(df_2, -'enginetype'), dummy_enginetype)
ncol(df_3)
# 53 variables after adding dummy variables for enginetype

# enginelocation
length(levels(df_3$fuelsystem))
# Total 8 unique drivewheel

# Creating dummy variable
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = df_3))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
length(dummy_fuelsystem)
# 7 columns (n-1) in dummy variable

# Add dummy variables to dataframe
df_4 <- cbind(select(df_3, -'fuelsystem'), dummy_fuelsystem)
ncol(df_4)
# 59 variables after adding dummy variables for fuelsystem

# -------------------------- Linear Regression -------------------------------------

# Total 59 variables for Linear Regression

# Creating training and test dataset
set.seed(1000)
trainindices= sample(1:nrow(df_4), 0.7*nrow(df_4))
train = df_4[trainindices,]
test = df_4[-trainindices,]

# Creating 1st model using all variables

model_1 = lm(price~.,data = train)
summary(model_1)
# Multiple R-squared:  0.9813,	Adjusted R-squared:  0.9702
# Using stepAIC to optimize the model

step_model_1<- stepAIC(model_1, direction="both")
summary(step_model_1)
step_model_1

# Store model created using stepAIC funtion in model_2
model_2 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                carwidth + curbweight + enginesize + boreratio + stroke + peakrpm + citympg + 
                CarCompanyaudi + CarCompanybmw + CarCompanychevrolet + CarCompanydodge + 
                CarCompanyhonda + CarCompanyisuzu + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
                CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi, data = train)

# Summary of model_2
summary(model_2)
# Multiple R-squared:  0.9805,	Adjusted R-squared:  0.9736

model_2 <- lm(formula = price ~ car_ID + symboling + fueltype + aspiration + 
                enginelocation + carwidth + curbweight + cylindernumber + 
                enginesize + boreratio + stroke + compressionratio + horsepower + 
                peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
                + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_2)
# Multiple R-squared:   0.97,	Adjusted R-squared:  0.9575  

# fueltype is insignificant
model_3 <- lm(formula = price ~ car_ID + symboling  + aspiration + 
                enginelocation + carwidth + curbweight + cylindernumber + 
                enginesize + boreratio + stroke + compressionratio + horsepower + 
                peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)


summary(model_3)
# Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9569  

# horsepower is insignificant
model_4 <- lm(formula = price ~ car_ID + symboling  + aspiration + 
                enginelocation + carwidth + curbweight + cylindernumber + 
                enginesize + boreratio + stroke + compressionratio + 
                peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_4)
# Multiple R-squared:  0.9688,	Adjusted R-squared:  0.9565  

# symboling is insignificant.
model_5 <- lm(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + curbweight + cylindernumber + 
                enginesize + boreratio + stroke + compressionratio + 
                peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)


summary(model_5)
# Multiple R-squared:  0.9681,	Adjusted R-squared:  0.956 

# curbweight is insignificant.
model_6 <- lm(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + cylindernumber + 
                enginesize + boreratio + stroke + compressionratio + 
                peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_6)
# Multiple R-squared:  0.9671,	Adjusted R-squared:  0.955

# compressionratio is insignificant.
model_7 <- lm(formula = price ~ car_ID + aspiration + 
               enginelocation + carwidth + cylindernumber + 
               enginesize + boreratio + stroke  + 
               peakrpm + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
               CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
               CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
               CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
               CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
             + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_7)
# Multiple R-squared:  0.9662,	Adjusted R-squared:  0.9543

# CarCompanybuick is insignificant.
model_8 <- lm(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + cylindernumber + 
                enginesize + boreratio + stroke  + 
                peakrpm + CarCompanybmw  + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanyjaguar + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_8)
# Multiple R-squared:  0.9654,	Adjusted R-squared:  0.9536 

# CarCompanyjaguar is insignificant.
model_9 <- lm(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + cylindernumber + 
                enginesize + boreratio + stroke  + 
                peakrpm + CarCompanybmw  + CarCompanychevrolet + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyporsche + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_9)
# Multiple R-squared:  0.9643,	Adjusted R-squared:  0.9526

# CarCompanyporsche is insignificant.
model_10 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + 
                 peakrpm + CarCompanybmw  + CarCompanychevrolet + 
                 CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_10)
# Multiple R-squared:  0.9641,	Adjusted R-squared:  0.9528 

# CarCompanychevrolet is insignificant.
model_11 <- m(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + cylindernumber + 
                enginesize + boreratio + stroke  + 
                peakrpm + CarCompanybmw + 
                CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_11)
# Multiple R-squared:  0.9623,	Adjusted R-squared:  0.9513 

# CarCompanyisuzu is insignificant.
model_12 <- lm(formula = price ~ car_ID + aspiration + 
                enginelocation + carwidth + cylindernumber + 
                enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                CarCompanydodge + CarCompanyhonda + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
              + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_12)
# Multiple R-squared:  0.9623,	Adjusted R-squared:  0.9513

# Checking multicollinearity
vif(model_12)
train_variables <- c('car_ID','aspiration', 'cylindernumber', 'enginesize','boreratio', 'carbodysedan', 'CarCompanynissan','CarCompanytoyota','CarCompanyvolkswagen', 'carbodyhatchback','CarCompanyvolvo' )
corr <- cor( train[, train_variables], use ='pairwise.complete.obs')
corrplot(corr,  type = "upper",tl.col = "black", tl.cex = 0.7)

# car_ID is highly correlated to boreration, carcompanytoyota, carcompanyvolkswagen and carcompanyvolvo.
# car_ID is least significant among these variables. Therefore, removing it.
# Therefore, removing car_ID.
model_13 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_13)
# Multiple R-squared:  0.9564,	Adjusted R-squared:  0.9442

# CarCompanysaab is insignificant.
model_14 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + CarCompanyvolvo + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_14)
# Multiple R-squared:  0.9563,	Adjusted R-squared:  0.9446

# CarCompanyvolvo is insignificant.
model_15 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, data = train)

summary(model_15)
# Multiple R-squared:  0.956,	Adjusted R-squared:  0.9447

# fuelsystemspdi is insignificant.
model_16 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi + fuelsystemmpfi , data = train)

summary(model_16)
# Multiple R-squared:  0.9558,	Adjusted R-squared:  0.9449 

# fuelsystemmpfi is insignificant.
model_17 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               + fuelsystemmfi , data = train)

summary(model_17)
# Multiple R-squared:  0.9549,	Adjusted R-squared:  0.9443 

# fuelsystemmfi is insignificant.
model_18 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke  + peakrpm + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
                 , data = train)

summary(model_18)
# Multiple R-squared:  0.9546,	Adjusted R-squared:  0.9445 

# peakrpm is insignificant.
model_19 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + cylindernumber + 
                 enginesize + boreratio + stroke + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               , data = train)

summary(model_19)
# Multiple R-squared:  0.9526,	Adjusted R-squared:  0.9425

# Checking multicollinearity
vif(model_19)
# cylindernumber is highly correlated to enginesize and it is less significant than engine size.
# Therefore, removing cylindernumner.
model_20 <- lm(formula = price ~ aspiration + 
                 enginelocation + carwidth + 
                 enginesize + boreratio + stroke + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               , data = train)

summary(model_20)
# Multiple R-squared:  0.9497,	Adjusted R-squared:  0.9394 

# aspiration is very less significant. Therefore, removing it.
model_21 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio + stroke + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               , data = train)

summary(model_21)
# Multiple R-squared:  0.944,	Adjusted R-squared:  0.9332

# stroke is very less significant. 
# Therefore, removing it.
model_22 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor
               , data = train)

summary(model_22)
# Multiple R-squared:  0.9418,	Adjusted R-squared:  0.9312

# enginetypeohcv is insignificant.
model_23 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetyperotor
               , data = train)

summary(model_23)
# Multiple R-squared:  0.941,	Adjusted R-squared:  0.9307

# carbodyhardtop is very less significant.
model_24 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen  + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetyperotor
               , data = train)

summary(model_24)
# Multiple R-squared:  0.9382,	Adjusted R-squared:  0.9281

# carbodyhatchback is very less significant.
model_25 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen  + carbodysedan + carbodywagon + enginetyperotor
               , data = train)

summary(model_25)
# Multiple R-squared:  0.9348,	Adjusted R-squared:  0.9247

# carbodysedan is insignificant.
model_26 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen  + carbodywagon + enginetyperotor
               , data = train)

summary(model_26)
# Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9252

# carbodywagon is insignificant.
model_27 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + boreratio  + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + enginetyperotor
               , data = train)

summary(model_27)
# Multiple R-squared:  0.9346,	Adjusted R-squared:  0.9258 

# Checking Multicollieanity
vif(model_27)

train_variable <- c('enginelocation','carwidth','enginesize','boreratio', 'CarCompanybmw','CarCompanydodge','CarCompanyhonda','CarCompanymazda', 'CarCompanymitsubishi','CarCompanynissan','CarCompanypeugeot','CarCompanyplymouth','CarCompanyrenault','CarCompanysubaru','CarCompanytoyota','CarCompanyvolkswagen','enginetyperotor')
corr <- cor(train[,train_variable])
corrplot(corr,  type = "upper",tl.col = "black", tl.cex = 0.7)

# boreratio is not significant.
model_28 <- lm(formula = price ~ enginelocation + carwidth + 
                  enginesize + CarCompanybmw + 
                  CarCompanydodge + CarCompanyhonda + 
                  CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                  CarCompanypeugeot + CarCompanyplymouth + 
                  CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                  CarCompanyvolkswagen + enginetyperotor
                , data = train)

summary(model_28)
# Multiple R-squared:  0.9308,	Adjusted R-squared:  0.922

# CarCompanyplymouthis not significant.
model_29 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                 CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot +  
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + enginetyperotor
               , data = train)

summary(model_29)
# Multiple R-squared:  0.9258,	Adjusted R-squared:  0.917 

# CarCompanyhonda is not significant.
model_30 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                 CarCompanydodge +
                 CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot +  
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + enginetyperotor
               , data = train)

summary(model_30)
# Multiple R-squared:  0.9222,	Adjusted R-squared:  0.9137 

# CarCompanydodge is not significant.
model_31 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot +  
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + enginetyperotor
               , data = train)

summary(model_31)
# Multiple R-squared:  0.9192,	Adjusted R-squared:  0.911 

# CarCompanymazda is not significant.
model_32 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot +  
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota + 
                 CarCompanyvolkswagen + enginetyperotor
               , data = train)

summary(model_32)
# Multiple R-squared:  0.9158,	Adjusted R-squared:  0.908 

# CarCompanyvolkswagen is not significant.
model_33 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot +  
                 CarCompanyrenault + CarCompanysubaru + CarCompanytoyota +  enginetyperotor
               , data = train)

summary(model_33)
# Multiple R-squared:  0.9137,	Adjusted R-squared:  0.9064

# CarCompanymitsubishi is not significant.
model_34 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  CarCompanynissan + 
                 CarCompanypeugeot +  CarCompanyrenault + CarCompanysubaru + CarCompanytoyota +  enginetyperotor
               , data = train)

summary(model_34)
# Multiple R-squared:  0.9098,	Adjusted R-squared:  0.903

# CarCompanysubaru is not significant.
model_35 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  CarCompanynissan + 
                 CarCompanypeugeot +  CarCompanyrenault+ CarCompanytoyota +  enginetyperotor
               , data = train)

summary(model_35)
# Multiple R-squared:  0.9074,	Adjusted R-squared:  0.9011

# CarCompanytoyota is not significant.
model_36 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  CarCompanynissan + 
                 CarCompanypeugeot +  CarCompanyrenault +  enginetyperotor
               , data = train)

summary(model_36)
# Multiple R-squared:  0.9061,	Adjusted R-squared:  0.9005 

# CarCompanynissan is not significant
model_36 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + 
                 CarCompanypeugeot +  CarCompanyrenault +  enginetyperotor
               , data = train)

summary(model_36)
# Multiple R-squared:  0.9042,	Adjusted R-squared:  0.8992 

# CarCompanypeugeot is not significant.
model_37 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  CarCompanyrenault +  enginetyperotor
               , data = train)

summary(model_37)
# Multiple R-squared:  0.9013,	Adjusted R-squared:  0.8969

# CarCompanyrenault is not significant.
model_38 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  enginetyperotor
               , data = train)

summary(model_38)
# Multiple R-squared:  0.8969,	Adjusted R-squared:  0.8931 

model_38 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw +  enginetyperotor
               , data = train)

summary(model_38)
# Multiple R-squared:  0.8969,	Adjusted R-squared:  0.8931 

# Final model has 5 variables :
# enginelocation
# carwidth
# enginesize
# CarCompanybmw
# enginetyperotor

# --------------------- Testing Regression model on Test dataset ---------------------

predictedPrice <-predict(model_38, newdata = test)
test$predictedPrice <- predictedPrice
View(test)

r <- cor(test$price,test$predictedPrice)
rsquared <- cor(test$price,test$predictedPrice)^2
rsquared # 0.8963

# R-squared for training dataset = 0.8969 = 89.69%
# Correlation-squared on test dataset =0.8963 = 89.63%