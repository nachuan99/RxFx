library(readr)
suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(data.table))
library(lubridate)
data<-read_csv("//Users//wenhuizeng//Downloads//Quantified-Mike-master//measurement_export_230.csv")#need modify 

str(data)

data$time <- ymd_hms(data$`Measurement Event Time`)
#data$hour<-hour(data$time)
data$month<-month(data$time)
data$year<-year(data$time)
data$mday<-mday(data$time)
#library(zoo)
#data$date <- as.yearmon(paste(data$year, data$month), "%Y %m")

data$date <- as.Date(paste(data$year, data$month,data$mday), "%Y %m %d")


subdata<-function(dataset,var){
  newdata<-subset(dataset, `Variable Name`==var )
  return(newdata)
}


#outcome
mood<-subset(data,`Variable Name`=="Overall Mood")

#predictors
sleep_quality<-subset(data, `Variable Name`=="Sleep Quality" )
#bmi<-subset(data,`Variable Name`=="Body Mass Index or BMI")
imflammtory_pain<-subdata(data,"Inflammatory Pain")
Acetyl<-subdata(data,"Acetyl-L-Carnitine")
Remeron<-subset(data,`Variable Name`=="Remeron Powder"|`Variable Name`=="Remeron")

blood<-subdata(data,"Blood Pressure (Systolic - Top Number)")

Omega<-subdata(data,"Omega 3 Fatty Acids")

Pasta_oil<-subdata(data,"Gluten Free Pasta with Olive Oil")

Fitbit_activeScore<-subdata(data,"Fitbit_activeScore")

Heart_rate<-subdata(data,"Heart Rate (Pulse)")

Acne_Severity<-subdata(data,"Acne Severity")

Pro_biotics<-subset(data,`Variable Name`=="Pro-Biotics Plus"|`Variable Name`=="NWC Pro-Biotics Plus")

Calories_burn<-subset(data,`Variable Name`=="Calories Burned"|`Variable Name`=="CaloriesBurned"|`Variable Name`=="CaloriesBurnt")




#aggregation: calculate the average score for each daily value so we can merge the data
average<-function(data,var1,var2,var3){
  c<-data %>% group_by(var1) %>% summarise(var2=round(mean(var3),3))
  return(c)
}

sleep_average<-sleep_quality%>%group_by(date)%>% summarise(average_sleep=round(mean(Value),3))
mood_average<-mood %>%group_by(date) %>% summarise(average_mood=round(mean(Value),3))
imflammtory<-imflammtory_pain%>%group_by(date)%>% summarise(average_imflammtory_pain=round(mean(Value),3))

Acetyl_intake<-Acetyl%>%group_by(date)%>% summarise(average_Acetyl_intake=round(mean(Value),3))

Remeron_intake<-Remeron%>%group_by(date)%>% summarise(average_Remeron_intake=round(mean(Value),3))

blood_pressure<-blood%>%group_by(date)%>% summarise(average_blood_pressure=round(mean(Value),3))

Omega_intake<-Omega%>%group_by(date)%>% summarise(average_Omega_intake=round(mean(Value),3))

Pasta_oil_intake<-Pasta_oil%>%group_by(date)%>% summarise(average_Pasta_oil_intake=round(mean(Value),3))

Fitbit<-Fitbit_activeScore%>%group_by(date)%>% summarise(average_Fitbit_activeScore=round(mean(Value),3))

Heart<-Heart_rate%>%group_by(date)%>% summarise(average_Heart_rate=round(mean(Value),3))

Acne_score<-Acne_Severity%>%group_by(date)%>% summarise(average_Acne_Severity=round(mean(Value),3))

Probiotics<-Pro_biotics%>%group_by(date)%>% summarise(average_Pro_biotics=round(mean(Value),3))

Calories<-Calories_burn%>%group_by(date)%>% summarise(average_Calories_burn=round(mean(Value),3))
library(dplyr)
mood_average$lag_date<-lag(mood_average$date)
#Merge the data

#mergedData <- merge(a, b, by.x=c(“colNameA”),by.y=c(“colNameB”))




combine<-merge(x =mood_average, y = sleep_average, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = imflammtory, by.x  = "lag_date", by.y="date",all.x = TRUEE)
combine<-merge(x =combine, y = Acetyl_intake, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Remeron_intake, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = blood_pressure, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Omega_intake, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Pasta_oil_intake, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Fitbit, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Heart, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Acne_score, by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Probiotics,by.x  = "lag_date", by.y="date",all.x = TRUE)
combine<-merge(x =combine, y = Calories, by.x  = "lag_date", by.y="date",all.x = TRUE)

combine$Acetyl_intake<- as.factor(ifelse(is.null(combine$average_Acetyl_intake),"0","1") )
combine$Remeron_intake<-as.factor(ifelse(is.null(combine$average_Remeron_intake),"0","1") )
combine$Omega_intake<- as.factor(ifelse(is.null(combine$average_Omega_intake),"0","1") )
combine$Pasta_oil_intake<- as.factor(ifelse(is.null(combine$average_Pasta_oil_intake),"0","1") )

summary(combine.nonmissing)
#combine.nonmissing<-na.omit(combine[,c("Pasta_oil_intake","Omega_intake","Remeron_intake","average_sleep","average_mood","average_Fitbit_activeScore"
#                                       ,"average_Calories_burn","date","Acetyl_intake")])

combine.nonmissing<-na.omit(combine[,c("average_sleep","average_mood","average_Fitbit_activeScore",
                                      "average_Calories_burn","date")])

####Buidling Markov Chain

require(rjags)         # Kruschke, J. K. (2011). Doing Bayesian Data Analysis:
# A Tutorial with R and BUGS. Academic Press / Elsevier.
setwd("//Users//wenhuizeng//Library//Mobile Documents//com~apple~CloudDocs//hackthon")
source("DBDA2E-utilities.R")
#------------------------------------------------------------------------------
# THE MODEL.

#we want to do a test of the cholestrol measures in type A and non-type A behavior.
modelString = "
model {
for (i in 1:n) { 
mood_score[i] ~ dnorm(beta0+beta1*sleep_quality[i]+beta2*Fitbit_score[i]+beta3*Calories_burn[i],kappa)
}

beta0 ~ dnorm(0,0.00000001)
beta1 ~ dnorm(0,0.0001)
beta2 ~ dnorm(0,0.0001)
beta3 ~ dnorm(0,0.0001)
#beta4 ~ dnorm(0,0.0001)
#beta5 ~ dnorm(0,0.0001)
#beta6 ~ dnorm(0,0.0001)
kappa ~ dgamma(1,1)

}
" 

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")

#------------------------------------------------------------------------------
# THE DATA.

# Specify the data in a form that is compatible with JAGS model, as a list:
mood_score<-combine.nonmissing$average_mood
sleep_quality<-combine.nonmissing$average_sleep
#Pasta_oil_intake<-as.factor(combine.nonmissing$Pasta_oil_intake)
#Omega_intake<-as.factor(combine.nonmissing$Omega_intake)
#Remeron_intake<-as.factor(combine.nonmissing$Remeron_intake)
#Acetyl_intake<-as.factor(combine.nonmissing$Acetyl_intake)
Fitbit_score<-combine.nonmissing$average_Fitbit_activeScore
Calories_burn<-combine.nonmissing$average_Calories_burn
n<-length(mood_score)

#dataList = list(n = n,mood_score=mood_score,sleep_quality=sleep_quality,Pasta_oil_intake=Pasta_oil_intake,
                #Omega_intake=Omega_intake,Remeron_intake=Remeron_intake,Fitbit_score=Fitbit_score,
#                Calories_burn=Calories_burn)

dataList = list(n = n,mood_score=mood_score,sleep_quality=sleep_quality,
                Fitbit_score=Fitbit_score,Calories_burn=Calories_burn)


#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

# Can be done automatically in jags.model() by commenting out inits argument.
# Otherwise could be established as:

#we run three markov chain, we have three parameters. we have initial values for each markov chain 1
initsList = list( 
  list( beta0=0 , beta1=20,beta2=0,beta3=0,kappa = 0.0004) ,
  list(beta0=0 , beta1=0,beta2=0,beta3=0,kappa = 0.0001) ,
  list( beta0=1 , beta1=1,beta2=1,beta3=1,kappa = 0.00001) )

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "beta0","beta1","beta2","beta3", "kappa" )
adaptSteps = 20000              # Number of steps to "tune" the samplers.give it a chance to do the simulation
burnInSteps = 10000           # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=20000               # Total number of steps in chains to save.
thinSteps=20                 # Number of steps to "thin" (1=keep every step).
#if you put thinstep=10 then save every 10th one. we can increase it when




nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "model.txt" , data=dataList , inits=initsList , 
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )#only simulation only for burning
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )#saving and simulation
# resulting codaSamples object has these indices: 
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
str(codaSamples)
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS.

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )

beta0Sample   = mcmcChain[,"beta0"] # Put sampled values in a vector.
beta1Sample =mcmcChain[,"beta1"]
beta2Sample =mcmcChain[,"beta2"]
beta3Sample =mcmcChain[,"beta3"]
#beta4Sample =mcmcChain[,"beta4"]
#beta5Sample =mcmcChain[,"beta5"]
#beta6Sample =mcmcChain[,"beta6"]
kappaSample = mcmcChain[,"kappa"]



windows( 9 , 9 )
par( mfrow=c(1,1) )
chainlength=NROW(mcmcChain)
plot( beta0Sample , type="l")
hist( beta0Sample  )
plot( beta1Sample , type="l")
hist( beta1Sample   )
plot( beta2Sample , type="l")
hist( beta2Sample   )
plot( beta3Sample , type="l")
hist( beta3Sample   )
#plot( beta4Sample , type="l")
#hist( beta4Sample   )
#plot( beta5Sample , type="l")
#hist( beta5Sample   )
#plot( beta6Sample , type="l")
#hist( beta6Sample   )
sigmaSample = 1/sqrt(kappaSample)

plot( sigmaSample , type="l" )
hist( sigmaSample  )

#

#from the plot, we can see it is already reach the steady state, but it is auto-correlation
#
windows( 7 , 9 )
par( mfrow=c(1,1) )
plotPost( beta0Sample )
plotPost( beta1Sample )
plotPost( beta2Sample )
plotPost( beta3Sample )
#plotPost( beta4Sample )
#plotPost( beta5Sample )
#plotPost( beta6Sample )
#plotPost( sigmaSample )


#Random Forest tree:
summary(combine.nonmissing)
library(randomForest)
#combine.nonmissing$Pasta_oil_intake<-as.factor(combine.nonmissing$Pasta_oil_intake)
#combine.nonmissing$Omega_intake<-as.factor(combine.nonmissing$Omega_intake)
#combine.nonmissing$Remeron_intake<-as.factor(combine.nonmissing$Remeron_intake)

library(dplyr)
combine.nonmissing$daily_mood<-combine.nonmissing$average_mood
combine.nonmissing$daily_sleep_quality<-combine.nonmissing$average_sleep
combine.nonmissing$daily_calories_burn<-combine.nonmissing$average_Calories_burn
combine.nonmissing$daily_fitbit_activity<-combine.nonmissing$average_Fitbit_activeScore

fit <- randomForest(daily_mood ~ daily_sleep_quality+daily_fitbit_activity+daily_calories_burn,data=combine.nonmissing, 
                    importance=TRUE, ntree=2000,mtry=3)
pred<-predict(fit,combine.nonmissing)

mean((combine.nonmissing$average_mood-pred)^2)
VI_F=importance(fit)
library(caret)
varImp(fit)
varImpPlot(fit,type=1)


varImpPlot(fit)

