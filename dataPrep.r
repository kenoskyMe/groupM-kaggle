## myData Preparation
getwd()
setwd("C:/Users/kenosky/Documents/R/groupM-kaggle")
set.seed(555)

# ReadCSV
myData <- read.csv("train.csv",header = T)
head(myData)
str(myData)
nrow(myData)
head(myData,2)
tail(myData,2)

## Removing Missing Values
is.na(myData) = myData=='?'
is.na(myData) = myData==' ?'
data = na.omit(myData)

View(myData)
fix(myData)

############################## base variables #################################################

myData$customer_ID = as.factor(myData$customer_ID)
myData$record_type = as.factor(myData$record_type)

# day conversion
day = myData$day
myData$day = factor(NA,levels=c('workweek','weekend'))
myData$day[day >= 0 & day < 5] <- 'workweek'
myData$day[day >= 5] <- 'weekend'
rm(day)

# time conversion
time = unlist(lapply(strsplit(as.character(myData$time),":"),function(x){ return(round(as.numeric(x[1])+as.numeric(x[2])/60)) }))
myData$time = factor(NA,levels=c('midnight','morning','noon','afternoon','evening'))
myData$time[time>22 | time<=6] <- 'midnight'
myData$time[time>6 & time<=10] <- 'morning'
myData$time[time>10 & time <= 13] <- 'noon'
myData$time[time>13 & time <= 18] <- 'afternoon'
myData$time[time>18 & time <= 22] <- 'evening'
rm(time)

myData$location = as.factor(myData$location)
myData$homeowner = as.factor(myData$homeowner)

myData$car_value = as.ordered(myData$car_value)
myData$car_value[myData$car_value == ""] <- NA
myData$car_value = droplevels(myData$car_value)

myData$risk_factor = as.ordered(myData$risk_factor)
myData$married_couple = as.factor(myData$married_couple)
myData$C_previous = as.ordered(myData$C_previous)
myData$A = as.ordered(myData$A)
myData$B = as.ordered(myData$B)
myData$C = as.ordered(myData$C)
myData$D = as.ordered(myData$D)
myData$E = as.ordered(myData$E)
myData$F = as.ordered(myData$F)
myData$G = as.ordered(myData$G)

# group_size correction
myData$group_size[which(myData$group_size == 1 & myData$married_couple == 1)] = 2
# sum(myData$age_oldest != myData$age_youngest & myData$group_size==1) ?????? 

# for convenient exploration
if (training)
  myData$customer_ID = as.numeric(myData$customer_ID)

################################# new variables ###################################
myData$age_diff = myData$age_oldest - myData$age_youngest
myData$age_mean = rowMeans(cbind(myData$age_youngest,myData$age_oldest))
myData$senior = as.factor((myData$age_oldest >= 55 & myData$age_youngest >= 55)*1)
myData$teenWithParents = as.factor((myData$age_youngest <= 18 & myData$age_diff >= 18)*1)
myData$teenAlone = as.factor((myData$age_youngest <= 18 & myData$group_size == 1)*1)
myData$studentAlone = as.factor((myData$group_size == 1 & myData$age_youngest > 18 & myData$age_youngest <= 25)*1)
myData$youngMarriage = as.factor((myData$group_size == 2 & myData$married_couple == 1 & myData$age_oldest <= 25)*1)

myData$newCar = as.factor((myData$car_age <= 1)*1)
myData$oldCar = as.factor((myData$car_age > 40)*1)

myData$sameC = as.factor((myData$C == myData$C_previous)*1)

myData$unknownRisk <- as.factor((is.na(myData$risk_factor))*1)
myData$newCustomer <- as.factor((is.na(myData$duration_previous) | is.na(myData$C_previous))*1)

myData$hasA = as.factor((myData$A != "0")*1)
myData$hasB = as.factor((myData$B != "0")*1)
myData$hasE = as.factor((myData$E != "0")*1)
myData$hasF = as.factor((myData$F != "0")*1)

##### defining the time-varying variables
myData$cost_mean = NA
myData$cost_grad = NA
myData$cost_cumgrad = 0
myData$cost_absgrad = 0
myData$numChanges = 0

myData$hadA = factor(0,levels=c(0,1))
myData$hadB = factor(0,levels=c(0,1))
myData$hadE = factor(0,levels=c(0,1))
myData$hadF = factor(0,levels=c(0,1))

myData$prevA = factor(NA,levels=levels(myData$A))
myData$prevB = factor(NA,levels=levels(myData$B))
myData$prevC = factor(NA,levels=levels(myData$C))
myData$prevD = factor(NA,levels=levels(myData$D))
myData$prevE = factor(NA,levels=levels(myData$E))
myData$prevF = factor(NA,levels=levels(myData$F))
myData$prevG = factor(NA,levels=levels(myData$G))


myData$last <- factor(0,levels=c(0,1))
if (training)
{
  myData$willChange <- factor(NA,levels=c(0,1))
  myData$targetA = factor(NA,levels=levels(myData$A))
  myData$targetB = factor(NA,levels=levels(myData$B))
  myData$targetC = factor(NA,levels=levels(myData$C))
  myData$targetD = factor(NA,levels=levels(myData$D))
  myData$targetE = factor(NA,levels=levels(myData$E))
  myData$targetF = factor(NA,levels=levels(myData$F))
  myData$targetG = factor(NA,levels=levels(myData$G))
}


if (training)
{
  target = myData[which(myData$record_type==1),]
  myData = myData[-which(myData$record_type==1),]
  target$record_type <- NULL
}
myData$record_type <- NULL

###################### filling in time-varying variables ##########################
mat <- data.matrix(myData)
targetMat = myData.matrix(target)
isNullFactor = rep(F,ncol(myData))
for(col in 1:ncol(myData)) { if('factor' %in% class(myData[,col]) & "0" %in% levels(myData[,col])) isNullFactor[col] = T }
mat[,isNullFactor] = mat[,isNullFactor]-1
targetMat[,isNullFactor] = targetMat[,isNullFactor]-1

allCustomers = unique(mat[,"customer_ID"])
for(cust in seq.int(1,length(allCustomers)))
{
  customer = which(mat[,"customer_ID"] == allCustomers[cust])
  mat[customer,"last"][length(customer)] = T # the last quote of each transaction
  
  if (training)
  {
    mat[customer,"willChange"] = changedPlansMatrix(mat[customer,],targetMat[cust,])
    mat[customer,"targetA"] = targetMat[cust,"A"]
    mat[customer,"targetB"] = targetMat[cust,"B"]
    mat[customer,"targetC"] = targetMat[cust,"C"]
    mat[customer,"targetD"] = targetMat[cust,"D"]
    mat[customer,"targetE"] = targetMat[cust,"E"]
    mat[customer,"targetF"] = targetMat[cust,"F"]
    mat[customer,"targetG"] = targetMat[cust,"G"]
  }
  
  for (obs in seq.int(2,length(customer)))
  {
    mat[customer,"cost_mean"][obs] = mean(mat[customer,"cost"][1:obs])
    mat[customer,"cost_grad"][obs] = mat[customer,"cost"][obs]-mat[customer,"cost"][obs-1]
    mat[customer,"cost_cumgrad"][obs] = mat[customer,"cost_cumgrad"][obs-1] + mat[customer,"cost_grad"][obs]
    mat[customer,"cost_absgrad"][obs] = mat[customer,"cost_absgrad"][obs-1] + abs(mat[customer,"cost_grad"][obs])
    mat[customer,"numChanges"][obs] = mat[customer,"numChanges"][obs-1] + as.numeric(changedPlansMatrix(mat[customer,][obs,],mat[customer,][obs-1,]))
    mat[customer,"hadA"][obs] = mat[customer,"hadA"][obs-1] | mat[customer,"hasA"][obs]
    mat[customer,"hadB"][obs] = mat[customer,"hadB"][obs-1] | mat[customer,"hasB"][obs]
    mat[customer,"hadE"][obs] = mat[customer,"hadE"][obs-1] | mat[customer,"hasE"][obs]
    mat[customer,"hadF"][obs] = mat[customer,"hadF"][obs-1] | mat[customer,"hasF"][obs]
    mat[customer,"prevA"][obs] = mat[customer,"A"][obs-1]
    mat[customer,"prevB"][obs] = mat[customer,"B"][obs-1]
    mat[customer,"prevC"][obs] = mat[customer,"C"][obs-1]
    mat[customer,"prevD"][obs] = mat[customer,"D"][obs-1]
    mat[customer,"prevE"][obs] = mat[customer,"E"][obs-1]
    mat[customer,"prevF"][obs] = mat[customer,"F"][obs-1]
    mat[customer,"prevG"][obs] = mat[customer,"G"][obs-1]
  }
  if (cust %% 10 == 0)
    cat('Customers processed: ',cust/length(allCustomers)*100,'\n')
}
myData$cost_mean = mat[,"cost_mean"]
myData$cost_grad = mat[,"cost_grad"]
myData$cost_cumgrad = mat[,"cost_cumgrad"]
myData$cost_absgrad = mat[,"cost_absgrad"]
myData$numChanges = mat[,"numChanges"]
myData$hadA = as.factor(mat[,"hadA"])
myData$hadB = as.factor(mat[,"hadB"])
myData$hadE = as.factor(mat[,"hadE"])
myData$hadF = as.factor(mat[,"hadF"])
myData$prevA = as.ordered(mat[,"prevA"])
myData$prevB = as.ordered(mat[,"prevB"])
myData$prevC = as.ordered(mat[,"prevC"])
myData$prevD = as.ordered(mat[,"prevD"])
myData$prevE = as.ordered(mat[,"prevE"])
myData$prevF = as.ordered(mat[,"prevF"])
myData$prevG = as.ordered(mat[,"prevG"])
myData$last = as.factor(mat[,"last"])
if (training)
{
  myData$willChange = as.factor(mat[,"willChange"])
  myData$targetA = as.ordered(mat[,"targetA"])
  myData$targetB = as.ordered(mat[,"targetB"])
  myData$targetC = as.ordered(mat[,"targetC"])
  myData$targetD = as.ordered(mat[,"targetD"])
  myData$targetE = as.ordered(mat[,"targetE"])
  myData$targetF = as.ordered(mat[,"targetF"])
  myData$targetG = as.ordered(mat[,"targetG"])
}

# deleting the first quote of each customer
myData = myData[-which(myData$shopping_pt==1),]


########################### popularities ##############################

##### options
myDataOption = factor((as.numeric(myData$A)-1)*1 + 
                      ((as.numeric(myData$B)-1))*10 + 
                      ((as.numeric(myData$C)))*100 + 
                      ((as.numeric(myData$D)))*1000 +
                      ((as.numeric(myData$E)-1))*10000 + 
                      ((as.numeric(myData$F)-1))*100000 +
                      ((as.numeric(myData$G))*1000000 ) )

targetOption = factor((as.numeric(target$A)-1)*1 + 
                        ((as.numeric(target$B)-1))*10 + 
                        ((as.numeric(target$C)))*100 + 
                        ((as.numeric(target$D)))*1000 +
                        ((as.numeric(target$E)-1))*10000 + 
                        ((as.numeric(target$F)-1))*100000 +
                        ((as.numeric(target$G))*1000000 ) )

a = table(targetOption)
myData$popularity = ordered(ifelse( a[myDataOption] <= mean(a) ,'rare',
                                  ifelse(a[myDataOption] <= mean(a)+sd(a),'popular','very popular'))
                          ,levels=c('rare','popular','very popular'))
myData$popularity[is.na(myData$popularity)] = 'rare'

#####  locations
loc = table(target$location)

myData$loc_visited = ordered(ifelse( loc[myData$location] <= mean(loc) ,'rarely',
                                   ifelse(loc[myData$location] <= mean(loc)+sd(loc),'medium','often'))
                           ,levels=c('rarely','medium','often'))
myData$loc_visited[is.na(myData$loc_visited)] = 'rarely'

################################### SAVE ##########################################
if (training) {
  save(myData,target,file='myData/train_2014.04.05.RmyData')
} else {
  test = myData
  save(test,file='myData/test_2014.04.05.RmyData')
}


############### auxiliary variables ##################
# both preprocessed train and test sets are required to add these
car_age = (myData$car_age-mean(c(target$car_age,test$car_age[test$last==1])))/sd(c(target$car_age,test$car_age[test$last==1]))
car_value = as.numeric(myData$car_value)
car_value = (car_value-mean(as.numeric(c(target$car_value,test$car_value[test$last==1])),na.rm=T))/sd(as.numeric(c(target$car_value,test$car_value[test$last==1])),na.rm=T)
age_mean = (myData$age_mean-mean(c(target$age_mean,test$age_mean[test$last==1])))/sd(c(target$age_mean,test$age_mean[test$last==1]))

myData$aux1 = car_value*car_age
myData$aux2 = car_age - age_mean
myData$aux3 = car_value - car_age
myData$aux4 = car_age + age_mean
myData$aux5 = car_value + car_age
myData$aux6 = car_value + age_mean