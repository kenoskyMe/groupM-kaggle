getwd()
setwd("C:\\Users\\G9Q\\Desktop\\kaggel\\kaggelCompetition")
set.seed(777)

# ReadCSV
myData <- read.csv("train.csv", header = T)
head(myData)
str(myData)


# Set sample of 10% of 665249 = 66524
sampleData <- myData[sample(nrow(myData)/10), ]
str(sampleData) # Correct sample 66524 objects

# Sample TRAIN data = 70%
trainSample <- sampleData[sample(nrow(sampleData)*.7), ]
str(trainSample)  # 46566 objects = (66524*0.7)

# Sample TEST data = 30%
testSample <- sampleData[sample(nrow(sampleData)*.3), ]
str(testSample)