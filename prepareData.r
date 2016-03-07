## This is a script that contains code for Data Preparation
allState1 <- read.csv('~/R/groupM-kaggle/test_v2.csv', sep=",", header = TRUE, stringsAsFactors = FALSE)


## Load the Trainning Dataset
allState1 <- read.csv('~/R/groupM-kaggle/train.csv', sep=",", header = TRUE, stringsAsFactors = FALSE)

# To see the number of rows
nrow(allState1)

# taking a sneek pick of the data frame
head(allState1,2)
hist(allState1)
