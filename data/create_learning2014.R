# Paula Thitz, 7.11.2019, IODS exercise week 2 ('data wrangling' -part)

# import the data to R, explore structure and dimensions ----
learning2014 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", header=TRUE, sep= "\t")
dim(learning2014) # Learning2014 is a matrix with 183 rows and 60 columns.
str(learning2014) # Most variables (columns) have obscure abbreviations as names and contain integers (typically 1-5), but there's also more descriptive numerical columns named Age, Attitude and Points.
                  # The last variable (gender) is coded as a factor (values F or M).

# creating analysis dataset 'dat'----
require(dplyr)

# a) gender, age, attitude, and points (which can be directly copied from original dataset)
dat <- learning2014[,c("gender","Age","Attitude","Points")] #reorder variables in the wanted order
dat <- dat %>% rename(age=Age,attitude=Attitude,points=Points) #change variable names to match the instructions

# b) generating variables deep, stra, surf according to DataCamp example
# list of variable names to be included in calculation of deep, stra and surf
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28") 

# create a vector of columns in learning2014 over which we want to calculate means
deep_columns <- select(learning2014, one_of(deep_questions))
surface_columns <- select(learning2014, one_of(surface_questions))
strategic_columns <- select(learning2014, one_of(strategic_questions))

# for each row in learning2014, calculate mean of selected columns, and attach these also to dat
dat$deep <- learning2014$deep <- rowMeans(deep_columns)
dat$surf <- learning2014$surf <- rowMeans(surface_columns)
dat$stra <- learning2014$stra <- rowMeans(strategic_columns)

# from 'dat', keep only observations where points>0
dat <- filter(dat,points>0)
dim(dat) # check that dimensions are 166 x 7
names(dat) #check that the names of columns are as wanted

# reorder columns in 'dat'
dat <- dat[,c(1:3,5:7,4)]
names(dat) #check that the names of columns are as wanted

# set working directory to IODS project folder ----

#check the current working directory - this looks correct, so no need to setwd
getwd() # [1] "D:/Opiskelu/MOOC-kurssi/IODS-project"

# save dataset and check you can read it
write.csv(dat,file="lrn14_dat.csv",row.names=FALSE) #if row.names are not set to false, write.csv will end up having an extra colum
dat1 <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/lrn14_dat.csv") #bring the data to R (with a different name so that you can check for the differences, if necessary)

str(dat1)

