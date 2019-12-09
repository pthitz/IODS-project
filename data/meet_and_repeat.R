# Paula Thitz
# 9.12.2019
# Data wrangling exercises on week 6
#####

require(dplyr)
require(tidyr)

# load, save and explore datasets (1) ----
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt",header = TRUE)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt",header = TRUE)

write.table(BPRS,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/BPRS.txt",row.names = TRUE) #save original data (wide format)
write.table(RATS,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/RATS.txt",row.names = TRUE)

str(BPRS) # check that data was read correctly
str(RATS) 
# - note: str() shows the same information as names(), dim() or head(), so no separate check is required

# convert categoricals to factors (2) ----
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

str(BPRS) # check that factorization worked; treatment has 2 levels, subject (patient) has 20 levels
str(RATS) # ID (rat) has 16 levels, Group (diet) has 3 levels

# convert data into long form, add variables Week and Time (3) ----
BPRSL <-  BPRS %>% gather(key = Week, value = bprs, -treatment, -subject) # make a new dataset (BPRSL) by stacking columns referring to different weeks "on top of each other", maintaining the treatment and subject columns on each observation
RATSL <-  RATS %>% gather(key = Time, value = weight, -ID, -Group)

str(BPRSL$Week) # values of Week and Time are character vectors which is not optimal for plotting
str(RATSL$Time)

BPRSL <-  BPRSL %>% mutate(Week = as.integer(substr(Week,5,5))) # pick out the 5th character of "Week" and use it to replace the original Week
RATSL <-  RATSL %>% mutate(Time = as.integer(substr(Time,3,4)))

str(BPRSL) # check that the substr() worked and Week and Time contain integers telling at which time the measurements (bprs or Weight) were taken
str(RATSL) 

# comparing the datasets (4) ----

# Wide format ----
names(BPRS)
head(BPRS)
str(BPRS) # 40 rows, 11 columns
summary(BPRS) # some summary variables for bprs on each week separately
  # - 40 observations of 11 variables (treatment, subject=patient, repeated BPRS-symptom measurements over 9 weeks)

names(RATS)
head(RATS)
str(RATS) # 16 rows, 13 columns
summary(RATS) # summary variables for weight for each time point separately
  # - 16 observations of 13 variables (ID=rat, group=diet, repeated measurements of weights measured 11 times in days 1-64)

# When data is in wide format, observations made at different time points from each patient/rat are on the same row.
# So, in wide format, rows contain subjects (rats/patients).

# Long format ----
names(BPRSL)
head(BPRSL)
str(BPRSL) # 360 rows, 4 columns
summary(BPRSL)# each subject=patient is observed in data 18 times 
              # now, only grand mean (or median, quartiles, or extreme values) for bprs is shown, and it is easy to see that the bprs values vary between 18-95.

names(RATSL)
head(RATSL)
str(RATSL) # 176 rows, 4 columns
summary(RATSL)# each ID=rat is observed in data 11 times 
              # Weight of rats in the whole data set varies between 225-628 grams.

# When data is in long format, each observation has its own row. 
# So, for each combination of patient and treatment (or rat), the data contains 9 (or 11) rows.

# saving data (long format) and check that file can be read ----
write.table(BPRSL,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/BPRSL.txt",row.names = TRUE) #save wrangled data (long format)
write.table(RATSL,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/RATSL.txt",row.names = TRUE)

BPRSL_test <- read.table("D:/Opiskelu/MOOC-kurssi/IODS-project/data/BPRSL.txt",header=TRUE) #check that the saved data can be read
str(BPRSL_test) # treatment and subject are not coded as factors but the first few observations seem similar to earlier data
identical(BPRSL[,3:4],BPRSL[,3:4]) # columns 3 and 4 are identical, looks like data was saved ok

RATSL_test <- read.table("D:/Opiskelu/MOOC-kurssi/IODS-project/data/RATSL.txt",header=TRUE) 
str(RATSL_test)
identical(RATSL[,3:4],RATSL[,3:4]) # TRUE, looks good
