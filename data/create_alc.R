# Paula Thitz
# 13.11.19
# IODS exercise: week 3 (data wrangling)
# - datasets from https://archive.ics.uci.edu/ml/machine-learning-databases/00320/
##### 
setwd("D:/Opiskelu/MOOC-kurssi/IODS-project")
require(dplyr) 

####
# step 3: datasets ----
# read two datasets into objects 'student_mat' and 'student_por'
student_mat <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/data/student-mat.csv",header = TRUE,sep = ";")
student_por <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/data/student-por.csv",header = TRUE,sep = ";")

# explore dimensions
dim(student_mat) 
dim(student_por)
# - 'student_mat' (dataset on maths performance) contains 395 rows (students) and 33 variables
# - 'student_por' (dataset on Portuguese performance) contains 649 rows (students) and 33 variables

# explore structure
str(student_mat)
str(student_por)

# check if 33 variables in two datasets are similar 
# - create a dataframe with variable names in two datasets (columns 1 and 2) 
# - add a column 'same' to the dataframe. This contains results of a logical test on whether the column names are exactly the same
# - check the values in the column 'same' by using summary()
col_names <- data.frame(colnames(student_mat),colnames(student_por))
col_names$same <- col_names[,1]==col_names[,2]
summary(col_names$same) # all TRUE, so the column names are identical and students answered a similar questionnaire

####
# step 4: join datasets into a combined dataset 'student' ----

# create a vector with identifier variables
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")

# use inner_join() to combine two datasets (this function keeps only rows common to both parent datasets)
student <- inner_join(student_mat,student_por,by=join_by,suffix=c(".math",".por")) 

# explore structure and dimensions of joined dataset
str(student) 
dim(student) # 382 rows, 53 columns
# - dataframe has 13 variables used as identifiers when joining data, and 
#   2 x 20 potentially unique variables coming from either student_math or student_por

####
# step 5: combining 'duplicated' answers into single columns ----
# The aim is to combine corresponding ".math" and ".por" variables with identical values into a 
# single variable, and to decide what to do with variables with different values.

# Logically:
# If answers are similar, they can be combined
# If answers are not similar
# - if integer/numeric: take average 
# - if factor: it is not obvious what would be the best possible solution. 
#   The for non-similar factor variables could be that the student changed 
#   his/her answer - maybe the questionnaires were filled at different times, and
#   e.g. relationship status of students had time to change before answering
#   the same questions again), or the students are actually different and the 
#   selection of identifier variables was not optimal.

#   Here, we will follow the Datacamp solutions, selecting the first column 
#   of nonnumerical variables to combined data.

# create vector of variables not used as identifiers
# - inside [], we define the conditions used for picking variable from all variables of 'student_mat'
not_joined <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

# In preparation for if-else:
# - create vector 'column' containing column names in joined dataset
# - create new dataset 'alc' containing only joined columns (new columns will be added to this dataset with the if-else -loop)
column <- names(student)
alc <- select(student,one_of(join_by))

# In the if-else loop: 
# for each column not used in joining, we 
# - define the object 'two_columns' by selecting column names starting similarly
# - define the object 'first_column' 
# check if the first column is numeric
# - if yes, calculate the average of the two corresponding variables for each row
#   and put in to the dataset 'alc'
# - if no, we will "blindly" select the first column to represent 'the right answer'

for(column in not_joined) {
  two_columns <- select(student,starts_with(column))
  first_column <- select(two_columns,1)[[1]]
  
  if(is.numeric(first_column)) {
    alc[column] <- round(rowMeans(two_columns))
  } else
    alc[column] <- first_column
}

####
# step 6: make new variables 'alc_use' and 'high-use' ----

# use mutate on data 'alc' to create new columns
alc <- alc %>% mutate(
  alc_use = ((Walc+Dalc)/2),
  high_use = alc_use>2
)

# print the first few values on selected columns to check that the new variables make sense
head(alc[,c("Dalc","Walc","alc_use","high_use")])
# - seems to work as intended

####
# step 7: check and save modified data ----

# check the data using glimpse() and save
glimpse(alc)
write.csv(alc,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/alc.csv",row.names = FALSE)

# check if there are differences in the produced data file and the "example data" from the url provided
alc_check <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt",sep=",",header=TRUE)
glimpse(alc_check)
summary(alc_check==alc)
# - it looks like the datasets have some differences in variables
#   "traveltime", "studytime", "failures", "famrel", "freetime", 
#   "goout", "Dalc", "Walc", "health", "absences" and grade-variables G1-G3
# - since I am not sure where these differences came from, might be wise to use the url-provided data for analysis
