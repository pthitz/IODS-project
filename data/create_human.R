# Paula Thitz
# 25.11.2019, 2.12.2019
# Data wrangling exercises done on weeks 4 and 5
#####
# Week 4 ----
require(data.table)
require(dplyr)

# reading data to into R (2) 
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# exploring datasets (3)
str(hd) # a data matrix containing HDI (human development index), HDI rank and 5 other variables used for calculating HDI for each country
str(gii) # a data matrix containing GII (gender inequality index), GII rank and 7 other variables used for calculating GII of each country

dim(hd) # 195 rows (countries), 8 columns
dim(gii) # 195 rows (countries), 10 columns

summary(hd) 
summary(gii)

# renaming variables (4)
names(hd) # checking current names to decide shorter but still descriptive names of variables
new_names_hd <- c("HDI_rank","country","HDI","life_exp","exp_edu","mean_edu","GNN","dif_GNNvsHDI") # create a vector with desired names

names(gii)
new_names_gii <- c("GII_rank","country","GII","mat_mortal","adol_birth","fem_parl","fem_educ","male_educ","fem_work","male_work")
# OBS! From observing values and countries in 'gii' (e.g. using view(), and sorting by Percent.Representation.in.Parliament) it looks more reasonable to assume that this variable represents female than male representation in the parliament

setnames(hd,old=names(hd),new=new_names_hd) #using function setnames() from library 'data.table' to change multiple column names at once
setnames(gii,old=names(gii),new=new_names_gii)

# mutating variables (5)
gii <- gii %>% mutate(FtoM_edu = fem_educ/male_educ) # create variable 'FtoM_edu', showing the ratio of female population with secondary education to male population with secondary education 
gii <- gii %>% mutate(FtoM_work = fem_work/male_work) # create variable 'FtoM_work', showing the ratio of female working population to male working population

# joining datasets (6)
human <- inner_join(hd,gii,by="country") #join datasets by country (only rows present in both datasets are kept)
dim(human) #chek dimentions (195 x 19, ok)
write.csv(human,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/human.csv",row.names = FALSE)

human_check <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/data/human.csv",stringsAsFactors = F)
all.equal(human,human_check) # checking that dataset did not change (is equal) to the original

# Week 5 ----
require(stringr)

# load data, explore structure and dimensions, describe briefly ----
human <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/data/human.csv",stringsAsFactors = F, na.strings="NA") # na.strings specifies how my NA-values are coded; if it is missing, all columns with NAs are coded as characters
str(human) # dataset (195 x 19 -matrix) of 195 observations on 19 variables collected from different countries/areas of the world (separated by column 'country'). 

# Variables include three indexes (HDI=human development index, GNN=gross national income per capita, GII=gender inequality index), ranks of countries/areas when ordered by index (HDI_rank, GII_rank) or difference in ranking by HDI compared to ranking by GNN (dif_GNNvsHDI).
# They remaining 12 variables were used for calculating HDI and GII indexes, and they include
  # - life_exp=life expectancy (years)
  # - exp_edu=time required for expected (full) education (years)
  # - mean_edu=mean of realized time spent on education (years)
  # - mat_mortal=maternal mortality (deaths/100000 live births)
  # - adol_birth=births given by 15-19 year-olds (/1000 15-19-year-old-women)
  # - fem_parl=female presentation in the parliament (%),
# proportions (%) of population receiving at least secondary education or within the working population (suffixes "edu" and "_work") separately for females or males (prefixes "fem_" or "male_"),
# as well as relatio of female to male (proportionate) populations receiving at least secondary education (FtoM_edu), or within the working population (FtoM_work).

# 1: mutate GNN into numeric ----
# - note: 'GNN' in this data corresponds to 'GNI' in the example (course-provided) data
str(human$GNN) # check how R interpreted commas in the column 'GNN'; data type is character, so this data was not read correctly
human <- mutate(human,GNN=str_replace( human$GNN, pattern=",", replace ="")
  %>% as.numeric) # replace dataset 'human' by mutated 'human' where the content of column 'GNN' has been modified with function 'str_replace()' and then converted into a numeric with function 'as.numeric()'
str(human$GNN) # check again, now data type in GNN should be numeric

# 2: exclude unneeded variables ----
keep <- c("country", "FtoM_edu", "FtoM_work", "exp_edu", "life_exp", "GNN", "mat_mortal", "adol_birth", "fem_parl") #vector containing my names for variables to be kept
human <- select(human,one_of(keep)) #use 'keep' as filter for variables names in the modified 'human'
dim(human) # 195 x 9 matrix, so 10 columns (variables) were not included in 'keep'

# 3: remove rows with missing values ----
complete.cases(human) # prints a vector corresponding to 195 rows in human, with 'TRUE' if there is no missing values in any columns of that row
human <- filter(human,complete.cases(human)) # filter() selects rows from dataset, where the condition (set by a logical column complete.cases(human))
dim(human) # 162 rows, 9 columns, so at least some of the data was missing for 33 countries/areas

# 4: remove region-level observations ----
human$country # print column 'country' to manually check which rows contain region-level observations and should be removed
# - this is time-consuming but doable for 162 observations
# - alternatively, could have tried importing a list of countries and use it to filter the rows, but it is likely that some (differently spelled) rows would still require manual checking

# It looks like the region-level observations are the final 7 rows, so these will be removed.
countries <- nrow(human) - 7 #count how many (first) rows to keep
human <- human[1:countries,] #re-define 'human' to contain only 155 rows to keep
dim(human) # 155 rows (countries), 9 variables

# 5. define row names by country, save data without country-column ----
rownames(human) <- human$country # define row names into the data matrix 'human'
human <- select(human, -country) # select columns to exclude (indicated by '-' sign) in the data

head(human) # print some of the first rows of the data frame to check how countries are now visible 
str(human) # check dimensions and type of columns still remaining in human
# - 155 observations of 8 variables, looks good
# - all columns either numerical or integers, which is good news for plotting

write.csv(human,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/human_.csv",row.names = TRUE) #save data into a human_.csv file for checking that the saved file did not change
human_check <- read.csv("D:/Opiskelu/MOOC-kurssi/IODS-project/data/human_.csv",row.names =1) 
all.equal(human,human_check)
str(human_check) 
head(human_check) # all looks good, so the original human.csv can be replaced by the new one

write.csv(human,file="D:/Opiskelu/MOOC-kurssi/IODS-project/data/human.csv",row.names = TRUE) #save data into a human_.csv file

