# Paula Thitz
# 25.11.2019
# Data wrangling exercises done on week 4 (generating dataset for week 5)
#####
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
