##########################################################################
#                   R code sample for missing values handling            #                     
#                        by: Harouna TRAORE                              #
##########################################################################


#### Loading packages
### loading packages
source(here::here("scripts","Packages.R"))

######## Handling Missing values in R 

#### We used NHANES DATASET and the package naniar
#make a selection
nhanes_long <- NHANES %>% select(Age,AgeDecade,Education,Poverty,Work,LittleInterest,Depressed,BMI,Pulse,BPSysAve,BPDiaAve,DaysPhysHlthBad,PhysActiveDays)
#select 500 random indices
rand_ind <- sample(1:nrow(nhanes_long),500)
nhanes <- nhanes_long[rand_ind,]


###### Missing data detection 

# Are there missing values in the dataset?
any_na(nhanes)
# How many?
n_miss(nhanes)
prop_miss(nhanes)
# Which variables are affected?
nhanes %>% is.na() %>% colSums()
# Get number of missings per variable (n and %)
miss_var_summary(nhanes)
miss_var_table(nhanes)
# Get number of missings per participant (n and %)
miss_case_summary(nhanes)
miss_case_table(nhanes)

#Visualization: Which variables contain the most missing variables?
gg_miss_var(nhanes)

# Which combinations of variables occur to be missing together?
gg_miss_upset(nhanes)