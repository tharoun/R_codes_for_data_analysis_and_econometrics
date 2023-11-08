##########################################################################
#                   R code sample for data manipulation and exploration  #                     
#                        by: Harouna TRAORE                              #
##########################################################################

### loading packages
source(here::here("scripts","Packages.R"))

############################## Part I: Data manipulation and exploration ###################

######## Reading and writing data files (Common data extensions) 
### Reading excel file
excel_file<- read_excel(here::here("raw_data", "test_data_exercise_3.xlsx"))

### Reading csv file
csv_file<- read.csv(here::here("raw_data", "ACS.csv"))

## Reading RDS file
RDS_file<- readRDS(here::here("raw_data", "ACS.rds"))

### Depending on the data type, we can also use the Foreign package 
### for reading and writing data stored by some versions of 'Epi Info', 'Minitab', 'S', 'SAS', 'SPSS', 
#### 'Stata', 'Systat', 'Weka', and for reading and writing some 'dBase' files.
####################################################################################################################

###### Explore the dataset usinr R base functions

#open the data set.
View(excel_file)
# reports object type of type of data stored.
class(excel_file)
# reports the size of each one of an object's dimension.
dim(excel_file)
#returns the variable names of a dataset.
names(excel_file)
#general information on an R object
str(excel_file)
#summary information about the variables in a data frame
summary(excel_file)
#shows the first few observations in the dataset
head(excel_file)
#shows the last few observations in the dataset
tail(excel_file)
#The number of distinct values of a particular variable:
n_distinct(excel_file$district, na.rm = TRUE)

#######################################################################################################################

############ Manipulating data tables with dplyr
data<- read.csv(here::here("raw_data", "FAO_grains_NA.csv"))

#to get information about your variables
data%>%
  glimpse()

## a-Subset by row values

data_sub_rows <- data %>%filter(Information == "Yield (Hg/Ha)", 
                            Crop == "Oats" | Crop == "Buckwheat",
                            Year >= 2005 & Year <= 2011)

## data_sub_rows contains all rows associated with the string Oats or Buckwheat in the Crop,Yield (Hg/Ha) in the Information and Year between 2005 and 2011

## b- Subset columns 
## Select only Information an crop columns
data_sub_column_1 <- data %>% dplyr::select(Information,Crop)
### Exclude Crop , Year, Value from the dataset
data_sub_column_2 <- data %>% dplyr::select(-Crop, -Year, -Value)

## C- mutate: Creating and/or calculating column values

## Modifying existing column Country
data_mutate_1 <- mutate(data, Country = ifelse(Country == "Canada", "CAN", "USA"))
## Create new column Ctr_abbr
data_mutate_2 <-mutate(data, Ctr_abbr = ifelse(Country == "Canada", "CAN", "USA"))

## D- mutate_at: Creating and/or calculating values across multiple columns

example_data <- data.frame(Wind = c(3.4, 5.0, 99, 4.1, 1.5),
                  Dir  = c(181, 220, 15,  15,  99 ),
                  Prec = c(99 , 0.5,  0,  99,  99))
### Modifying two variables 
example_data <- mutate_at( example_data, vars(Wind, Prec),
                   list(~ ifelse( . == 99, NA, .)))

## E- summarise: Summarize columns

columns_summary <- summarise(data,Value_mean=mean(Value),Value_median=median(Value),yr_min = min(Year), yr_max=max(Year))

columns_summary

## F- Sort rows by column values

#sort the table by Crop in ascending order then by Year in descending order
data_sorted <- arrange(data, Crop, desc(Year))

## G- Combining data manipulation functions using the pipe
data.final <- data %>%
  filter(Information == "Yield (Hg/Ha)", 
         Crop == "Oats" | Crop == "Buckwheat",
         Year >= 2005 & Year <= 2011)  %>% 
  mutate(Country = ifelse(Country == "Canada", "CAN", "USA")) %>%
  select(Country, Value,Crop,Year) %>% arrange(Crop, desc(Year))

head(data.final, 3)

###### Summarizing data by group

#The minimum andmaximum years from the Year column and mean values for which crop data are available by crop type
Data_group_1 <- data %>% 
  group_by(Crop) %>% 
  summarise(yr_min = min(Year),mean_Value=mean(Value), yr_max=max(Year))

# the number of records by Crop type 
Data_group_2 <- data %>%
  filter(Information == "Yield (Hg/Ha)", 
         Year >= 2005 & Year <=2010, 
         Country=="United States of America") %>%
  group_by(Crop) %>%
  summarise(Count = n())

# Summarize by mean yield and year range

Data_group_3 <- data %>%
  filter(Information == "Yield (Hg/Ha)", 
         Year >= 2005 & Year <=2010, 
         Country=="United States of America") %>%
  group_by(Crop) %>%
  summarise( Yield = mean(Value), `Number of Years` = max(Year) - min(Year)) 
######################################################################################

##### Tidying/reshaping tables with tidyr

# A 2014 Boston (Logan airport) flight data summary table will be used in this exercise

## df dataset is a wide shape
df <- data.frame( Weekday = c( "Mon", "Tues", "Wed", "Thurs", "Fri" ),
                  Q1      = c(  9.9 ,  4.9  ,  8.8 ,   12.2 ,  12.2 ),
                  Q2      = c(  5.4 ,  9.7  , 11.1 ,   10.2 ,   8.1 ),
                  Q3      = c(  8.8 ,  7.9  , 10.2 ,   9.2  ,   7.9 ),
                  Q4      = c(  6.9 ,    5  ,  9.3 ,   9.7  ,   5.6 ) )

### Reshape from Wide to long
df.long <- pivot_longer(df, cols=c(Q1,Q2,Q3,Q4), names_to = "Quarter", values_to = "Delay")

## Reshape from long to wide 
df.wide <- pivot_wider(df.long, names_from = Quarter, values_from = Delay) 

#################################################################################################

###### Joining tables in R 

df <- data.frame( x = c(1, 23, 4, 43, 2, 17),
                  y = c("a", "b", "b", "b", "a", "d"),
                  stringsAsFactors = FALSE)

dj <- data.frame( z = c("apple", "pear", "orange"),
                  y = c("a", "b", "c"),
                  stringsAsFactors = FALSE)

# left joint
left <- df %>% 
  left_join(dj, by = "y")
# right joint
right <- df %>% 
  right_join(dj, by="y")
# Inner join
inner <- df %>% 
  inner_join(dj, by="y")

# Full join 
full <- df %>% 
  full_join(dj, by="y")




