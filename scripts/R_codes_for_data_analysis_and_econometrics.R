##########################################################################
#                        R code sample for data analysis and econometrics#                     
#                        by: Harouna TRAORE                              #
##########################################################################


##### Efficient way to install missing packages if needed and load all packages 

# Package names
packages <- c("stargazer","AER","huxtable","fixest","modelsummary","ggpmisc","naniar","NHANES","conflicted","lme4","tidyverse","ggplot2","here", "readxl", "dplyr", "tidyr", "ggfortify", "DT", "reshape2",
              "knitr", "lubridate", "pwr", "psy", "car", "doBy", 
              "RcmdrMisc", "questionr", "vcd", "multcomp", "KappaGUI", "rcompanion", 
              "FactoMineR", "factoextra", "corrplot", "ltm", "goeveg", "corrplot", "FSA",
              "MASS", "scales", "nlme", "psych", "ordinal", "lmtest", "ggpubr", "dslabs", 
              "stringr", "assist", "ggstatsplot", "forcats", "styler", "remedy", 
              "addinslist", "esquisse", "summarytools", "magrittr","fmsb", 
              "pander", "cluster", "abind","janitor","skimr","rstatix","flextable","gtsummary")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#####Efficient way to manage packages conflicts in R 
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("SD", "modelsummary")


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
data_sub_column_1 <- data %>% dplyr::select(Information,crop)
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
####################################################################################


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

#############################################################################################

####### Descriptive statitics in R 
## The dataset of cases from a simulated Ebola epidemic
linelist <- readRDS(here::here("raw_data", "linelist_cleaned.rds"))

## get information about each variable in a dataset with skimr packages
skim(linelist)

## Summary statistics for some variables with rstatix
sum_stats <- linelist %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp,  # columns to calculate for
    type = "common")  
## Summary statistics for some variables with dplyr
sum_stats_dplyr <- linelist %>% 
  group_by(outcome) %>% 
  summarise(across(.cols = c(age_years, temp, wt_kg, ht_cm), # columns
                   .fns = list("mean" = mean, "sd" = sd),    # multiple functions 
                   na.rm=T))                                 # extra arguments

### Cross tabulation 
cross_tab <- linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

### Printable version 
linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%                     # convert to image
  flextable::autofit() %>%                       # ensure only one line per row
  flextable::save_as_docx(path = "Outputs/tabyl.docx")   # save as Word document to filepath


#### Descriptive using modelsummary package and export into excel file

# Load data
census <-
  read_rds(
    here("raw_data",
      "census.rds"
    )
  )
### Formula 
descriptives <-All(census) ~ N + Mean + SD + Median + Min + Max

## Creating table
summary_stats_table <-datasummary(
  descriptives,
  data = census,
 output = "huxtable"
)
# Format table
summary_stats_table %>%
  # Use first row as table header
  set_header_rows(1, TRUE) %>%
  # Use first column as row header
  set_header_cols(1, TRUE) %>%
  # Don't round large numbers
  set_number_format(everywhere, 2:ncol(.), "%9.0f") %>%
  # Centralize cells in first row
  set_align(1, everywhere, "center") %>%
  # Set a theme for quick formatting
  theme_basic()
##Export table into excel format
quick_xlsx(
  summary_stats_table, # object to be exported
  file = here( # file path to output file
    "Outputs",
    "summary-stats.xlsx"
  )
)
##############################################################################################

########## Visualisation using ggplot ######

#### Bar plots : data etalons AFCON
data_etalons <- read_excel(here::here("raw_data", "data_etalons.xlsx"))

graph_data<-data_etalons %>% group_by(Adversaire,Resultat) %>%summarise(Nombre= n()) 

graph_data$Nombre<-as.integer(graph_data$Nombre)

# Use position = position_dodge() 
p <- ggplot(graph_data, aes(x = Adversaire, y =Nombre )) +
  geom_col(aes(color = Resultat, fill = Resultat), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#FC4E07","#E7B800","#00AFBB"))+
  scale_fill_manual(values = c("#FC4E07","#E7B800","#00AFBB"))
p

p + geom_text(
  aes(label = Nombre, group = Resultat), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
)

result_graph<-p+labs(title = "Résultat des matchs étalons vs futurs adversaires à la CAN",
                     x = " ADVERSAIRE", y = "NOMBRE",caption = "Source: Matchendirect.fr et travaux de l'auteur (T6H7)")+scale_y_continuous(limits = c(0,max(graph_data$Nombre)+1))+ geom_text(
                       aes(label = Nombre, group = Resultat), 
                       position = position_dodge(0.8),
                       vjust = -0.3, size = 7
                     )+
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 15,color = "darkgreen",hjust = 0.5),
    legend.background = element_rect(
      fill = "white", 
      linewidth = 4, 
      colour = "white"
    ),
    legend.justification = c(0.5,0.85),
    legend.position = c(0.5,0.85),
    legend.text=element_text(size=9,face="bold"),
    legend.direction="horizontal",
    axis.ticks = element_line(colour = "grey70", linewidth = 9),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
    plot.caption = element_text(hjust=0,face = "bold")
  )

result_graph

#### Boxplot with points 

### Sample data set
# Sample dataset
set.seed(3)
df <- data.frame(x = rexp(100),
                 group = sample(paste("Group", 1:3),
                                size = 100,
                                replace = TRUE))

# Vertical box plot by group
boxplot(x ~ group, data = df, col = "white")

# Points
stripchart(x ~ group,
           data = df,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)

###### lineplot  
# Data
df <- economics[economics$date > as.Date("2000-01-01"), ]

# Shade from 2000 to 2004 and from 2010 to 2015
shade <- data.frame(x1 = c(as.Date("2000-01-01"), as.Date("2013-01-01")),
                    x2 = c(as.Date("2003-01-01"), as.Date("2015-01-01")),
                    min = c(-Inf, -Inf), max = c(Inf, Inf))

ggplot() +
  geom_line(data = df, aes(x = date, y = unemploy),linewidth = 1)+
  geom_vline(xintercept = as.Date("2007-09-15"),
             linetype = 2, color = 2, linewidth = 1) +
  geom_rect(data = shade, aes(xmin = x1, xmax = x2, ymin = min, ymax = max),
            fill = c("green", "red"), alpha = 0.2)



#### Radar plot 
# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

  
###################################################################################################################################################


################################### Econometrics using R ##########################################

## Linear regressions
reg1 <- lm(divorce~pop+popurban+marriage,census)
summary(reg1)
#The feols command from package fixest for more flexibility in model specification:
reg2 <-
  feols(
    divorce ~ pop + popurban + marriage | region,
    census,
    se = "iid"
  )
summary(reg2)

### Nonlinear Regression

# estimate a log-linear model 
reg3 <-
  feols(
    log(divorce) ~ pop + popurban + marriage | region,
    census,
    se = "iid"
  )
summary(reg3)
##Log-log linear model
reg4 <-
  feols(
    log(divorce) ~  log(pop)+ log(popurban) + log(marriage) | region,
    census,
    se = "iid"
  )
summary(reg4)

# export regressions outputs 
huxreg(
  reg1, reg2, reg3,reg4)


##### Binary Dependent Variables

## HMDA from AER packages
data(HMDA)
# estimate the simple probit model
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

summary(denyprobit)
### Model diagnostic 
## Coefficient significativity test
coeftest(denyprobit, vcov. = vcovHC, type = "HC1")
# estimate the simple logit model
denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")

