##########################################################################
#                   R code sample for descriptives statistics            #                     
#                        by: Harouna TRAORE                              #
##########################################################################


### loading packages
source(here::here("scripts","Packages.R"))


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