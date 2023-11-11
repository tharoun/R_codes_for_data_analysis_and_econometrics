##### Efficient way to install missing packages if needed and load all packages 

# Package names
packages <- c("sjPlot","plotly","stargazer","AER","huxtable","fixest","modelsummary","ggpmisc","naniar","NHANES","conflicted",
              "lme4","tidyverse","ggplot2","here", "readxl", "dplyr", "tidyr", "ggfortify", "DT", "reshape2",
              "knitr", "lubridate", "pwr", "psy", "car", "doBy", 
              "RcmdrMisc", "questionr", "vcd", "multcomp", "KappaGUI", "rcompanion", 
              "FactoMineR", "factoextra", "corrplot", "ltm", "goeveg", "corrplot", "FSA",
              "MASS", "scales", "nlme", "psych", "ordinal", "lmtest", "ggpubr", "dslabs", 
              "stringr", "assist", "ggstatsplot", "forcats", "styler", "remedy", 
              "addinslist", "esquisse", "summarytools", "magrittr","fmsb","likert","lme4","lmerTest", 
              "pander", "cluster", "abind","janitor","skimr","rstatix","flextable","gtsummary","gganimate")

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
conflict_prefer("filter", "plotly")