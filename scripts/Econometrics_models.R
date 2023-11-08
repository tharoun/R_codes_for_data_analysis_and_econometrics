##########################################################################
#                   R code sample for econometrics                       #                     
#                        by: Harouna TRAORE                              #
##########################################################################


### loading packages
source(here::here("scripts","Packages.R"))

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