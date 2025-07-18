###############################
## Project: Pencil Profiling
## Author: Aisha Washington
## Last Update: 2025-07-17
##
## Summary: Build step-wise regression models from dummy variables to 
##          identify high and low value Pencil customer traits for each 
##          product stock keeping unit (SKU).
## Resources: https://www.statology.org/stepwise-regression-r/
###############################

###############################
## Load Packages ##############
###############################
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyverse")

library(dplyr)
library(ggplot2)
library(tidyverse)

###############################
## Load Data ##################
###############################
df <- read.csv("clean_profile_dataset.csv")
head(df)

###############################
## Build Model ################
###############################
# Select feature to model (usage or gross_revenue)
feature <- df$gross_revenue

# Build Initial Model
model <- lm(feature~age_26.45+age_46.65+age_.65+
              sex_Male+sex_Other+
              income_.500..1000+income_Above..1000+
              state_IA+state_IN+state_MN+state_MO+state_WI
            ,data = df
            ,weights = cust_id)

summary(model)

## Check Normality of Residuals
hist(residuals(model), col = "steelblue")
# Check not passed:
# Results show multi-modality (two peaks instead of one)

## Check Variance of Residuals
plot(fitted(model), residuals(model))
# Check not passed:
# Results show trends in variance

# Build Models by SKU
model_AB <- lm(feature~age_26.45+age_46.65+age_.65+
                 sex_Male+sex_Other+
                 income_.500..1000+income_Above..1000+
                 state_IA+state_IN+state_MN+state_MO+state_WI
               ,data = df
               ,subset = (df$product_sku=="AB")
               ,weights = cust_id)
model_AC <- lm(feature~age_26.45+age_46.65+age_.65+
                 sex_Male+sex_Other+
                 income_.500..1000+income_Above..1000+
                 state_IA+state_IN+state_MN+state_MO+state_WI
               ,data = df
               ,subset = (df$product_sku=="AC")
               ,weights = cust_id)
model_BB <- lm(feature~age_26.45+age_46.65+age_.65+
                 sex_Male+sex_Other+
                 income_.500..1000+income_Above..1000+
                 state_IA+state_IN+state_MN+state_MO+state_WI
               ,data = df
               ,subset = (df$product_sku=="BB")
               ,weights = cust_id)
model_BC <- lm(feature~age_26.45+age_46.65+age_.65+
                 sex_Male+sex_Other+
                 income_.500..1000+income_Above..1000+
                 state_IA+state_IN+state_MN+state_MO+state_WI
               ,data = df
               ,subset = (df$product_sku=="BC")
               ,weights = cust_id)

# Build Intercept-only Models for each SKU
intercept_only <- lm(feature ~ 1, data=df, weights = cust_id)
intercept_only_AB <- lm(feature ~ 1, data=df
                     ,subset = (df$product_sku=="AB")
                     ,weights = cust_id)
intercept_only_AC <- lm(feature ~ 1, data=df
                        ,subset = (df$product_sku=="AC")
                        ,weights = cust_id)
intercept_only_BB <- lm(feature ~ 1, data=df
                        ,subset = (df$product_sku=="BB")
                        ,weights = cust_id)
intercept_only_BC <- lm(feature ~ 1, data=df
                        ,subset = (df$product_sku=="BC")
                        ,weights = cust_id)

# Forward Step-wise Regression Step
forward <- step(intercept_only, direction='forward', scope=formula(model), trace=0)
forward_AB <- step(intercept_only_AB, direction='forward', scope=formula(model_AB), trace=0)
forward_AC <- step(intercept_only_AC, direction='forward', scope=formula(model_AC), trace=0)
forward_BB <- step(intercept_only_BB, direction='forward', scope=formula(model_BB), trace=0)
forward_BC <- step(intercept_only_BC, direction='forward', scope=formula(model_BC), trace=0)

# Check Step-wise Results
forward$anova
forward_AB$anova
forward_AC$anova
forward_BB$anova
forward_BC$anova

# View Final Models
forward$coefficients
forward_AB$coefficients
forward_AC$coefficients
forward_BB$coefficients
forward_BC$coefficients


###############################
## Make Predictions ###########
###############################
df_pred <- df %>%
  mutate(pred = if_else(product_sku=="AB",predict(model_AB,df)
                        ,if_else(product_sku=="AC",predict(model_AC,df)
                                 ,if_else(product_sku=="BB",predict(model_BB,df)
                                          ,if_else(product_sku=="BC",predict(model_BC,df),0))))
  )

###############################
## Reshape Data ###############
###############################
df_reshaped <- df_pred %>%
  mutate(age = if_else(df$age_.25==1,"<25"
                       ,if_else(df$age_26.45==1,"26-45"
                                ,if_else(df$age_46.65==1,"46-65"
                                         ,if_else(df$age_.65==1,">65","missing"))))
         ,sex = if_else(df$sex_Female==1,"Female"
                        ,if_else(df$sex_Male==1,"Male"
                                 ,if_else(df$sex_Other==1,"Other","missing")))
         ,income = if_else(df$income_Under..500==1,"Under $500"
                           ,if_else(df$income_.500..1000==1,"$500-$1000"
                                    ,if_else(df$income_Above..1000==1,"Above $1000","missing")))
         ,state = if_else(df$state_IL==1,"IL"
                          ,if_else(df$state_WI==1,"WI"
                                   ,if_else(df$state_MN==1,"MN"
                                            ,if_else(df$state_IA==1,"IA"
                                                     ,if_else(df$state_MO==1,"MO"
                                                              ,if_else(df$state_IN==1,"IN","missing"))))))
         ,weighted_pred = cust_id*pred
         ) %>% 
  select(c("product_sku", "cust_id", "age", "sex", "income", "state","pred","weighted_pred"))

###############################
## Export Results #############
###############################
export <- df_reshaped

write.csv(export, "profile_counts.csv")
