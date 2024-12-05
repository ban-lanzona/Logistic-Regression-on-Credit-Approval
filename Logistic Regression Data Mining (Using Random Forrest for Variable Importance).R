#Cleaning the Dataset

#Initializing the R file

##Clearing the environment and plots
rm(list = ls()) 
dev.off()  # But only if there IS a plot
cat("\014")  # ctrl+L

##Packages
library(tidyverse)
library(gridExtra)
library(broom)
require(pacman)
library(caTools)
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages('caret', dependencies = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, gmodels,caret,ROCR)
pacman::p_load(pacman, tidyverse, gmodels,ROCR, rpart, rpart.plot,caret)

if (!require("gt")) install.packages("gt")
library(gt)
library(ggplot2)
library(reshape2)
library(cowplot)
library(psych)
library(ipred)
library(car)

germancredit = read.csv("GermanData.csv")

german.factors = c("X1","X3","X4","X6","X7","X9","X10","X12","X14","X15","X17","X19","X20","X21")
german.numerical = c("X2","X5","X8","X11","X13","X16","X18")
germancredit[german.factors] = lapply(germancredit[german.factors],as.factor)
germancredit[german.numerical] = lapply(germancredit[german.numerical],as.numeric)

#renaming using dplyr
germancredit= germancredit %>% rename("Debtor_CheckingStatus" = "X1", "Credit_Duration" = "X2",
                                      "Debtor_History" = "X3", "Credit_Purpose" = "X4",
                                      "Credit_Amount" = "X5", "Debtor_Savings" = "X6", 
                                      "Debtor_Employment" = "X7","Credit_Installment" = "X8", 
                                      "Debtor_PersonalStatus" = "X9","Credit_OtherDebtors" = "X10",
                                      "Debtor_LengthResidence" = "X11", "Debtor_Property" = "X12",
                                      "Debtor_Age" = "X13", "Debtor_OtherInstallments" = "X14",
                                      "Debtor_Housing" = "X15","Debtor_OtherCredits" = "X16", 
                                      "Debtor_Job" = "X17","Debtor_Dependents" = "X18", 
                                      "Debtor_Telephone" = "X19","Debtor_ForeignWorker" = "X20",
                                      "Credit_RiskRating" = "X21")

#Changing levels of factors
germancredit$Debtor_CheckingStatus = recode_factor(germancredit$Debtor_CheckingStatus, "A11"="Negative balance",
                                                   "A12"="0 <= or < 200 DM",
                                                   "A13"="Greater or equal to 200 DM",
                                                   "A14"="No Checking Account")

germancredit$Debtor_History = recode_factor(germancredit$Debtor_History, "A30"="No credits/All paid",
                                            "A31"="All paid at Bank",
                                            "A32"="Duly paid until now",
                                            "A33"="Delay in history",
                                            "A34"="Critical Account"
)

germancredit$Credit_Purpose = recode_factor(germancredit$Credit_Purpose, "A40"="Car (new)",
                                            "A41"="Car (used)",
                                            "A42"="Furniture/equipment", 
                                            "A43"="Radio/television",
                                            "A44"="Domestic appliances",
                                            "A45"="Repairs",
                                            "A46"="Education",
                                            "A48"="Retraining",
                                            "A49"="Business",
                                            "A410"="Others"
)

germancredit$Debtor_Savings = recode_factor(germancredit$Debtor_Savings, "A61"="Less than 100 DM",
                                            "A62"="100 <= or < 500 DM",
                                            "A63"="500 <= or < 1000 DM", 
                                            "A64"="Greater or equal to 1000 DM", 
                                            "A65"="Unknown or no account"
)

germancredit$Debtor_Employment = recode_factor(germancredit$Debtor_Employment, "A71"="Unemployed", 
                                               "A72"="Less than a year",
                                               "A73"="1 <= or < 4 years",
                                               "A74"="4 <= or < 7 years",
                                               "A75"="Greater or equal to 7 years"
)

germancredit$Credit_OtherDebtors = recode_factor(germancredit$Credit_OtherDebtors, "A101"="None", 
                                                 "A102"="Co-applicant",
                                                 "A103"="Guarantor"
)

germancredit$Debtor_Property = recode_factor(germancredit$Debtor_Property, "A121"="Real Estate",
                                             "A122"="SSA or Life Insurance",
                                             "A123"="Car or others",
                                             "A124"="Unknown or no property"
)

germancredit$Debtor_OtherInstallments = recode_factor(germancredit$Debtor_OtherInstallments, "A141"="Bank",
                                                      "A142"="Stores",
                                                      "A143"="None"
)

germancredit$Debtor_Housing = recode_factor(germancredit$Debtor_Housing, "A151"="Rent", 
                                            "A152"="Own",
                                            "A153"="For free"
)

germancredit$Debtor_Job = recode_factor(germancredit$Debtor_Job, "A171"="Non-resident Unemployed",
                                        "A172"="Resident Unemployed",
                                        "A173"="Skilled Employee",
                                        "A174"="Management/Self-employed/Officer"
)

germancredit$Debtor_Telephone = recode_factor(germancredit$Debtor_Telephone, "A191"="None",
                                              "A192"="Yes")

germancredit$Debtor_ForeignWorker = recode_factor(germancredit$Debtor_ForeignWorker, "A201"="Yes",
                                                  "A202"="No"
)

germancredit$Credit_RiskRating = recode_factor(germancredit$Credit_RiskRating, "1"="Good", 
                                               "2"="Bad"
)

germancredit$Debtor_PersonalStatus = recode_factor(germancredit$Debtor_PersonalStatus, "A91"="Male: divorced/separated",
                                                   "A92"="Female : divorced/separated/married",
                                                   "A93"="Male : single",
                                                   "A94"="Male : married/widowed"
)

germancredit$Debtor_Gender = ifelse(germancredit$Debtor_PersonalStatus == "Female : divorced/separated/married", "Female", "Male")

germancredit$Debtor_MaritalStatus = ifelse(germancredit$Debtor_PersonalStatus == "Male : single", "Single", "Not Single")

german.factors2 = c("Debtor_Gender","Debtor_MaritalStatus")
germancredit[german.factors2] = lapply(germancredit[german.factors2],as.factor)

#Creating the 1 variable

germancredit$one = 1
view(germancredit)


#Preliminary exploration of the variables

# Checking numerical variables 
## Checking density plots
germancredit_numplot = list()
germancredit_vars = as.list(colnames(dplyr::select_if(germancredit[,1:23], is.numeric)))

for (var_name in germancredit_vars) {
  plot = ggplot(data = germancredit, aes(x = .data[[var_name]], fill = Credit_RiskRating)) +
    geom_density(alpha=0.5) + scale_fill_manual(values=c("darkslategray1", "coral")) +
    labs(title = var_name) 
  germancredit_numplot[[var_name]] = plot
}

germancredit_crossnumplot = gridExtra::grid.arrange(grobs = germancredit_numplot, ncol = 3)

##Checking the impact of Credit Risk Rating
describeBy(Credit_Duration+Credit_Amount+Credit_Installment+Debtor_LengthResidence+
             Debtor_Age+Debtor_OtherCredits+Debtor_Dependents~Credit_RiskRating, data=germancredit)

#______________________________________________________________________________________________________________#

#Test for linearity for continuous

##Splitting data for training and testing
set.seed(13)
germancredit_ind = germancredit$Credit_RiskRating %>%
  createDataPartition(p = 0.70, list = FALSE)

germancredit_train = germancredit[germancredit_ind, ]
germancredit_test = germancredit[-germancredit_ind, ] 

germancredit_numeric_samplemodel = glm(Credit_RiskRating ~., data = germancredit_train[,1:21],family = "binomial")
germancredit_numeric_samplepredict = predict(germancredit_numeric_samplemodel, type = "response")

germancredit_numeric = germancredit_train %>% dplyr::select(Credit_Amount,Credit_Duration,Debtor_Age)
germancredit_numeric$logit = log(germancredit_numeric_samplepredict/(1-germancredit_numeric_samplepredict))
germancredit_numeric = germancredit_numeric %>% 
  mutate(logit = log(germancredit_numeric_samplepredict/(1-germancredit_numeric_samplepredict))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(germancredit_numeric, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") + labs(title = "Analysis of Continuous Predictors for Linearity Assumption")

#______________________________________________________________________________________________________________#

#VIF Assumptions for multicolinearity

vif(germancredit_numeric_samplemodel)

#______________________________________________________________________________________________________________#

#Chart for the report
#Notable variables

#Credit Duration

ggplot(data = germancredit) +
  geom_density(mapping = aes(x = Credit_Duration, fill = Credit_RiskRating), alpha=0.5) + 
  scale_fill_manual(values=c("deepskyblue", "coral")) +
  theme_minimal() +
  labs(title = "Credit applications with good risk rating are mostly within low credit duration.", y = "Density", x="Duration", 
       subtitle = "Analysis of Credit Duration in months by Risk Rating") + 
  theme(plot.title=element_text(face="bold")) + theme(legend.position = "bottom") + 
  theme(axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none") + scale_x_continuous(limits = c(0, max(germancredit$Credit_Duration))) +
  annotate("label", x = 25, y = 0.041, label = "Good", color="black", fill= "deepskyblue") +
  annotate("label", x = 50, y = 0.014, label = "Bad", color="black", fill= "coral") +
  labs(caption="*Credit duration is based on the credit payment terms given to the debtor.") + 
  theme(plot.caption = element_text(hjust = 0))

#Credit Amount

ggplot(data = germancredit) +
  geom_density(mapping = aes(x = Credit_Amount, fill = Credit_RiskRating), alpha=0.5) + 
  scale_fill_manual(values=c("deepskyblue", "coral")) +
  theme_minimal() +
  labs(title = "Good risk ratings are mostly concentrated below 5,000DM credit amount.", y = "Density", x="Duration", 
       subtitle = "Analysis of Credit Amount in Deutsche mark (DM) by Risk Rating") + 
  theme(plot.title=element_text(face="bold")) + theme(legend.position = "bottom") + 
  theme(axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none") + scale_x_continuous(limits = c(0, max(germancredit$Credit_Amount))) +
  annotate("label", x = 4250, y = 2e-04, label = "Good", color="black", fill= "deepskyblue") +
  annotate("label", x = 8950, y = 0.5e-04, label = "Bad", color="black", fill= "coral")

#______________________________________________________________________________________________________________#

#Utilising t-test and chisquare to further evaluate the variables
## Checking significance of numeric variables to dependent variable

Credit_Duration_test = t.test(germancredit$Credit_Duration ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Credit_Amount_test = t.test(germancredit$Credit_Amount ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Credit_Installment_test = t.test(germancredit$Credit_Installment ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Debtor_LengthResidence_test = t.test(germancredit$Debtor_LengthResidence ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Debtor_Age_test = t.test(germancredit$Debtor_Age ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Debtor_OtherCredits_test = t.test(germancredit$Debtor_OtherCredits ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)
Debtor_Dependents_test = t.test(germancredit$Debtor_Dependents ~ germancredit$Credit_RiskRating,mu=0,alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)

numeric_t_test = as.data.frame(map_df(list(Credit_Duration_test,Credit_Amount_test,Credit_Installment_test,Debtor_LengthResidence_test,
                                           Debtor_Age_test,Debtor_OtherCredits_test,Debtor_Dependents_test), tidy)) %>% select(estimate,statistic,p.value) 
row.names(numeric_t_test) = c("Credit_Duration","Credit_Amount","Credit_Installment","Debtor_LengthResidence",
                              "Debtor_Age","Debtor_OtherCredits","Debtor_Dependents")
numeric_t_test$Significance_at_05 = ifelse(numeric_t_test$p.value<=0.05,"Significant","Not Significant")
numeric_t_test$Predictors = c(rownames(numeric_t_test))
numeric_t_test

#______________________________________________________________________________________________________________#

#Table for report

numeric_t_test$statistic = numeric_t_test$statistic %>% round(digits = 2)
numeric_t_test$p.value = numeric_t_test$p.value %>% round(digits = 4)

numeric_t_test %>% select("Predictors","statistic","p.value","Significance_at_05") %>% gt() %>%
  tab_header(title= md("Significance test on Numerical Variables"),
             subtitle = md("Analysis of numerical variables by t-test for independence")) %>% 
  opt_align_table_header(align="left") %>%
  cols_move_to_start(Predictors) %>%
  tab_style(
    locations = cells_body(columns= everything(), rows= Significance_at_05 == "Not Significant"),
    style = cell_fill(color = "#ffcccb") 
  ) %>%
  tab_style(
    locations = cells_body(columns=Predictors),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_title("title"),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(weight="bold")
  ) %>%
  cols_label(Significance_at_05 = "Significance") %>%
  tab_footnote(
    footnote = "Significance at 0.05 alpha level",
    locations = cells_column_labels(columns=`Significance_at_05`))

#______________________________________________________________________________________________________________#

# Checking factor variables 
## Checking bar plots

germancredit_facplot = list()
germancredit_facs = as.list(colnames(dplyr::select_if(germancredit, is.factor)))

for (fac_name in germancredit_facs) {
  plot = ggplot(data = germancredit, aes(x = .data[[fac_name]], fill = Credit_RiskRating)) +
    geom_bar() +
    labs(title = fac_name) + coord_flip()
  germancredit_facplot[[fac_name]] = plot
}

germancredit_crossfaclot = gridExtra::grid.arrange(grobs = germancredit_facplot, ncol = 3)

##xtabs
germancredit_facs_tabs = lapply(germancredit[,unlist(germancredit_facs)],
                                function(fac_name) xtabs(~fac_name + germancredit$Credit_RiskRating))
germancredit_facs_tabs

#______________________________________________________________________________________________________________#

#Chart for the report
#Notable variables

#Debtor_CheckingStatus

ggplot(data = germancredit, aes(x = Debtor_CheckingStatus, fill = Credit_RiskRating)) +
  geom_bar(position = "dodge") + theme_minimal() +
  geom_text(stat='count', aes(label=after_stat(count), x = Debtor_CheckingStatus),  position=position_dodge(width=0.9), vjust=-1) +
  scale_fill_manual(values=c("deepskyblue", "coral")) + scale_y_continuous(limits = c(0,400)) +
  theme(plot.title=element_text(face="bold")) + theme(legend.position = "bottom") + 
  theme(axis.text.y = element_blank(),axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none", axis.title.y = element_blank()) + 
  labs(title = "Credit applications with bad risk rating are mostly with checking accounts having less than 200DM.", x="Status of Account", 
       subtitle = "Analysis of Existing Checking Accounts in DM by Risk Rating",
       caption="*Checking accounts are type of deposit accounts within the bank.") + 
  theme(plot.caption = element_text(hjust = 0))

#______________________________________________________________________________________________________________#

# Significance Tests
## Checking factor variables 

germancredit_fact = as.list(colnames(dplyr::select_if(germancredit, is.factor)))
germancredit_fact

##Checking if variable needs Fisher with less than 5 as assumption of Chisqaure

fact_x_tabs = as.data.frame(list(
  min(xtabs(~Credit_RiskRating + Debtor_CheckingStatus,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_History,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Credit_Purpose,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Savings,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Employment,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_PersonalStatus,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Credit_OtherDebtors,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Property,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_OtherInstallments,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Housing,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Job,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Telephone,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_ForeignWorker,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_Gender,  data=germancredit)),
  min(xtabs(~Credit_RiskRating + Debtor_MaritalStatus,  data=germancredit))
)
) %>% pivot_longer(cols = 1:15) %>% as.data.frame() %>% select(value)

row.names(fact_x_tabs) = c(germancredit_fact[1:13],germancredit_fact[15:16])
fact_x_tabs$Test_Required = ifelse(fact_x_tabs$value<=5,"Fisher exact test","Chi-square Test")
fact_x_tabs

## Checking relationship between factor variables and dependent variable

Debtor_CheckingStatus_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_CheckingStatus)
Debtor_History_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_History)
Credit_Purpose_test = fisher.test(germancredit$Credit_RiskRating, germancredit$Credit_Purpose, simulate.p.value=TRUE)
Debtor_Savings_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Savings)
Debtor_Employment_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Employment)
Debtor_PersonalStatus_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_PersonalStatus)
Credit_OtherDebtors_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Credit_OtherDebtors)
Debtor_Property_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Property)
Debtor_OtherInstallments_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_OtherInstallments)
Debtor_Housing_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Housing)
Debtor_Job_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Job)
Debtor_Telephone_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Telephone)
Debtor_ForeignWorker_test = fisher.test(germancredit$Credit_RiskRating, germancredit$Debtor_ForeignWorker)
Debtor_Gender_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_Gender)
Debtor_MaritalStatus_test = chisq.test(germancredit$Credit_RiskRating, germancredit$Debtor_MaritalStatus)

fact_chi_test = as.data.frame(map_df(list(Debtor_CheckingStatus_test,Debtor_History_test,Credit_Purpose_test,Debtor_Savings_test,
                                          Debtor_Employment_test,Debtor_PersonalStatus_test,Credit_OtherDebtors_test,Debtor_Property_test,
                                          Debtor_OtherInstallments_test,Debtor_Housing_test,Debtor_Job_test,Debtor_Telephone_test,
                                          Debtor_ForeignWorker_test,Debtor_Gender_test,Debtor_MaritalStatus_test), tidy)) %>% select(statistic,p.value) 
row.names(fact_chi_test) = c(germancredit_fact[1:13],germancredit_fact[15:16])
fact_chi_test$Significance_at_05 = ifelse(fact_chi_test$p.value<=0.05,"Significant","Not Significant")
fact_chi_test$Predictors = c(rownames(fact_chi_test))
fact_chi_test 

#______________________________________________________________________________________________________________#

#Table for report

fact_chi_test$statistic = fact_chi_test$statistic %>% round(digits = 2)
fact_chi_test$p.value = fact_chi_test$p.value %>% round(digits = 4)
fact_chi_test

fact_chi_test %>% gt() %>%
  tab_header(title= md("Significance test on Categorical Variables"),
             subtitle = md("Analysis of categorical variables by Chi-square or Fisher Exact Test")) %>% 
  opt_align_table_header(align="left") %>%
  cols_move_to_start(Predictors) %>%
  tab_style(
    locations = cells_body(columns= everything(), rows= Significance_at_05 == "Not Significant"),
    style = cell_fill(color = "#ffcccb") 
  ) %>%
  tab_style(
    locations = cells_body(columns=Predictors),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_title("title"),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(weight="bold")
  ) %>%
  cols_label(Significance_at_05 = "Significance") %>%
  tab_footnote(
    footnote = "Significance at 0.05 alpha level",
    locations = cells_column_labels(columns=`Significance_at_05`)) %>%
  tab_footnote(
    footnote = "Chi-square is used for most variables; however, as some violate the assumption of having less than five samples, Fisher test is used instead.",
  )


#Pre-evaluation of Variables 
#(to filter important factors and numerical variables for further statistical tets)

##Creating other data sets through multiple training data

germancredit_train_ind = germancredit_train$Credit_RiskRating %>%
  createDataPartition(p = 0.66, list = FALSE)

germancredit_train_subAB = germancredit_train[germancredit_train_ind, ]
germancredit_train_subC = germancredit_train[-germancredit_train_ind, ]

germancredit_trainsubAB_ind = germancredit_train_subAB$Credit_RiskRating %>%
  createDataPartition(p = 0.5, list = FALSE)

germancredit_train_subA = germancredit_train_subAB[germancredit_trainsubAB_ind, ]
germancredit_train_subB = germancredit_train_subAB[-germancredit_trainsubAB_ind, ]

#______________________________________________________________________________________________________________#

#Creating the test set for variable selection through random forests method through MDAMDG approach (Han et al., 2013)

#Full training set (for an overview)
germancredit_seltrain = randomForest(Credit_RiskRating ~ .,
                                     data = germancredit_train, importance=T)
germancredit_Importanceplot = varImpPlot(germancredit_seltrain)
germancredit_Importance_rows = as.data.frame(rownames(germancredit_Importanceplot))

germancredit_Importance_MDA = as.data.frame(varImpPlot(germancredit_seltrain)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rows) 
germancredit_MDAorder = order(germancredit_Importance_MDA$MeanDecreaseAccuracy)
germancredit_Importance_MDA_ranked = germancredit_Importance_MDA[germancredit_MDAorder,] 
germancredit_Importance_MDA_ranked$MDAScore = rank(germancredit_Importance_MDA_ranked$MeanDecreaseAccuracy)

germancredit_Importance_MDG = as.data.frame(varImpPlot(germancredit_seltrain)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rows)
germancredit_MDGorder = order(germancredit_Importance_MDG$MeanDecreaseGini)
germancredit_Importance_MDG_ranked = germancredit_Importance_MDG[germancredit_MDGorder,] 
germancredit_Importance_MDG_ranked$MDGScore = rank(germancredit_Importance_MDG_ranked$MeanDecreaseGini)

germancredit_Importance_ranked = merge.data.frame(germancredit_Importance_MDA_ranked, germancredit_Importance_MDG_ranked, by="rownames(germancredit_Importanceplot)") 
germancredit_Importance_ranked = mutate(germancredit_Importance_ranked, MDAMDG = MDAScore + MDGScore) 
germancredit_MDAMDGorder = order(germancredit_Importance_ranked$MDAMDG,decreasing = TRUE)
germancredit_Importance_rankedbyMDGMDA = germancredit_Importance_ranked[germancredit_MDAMDGorder,]%>% top_n(10, MDAMDG) %>% rename("Predictors" = "rownames(germancredit_Importanceplot)") 
germancredit_Importance_rankedbyMDGMDA

#______________________________________________________________________________________________________________#

#For variable selection, the sets will be utilized.

#Set A
germancredit_seltrainA = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subA, importance=T)
germancredit_ImportanceplotA = varImpPlot(germancredit_seltrainA)
germancredit_Importance_rowsA = as.data.frame(rownames(germancredit_ImportanceplotA))

germancredit_Importance_MDAA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA) 
germancredit_MDAorderA = order(germancredit_Importance_MDAA$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedA = germancredit_Importance_MDAA[germancredit_MDAorderA,] 
germancredit_Importance_MDA_rankedA$MDAScore = rank(germancredit_Importance_MDA_rankedA$MeanDecreaseAccuracy)

germancredit_Importance_MDGA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA)
germancredit_MDGorderA = order(germancredit_Importance_MDGA$MeanDecreaseGini)
germancredit_Importance_MDG_rankedA = germancredit_Importance_MDGA[germancredit_MDGorderA,] 
germancredit_Importance_MDG_rankedA$MDGScore = rank(germancredit_Importance_MDG_rankedA$MeanDecreaseGini)

germancredit_Importance_rankedA = merge.data.frame(germancredit_Importance_MDA_rankedA, germancredit_Importance_MDG_rankedA, by="rownames(germancredit_ImportanceplotA)") 
germancredit_Importance_rankedA = mutate(germancredit_Importance_rankedA, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotA)",
                                                                                                                   "MDAMDG_subsetA" = "MDAMDG") 

#Set B
germancredit_seltrainB = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subB, importance=T)
germancredit_ImportanceplotB = varImpPlot(germancredit_seltrainB)
germancredit_Importance_rowsB = as.data.frame(rownames(germancredit_ImportanceplotB))

germancredit_Importance_MDAB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB) 
germancredit_MDAorderB = order(germancredit_Importance_MDAB$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedB = germancredit_Importance_MDAB[germancredit_MDAorderB,] 
germancredit_Importance_MDA_rankedB$MDAScore = rank(germancredit_Importance_MDA_rankedB$MeanDecreaseAccuracy)

germancredit_Importance_MDGB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB)
germancredit_MDGorderB = order(germancredit_Importance_MDGB$MeanDecreaseGini)
germancredit_Importance_MDG_rankedB = germancredit_Importance_MDGB[germancredit_MDGorderB,] 
germancredit_Importance_MDG_rankedB$MDGScore = rank(germancredit_Importance_MDG_rankedB$MeanDecreaseGini)

germancredit_Importance_rankedB = merge.data.frame(germancredit_Importance_MDA_rankedB, germancredit_Importance_MDG_rankedB, by="rownames(germancredit_ImportanceplotB)") 
germancredit_Importance_rankedB = mutate(germancredit_Importance_rankedB, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotB)",
                                                                                                                   "MDAMDG_subsetB" = "MDAMDG") 

#Set C
germancredit_seltrainC = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subC, importance=T)
germancredit_ImportanceplotC = varImpPlot(germancredit_seltrainC)
germancredit_Importance_rowsC = as.data.frame(rownames(germancredit_ImportanceplotC))

germancredit_Importance_MDAC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC) 
germancredit_MDAorderC = order(germancredit_Importance_MDAC$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedC = germancredit_Importance_MDAC[germancredit_MDAorderC,] 
germancredit_Importance_MDA_rankedC$MDAScore = rank(germancredit_Importance_MDA_rankedC$MeanDecreaseAccuracy)

germancredit_Importance_MDGC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC)
germancredit_MDGorderC = order(germancredit_Importance_MDGC$MeanDecreaseGini)
germancredit_Importance_MDG_rankedC = germancredit_Importance_MDGC[germancredit_MDGorderC,] 
germancredit_Importance_MDG_rankedC$MDGScore = rank(germancredit_Importance_MDG_rankedC$MeanDecreaseGini)

germancredit_Importance_rankedC = merge.data.frame(germancredit_Importance_MDA_rankedC, germancredit_Importance_MDG_rankedC, by="rownames(germancredit_ImportanceplotC)") 
germancredit_Importance_rankedC = mutate(germancredit_Importance_rankedC, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotC)",
                                                                                                                   "MDAMDG_subsetC" = "MDAMDG") 

#Storing results of first seed.
germancredit_Importance_ranked_results = select(germancredit_Importance_rankedA, Predictors, MDAMDG_subsetA) %>%
  merge.data.frame(select(germancredit_Importance_rankedB, Predictors, MDAMDG_subsetB), by="Predictors") %>%
  merge.data.frame(select(germancredit_Importance_rankedC, Predictors, MDAMDG_subsetC), by="Predictors") %>%
  rename("MDAMDG_subsetA_seed1" = "MDAMDG_subsetA",
         "MDAMDG_subsetB_seed1" = "MDAMDG_subsetB",
         "MDAMDG_subsetC_seed1" = "MDAMDG_subsetC")

germancredit_Importance_ranked_results

#______________________________________________________________________________________________________________#

#Using a second seed to get new MDAMDG results

##Splitting data for training and testing
set.seed(113)

germancredit_ind2 = germancredit$Credit_RiskRating %>%
  createDataPartition(p = 0.70, list = FALSE)

germancredit_train2 = germancredit[germancredit_ind2, ]
germancredit_test2 = germancredit[-germancredit_ind2, ] 

germancredit_train_ind = germancredit_train2$Credit_RiskRating %>%
  createDataPartition(p = 0.66, list = FALSE)

germancredit_train_subAB = germancredit_train2[germancredit_train_ind, ]
germancredit_train_subC = germancredit_train2[-germancredit_train_ind, ]

germancredit_trainsubAB_ind = germancredit_train_subAB$Credit_RiskRating %>%
  createDataPartition(p = 0.5, list = FALSE)

germancredit_train_subA = germancredit_train_subAB[germancredit_trainsubAB_ind, ]
germancredit_train_subB = germancredit_train_subAB[-germancredit_trainsubAB_ind, ]

#______________________________________________________________________________________________________________#

#Second seed

#Set A
germancredit_seltrainA = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subA, importance=T)
germancredit_ImportanceplotA = varImpPlot(germancredit_seltrainA)
germancredit_Importance_rowsA = as.data.frame(rownames(germancredit_ImportanceplotA))

germancredit_Importance_MDAA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA) 
germancredit_MDAorderA = order(germancredit_Importance_MDAA$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedA = germancredit_Importance_MDAA[germancredit_MDAorderA,] 
germancredit_Importance_MDA_rankedA$MDAScore = rank(germancredit_Importance_MDA_rankedA$MeanDecreaseAccuracy)

germancredit_Importance_MDGA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA)
germancredit_MDGorderA = order(germancredit_Importance_MDGA$MeanDecreaseGini)
germancredit_Importance_MDG_rankedA = germancredit_Importance_MDGA[germancredit_MDGorderA,] 
germancredit_Importance_MDG_rankedA$MDGScore = rank(germancredit_Importance_MDG_rankedA$MeanDecreaseGini)

germancredit_Importance_rankedA = merge.data.frame(germancredit_Importance_MDA_rankedA, germancredit_Importance_MDG_rankedA, by="rownames(germancredit_ImportanceplotA)") 
germancredit_Importance_rankedA = mutate(germancredit_Importance_rankedA, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotA)",
                                                                                                                   "MDAMDG_subsetA" = "MDAMDG") 

#Set B
germancredit_seltrainB = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subB, importance=T)
germancredit_ImportanceplotB = varImpPlot(germancredit_seltrainB)
germancredit_Importance_rowsB = as.data.frame(rownames(germancredit_ImportanceplotB))

germancredit_Importance_MDAB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB) 
germancredit_MDAorderB = order(germancredit_Importance_MDAB$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedB = germancredit_Importance_MDAB[germancredit_MDAorderB,] 
germancredit_Importance_MDA_rankedB$MDAScore = rank(germancredit_Importance_MDA_rankedB$MeanDecreaseAccuracy)

germancredit_Importance_MDGB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB)
germancredit_MDGorderB = order(germancredit_Importance_MDGB$MeanDecreaseGini)
germancredit_Importance_MDG_rankedB = germancredit_Importance_MDGB[germancredit_MDGorderB,] 
germancredit_Importance_MDG_rankedB$MDGScore = rank(germancredit_Importance_MDG_rankedB$MeanDecreaseGini)

germancredit_Importance_rankedB = merge.data.frame(germancredit_Importance_MDA_rankedB, germancredit_Importance_MDG_rankedB, by="rownames(germancredit_ImportanceplotB)") 
germancredit_Importance_rankedB = mutate(germancredit_Importance_rankedB, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotB)",
                                                                                                                   "MDAMDG_subsetB" = "MDAMDG") 

#Set C
germancredit_seltrainC = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subC, importance=T)
germancredit_ImportanceplotC = varImpPlot(germancredit_seltrainC)
germancredit_Importance_rowsC = as.data.frame(rownames(germancredit_ImportanceplotC))

germancredit_Importance_MDAC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC) 
germancredit_MDAorderC = order(germancredit_Importance_MDAC$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedC = germancredit_Importance_MDAC[germancredit_MDAorderC,] 
germancredit_Importance_MDA_rankedC$MDAScore = rank(germancredit_Importance_MDA_rankedC$MeanDecreaseAccuracy)

germancredit_Importance_MDGC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC)
germancredit_MDGorderC = order(germancredit_Importance_MDGC$MeanDecreaseGini)
germancredit_Importance_MDG_rankedC = germancredit_Importance_MDGC[germancredit_MDGorderC,] 
germancredit_Importance_MDG_rankedC$MDGScore = rank(germancredit_Importance_MDG_rankedC$MeanDecreaseGini)

germancredit_Importance_rankedC = merge.data.frame(germancredit_Importance_MDA_rankedC, germancredit_Importance_MDG_rankedC, by="rownames(germancredit_ImportanceplotC)") 
germancredit_Importance_rankedC = mutate(germancredit_Importance_rankedC, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotC)",
                                                                                                                   "MDAMDG_subsetC" = "MDAMDG") 

#Storing results of second seed.
germancredit_Importance_ranked_results = germancredit_Importance_ranked_results %>% merge.data.frame(select(germancredit_Importance_rankedA, Predictors, MDAMDG_subsetA), by="Predictors") %>%
  merge.data.frame(select(germancredit_Importance_rankedB, Predictors, MDAMDG_subsetB), by="Predictors") %>%
  merge.data.frame(select(germancredit_Importance_rankedC, Predictors, MDAMDG_subsetC), by="Predictors") %>%
  rename("MDAMDG_subsetA_seed2" = "MDAMDG_subsetA",
         "MDAMDG_subsetB_seed2" = "MDAMDG_subsetB",
         "MDAMDG_subsetC_seed2" = "MDAMDG_subsetC")
germancredit_Importance_ranked_results
#______________________________________________________________________________________________________________#

#Using a third seed to get new MDAMDG results

##Splitting data for training and testing
set.seed(1113)

germancredit_ind3 = germancredit$Credit_RiskRating %>%
  createDataPartition(p = 0.70, list = FALSE)

germancredit_train3 = germancredit[germancredit_ind3, ]
germancredit_test3 = germancredit[-germancredit_ind3, ] 

germancredit_train_ind = germancredit_train3$Credit_RiskRating %>%
  createDataPartition(p = 0.66, list = FALSE)

germancredit_train_subAB = germancredit_train3[germancredit_train_ind, ]
germancredit_train_subC = germancredit_train3[-germancredit_train_ind, ]

germancredit_trainsubAB_ind = germancredit_train_subAB$Credit_RiskRating %>%
  createDataPartition(p = 0.5, list = FALSE)

germancredit_train_subA = germancredit_train_subAB[germancredit_trainsubAB_ind, ]
germancredit_train_subB = germancredit_train_subAB[-germancredit_trainsubAB_ind, ]

#______________________________________________________________________________________________________________#

#Third seed

#Set A
germancredit_seltrainA = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subA, importance=T)
germancredit_ImportanceplotA = varImpPlot(germancredit_seltrainA)
germancredit_Importance_rowsA = as.data.frame(rownames(germancredit_ImportanceplotA))

germancredit_Importance_MDAA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA) 
germancredit_MDAorderA = order(germancredit_Importance_MDAA$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedA = germancredit_Importance_MDAA[germancredit_MDAorderA,] 
germancredit_Importance_MDA_rankedA$MDAScore = rank(germancredit_Importance_MDA_rankedA$MeanDecreaseAccuracy)

germancredit_Importance_MDGA = as.data.frame(varImpPlot(germancredit_seltrainA)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsA)
germancredit_MDGorderA = order(germancredit_Importance_MDGA$MeanDecreaseGini)
germancredit_Importance_MDG_rankedA = germancredit_Importance_MDGA[germancredit_MDGorderA,] 
germancredit_Importance_MDG_rankedA$MDGScore = rank(germancredit_Importance_MDG_rankedA$MeanDecreaseGini)

germancredit_Importance_rankedA = merge.data.frame(germancredit_Importance_MDA_rankedA, germancredit_Importance_MDG_rankedA, by="rownames(germancredit_ImportanceplotA)") 
germancredit_Importance_rankedA = mutate(germancredit_Importance_rankedA, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotA)",
                                                                                                                   "MDAMDG_subsetA" = "MDAMDG") 

#Set B
germancredit_seltrainB = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subB, importance=T)
germancredit_ImportanceplotB = varImpPlot(germancredit_seltrainB)
germancredit_Importance_rowsB = as.data.frame(rownames(germancredit_ImportanceplotB))

germancredit_Importance_MDAB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB) 
germancredit_MDAorderB = order(germancredit_Importance_MDAB$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedB = germancredit_Importance_MDAB[germancredit_MDAorderB,] 
germancredit_Importance_MDA_rankedB$MDAScore = rank(germancredit_Importance_MDA_rankedB$MeanDecreaseAccuracy)

germancredit_Importance_MDGB = as.data.frame(varImpPlot(germancredit_seltrainB)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsB)
germancredit_MDGorderB = order(germancredit_Importance_MDGB$MeanDecreaseGini)
germancredit_Importance_MDG_rankedB = germancredit_Importance_MDGB[germancredit_MDGorderB,] 
germancredit_Importance_MDG_rankedB$MDGScore = rank(germancredit_Importance_MDG_rankedB$MeanDecreaseGini)

germancredit_Importance_rankedB = merge.data.frame(germancredit_Importance_MDA_rankedB, germancredit_Importance_MDG_rankedB, by="rownames(germancredit_ImportanceplotB)") 
germancredit_Importance_rankedB = mutate(germancredit_Importance_rankedB, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotB)",
                                                                                                                   "MDAMDG_subsetB" = "MDAMDG") 

#Set C
germancredit_seltrainC = randomForest(Credit_RiskRating ~ .,
                                      data = germancredit_train_subC, importance=T)
germancredit_ImportanceplotC = varImpPlot(germancredit_seltrainC)
germancredit_Importance_rowsC = as.data.frame(rownames(germancredit_ImportanceplotC))

germancredit_Importance_MDAC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseAccuracy") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC) 
germancredit_MDAorderC = order(germancredit_Importance_MDAC$MeanDecreaseAccuracy)
germancredit_Importance_MDA_rankedC = germancredit_Importance_MDAC[germancredit_MDAorderC,] 
germancredit_Importance_MDA_rankedC$MDAScore = rank(germancredit_Importance_MDA_rankedC$MeanDecreaseAccuracy)

germancredit_Importance_MDGC = as.data.frame(varImpPlot(germancredit_seltrainC)) %>% select("MeanDecreaseGini") %>% as.data.frame() %>% add_column(germancredit_Importance_rowsC)
germancredit_MDGorderC = order(germancredit_Importance_MDGC$MeanDecreaseGini)
germancredit_Importance_MDG_rankedC = germancredit_Importance_MDGC[germancredit_MDGorderC,] 
germancredit_Importance_MDG_rankedC$MDGScore = rank(germancredit_Importance_MDG_rankedC$MeanDecreaseGini)

germancredit_Importance_rankedC = merge.data.frame(germancredit_Importance_MDA_rankedC, germancredit_Importance_MDG_rankedC, by="rownames(germancredit_ImportanceplotC)") 
germancredit_Importance_rankedC = mutate(germancredit_Importance_rankedC, MDAMDG = MDAScore + MDGScore) %>% rename("Predictors" = "rownames(germancredit_ImportanceplotC)",
                                                                                                                   "MDAMDG_subsetC" = "MDAMDG") 

#Storing results of third seed.
germancredit_Importance_ranked_results = germancredit_Importance_ranked_results %>% merge.data.frame(select(germancredit_Importance_rankedA, Predictors, MDAMDG_subsetA), by="Predictors") %>%
  merge.data.frame(select(germancredit_Importance_rankedB, Predictors, MDAMDG_subsetB), by="Predictors") %>%
  merge.data.frame(select(germancredit_Importance_rankedC, Predictors, MDAMDG_subsetC), by="Predictors") %>%
  rename("MDAMDG_subsetA_seed3" = "MDAMDG_subsetA",
         "MDAMDG_subsetB_seed3" = "MDAMDG_subsetB",
         "MDAMDG_subsetC_seed3" = "MDAMDG_subsetC")

#______________________________________________________________________________________________________________#

germancredit_Importance_ranked_results = mutate(germancredit_Importance_ranked_results, MDAMDG_AVE = rowMeans(germancredit_Importance_ranked_results[2:9]))

germancredit_MDAMDGorder_results = order(germancredit_Importance_ranked_results$MDAMDG_AVE,decreasing = TRUE)
germancredit_Importance_ranked_results_byaverage = germancredit_Importance_ranked_results[germancredit_MDAMDGorder_results,]%>% top_n(10, MDAMDG_AVE)

germancredit_Importance_ranked_results_byaverage

#______________________________________________________________________________________________________________#

#Table for selected variables/predictors

germancredit_predictors = dplyr::left_join(germancredit_Importance_ranked_results_byaverage, dplyr::full_join(numeric_t_test,fact_chi_test)[4:5], 
                                           by="Predictors") %>% rename("Statistical Significance at 5%" = "Significance_at_05")

colnames(germancredit_predictors) = c("Predictors","Seed 1A","Seed 1B","Seed 1C","Seed 2A","Seed 2B","Seed 2C",
                                      "Seed 3A","Seed 3B","Seed 3C","Average","Significance at 5%")
germancredit_predictors %>% gt() %>%
  tab_header(title= md("Top 10 predictors by MDAMDG points"),
             subtitle = md("Analysis of top predictors to be selected for modeling")) %>% 
  opt_align_table_header(align="left") %>%
  tab_footnote(
    footnote = "The use of MDAMDG method is based on the study of Han et al. (2016)."
  ) %>%
  tab_footnote(
    footnote = "Significance is based on the either Chi-square or t-test for independence depending on the variable type",
    locations = cells_column_labels(columns=`Significance at 5%`)
  ) %>%
  tab_style(
    locations = cells_body(columns=Predictors),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_title("title"),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(weight="bold")
  )


#______________________________________________________________________________________________________________#
#Logistic Regression

##Modeling Logistic Regression
###Null model

gc_model_null = glm(Credit_RiskRating ~ one,
                    data=germancredit_train, family="binomial")

df_model_null = gc_model_null$df.residual
deviance_model_null = deviance(gc_model_null)

###Preliminary models

#Variables to be used

Predictors = as.list(germancredit_predictors$Predictors)
Predictors

#Setting reference
germancredit_train$Credit_RiskRating = relevel(germancredit_train$Credit_RiskRating, ref = "Bad")
germancredit_train2$Credit_RiskRating = relevel(germancredit_train$Credit_RiskRating, ref = "Bad")
germancredit_train3$Credit_RiskRating = relevel(germancredit_train$Credit_RiskRating, ref = "Bad")
germancredit_test$Credit_RiskRating = relevel(germancredit_test$Credit_RiskRating, ref = "Bad")
germancredit_test2$Credit_RiskRating = relevel(germancredit_test2$Credit_RiskRating, ref = "Bad")
germancredit_test3$Credit_RiskRating = relevel(germancredit_test3$Credit_RiskRating, ref = "Bad")

#______________________________________________________________________________________________________________#

#### Logistic models by ranking

rank1 = Credit_RiskRating ~  Debtor_CheckingStatus
rank2 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration
rank3 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount
rank4 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age
rank5 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History
rank6 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose 
rank7 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Property
rank8 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Property + Debtor_Savings
rank9 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Property + Debtor_Savings + Debtor_Employment
rank10 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Property + Debtor_Savings + Debtor_Employment + Debtor_OtherInstallments

gc_model_1 = glm(rank1, data=germancredit_train, family="binomial")
gc_model_2 = glm(rank2, data=germancredit_train, family="binomial")
gc_model_3 = glm(rank3, data=germancredit_train, family="binomial")
gc_model_4 = glm(rank4, data=germancredit_train, family="binomial")
gc_model_5 = glm(rank5, data=germancredit_train, family="binomial")
gc_model_6 = glm(rank6, data=germancredit_train, family="binomial")
gc_model_7 = glm(rank7, data=germancredit_train, family="binomial")
gc_model_8 = glm(rank8, data=germancredit_train, family="binomial")
gc_model_9 = glm(rank9, data=germancredit_train, family="binomial")
gc_model_10 = glm(rank10, data=germancredit_train, family="binomial")

deviance_model_1 = deviance(gc_model_1)
deviance_model_2 = deviance(gc_model_2)
deviance_model_3 = deviance(gc_model_3)
deviance_model_4 = deviance(gc_model_4)
deviance_model_5 = deviance(gc_model_5)
deviance_model_6 = deviance(gc_model_6)
deviance_model_7 = deviance(gc_model_7)
deviance_model_8 = deviance(gc_model_8)
deviance_model_9 = deviance(gc_model_9)
deviance_model_10 = deviance(gc_model_10)

df_model_1 = gc_model_1$df.residual
df_model_2 = gc_model_2$df.residual
df_model_3 = gc_model_3$df.residual
df_model_4 = gc_model_4$df.residual
df_model_5 = gc_model_5$df.residual
df_model_6 = gc_model_6$df.residual
df_model_7 = gc_model_7$df.residual
df_model_8 = gc_model_8$df.residual
df_model_9 = gc_model_9$df.residual
df_model_10 = gc_model_10$df.residual

#### Testing with Null (LLR)

LLRTest_1 = deviance_model_null - deviance_model_1 
LLRTest_1_result = pchisq(LLRTest_1, df=df_model_null-df_model_1, lower.tail=FALSE)
McFaddenModel_1 = 1-deviance_model_1/deviance_model_null

LLRTest_2 = deviance_model_null - deviance_model_2
LLRTest_2_result = pchisq(LLRTest_2, df=df_model_null-df_model_2, lower.tail=FALSE)
McFaddenModel_2 = 1-deviance_model_2/deviance_model_null

LLRTest_3 = deviance_model_null - deviance_model_3
LLRTest_3_result = pchisq(LLRTest_3, df=df_model_null-df_model_3, lower.tail=FALSE)
McFaddenModel_3 = 1-deviance_model_3/deviance_model_null

LLRTest_4 = deviance_model_null - deviance_model_4
LLRTest_4_result = pchisq(LLRTest_4, df=df_model_null-df_model_4, lower.tail=FALSE)
McFaddenModel_4 = 1-deviance_model_4/deviance_model_null

LLRTest_5 = deviance_model_null - deviance_model_5
LLRTest_5_result = pchisq(LLRTest_5, df=df_model_null-df_model_5, lower.tail=FALSE)
McFaddenModel_5 = 1-deviance_model_5/deviance_model_null

LLRTest_6 = deviance_model_null - deviance_model_6
LLRTest_6_result = pchisq(LLRTest_6, df=df_model_null-df_model_6, lower.tail=FALSE)
McFaddenModel_6 = 1-deviance_model_6/deviance_model_null

LLRTest_7 = deviance_model_null - deviance_model_7
LLRTest_7_result = pchisq(LLRTest_7, df=df_model_null-df_model_7, lower.tail=FALSE)
McFaddenModel_7 = 1-deviance_model_7/deviance_model_null

LLRTest_8 = deviance_model_null - deviance_model_8
LLRTest_8_result = pchisq(LLRTest_8, df=df_model_null-df_model_8, lower.tail=FALSE)
McFaddenModel_8 = 1-deviance_model_8/deviance_model_null

LLRTest_9 = deviance_model_null - deviance_model_9
LLRTest_9_result = pchisq(LLRTest_9, df=df_model_null-df_model_9, lower.tail=FALSE)
McFaddenModel_9 = 1-deviance_model_9/deviance_model_null

LLRTest_10 = deviance_model_null - deviance_model_10
LLRTest_10_result = pchisq(LLRTest_10, df=df_model_null-df_model_10, lower.tail=FALSE)
McFaddenModel_10 = 1-deviance_model_10/deviance_model_null

LLRTest_all = data.frame(list=c(LLRTest_1_result, LLRTest_2_result,LLRTest_3_result,LLRTest_4_result,
                                LLRTest_5_result,LLRTest_6_result,LLRTest_7_result,LLRTest_8_result,
                                LLRTest_9_result,LLRTest_10_result)) %>% rename("LLR"="list") %>% mutate(Significance = ifelse(LLR<=0.05,"Significant","Not Sinificant")) %>%
  add_column(c(McFaddenModel_1,McFaddenModel_2,McFaddenModel_3,McFaddenModel_4,McFaddenModel_5,
               McFaddenModel_6,McFaddenModel_7,McFaddenModel_8,McFaddenModel_9,McFaddenModel_10)) %>% rename("McFadden"="c(...)")

LLRTest_all

#### Using the NestTestStat between the models

## Model 1

NestTestStat_1_2 = deviance_model_1 - deviance_model_2
ChisqNTS_1_2 = pchisq(NestTestStat_1_2, df=df_model_1 - df_model_2, lower.tail=FALSE)

NestTestStat_1_3 = deviance_model_1 - deviance_model_3
ChisqNTS_1_3 = pchisq(NestTestStat_1_3, df=df_model_1 - df_model_3, lower.tail=FALSE)

NestTestStat_1_4 = deviance_model_1 - deviance_model_4
ChisqNTS_1_4 = pchisq(NestTestStat_1_4, df=df_model_1 - df_model_4, lower.tail=FALSE)

NestTestStat_1_5 = deviance_model_1 - deviance_model_5
ChisqNTS_1_5 = pchisq(NestTestStat_1_5, df=df_model_1 - df_model_5, lower.tail=FALSE)

NestTestStat_1_6 = deviance_model_1 - deviance_model_6
ChisqNTS_1_6 = pchisq(NestTestStat_1_6, df=df_model_1 - df_model_6, lower.tail=FALSE)

NestTestStat_1_7 = deviance_model_1 - deviance_model_7
ChisqNTS_1_7 = pchisq(NestTestStat_1_7, df=df_model_1 - df_model_7, lower.tail=FALSE)

NestTestStat_1_8 = deviance_model_1 - deviance_model_8
ChisqNTS_1_8 = pchisq(NestTestStat_1_8, df=df_model_1 - df_model_8, lower.tail=FALSE)

NestTestStat_1_9 = deviance_model_1 - deviance_model_9
ChisqNTS_1_9 = pchisq(NestTestStat_1_9, df=df_model_1 - df_model_9, lower.tail=FALSE)

NestTestStat_1_10 = deviance_model_1 - deviance_model_10
ChisqNTS_1_10 = pchisq(NestTestStat_1_10, df=df_model_1 - df_model_10, lower.tail=FALSE)

##

## Model 2

NestTestStat_2_3 = deviance_model_2 - deviance_model_3
ChisqNTS_2_3 = pchisq(NestTestStat_2_3, df=df_model_2 - df_model_3, lower.tail=FALSE)

NestTestStat_2_4 = deviance_model_2 - deviance_model_4
ChisqNTS_2_4 = pchisq(NestTestStat_2_4, df=df_model_2 - df_model_4, lower.tail=FALSE)

NestTestStat_2_5 = deviance_model_2 - deviance_model_5
ChisqNTS_2_5 = pchisq(NestTestStat_2_5, df=df_model_2 - df_model_5, lower.tail=FALSE)

NestTestStat_2_6 = deviance_model_2 - deviance_model_6
ChisqNTS_2_6 = pchisq(NestTestStat_2_6, df=df_model_2 - df_model_6, lower.tail=FALSE)

NestTestStat_2_7 = deviance_model_2 - deviance_model_7
ChisqNTS_2_7 = pchisq(NestTestStat_2_7, df=df_model_2 - df_model_7, lower.tail=FALSE)

NestTestStat_2_8 = deviance_model_2 - deviance_model_8
ChisqNTS_2_8 = pchisq(NestTestStat_2_8, df=df_model_2 - df_model_8, lower.tail=FALSE)

NestTestStat_2_9 = deviance_model_2 - deviance_model_9
ChisqNTS_2_9 = pchisq(NestTestStat_2_9, df=df_model_2 - df_model_9, lower.tail=FALSE)

NestTestStat_2_10 = deviance_model_2 - deviance_model_10
ChisqNTS_2_10 = pchisq(NestTestStat_2_10, df=df_model_2 - df_model_10, lower.tail=FALSE)

##

## Model 3

NestTestStat_3_4 = deviance_model_3 - deviance_model_4
ChisqNTS_3_4 = pchisq(NestTestStat_3_4, df=df_model_3 - df_model_4, lower.tail=FALSE)

NestTestStat_3_5 = deviance_model_3 - deviance_model_5
ChisqNTS_3_5 = pchisq(NestTestStat_3_5, df=df_model_3 - df_model_5, lower.tail=FALSE)

NestTestStat_3_6 = deviance_model_3 - deviance_model_6
ChisqNTS_3_6 = pchisq(NestTestStat_3_6, df=df_model_3 - df_model_6, lower.tail=FALSE)

NestTestStat_3_7 = deviance_model_3 - deviance_model_7
ChisqNTS_3_7 = pchisq(NestTestStat_3_7, df=df_model_3 - df_model_7, lower.tail=FALSE)

NestTestStat_3_8 = deviance_model_3 - deviance_model_8
ChisqNTS_3_8 = pchisq(NestTestStat_3_8, df=df_model_3 - df_model_8, lower.tail=FALSE)

NestTestStat_3_9 = deviance_model_3 - deviance_model_9
ChisqNTS_3_9 = pchisq(NestTestStat_3_9, df=df_model_3 - df_model_9, lower.tail=FALSE)

NestTestStat_3_10 = deviance_model_3 - deviance_model_10
ChisqNTS_3_10 = pchisq(NestTestStat_3_10, df=df_model_3 - df_model_10, lower.tail=FALSE)

##

## Model 4

NestTestStat_4_5 = deviance_model_4 - deviance_model_5
ChisqNTS_4_5 = pchisq(NestTestStat_4_5, df=df_model_4 - df_model_5, lower.tail=FALSE)

NestTestStat_4_6 = deviance_model_4 - deviance_model_6
ChisqNTS_4_6 = pchisq(NestTestStat_4_6, df=df_model_4 - df_model_6, lower.tail=FALSE)

NestTestStat_4_7 = deviance_model_4 - deviance_model_7
ChisqNTS_4_7 = pchisq(NestTestStat_4_7, df=df_model_4 - df_model_7, lower.tail=FALSE)

NestTestStat_4_8 = deviance_model_4 - deviance_model_8
ChisqNTS_4_8 = pchisq(NestTestStat_4_8, df=df_model_4 - df_model_8, lower.tail=FALSE)

NestTestStat_4_9 = deviance_model_4 - deviance_model_9
ChisqNTS_4_9 = pchisq(NestTestStat_4_9, df=df_model_4 - df_model_9, lower.tail=FALSE)

NestTestStat_4_10 = deviance_model_4 - deviance_model_10
ChisqNTS_4_10 = pchisq(NestTestStat_4_10, df=df_model_4 - df_model_10, lower.tail=FALSE)

##

## Model 5

NestTestStat_5_6 = deviance_model_5 - deviance_model_6
ChisqNTS_5_6 = pchisq(NestTestStat_5_6, df=df_model_5 - df_model_6, lower.tail=FALSE)

NestTestStat_5_7 = deviance_model_5 - deviance_model_7
ChisqNTS_5_7 = pchisq(NestTestStat_5_7, df=df_model_5 - df_model_7, lower.tail=FALSE)

NestTestStat_5_8 = deviance_model_5 - deviance_model_8
ChisqNTS_5_8 = pchisq(NestTestStat_5_8, df=df_model_5 - df_model_8, lower.tail=FALSE)

NestTestStat_5_9 = deviance_model_5 - deviance_model_9
ChisqNTS_5_9 = pchisq(NestTestStat_5_9, df=df_model_5 - df_model_9, lower.tail=FALSE)

NestTestStat_5_10 = deviance_model_5 - deviance_model_10
ChisqNTS_5_10 = pchisq(NestTestStat_5_10, df=df_model_5 - df_model_10, lower.tail=FALSE)

##

## Model 6

NestTestStat_6_7 = deviance_model_6 - deviance_model_7
ChisqNTS_6_7 = pchisq(NestTestStat_6_7, df=df_model_6 - df_model_7, lower.tail=FALSE)

NestTestStat_6_8 = deviance_model_6 - deviance_model_8
ChisqNTS_6_8 = pchisq(NestTestStat_6_8, df=df_model_6 - df_model_8, lower.tail=FALSE)

NestTestStat_6_9 = deviance_model_6 - deviance_model_9
ChisqNTS_6_9 = pchisq(NestTestStat_6_9, df=df_model_6 - df_model_9, lower.tail=FALSE)

NestTestStat_6_10 = deviance_model_6 - deviance_model_10
ChisqNTS_6_10 = pchisq(NestTestStat_6_10, df=df_model_6 - df_model_10, lower.tail=FALSE)

##

## Model 7

NestTestStat_7_8 = deviance_model_7 - deviance_model_8
ChisqNTS_7_8 = pchisq(NestTestStat_7_8, df=df_model_7 - df_model_8, lower.tail=FALSE)

NestTestStat_7_9 = deviance_model_7 - deviance_model_9
ChisqNTS_7_9 = pchisq(NestTestStat_7_9, df=df_model_7 - df_model_9, lower.tail=FALSE)

NestTestStat_7_10 = deviance_model_7 - deviance_model_10
ChisqNTS_7_10 = pchisq(NestTestStat_7_10, df=df_model_7 - df_model_10, lower.tail=FALSE)

##

## Model 8

NestTestStat_8_9 = deviance_model_8 - deviance_model_9
ChisqNTS_8_9 = pchisq(NestTestStat_8_9, df=df_model_8 - df_model_9, lower.tail=FALSE)

NestTestStat_8_10 = deviance_model_8 - deviance_model_10
ChisqNTS_8_10 = pchisq(NestTestStat_8_10, df=df_model_8 - df_model_10, lower.tail=FALSE)

##

## Model 9

NestTestStat_9_10 = deviance_model_9 - deviance_model_10
ChisqNTS_9_10 = pchisq(NestTestStat_9_10, df=df_model_9 - df_model_10, lower.tail=FALSE)

##

ChisqNTS_matrix = matrix(data = c(
  NA,ChisqNTS_1_2,ChisqNTS_1_3,ChisqNTS_1_4,ChisqNTS_1_5,ChisqNTS_1_6,ChisqNTS_1_7,ChisqNTS_1_8,ChisqNTS_1_9,ChisqNTS_1_10,
  ChisqNTS_1_2, NA, ChisqNTS_2_3,	ChisqNTS_2_4,	ChisqNTS_2_5,	ChisqNTS_2_6,	ChisqNTS_2_7,	ChisqNTS_2_8,	ChisqNTS_2_9,	ChisqNTS_2_10,
  ChisqNTS_1_3, ChisqNTS_2_3, NA, ChisqNTS_3_4,	ChisqNTS_3_5,	ChisqNTS_3_6,	ChisqNTS_3_7,	ChisqNTS_3_8,	ChisqNTS_3_9,	ChisqNTS_3_10,
  ChisqNTS_1_4, ChisqNTS_2_4, ChisqNTS_3_4, NA, ChisqNTS_4_5,	ChisqNTS_4_6,	ChisqNTS_4_7,	ChisqNTS_4_8,	ChisqNTS_4_9,	ChisqNTS_4_10,
  ChisqNTS_1_5, ChisqNTS_2_5, ChisqNTS_3_5, ChisqNTS_4_5, NA, ChisqNTS_5_6,	ChisqNTS_5_7,	ChisqNTS_5_8,	ChisqNTS_5_9,	ChisqNTS_5_10,
  ChisqNTS_1_6, ChisqNTS_2_6, ChisqNTS_3_6, ChisqNTS_4_6, ChisqNTS_5_6, NA, ChisqNTS_6_7,	ChisqNTS_6_8,	ChisqNTS_6_9,	ChisqNTS_6_10,
  ChisqNTS_1_7, ChisqNTS_2_7, ChisqNTS_3_7, ChisqNTS_4_7, ChisqNTS_5_7, ChisqNTS_6_7, NA, ChisqNTS_7_8,	ChisqNTS_7_9,	ChisqNTS_7_10,
  ChisqNTS_1_8, ChisqNTS_2_8, ChisqNTS_3_8, ChisqNTS_4_8, ChisqNTS_5_8, ChisqNTS_6_8, ChisqNTS_7_8, NA, ChisqNTS_8_9,	ChisqNTS_8_10,
  ChisqNTS_1_9, ChisqNTS_2_9, ChisqNTS_3_9, ChisqNTS_4_9, ChisqNTS_5_9, ChisqNTS_6_9, ChisqNTS_7_9, ChisqNTS_8_9, NA, ChisqNTS_9_10,
  ChisqNTS_1_10, ChisqNTS_2_10, ChisqNTS_3_10, ChisqNTS_4_10, ChisqNTS_5_10, ChisqNTS_6_10,ChisqNTS_7_10, ChisqNTS_8_10, ChisqNTS_9_10, NA
), nrow=10, ncol=10)

colnames(ChisqNTS_matrix)= paste0("Model", 1:10)                             # Column names
rownames(ChisqNTS_matrix)= paste0("Model", 1:10) 
ChisqNTS_matrix_melt = melt(ChisqNTS_matrix) %>% mutate(Significance = ifelse(value <= 0.05 | is.na(value) == TRUE, " ", round(value,3)))

ChisqNTS_matrix_melt

ChisqNTS_heatmap = ggplot(as.data.frame(ChisqNTS_matrix_melt), aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + theme_minimal() +
  labs(x = "Models", y = "Models", title = "Nested Stat Significance") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_gradient2(low="green", mid="white", high="red", 
                       breaks=seq(0,0.6,0.1),
  ) +
  geom_text(aes(label = Significance), colour="black")

ChisqNTS_heatmap

#Based on the heat map, we will be analysing model 2, 6, and 8.
#Further a new model in which predictor 3,7,9 and 10 will be removed.

rank_filtered = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Savings
gc_model_filtered = glm(rank_filtered, data=germancredit_train, family="binomial")

#new model with variable 3

rank_filtered2 = Credit_RiskRating ~  Debtor_CheckingStatus + Credit_Duration +  Credit_Amount + Debtor_Age + Debtor_History + Credit_Purpose + Debtor_Savings
gc_model_filtered2 = glm(rank_filtered2, data=germancredit_train, family="binomial")

#______________________________________________________________________________________________________________#
#Seed 1
#______________________________________________________________________________________________________________#

# Prediction and Model Fit

##Model 2
prediction_model_2 = predict(gc_model_2, germancredit_test, type="response")
germancredit_test = mutate(germancredit_test, Predictions_model2 = prediction_model_2)
germancredit_test$Predictions_model2 = as.factor(ifelse(prediction_model_2 >= 0.5,"Good","Bad"))

##Confusion Matrix 2

gc_model_2_cm <- confusionMatrix(data = germancredit_test$Predictions_model2,       
                                 reference = germancredit_test$Credit_RiskRating,
                                 positive = "Good")
gc_model_2_cm

##Model 6
prediction_model_6 = predict(gc_model_6, germancredit_test, type="response")
germancredit_test = mutate(germancredit_test, Predictions_model6 = prediction_model_6)
germancredit_test$Predictions_model6 = as.factor(ifelse(prediction_model_6 >= 0.5,"Good","Bad"))

##Confusion Matrix 6

gc_model_6_cm <- confusionMatrix(data = germancredit_test$Predictions_model6,       
                                 reference = germancredit_test$Credit_RiskRating,
                                 positive = "Good")
gc_model_6_cm

##Model 8
prediction_model_8 = predict(gc_model_8, germancredit_test, type="response")
germancredit_test = mutate(germancredit_test, Predictions_model8 = prediction_model_8)
germancredit_test$Predictions_model8 = as.factor(ifelse(prediction_model_8 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_8_cm <- confusionMatrix(data = germancredit_test$Predictions_model8,       
                                 reference = germancredit_test$Credit_RiskRating,
                                 positive = "Good")
gc_model_8_cm


##Model Filtered
prediction_model_filtered = predict(gc_model_filtered, germancredit_test, type="response")
germancredit_test = mutate(germancredit_test, Predictions_modelfiltered = prediction_model_filtered)
germancredit_test$Predictions_modelfiltered = as.factor(ifelse(prediction_model_filtered >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered_cm <- confusionMatrix(data = germancredit_test$Predictions_modelfiltered,       
                                        reference = germancredit_test$Credit_RiskRating,
                                        positive = "Good")
gc_model_filtered_cm

##Model Filtered2
prediction_model_filtered2 = predict(gc_model_filtered2, germancredit_test, type="response")
germancredit_test = mutate(germancredit_test, Predictions_modelfiltered2 = prediction_model_filtered2)
germancredit_test$Predictions_modelfiltered2 = as.factor(ifelse(prediction_model_filtered2 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered2_cm <- confusionMatrix(data = germancredit_test$Predictions_modelfiltered2,       
                                         reference = germancredit_test$Credit_RiskRating,
                                         positive = "Good")
gc_model_filtered2_cm

#______________________________________________________________________________________________________________#
#Seed 2
#______________________________________________________________________________________________________________#

# Prediction and Model Fit

##Model 2
prediction_model_2_sd2 = predict(gc_model_2, germancredit_test2, type="response")
germancredit_test2 = mutate(germancredit_test2, Predictions_model2 = prediction_model_2_sd2)
germancredit_test2$Predictions_model2 = as.factor(ifelse(prediction_model_2_sd2 >= 0.5,"Good","Bad"))

##Confusion Matrix 2

gc_model_2_cm_sd2 <- confusionMatrix(data = germancredit_test2$Predictions_model2,       
                                     reference = germancredit_test2$Credit_RiskRating,
                                     positive = "Good")
gc_model_2_cm_sd2

##Model 6
prediction_model_6_sd2 = predict(gc_model_6, germancredit_test2, type="response")
germancredit_test2 = mutate(germancredit_test2, Predictions_model6 = prediction_model_6_sd2)
germancredit_test2$Predictions_model6 = as.factor(ifelse(prediction_model_6_sd2 >= 0.5,"Good","Bad"))

##Confusion Matrix 6

gc_model_6_cm_sd2 <- confusionMatrix(data = germancredit_test2$Predictions_model6,       
                                     reference = germancredit_test2$Credit_RiskRating,
                                     positive = "Good")
gc_model_6_cm_sd2

##Model 8
prediction_model_8_sd2 = predict(gc_model_8, germancredit_test2, type="response")
germancredit_test2 = mutate(germancredit_test2, Predictions_model8 = prediction_model_8_sd2)
germancredit_test2$Predictions_model8 = as.factor(ifelse(prediction_model_8_sd2 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_8_cm_sd2 <- confusionMatrix(data = germancredit_test2$Predictions_model8,       
                                     reference = germancredit_test2$Credit_RiskRating,
                                     positive = "Good")
gc_model_8_cm_sd2

##Model filtered
prediction_model_filtered_sd2 = predict(gc_model_filtered, germancredit_test2, type="response")
germancredit_test2 = mutate(germancredit_test2, Predictions_modelfiltered = prediction_model_filtered_sd2)
germancredit_test2$Predictions_modelfiltered = as.factor(ifelse(prediction_model_filtered_sd2 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered_cm_sd2 <- confusionMatrix(data = germancredit_test2$Predictions_modelfiltered,       
                                            reference = germancredit_test2$Credit_RiskRating,
                                            positive = "Good")
gc_model_filtered_cm_sd2


##Model filtered2
prediction_model_filtered2_sd2 = predict(gc_model_filtered2, germancredit_test2, type="response")
germancredit_test2 = mutate(germancredit_test2, Predictions_modelfiltered2 = prediction_model_filtered2_sd2)
germancredit_test2$Predictions_modelfiltered2 = as.factor(ifelse(prediction_model_filtered2_sd2 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered2_cm_sd2 <- confusionMatrix(data = germancredit_test2$Predictions_modelfiltered2,       
                                             reference = germancredit_test2$Credit_RiskRating,
                                             positive = "Good")
gc_model_filtered2_cm_sd2

#______________________________________________________________________________________________________________#
#Seed 3
#______________________________________________________________________________________________________________#

# Prediction and Model Fit

##Model 2
prediction_model_2_sd3 = predict(gc_model_2, germancredit_test3, type="response")
germancredit_test3 = mutate(germancredit_test3, Predictions_model2 = prediction_model_2_sd3)
germancredit_test3$Predictions_model2 = as.factor(ifelse(prediction_model_2_sd3 >= 0.5,"Good","Bad"))

##Confusion Matrix 2

gc_model_2_cm_sd3 <- confusionMatrix(data = germancredit_test3$Predictions_model2,       
                                     reference = germancredit_test3$Credit_RiskRating,
                                     positive = "Good")
gc_model_2_cm_sd3

##Model 6
prediction_model_6_sd3 = predict(gc_model_6, germancredit_test3, type="response")
germancredit_test3 = mutate(germancredit_test3, Predictions_model6 = prediction_model_6_sd3)
germancredit_test3$Predictions_model6 = as.factor(ifelse(prediction_model_6_sd3 >= 0.5,"Good","Bad"))

##Confusion Matrix 6

gc_model_6_cm_sd3 <- confusionMatrix(data = germancredit_test3$Predictions_model6,       
                                     reference = germancredit_test3$Credit_RiskRating,
                                     positive = "Good")
gc_model_6_cm_sd3

##Model 8
prediction_model_8_sd3 = predict(gc_model_8, germancredit_test3, type="response")
germancredit_test3 = mutate(germancredit_test3, Predictions_model8 = prediction_model_8_sd3)
germancredit_test3$Predictions_model8 = as.factor(ifelse(prediction_model_8_sd3 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_8_cm_sd3 <- confusionMatrix(data = germancredit_test3$Predictions_model8,       
                                     reference = germancredit_test3$Credit_RiskRating,
                                     positive = "Good")
gc_model_8_cm_sd3

##Model Filtered
prediction_model_filtered_sd3 = predict(gc_model_filtered, germancredit_test3, type="response")
germancredit_test3 = mutate(germancredit_test3, Predictions_modelfiltered = prediction_model_filtered_sd3)
germancredit_test3$Predictions_modelfiltered = as.factor(ifelse(prediction_model_filtered_sd3 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered_cm_sd3 <- confusionMatrix(data = germancredit_test3$Predictions_modelfiltered,       
                                            reference = germancredit_test3$Credit_RiskRating,
                                            positive = "Good")
gc_model_filtered_cm_sd3

##Model filtered2
prediction_model_filtered2_sd3 = predict(gc_model_filtered2, germancredit_test3, type="response")
germancredit_test3 = mutate(germancredit_test3, Predictions_modelfiltered2 = prediction_model_filtered2_sd3)
germancredit_test3$Predictions_modelfiltered2 = as.factor(ifelse(prediction_model_filtered2_sd3 >= 0.5,"Good","Bad"))

##Confusion Matrix

gc_model_filtered2_cm_sd3 <- confusionMatrix(data = germancredit_test3$Predictions_modelfiltered2,       
                                             reference = germancredit_test3$Credit_RiskRating,
                                             positive = "Good")
gc_model_filtered2_cm_sd3

#______________________________________________________________________________________________________________#

#Creating tables for the results

gc_logmodel_results = data.frame(matrix(data = c(
  "Model 2",	"Model 2",	"Model 2",	"Model 2",	"Model 6",	"Model 6",	"Model 6",	"Model 6",	"Model 8",	"Model 8",	"Model 8",	"Model 8",	"Model Fil",	"Model Fil",	"Model Fil",	"Model Fil",	"Model Fil2",	"Model Fil2",	"Model Fil2",	"Model Fil2",
  "Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	
  "Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",
  gc_model_2_cm$overall["Accuracy"],	 gc_model_2_cm$byClass["Sensitivity"],	 gc_model_2_cm$byClass["Specificity"],	 gc_model_2_cm$byClass["Balanced Accuracy"],	 gc_model_6_cm$overall["Accuracy"],	 
  gc_model_6_cm$byClass["Sensitivity"],	 gc_model_6_cm$byClass["Specificity"],	 gc_model_6_cm$byClass["Balanced Accuracy"],	 gc_model_8_cm$overall["Accuracy"],	 gc_model_8_cm$byClass["Sensitivity"],	 
  gc_model_8_cm$byClass["Specificity"],	 gc_model_8_cm$byClass["Balanced Accuracy"],	 gc_model_filtered_cm$overall["Accuracy"],	 gc_model_filtered_cm$byClass["Sensitivity"],	 gc_model_filtered_cm$byClass["Specificity"],	 
  gc_model_filtered_cm$byClass["Balanced Accuracy"],	 gc_model_filtered2_cm$overall["Accuracy"],	 gc_model_filtered2_cm$byClass["Sensitivity"],	 gc_model_filtered2_cm$byClass["Specificity"],	 gc_model_filtered2_cm$byClass["Balanced Accuracy"],
  gc_model_2_cm_sd2$overall["Accuracy"],	gc_model_2_cm_sd2$byClass["Sensitivity"],	gc_model_2_cm_sd2$byClass["Specificity"],	gc_model_2_cm_sd2$byClass["Balanced Accuracy"],	gc_model_6_cm_sd2$overall["Accuracy"],
  gc_model_6_cm_sd2$byClass["Sensitivity"],	gc_model_6_cm_sd2$byClass["Specificity"],	gc_model_6_cm_sd2$byClass["Balanced Accuracy"],	gc_model_8_cm_sd2$overall["Accuracy"],	gc_model_8_cm_sd2$byClass["Sensitivity"],	gc_model_8_cm_sd2$byClass["Specificity"],	
  gc_model_8_cm_sd2$byClass["Balanced Accuracy"],	gc_model_filtered_cm_sd2$overall["Accuracy"],	gc_model_filtered_cm_sd2$byClass["Sensitivity"],	gc_model_filtered_cm_sd2$byClass["Specificity"],	gc_model_filtered_cm_sd2$byClass["Balanced Accuracy"],	gc_model_filtered2_cm_sd2$overall["Accuracy"],	
  gc_model_filtered2_cm_sd2$byClass["Sensitivity"],	gc_model_filtered2_cm_sd2$byClass["Specificity"],	gc_model_filtered2_cm_sd2$byClass["Balanced Accuracy"],
  gc_model_2_cm_sd3$overall["Accuracy"],	gc_model_2_cm_sd3$byClass["Sensitivity"],	gc_model_2_cm_sd3$byClass["Specificity"],	gc_model_2_cm_sd3$byClass["Balanced Accuracy"],	gc_model_6_cm_sd3$overall["Accuracy"],	gc_model_6_cm_sd3$byClass["Sensitivity"],	
  gc_model_6_cm_sd3$byClass["Specificity"],	gc_model_6_cm_sd3$byClass["Balanced Accuracy"],	gc_model_8_cm_sd3$overall["Accuracy"],	gc_model_8_cm_sd3$byClass["Sensitivity"],	gc_model_8_cm_sd3$byClass["Specificity"],	gc_model_8_cm_sd3$byClass["Balanced Accuracy"],	
  gc_model_filtered_cm_sd3$overall["Accuracy"],	gc_model_filtered_cm_sd3$byClass["Sensitivity"],	gc_model_filtered_cm_sd3$byClass["Specificity"],	gc_model_filtered_cm_sd3$byClass["Balanced Accuracy"],	gc_model_filtered2_cm_sd3$overall["Accuracy"],	
  gc_model_filtered2_cm_sd3$byClass["Sensitivity"],	gc_model_filtered2_cm_sd3$byClass["Specificity"],	gc_model_filtered2_cm_sd3$byClass["Balanced Accuracy"]), nrow=20,ncol=5))

gc_logmodel_results.num = c("X3","X4","X5")
gc_logmodel_results[gc_logmodel_results.num] = lapply(gc_logmodel_results[gc_logmodel_results.num],as.numeric) 
gc_logmodel_results$Average = rowMeans(gc_logmodel_results[,3:5]) %>% round(digits = 3)
gc_logmodel_results[gc_logmodel_results.num] = gc_logmodel_results[gc_logmodel_results.num] %>% round(digits = 3)  
colnames(gc_logmodel_results) = c("Model","Metric","Seed 1 Results","Seed 2 Results","Seed 3 Results","Three-seed Average")

gc_logmodel_results.fac = c("Model","Metric")
gc_logmodel_results[gc_logmodel_results.fac] = lapply(gc_logmodel_results[gc_logmodel_results.fac],as.factor) 

gc_logmodel_results[2:6] %>% gt() %>%
  tab_header(title= md("LogModel Confusion Matrix Results")) %>%
  tab_row_group(label="Model Filter2",rows = 17:20) %>%
  tab_row_group(label="Model Filter",rows = 13:16) %>%
  tab_row_group(label="Model 8",rows = 9:12) %>%
  tab_row_group(label="Model 6",rows = 5:8) %>%
  tab_row_group(label="Model 2",rows = 1:4) %>%
  tab_style(
    locations = cells_body(columns=Metric),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_title("title"),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_row_groups(),
    style = cell_text(weight="bold")
  )
#______________________________________________________________________________________________________________#

##Checking significance between filtered models (impact of adding Credit Amount)
deviance_model_filtered = deviance(gc_model_filtered)
df_model_filtered = gc_model_filtered$df.residual
deviance_model_filtered2 = deviance(gc_model_filtered2)
df_model_filtered2 = gc_model_filtered2$df.residual

NestTestStat_filtered = deviance_model_filtered - deviance_model_filtered2
ChisqNTS_filtered = pchisq(NestTestStat_filtered, df=df_model_filtered - df_model_filtered2, lower.tail=FALSE)
ChisqNTS_filtered

#Fundamentally, the second filtered model (with Credit Amount) does not add statistical significance to first filtered model.

#______________________________________________________________________________________________________________#

#Preliminary Results

#Model Filter has the best results with the best balance when it comes to Balanced Accuracy and relatively good accuracy among the models.
### ROC-AUC diagram

germancredit_test$Credit_RiskRating_01 = as.factor(ifelse(germancredit_test$Credit_RiskRating == "Good", 1, 0))
germancredit_test2$Credit_RiskRating_01 = as.factor(ifelse(germancredit_test2$Credit_RiskRating == "Good", 1, 0))
germancredit_test3$Credit_RiskRating_01 = as.factor(ifelse(germancredit_test3$Credit_RiskRating == "Good", 1, 0))

#Model 2

gc_predictionROCR_model2 = prediction(prediction_model_2, germancredit_test$Credit_RiskRating_01)
perfROCR_model2  = performance(gc_predictionROCR_model2, "tpr", "fpr")
plot(perfROCR_model2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model2_per = performance(gc_predictionROCR_model2, "auc")@y.values
gc_predictionROCR_model2_per

gc_predictionROCR_model2_sd2 = prediction(prediction_model_2_sd2, germancredit_test2$Credit_RiskRating_01)
perfROCR_model2_sd2  = performance(gc_predictionROCR_model2_sd2, "tpr", "fpr")
plot(perfROCR_model2_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model2_sd2_per = performance(gc_predictionROCR_model2_sd2, "auc")@y.values
gc_predictionROCR_model2_sd2_per

gc_predictionROCR_model2_sd3 = prediction(prediction_model_2_sd3, germancredit_test3$Credit_RiskRating_01)
perfROCR_model2_sd3  = performance(gc_predictionROCR_model2_sd3, "tpr", "fpr")
plot(perfROCR_model2_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model2_sd3_per=performance(gc_predictionROCR_model2_sd3, "auc")@y.values
gc_predictionROCR_model2_sd3_per

mean(c(unlist(gc_predictionROCR_model2_per),unlist(gc_predictionROCR_model2_sd2_per),
       unlist(gc_predictionROCR_model2_sd3_per)))

#Model 6

gc_predictionROCR_model6 = prediction(prediction_model_6, germancredit_test$Credit_RiskRating_01)
perfROCR_model6  = performance(gc_predictionROCR_model6, "tpr", "fpr")
plot(perfROCR_model6, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model6_per = performance(gc_predictionROCR_model6, "auc")@y.values
gc_predictionROCR_model6_per

gc_predictionROCR_model6_sd2 = prediction(prediction_model_6_sd2, germancredit_test2$Credit_RiskRating_01)
perfROCR_model6_sd2  = performance(gc_predictionROCR_model6_sd2, "tpr", "fpr")
plot(perfROCR_model6_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model6_sd2_per = performance(gc_predictionROCR_model6_sd2, "auc")@y.values
gc_predictionROCR_model6_sd2_per

gc_predictionROCR_model6_sd3 = prediction(prediction_model_6_sd3, germancredit_test3$Credit_RiskRating_01)
perfROCR_model6_sd3  = performance(gc_predictionROCR_model6_sd3, "tpr", "fpr")
plot(perfROCR_model6_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model6_sd3_per=performance(gc_predictionROCR_model6_sd3, "auc")@y.values
gc_predictionROCR_model6_sd3_per

mean(c(unlist(gc_predictionROCR_model6_per),unlist(gc_predictionROCR_model6_sd2_per),
       unlist(gc_predictionROCR_model6_sd3_per)))

#Model 8

gc_predictionROCR_model8 = prediction(prediction_model_8, germancredit_test$Credit_RiskRating_01)
perfROCR_model8  = performance(gc_predictionROCR_model8, "tpr", "fpr")
plot(perfROCR_model8, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model8_per = performance(gc_predictionROCR_model8, "auc")@y.values
gc_predictionROCR_model8_per

gc_predictionROCR_model8_sd2 = prediction(prediction_model_8_sd2, germancredit_test2$Credit_RiskRating_01)
perfROCR_model8_sd2  = performance(gc_predictionROCR_model8_sd2, "tpr", "fpr")
plot(perfROCR_model8_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model8_sd2_per = performance(gc_predictionROCR_model8_sd2, "auc")@y.values
gc_predictionROCR_model8_sd2_per

gc_predictionROCR_model8_sd3 = prediction(prediction_model_8_sd3, germancredit_test3$Credit_RiskRating_01)
perfROCR_model8_sd3  = performance(gc_predictionROCR_model8_sd3, "tpr", "fpr")
plot(perfROCR_model8_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_model8_sd3_per=performance(gc_predictionROCR_model8_sd3, "auc")@y.values
gc_predictionROCR_model8_sd3_per

mean(c(unlist(gc_predictionROCR_model8_per),unlist(gc_predictionROCR_model8_sd2_per),
       unlist(gc_predictionROCR_model8_sd3_per)))

#Model Filter

gc_predictionROCR_modelfilter = prediction(prediction_model_filtered, germancredit_test$Credit_RiskRating_01)
perfROCR_modelfilter  = performance(gc_predictionROCR_modelfilter, "tpr", "fpr")
plot(perfROCR_modelfilter, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_modelfilter_per = performance(gc_predictionROCR_modelfilter, "auc")@y.values
gc_predictionROCR_modelfilter_per

gc_predictionROCR_modelfilter_sd2 = prediction(prediction_model_filtered_sd2, germancredit_test2$Credit_RiskRating_01)
perfROCR_modelfilter_sd2  = performance(gc_predictionROCR_modelfilter_sd2, "tpr", "fpr")
plot(perfROCR_modelfilter_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_modelfilter_sd2_per = performance(gc_predictionROCR_modelfilter_sd2, "auc")@y.values
gc_predictionROCR_modelfilter_sd2_per

gc_predictionROCR_modelfilter_sd3 = prediction(prediction_model_filtered_sd3, germancredit_test3$Credit_RiskRating_01)
perfROCR_modelfilter_sd3  = performance(gc_predictionROCR_modelfilter_sd3, "tpr", "fpr")
plot(perfROCR_modelfilter_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_modelfilter_sd3_per=performance(gc_predictionROCR_modelfilter_sd3, "auc")@y.values
gc_predictionROCR_modelfilter_sd3_per

mean(c(unlist(gc_predictionROCR_modelfilter_per),unlist(gc_predictionROCR_modelfilter_sd2_per),
       unlist(gc_predictionROCR_modelfilter_sd3_per)))

#______________________________________________________________________________________________________________#

#Decision Trees

#LogModel
#Models to be evaluated: Model 8, Model Filtered and Model 6 as they are relatively close in the four metrics

#Modeling Decision Trees

##Using the filtered log model

gc_dtree_filtered = rpart(formula = rank_filtered,
                          data = germancredit_train, 
                          method =  "class")
rpart.plot(x = gc_dtree_filtered, yesno = 2, type = 0, extra = 0)
plotcp(gc_dtree_filtered)
printcp(gc_dtree_filtered)

###pruned
gc_dtree_filtered_pruned = prune(gc_dtree_filtered, cp = 0.012)
rpart.plot(x = gc_dtree_filtered_pruned, yesno = 2, type = 0, extra = 0)

##Using model 8

gc_dtree_8 = rpart(formula = rank8,
                   data = germancredit_train, 
                   method =  "class")
rpart.plot(x = gc_dtree_8, yesno = 2, type = 0, extra = 0)
plotcp(gc_dtree_8)
printcp(gc_dtree_8)

###pruned
gc_dtree_8_pruned = prune(gc_dtree_8, cp = 0.013)
rpart.plot(x = gc_dtree_8_pruned, yesno = 2, type = 0, extra = 0)

##Using model 6 for decision tree

gc_dtree_6 = rpart(formula = rank6,
                   data = germancredit_train, 
                   method =  "class")
rpart.plot(x = gc_dtree_6, yesno = 2, type = 0, extra = 0)
plotcp(gc_dtree_6)
printcp(gc_dtree_6)

###pruned
gc_dtree_6_pruned = prune(gc_dtree_6, cp = 0.015)
rpart.plot(x = gc_dtree_6_pruned, yesno = 2, type = 0, extra = 0)

#______________________________________________________________________________________________________________#

#Utilising the bagging technique for filtered model

gc_dtree_bag_filtered = bagging(formula = rank_filtered,
                                data = germancredit_train, 
                                coob = TRUE)
print(gc_dtree_bag_filtered)
varImp(gc_dtree_bag_filtered)

#Utilising the bagging technique for model 8

gc_dtree_bag_8 = bagging(formula = rank8,
                         data = germancredit_train, 
                         coob = TRUE)
print(gc_dtree_bag_8)
varImp(gc_dtree_bag_8)


#Utilising the bagging technique for model 6

gc_dtree_bag_6 = bagging(formula = rank6,
                         data = germancredit_train, 
                         coob = TRUE)
print(gc_dtree_bag_6)
varImp(gc_dtree_bag_6)


#______________________________________________________________________________________________________________#
#Seed 1
#______________________________________________________________________________________________________________#

##Predictions using the decision tree models

prediction_tree_filtered = predict(object = gc_dtree_filtered_pruned,  
                                   newdata = germancredit_test,   
                                   type = "class")
prediction_tree_8 = predict(object = gc_dtree_8_pruned,  
                            newdata = germancredit_test,   
                            type = "class")
prediction_tree_6 = predict(object = gc_dtree_6_pruned,  
                            newdata = germancredit_test,   
                            type = "class")
prediction_tree_bag_filtered = predict(object = gc_dtree_bag_filtered,  
                                       newdata = germancredit_test,   
                                       type = "class")
prediction_tree_bag_8 = predict(object = gc_dtree_bag_8,  
                                newdata = germancredit_test,   
                                type = "class")
prediction_tree_bag_6 = predict(object = gc_dtree_bag_6,  
                                newdata = germancredit_test,   
                                type = "class")

##Confusion Matrix of the models
gc_tree_filtered_cm = confusionMatrix(data = prediction_tree_filtered,
                                      reference = germancredit_test$Credit_RiskRating,
                                      positive = "Good")  
gc_tree_8_cm = confusionMatrix(data = prediction_tree_8,
                               reference = germancredit_test$Credit_RiskRating,
                               positive = "Good")
gc_tree_6_cm = confusionMatrix(data = prediction_tree_6,
                               reference = germancredit_test$Credit_RiskRating,
                               positive = "Good")  
gc_tree_bagfiltered_cm = confusionMatrix(data = prediction_tree_bag_filtered,
                                         reference = germancredit_test$Credit_RiskRating,
                                         positive = "Good")  
gc_tree_bag8_cm = confusionMatrix(data = prediction_tree_bag_8,
                                  reference = germancredit_test$Credit_RiskRating,
                                  positive = "Good")
gc_tree_bag6_cm = confusionMatrix(data = prediction_tree_bag_6,
                                  reference = germancredit_test$Credit_RiskRating,
                                  positive = "Good") 

#______________________________________________________________________________________________________________#
#Seed 2
#______________________________________________________________________________________________________________#

##Predictions using the decision tree models

prediction_tree_filtered_sd2 = predict(object = gc_dtree_filtered_pruned,  
                                       newdata = germancredit_test2,   
                                       type = "class")
prediction_tree_8_sd2 = predict(object = gc_dtree_8_pruned,  
                                newdata = germancredit_test2,   
                                type = "class")
prediction_tree_6_sd2 = predict(object = gc_dtree_6_pruned,  
                                newdata = germancredit_test2,   
                                type = "class")
prediction_tree_bag_filtered_sd2 = predict(object = gc_dtree_bag_filtered,  
                                           newdata = germancredit_test2,   
                                           type = "class")
prediction_tree_bag_8_sd2 = predict(object = gc_dtree_bag_8,  
                                    newdata = germancredit_test2,   
                                    type = "class")
prediction_tree_bag_6_sd2 = predict(object = gc_dtree_bag_6,  
                                    newdata = germancredit_test2,   
                                    type = "class")

##Confusion Matrix of the models
gc_tree_filtered_cm_sd2 = confusionMatrix(data = prediction_tree_filtered_sd2,
                                          reference = germancredit_test2$Credit_RiskRating,
                                          positive = "Good")  
gc_tree_8_cm_sd2 = confusionMatrix(data = prediction_tree_8_sd2,
                                   reference = germancredit_test2$Credit_RiskRating,
                                   positive = "Good")
gc_tree_6_cm_sd2 = confusionMatrix(data = prediction_tree_6_sd2,
                                   reference = germancredit_test2$Credit_RiskRating,
                                   positive = "Good")  
gc_tree_bagfiltered_cm_sd2 = confusionMatrix(data = prediction_tree_bag_filtered_sd2,
                                             reference = germancredit_test2$Credit_RiskRating,
                                             positive = "Good")  
gc_tree_bag8_cm_sd2 = confusionMatrix(data = prediction_tree_bag_8_sd2,
                                      reference = germancredit_test2$Credit_RiskRating,
                                      positive = "Good")
gc_tree_bag6_cm_sd2 = confusionMatrix(data = prediction_tree_bag_6_sd2,
                                      reference = germancredit_test2$Credit_RiskRating,
                                      positive = "Good") 

#______________________________________________________________________________________________________________#
#Seed 3
#______________________________________________________________________________________________________________#

##Predictions using the decision tree models

prediction_tree_filtered_sd3 = predict(object = gc_dtree_filtered_pruned,  
                                       newdata = germancredit_test3,   
                                       type = "class")
prediction_tree_8_sd3 = predict(object = gc_dtree_8_pruned,  
                                newdata = germancredit_test3,   
                                type = "class")
prediction_tree_6_sd3 = predict(object = gc_dtree_6_pruned,  
                                newdata = germancredit_test3,   
                                type = "class")
prediction_tree_bag_filtered_sd3 = predict(object = gc_dtree_bag_filtered,  
                                           newdata = germancredit_test3,   
                                           type = "class")
prediction_tree_bag_8_sd3 = predict(object = gc_dtree_bag_8,  
                                    newdata = germancredit_test3,   
                                    type = "class")
prediction_tree_bag_6_sd3 = predict(object = gc_dtree_bag_6,  
                                    newdata = germancredit_test3,   
                                    type = "class")

##Confusion Matrix of the models
gc_tree_filtered_cm_sd3 = confusionMatrix(data = prediction_tree_filtered_sd3,
                                          reference = germancredit_test3$Credit_RiskRating,
                                          positive = "Good")  
gc_tree_8_cm_sd3 = confusionMatrix(data = prediction_tree_8_sd3,
                                   reference = germancredit_test3$Credit_RiskRating,
                                   positive = "Good")
gc_tree_6_cm_sd3 = confusionMatrix(data = prediction_tree_6_sd3,
                                   reference = germancredit_test3$Credit_RiskRating,
                                   positive = "Good")  
gc_tree_bagfiltered_cm_sd3 = confusionMatrix(data = prediction_tree_bag_filtered_sd3,
                                             reference = germancredit_test3$Credit_RiskRating,
                                             positive = "Good")  
gc_tree_bag8_cm_sd3 = confusionMatrix(data = prediction_tree_bag_8_sd3,
                                      reference = germancredit_test3$Credit_RiskRating,
                                      positive = "Good")
gc_tree_bag6_cm_sd3 = confusionMatrix(data = prediction_tree_bag_6_sd3,
                                      reference = germancredit_test3$Credit_RiskRating,
                                      positive = "Good") 

#______________________________________________________________________________________________________________#

#Creating tables for the results

gc_dectree_results = data.frame(matrix(data = c(
  'Model 6',	'Model 6',	'Model 6',	'Model 6',	'Model 8',	'Model 8',	'Model 8',	'Model 8',	'Model Fil',	'Model Fil',	"Model Fil",	"Model Fil",	"Model Bag6",	
  "Model Bag6",	"Model Bag6",	"Model Bag6",	"Model Bag8",	"Model Bag8",	"Model Bag8",	"Model Bag8",	"Model BagFil",	"Model BagFil",	"Model BagFil",	"Model BagFil",
  "Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	
  "Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	"Sensitivity",	"Specificity",	"Balanced Accuracy",	"Accuracy",	
  "Sensitivity",	"Specificity",	"Balanced Accuracy",gc_tree_6_cm$overall["Accuracy"],	gc_tree_6_cm$byClass["Sensitivity"],	gc_tree_6_cm$byClass["Specificity"],	
  gc_tree_6_cm$byClass["Balanced Accuracy"],	gc_tree_8_cm$overall["Accuracy"],	gc_tree_8_cm$byClass["Sensitivity"],	gc_tree_8_cm$byClass["Specificity"],	gc_tree_8_cm$byClass["Balanced Accuracy"],	
  gc_tree_filtered_cm$overall["Accuracy"],	gc_tree_filtered_cm$byClass["Sensitivity"],	gc_tree_filtered_cm$byClass["Specificity"],	gc_tree_filtered_cm$byClass["Balanced Accuracy"],	
  gc_tree_bag6_cm$overall["Accuracy"],	gc_tree_bag6_cm$byClass["Sensitivity"],	gc_tree_bag6_cm$byClass["Specificity"],	gc_tree_bag6_cm$byClass["Balanced Accuracy"],	gc_tree_bag8_cm$overall["Accuracy"],	
  gc_tree_bag8_cm$byClass["Sensitivity"],	gc_tree_bag8_cm$byClass["Specificity"],	gc_tree_bag8_cm$byClass["Balanced Accuracy"],	gc_tree_bagfiltered_cm$overall["Accuracy"],	
  gc_tree_bagfiltered_cm$byClass["Sensitivity"],	gc_tree_bagfiltered_cm$byClass["Specificity"],	gc_tree_bagfiltered_cm$byClass["Balanced Accuracy"],
  gc_tree_6_cm_sd2$overall["Accuracy"],	gc_tree_6_cm_sd2$byClass["Sensitivity"],	gc_tree_6_cm_sd2$byClass["Specificity"],	gc_tree_6_cm_sd2$byClass["Balanced Accuracy"],	
  gc_tree_8_cm_sd2$overall["Accuracy"],	gc_tree_8_cm_sd2$byClass["Sensitivity"],	gc_tree_8_cm_sd2$byClass["Specificity"],	gc_tree_8_cm_sd2$byClass["Balanced Accuracy"],	
  gc_tree_filtered_cm_sd2$overall["Accuracy"],	gc_tree_filtered_cm_sd2$byClass["Sensitivity"],	gc_tree_filtered_cm_sd2$byClass["Specificity"],	gc_tree_filtered_cm_sd2$byClass["Balanced Accuracy"],	
  gc_tree_bag6_cm_sd2$overall["Accuracy"],	gc_tree_bag6_cm_sd2$byClass["Sensitivity"],	gc_tree_bag6_cm_sd2$byClass["Specificity"],	gc_tree_bag6_cm_sd2$byClass["Balanced Accuracy"],	
  gc_tree_bag8_cm_sd2$overall["Accuracy"],	gc_tree_bag8_cm_sd2$byClass["Sensitivity"],	gc_tree_bag8_cm_sd2$byClass["Specificity"],	gc_tree_bag8_cm_sd2$byClass["Balanced Accuracy"],	
  gc_tree_bagfiltered_cm_sd2$overall["Accuracy"],	gc_tree_bagfiltered_cm_sd2$byClass["Sensitivity"],	gc_tree_bagfiltered_cm_sd2$byClass["Specificity"],	gc_tree_bagfiltered_cm_sd2$byClass["Balanced Accuracy"],
  gc_tree_6_cm_sd3$overall["Accuracy"],	gc_tree_6_cm_sd3$byClass["Sensitivity"],	gc_tree_6_cm_sd3$byClass["Specificity"],	gc_tree_6_cm_sd3$byClass["Balanced Accuracy"],	
  gc_tree_8_cm_sd3$overall["Accuracy"],	gc_tree_8_cm_sd3$byClass["Sensitivity"],	gc_tree_8_cm_sd3$byClass["Specificity"],	gc_tree_8_cm_sd3$byClass["Balanced Accuracy"],	
  gc_tree_filtered_cm_sd3$overall["Accuracy"],	gc_tree_filtered_cm_sd3$byClass["Sensitivity"],	gc_tree_filtered_cm_sd3$byClass["Specificity"],	gc_tree_filtered_cm_sd3$byClass["Balanced Accuracy"],	
  gc_tree_bag6_cm_sd3$overall["Accuracy"],	gc_tree_bag6_cm_sd3$byClass["Sensitivity"],	gc_tree_bag6_cm_sd3$byClass["Specificity"],	gc_tree_bag6_cm_sd3$byClass["Balanced Accuracy"],	
  gc_tree_bag8_cm_sd3$overall["Accuracy"],	gc_tree_bag8_cm_sd3$byClass["Sensitivity"],	gc_tree_bag8_cm_sd3$byClass["Specificity"],	gc_tree_bag8_cm_sd3$byClass["Balanced Accuracy"],	
  gc_tree_bagfiltered_cm_sd3$overall["Accuracy"],	gc_tree_bagfiltered_cm_sd3$byClass["Sensitivity"],	gc_tree_bagfiltered_cm_sd3$byClass["Specificity"],	gc_tree_bagfiltered_cm_sd3$byClass["Balanced Accuracy"]
), nrow=24,ncol=5))

gc_dectree_results.num = c("X3","X4","X5")
gc_dectree_results[gc_dectree_results.num] = lapply(gc_dectree_results[gc_dectree_results.num],as.numeric) 
gc_dectree_results$Average = rowMeans(gc_dectree_results[,3:5]) %>% round(digits = 3)
gc_dectree_results[gc_dectree_results.num] = gc_dectree_results[gc_dectree_results.num] %>% round(digits = 3)  
colnames(gc_dectree_results) = c("Model","Metric","Seed 1 Results","Seed 2 Results","Seed 3 Results","Three-seed Average")

gc_dectree_results.fac = c("Model","Metric")
gc_dectree_results[gc_dectree_results.fac] = lapply(gc_dectree_results[gc_dectree_results.fac],as.factor) 

gc_dectree_results[2:6] %>% gt() %>%
  tab_header(title= md("Decision Tree Confusion Matrix Results")) %>%
  tab_row_group(label="Model BagFil",rows = 21:24) %>%
  tab_row_group(label="Model Bag8",rows = 17:20) %>%
  tab_row_group(label="Model Bag6",rows = 13:16) %>%
  tab_row_group(label="Model Fil",rows = 9:12) %>%
  tab_row_group(label="Model 8",rows = 5:8) %>%
  tab_row_group(label="Model 6",rows = 1:4) %>%
  tab_style(
    locations = cells_body(columns=Metric),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_title("title"),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(weight="bold")
  ) %>%
  tab_style(
    locations = cells_row_groups(),
    style = cell_text(weight="bold")
  )

#______________________________________________________________________________________________________________#

### ROC-AUC diagram

#Model 6 (bagged)

germancredit_test = mutate(germancredit_test, Predictions_treebag = prediction_tree_bag_6)
gc_predictionROCR_tree_bag6 = prediction(ifelse(prediction_tree_bag_6 == "Good", 1, 0), germancredit_test$Credit_RiskRating_01)
perfROCR_tree_bag6  = performance(gc_predictionROCR_tree_bag6, "tpr", "fpr")
plot(perfROCR_tree_bag6, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag6_per = performance(gc_predictionROCR_tree_bag6, "auc")@y.values
gc_predictionROCR_tree_bag6_per

germancredit_test2 = mutate(germancredit_test2, Predictions_treebag = prediction_tree_bag_6_sd2)
gc_predictionROCR_tree_bag6_sd2 = prediction(ifelse(prediction_tree_bag_6_sd2 == "Good", 1, 0), germancredit_test2$Credit_RiskRating_01)
perfROCR_tree_bag6_sd2  = performance(gc_predictionROCR_tree_bag6_sd2, "tpr", "fpr")
plot(perfROCR_tree_bag6_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag6_per_sd2 = performance(gc_predictionROCR_tree_bag6_sd2, "auc")@y.values
gc_predictionROCR_tree_bag6_per_sd2

germancredit_test3 = mutate(germancredit_test3, Predictions_treebag = prediction_tree_bag_6_sd3)
gc_predictionROCR_tree_bag6_sd3 = prediction(ifelse(prediction_tree_bag_6_sd3 == "Good", 1, 0), germancredit_test3$Credit_RiskRating_01)
perfROCR_tree_bag6_sd3  = performance(gc_predictionROCR_tree_bag6_sd3, "tpr", "fpr")
plot(perfROCR_tree_bag6_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag6_per_sd3 = performance(gc_predictionROCR_tree_bag6_sd3, "auc")@y.values
gc_predictionROCR_tree_bag6_per_sd3

mean(c(unlist(gc_predictionROCR_tree_bag6_per),unlist(gc_predictionROCR_tree_bag6_per_sd2),
       unlist(gc_predictionROCR_tree_bag6_per_sd3)))

#Model 8 (bagged)

germancredit_test = mutate(germancredit_test, Predictions_treebag = prediction_tree_bag_8)
gc_predictionROCR_tree_bag8 = prediction(ifelse(prediction_tree_bag_8 == "Good", 1, 0), germancredit_test$Credit_RiskRating_01)
perfROCR_tree_bag8  = performance(gc_predictionROCR_tree_bag8, "tpr", "fpr")
plot(perfROCR_tree_bag8, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag8_per = performance(gc_predictionROCR_tree_bag8, "auc")@y.values
gc_predictionROCR_tree_bag8_per

germancredit_test2 = mutate(germancredit_test2, Predictions_treebag = prediction_tree_bag_8_sd2)
gc_predictionROCR_tree_bag8_sd2 = prediction(ifelse(prediction_tree_bag_8_sd2 == "Good", 1, 0), germancredit_test2$Credit_RiskRating_01)
perfROCR_tree_bag8_sd2  = performance(gc_predictionROCR_tree_bag8_sd2, "tpr", "fpr")
plot(perfROCR_tree_bag8_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag8_per_sd2 = performance(gc_predictionROCR_tree_bag8_sd2, "auc")@y.values
gc_predictionROCR_tree_bag8_per_sd2

germancredit_test3 = mutate(germancredit_test3, Predictions_treebag = prediction_tree_bag_8_sd3)
gc_predictionROCR_tree_bag8_sd3 = prediction(ifelse(prediction_tree_bag_8_sd3 == "Good", 1, 0), germancredit_test3$Credit_RiskRating_01)
perfROCR_tree_bag8_sd3  = performance(gc_predictionROCR_tree_bag8_sd3, "tpr", "fpr")
plot(perfROCR_tree_bag8_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bag8_per_sd3 = performance(gc_predictionROCR_tree_bag8_sd3, "auc")@y.values
gc_predictionROCR_tree_bag8_per_sd3

mean(c(unlist(gc_predictionROCR_tree_bag8_per),unlist(gc_predictionROCR_tree_bag8_per_sd2),
       unlist(gc_predictionROCR_tree_bag8_per_sd3)))

#Model filter (bagged)

germancredit_test = mutate(germancredit_test, Predictions_treebag = prediction_tree_bag_filtered)
gc_predictionROCR_tree_bagfilter = prediction(ifelse(prediction_tree_bag_filtered == "Good", 1, 0), germancredit_test$Credit_RiskRating_01)
perfROCR_tree_bagfilter  = performance(gc_predictionROCR_tree_bagfilter, "tpr", "fpr")
plot(perfROCR_tree_bagfilter, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bagfilter_per = performance(gc_predictionROCR_tree_bagfilter, "auc")@y.values
gc_predictionROCR_tree_bagfilter_per

germancredit_test2 = mutate(germancredit_test2, Predictions_treebag = prediction_tree_bag_filtered_sd2)
gc_predictionROCR_tree_bagfilter_sd2 = prediction(ifelse(prediction_tree_bag_filtered_sd2 == "Good", 1, 0), germancredit_test2$Credit_RiskRating_01)
perfROCR_tree_bagfilter_sd2  = performance(gc_predictionROCR_tree_bagfilter_sd2, "tpr", "fpr")
plot(perfROCR_tree_bagfilter_sd2, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bagfilter_per_sd2 = performance(gc_predictionROCR_tree_bagfilter_sd2, "auc")@y.values
gc_predictionROCR_tree_bagfilter_per_sd2

germancredit_test3 = mutate(germancredit_test3, Predictions_treebag = prediction_tree_bag_filtered_sd3)
gc_predictionROCR_tree_bagfilter_sd3 = prediction(ifelse(prediction_tree_bag_filtered_sd3 == "Good", 1, 0), germancredit_test3$Credit_RiskRating_01)
perfROCR_tree_bagfilter_sd3  = performance(gc_predictionROCR_tree_bagfilter_sd3, "tpr", "fpr")
plot(perfROCR_tree_bagfilter_sd3, colorize = TRUE)+ abline(0,1)
gc_predictionROCR_tree_bagfilter_per_sd3 = performance(gc_predictionROCR_tree_bagfilter_sd3, "auc")@y.values
gc_predictionROCR_tree_bagfilter_per_sd3

mean(c(unlist(gc_predictionROCR_tree_bagfilter_per),unlist(gc_predictionROCR_tree_bagfilter_per_sd2),
       unlist(gc_predictionROCR_tree_bagfilter_per_sd3)))

#______________________________________________________________________________________________________________#
#END
