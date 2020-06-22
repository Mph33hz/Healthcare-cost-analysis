#   A nationwide survey of hospital costs conducted by the US Agency for Healthcare
#   consists of hospital records of inpatient samples. The given data is restricted to
#   the city of Wisconsin and relates to patients in the age group 0-17 years. The
#   agency wants to analyze the data to research on the healthcare costs and their
#   utilization

# Here is a detailed description of the variables in the dataset:

# AGE : Age of the patient discharged
# FEMALE : Binary variable that indicates if the patient is female
# LOS : Length of stay, in days
# RACE : Race of the patient (specified numerically)
# TOTCHG : Hospital discharge costs
# APRDRG : All Patient Refined Diagnosis Related Groups


#*****************************************************************************************
#                           Loading Data                                                 #
#*****************************************************************************************
healthcare_costs <- read.csv('HospitalCosts.csv')
head(healthcare_costs,5)
summary(healthcare_costs)
#structure of the data frame 
str(healthcare_costs)



##############################################################################################
#                             DATA PREPERATION
##############################################################################################

#We see that the variables RACE and FEMALE are integer variables so we change them to factors
var <- c('RACE','FEMALE')
for (i in var){
  healthcare_costs[,i] = as.factor(healthcare_costs[,i])}
str(healthcare_costs)


#Finding Missing Values in the data
colSums(is.na(healthcare_costs))
#The variable Race has 1 missing value so when we use the variable race we're going to 
#have to drop the variable




#############################################################################################

#********************************************************************************************
#  To record the patient statistics, the agency wants to find the age                       *
#  category of people who frequent the hospital and have the maximum expenditure.           *
# we need find the age category that has the highest frequency of hospital visits,          *
# we can visualise the frequency using a histogram                                          *
#********************************************************************************************

#1. Find the age group with the highest frequency of hospital visits


breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
tags <- c('Group: 0-1','Group: 1-2','Group: 2-3','Group: 3-4','Group: 4-5','Group: 5-6',
          'Group: 6-7','Group: 7-8','Group: 8-9','Group: 9-10','Group: 10-11','Group: 11-12',
          'Group: 12-13','Group: 13-14','Group: 14-15','Group: 15-16','Group: 16-17')
g
group_tags <- cut(healthcare_costs$AGE,breaks = breaks,include.lowest = TRUE,right = FALSE,
                  labels = tags)
summary(group_tags)



hist(healthcare_costs$AGE, 
     main="Frequency of Hospital visits by age categories", 
     xlab="Age Categories", 
     border="black", 
     col="blue")

#Result: The histogram shows that the age category of 0-1 years 
# has the highest frequency of hopital visits,
# The summary shows that  we have  307 hospital visits for people in the age range 0-1 years


#2.To find the Age category with the maximum Expenditure 

#We first need to find the sum of expendicture for each age group and then find the maximum sum
#to compute the summary for the data subset we can use the function aggregare

summary_costs<-aggregate(TOTCHG ~ AGE, FUN = sum, data = healthcare_costs)
summary_costs
Max_cost <- max(aggregate(TOTCHG ~ AGE, FUN = sum, data = healthcare_costs))
Max_cost
#From the summary we see that the maximum expenditure per age group for
# The total hospital discharge cost(TOTCHG) is 678 118 
#  Which is also confirmed by the max function

summary_costs[(summary_costs$TOTCHG == max(summary_costs$TOTCHG)),]
#Result : The age category with the maximum hospital costs is Age category 0-1(AGE =0) 
# with a costs 678 118 which makes sense
#  since the same age category had the highest frequency of hopital visits, followed 
#   by AGE = 17 and AGE = 15 respectively.




#######################################################################################################################





#**********************************************************************************************************************
# In order severity of the diagnosis and treatments and to find out the expensive treatmetents, the agency wants to   *             
#  find the diagnosis realted group that has maximum hospitalization and expenditure.                                 *
#                                                                                                                     *         
#**********************************************************************************************************************

#1. Find diagnosis related group that has the maximum hospitalization

unique(healthcare_costs$APRDRG)

healthcare_costs$APRDRG_Factor <- as.factor(healthcare_costs$APRDRG)
str(healthcare_costs)
#We see that the variable APRDRG has 63 levels

summary(healthcare_costs$APRDRG_Factor)

hist(healthcare_costs$APRDRG, 
     main="Frequency of Hospital visits by diagnostic related group",breaks = 10, 
     xlab="All Patient Refined Diagnosis Related Group", 
     border="black", 
     col="darkgreen")
which.max(summary(healthcare_costs$APRDRG_Factor))

# Result: The histogram shows that the All Patient Refined diagnosis Related Group(APRDRG) 
# with the maximum hospitalization
# is the diagnosis related group betwen 600 and 700
# The summary shows that there were 267 hospital visits for the diagnosis related group 640


#2. To find the diagnosis related group with the maximum expenditure,


#We sum up the expenditure of each diagnosis related group, we then find the maximum value of expenditure 
# for each group using, we use the aggregate function to sum up the expenditure an  then use
# the max function to find the max value

data <-aggregate(TOTCHG ~ APRDRG, FUN = sum, data = healthcare_costs)
max_costs <- max(aggregate(TOTCHG ~ APRDRG, FUN = sum, data = healthcare_costs))
max_costs
data[(data$TOTCHG == max(data$TOTCHG)),]

#   APRDRG  TOTCHG
#44    640  437978

# Result:  The Diagnosis Related Group with the maximum expenditure is group APRDRG = 640 
# with a cost of 437 978
# which is in accrodance with the APRDRG with  maximum hospitalization.

####################################################################################################################



#*****************************************************************************************
# To make sure that there is no malpractice, the agency needs to analyse if the          *
#  race of the patient is related to the hospitalization costs.                          *
#*****************************************************************************************   

#We want to test if whether race has an impact on hospital discharge cost(TOTCHG)
#First we want to find out how many patient fall into each RACE group.  
colSums(is.na(healthcare_costs))
#The variable Race has 1 missing value so we're going to drop the row that contains
#this missing variable.
healthcare_costs <- na.omit(healthcare_costs)

summary(healthcare_costs$RACE)
#The summary shows that 484 of patients that visit the hospital were of RACE 1 this signifies 
#skewwed distribution of obervations.

# We can now check if  whether the race makes an impact on the costs by perform an ANOVA.

# Defining the null and alternative hypothesis
# Ho: The race had no an impact on the costs
# H1: The race had an impact on the costs

# ANOVA dependent variable: TOTCHG 

anova_model <- aov(TOTCHG ~ RACE, data = healthcare_costs)
anova_model
summary(anova_model)

alpha = 0.05

pvalue = 0.943
# We reject the null hypothesis if the p-value is less than the level of significance which is alpha
# therefore since :
pvalue > alpha 
# is true we don't reject the null hypothesis and we conclude that RACE does not have an 
# impact on the hospital cost 

#################################################################################################################


#******************************************************************************************
#    To properly utilize the costs, the agency has to analyze the severity of the         #
#     hospital costs by age and gender for proper allocation of resources.                #
#******************************************************************************************

#We want to test the impact of of AGE and Gender of the hospotal costs 
summary(healthcare_costs$FEMALE)
#The summary shows that gender distribution is almost symmetric 

# We can now check if  whether the Gender and AGE make an impact on the costs by perform an ANOVA.

# Defining the null and alternative hypothesis

# Ho: AGE and Gender have no to impact on hospital costs
# H1:  At least AGE or  Gender have an impact on hospital costs

# ANOVA dependent variable: TOTCHG 

model_anova <- aov(TOTCHG ~ AGE+FEMALE, data = healthcare_costs)
summary(model_anova)
#from the summary of the anova model we see that the variable AGE 
#does have an impact on the hospital cost at at 1% significance level  
# We also see that the variable gender has an impact on the hospital cost at 5% significance level

alpha = 0.05

pvalue_AGE = 0.00323 # pvalue for AGE
pvalue_gender = 0.03638 # pvalue for gender

# We reject the null hypothesis if the p-value is less than the level of significance which is alpha
# therefore since :
pvalue_AGE < alpha
pvalue_gender < alpha
# are both true(both pvalues are less than the significance level) we reject the null hypothesis 
#and conlude that both AGE and gender have an impact on the hospital costs
# however age has more impact than gender

############################################################################################################


#****************************************************************************************
#     Since, the length of stay is the crucial factor for inpatients, the agency wants  *
#        to find if the length of stay can be predicted from age, gender, and race.     *
#****************************************************************************************    

# since we want to predict the length of stay, this is a regression problem and we're going to 
#  use a linear regression model


linear_model <- lm(formula = LOS ~ AGE+FEMALE+RACE,data = healthcare_costs)
linear_model
summary(linear_model)

# Results : From the summary of the linear regression model we see that the variables gender and race 
#            are not significant to the model and thus have no impact on length of stay
#            we also see that the variable AGE is significant to the model at 10% significance level
#             which means at that level the variable AGE can be used to predict the length of stay
#         
#         #The adjusted R-sqaured = -0.005433, Negative Adjusted R2 means insignificance of 
#         #explanatory variables. The results may be improved with the increase in sample size.

#############################################################################################################


#*******************************************************************************************************
# To perform a complete analysis, the agency wants to find the variable that                           #
# mainly affects the hospital costs.                                                                   #
#*******************************************************************************************************


#To find the variables that mainly affect the hospital costs we can construct a linear model 
# with  AGE,FEMALE ,LOS,RACE,APRDRG as predictor variables and TOTCHG as the dependent or target variable

linear_model_complete <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + RACE +  APRDRG,data = healthcare_costs)
linear_model_complete
summary(linear_model_complete)

#Result : From the sumamry of the model we see that the variables AGE,LOS and APRDRG are significant 
#          to the model at 0.1%  
#         We see that the variables LOS and APRDRG both have a pvalue = < 2e-16 thus they are equally 
#         significant and affect the the hospital costs equally

#      : The adjusted R-Square = 0.5462, which means our model has an accuarcy of 54.6%

#################################################################################################################    
#################################################################################################################    
