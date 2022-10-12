## SHIVAKRISHNA KARNATI 
## MATRICULATION NUMBER : 56966, 
## MA21W1


#===========================================================================================================================


# Task 2 
# What are the scales of each variable?

# To import the data
Dataset <- read.csv("/home/shivakrishnakarnati/Documents/computational statistics/Project/S2Dataset.csv")
Dataset
#Duplicating the Dataset as Data for further calculations

    
#    • link :-               It is a variable for linking the two data sets. It is a "Nominal scaled variable"

#    • group:-               Finitely many (a few ) possible values, so it’s a "Nominal scaled variable" (Categorical).
#                            Here only 3 groups are present,
#                            1. Fit bit only,
#                            2. the remainder,
#                            3. the goal setting,

#    • male:-               "Nominal scaled variable", and it is categorized as male and female 

#    • height:-             It is appropriate to perform numerical calculations (mean, variance etc).
#                           So it’s a "Metric variable".

#    • weight_v1, v2, v3:-  These are weights at visit1, visit2, visit 3. These are also appropriate to perform numerical calculations
#                           So these are the "metric variables".

#    • bmi_v1, v2, v3:-     (Body Mass Index) (kg/m^2), We can perform numerical
#                           calculations, so these are the "metric variables".

#    • sbp_v1, v2, v3:-     (Systolic Blood Pressure) measures the pressure in your arteries
#                           when your heart beats. These are the "metric variables".

#    • dbp_v1, v2, v3:-     (Diastolic Blood Pressure) measures the pressure in your arteries
#                           when your heart rests between beats. These are the "metric variables".

#    • glucose_v1, v2, v3:- Numerical data, so these can be considered as the "metric
#                           variables".

#    • insulin_v1, v2, v3:- Arithmetic operations can be performed on this data, so
#                           these can be considered as the "metric variables".



#=================================================================================================================================



# Task 3
#  Change the format in R, i.e., code each categorical variable as factor. Code “group” by factor levels
#  “Fitbit-Only”, “Fitbit-Message”, “Fitbit-Message + goal” instead of 0, 1, and 2


Data <-as.data.frame(Dataset) ## Data frame
Data

# Converting into factors
Data$male <- as.factor(Data$male)  
Data$group <- as.factor(Data$group)
Data$link <- as.factor(Data$link)

# To convert a categorical variable as factor
levels(Data$male) <- c("Female","Male")
levels(Data$group) <- c("Fitbit-only", "Fitbit-Message", "Fitbit-Message+goal")  # To access to the levels attribute of a variable

summary(Data)



#==================================================================================================================================



# Task 4 
#  Define a new variable “obesity” as follows. Obesity is “underweight”, “normal”, “overweight”, “obese”,
#  “extremely obese” by bmi_v1 < 18.5, 18,5<= bmi_v1 < 25, 25<=bmi_v1<30, 30<=bmi_v1<35,
#  35<=bmi_v1.
  
Data$obesity <- NA # Defined a new empty variable 
Dt <- Data$bmi_v1

for(i in 1:length(Dt)){
  if(Dt[i] <18.5){
    Data$obesity[i] <- "underweight"
  } else if (Dt[i] >18 & Dt[i] <25){
    Data$obesity[i] <- "normal"
  } else if (Dt[i] >25 & Dt[i] <30){
    Data$obesity[i] <- "overweight"
  }else if(Dt[i] >30 & Dt[i] <35){
    Data$obesity[i] <- "obese"
  }else if(Dt[i] >35){
    Data$obesity[i] <- "extremely obese"
  }
}




#================================================================================================================



# Task 5
#  Provide a frequency table (absolute and relative counts + marginals) of the variable “obesity” vs. “male”
#  categorical variables alongside with bar charts.

# Absolute frequency
# f1+f2+..+fn = N
# fi - frequency of each value
# N - total no. of data values
# We use table() function for absolute frequency
af <- table(Data$obesity,Data$male)
af
# Relative frequency (ni) = fi/length(fi) ( Generally expressed as proportions of the row or column totals)
rf <- af/length(af)
rf
# Marginal table ( the sums of the counts along one or the other dimension of a table)
margin.table(af,1) # no. of overweight, obese, extremely obese are 79, 39, 9
margin.table(af,2) # females are 96 and males are 31
# Barplot
barplot(af)
barplot(rf)



#=================================================================================================




# Task 6
#  Provide summary statistics for the variables” weight_v1”, “sbp_v1”, “dbp_v1”, “glucose_v1”,
#  “insulin_v1”, boxplots, and histograms

attach(Data) # To mask the objects from Data set

summary(weight_v1)
summary(sbp_v1)
summary(dbp_v1)
summary(glucose_v1)
summary(insulin_v1)

# box plot of weight_v1, dbp_v1, glucose_v1, insulin_v1
boxplot(weight_v1,sbp_v1,dbp_v1,glucose_v1,insulin_v1)
# histograms 
hist(weight_v1)
hist(sbp_v1)
hist(dbp_v1)
hist(glucose_v1)
hist(insulin_v1)



#=====================================================================================================================



# Task 7
#  Investigate any if there is a correlation between “weight_v1” and “glucose_v1”. Provide a scatter plot.
#  Which correlation coefficient do you prefer in this case? Can you come up with a hypothesis? Perform
#  a test (based on your hypothesis) concerning the correlation coefficient



cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("pearson")) #0.06784542
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("spearman")) #0.04475156
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("kendall")) # 0.04016933
# The correlation coefficient is close to 0!
# Here there is no specifically difference between pearson, spearman, kendall coefficients, 
# But we can use "pearson" correlation coefficients, because there are not much outliers in data



# Scatter plot
plot(Data$weight_v1,Data$glucose_v1)


# Test for correlation
#              Null Hypothesis H0 := Weight_v1 and glucose_v1 are uncorrelated
#       Alternative Hypothesis HA := weight_v1 and glucose_v1 are correlated 


cor.test(Data$weight_v1,Data$glucose_v1,alternative = "two.sided", method="pearson") # independent samples

# Based on p-value(>0.05) we are failed to reject Null hypothesis,
# Which means weight_v1 and glucose_v1 are uncorrelated.
# Type II error 



#================================================================================================================



# Task 8
#  To see whether the distributions of “glucose_v1” is comparable among males and females, perform a
#  two-sided test (t-test and a non-parametric alternative). Check whether you can safely assume equal
#  variances (test for this too). If the variances are equal, would you use the Welch-correction (yes or no
#  and why)?


# H0 : distribution can't be comparable among males and females
# HA : distribution can be comparable among males and females
# significance level alpha = 0.05

Data$glucose_v1
Data$male

# Assigning the glucose_v1 with female and male respectively 
x1<-  Data$glucose_v1[Data$male=="Female"]
x2 <- Data$glucose_v1[Data$male=="Male"]

hist(x1)
hist(x2)

t.test(x1,x2, alternative = "less",var.equal = TRUE)  # t = -2.7596
# p - value 0.03047< 0.05, the observation lies in rejection region
# so we can reject the null hypothesis and we can't compare the distribution among males and females

# To check the variances are equal or not, we perform following 
var(x1,na.rm=TRUE)
var(x2,na.rm=TRUE)

# variances are not equal
# Let's assume they have equal variances
n <- sum(!is.na(x1)) # sum of glucose_v1 with males
m <- sum(!is.na(x2)) # sum of glucose_v1 with females

sp <- sqrt((n-1)/(n+m-2)*var(x1,na.rm = TRUE) + (m-1)/(n+m-2)*var(x2,na.rm = TRUE)) # pooled standard deviation

X <- mean(x1,na.rm = TRUE) 
Y <- mean(x2,na.rm = TRUE)

# Test statistics
(X-Y-0)/(sp*sqrt(1/n+1/m)) #-2.759553


# Non-parametric alternative (Unpaired two-samples Wilcoxon test)
# is a non-parametric alternative to the unpaired two-samples t-test, 
# which can be used to compare two independent groups of samples,
# to 

hist(x1)
hist(x2)

#  Here the data is not normally distributed, so we can perform non-parametric test

wilcox.test(x1,x2,alternative = "two.sided")

#p-value 0.01943<0.05, Here we can assure that the two variables independent. 

# Test for equality of variance

#             Null Hypothesis: sample variances are Equal
#      Alternative Hypothesis: sample variances are not Equal

# F-Test formula :       Larger sample variance
#                  F = -------------------------
#                        Smaller sample variance

# Alpha = 0.05

var.test(x1,x2) # p-value < 0.05, we can reject the null hypothesis, 

# and it shows that the sample variances are not equal.

# We can assure that variances are not equal
# To check manually
var(Data$glucose_v1,Data$male=="Female",na.rm = TRUE)
var(Data$glucose_v1,Data$male=="Male",na.rm = TRUE)

# Here we can perform Welch correction because the variances are not equal



#=========================================================================================================



# Task 9
# Compare the distribution of “glucose_v1” across the three different groups “Fitbit-Only”, “Fitbit-
# Message”, “Fitbit-Message + goal”. Perform a 1-Way ANOVA and a nonparametric alternative. Which
# would you prefer in this case? (Argue based on whether the assumptions for the respective tests hold –
# this might require even more testing.) If age cannot be assumed to be identical across the groups,
# perform pairwise t-tests (with a common estimate for variance) and identify which groups are different.
# Apply a multiple test-correction. Which test correction would you prefer?




# Comparison of glucose_v1 across the three different groups "Fitbit-only", "Fitbit-Message", "Fitbit-Message+goal"

x1 <- Data$glucose_v1[Data$group == "Fitbit-only"]
x2 <- Data$glucose_v1[Data$group == "Fitbit-Message"]
x3 <- Data$glucose_v1[Data$group == "Fitbit-Message+goal"]

# 1-Way Anova

# To check the distribution among the 3 groups, we perform 1 - way anova
# H0 : Means of all samples are equal
# H1 : Means of all samples are not equal 


boxplot(Data$glucose_v1 ~ Data$group)

oneway <- anova(lm(Data$glucose_v1 ~ Data$group)) # linear model for glucose_v1 as dependent variable and group as independent 
summary(lm(Data$glucose_v1 ~ Data$group)) 
# p -value 0.1353 > 0.05 which means that means are unequal, we fail to reject the null hypothesis

# Let's assume all groups have equal variances
# To check whether the respective assumption hold, we can proceed with another approach
# which is called "Levene test"
# install.packages("lawstat") # To get the test, we need to install this package

library(lawstat)

glu <- Data[!is.na(glucose_v1) & !is.na(group),]
 
# Levene test to check the equal variances
levene.test(glu$glucose_v1,glu$group)

# p -value 0.2438 > 0.05 -> variances are equal


# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test,
# which extends the two-samples Wilcoxon test in the situation where there are more than two groups. 
# It’s recommended when the assumptions of one-way ANOVA test are not met. 

# significance level (alpha) = 0.05

kruskal.test(glucose_v1 ~group)

# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences between the glucose and  fitbit groups.


#The paired samples t-test is used to compare the means between two related groups of samples

pairwise.t.test(glucose_v1,group,var.equal=TRUE,alternative = "two.sided") 


# To check glucose_v1 assumed to be identical across the 3 groups,
# Multiple test correction
#  Holm 
pairwise.t.test(Data$glucose_v1,Data$group,pool.sd = TRUE,p.adj= "holm" ) 
# Hochberg 
pairwise.t.test(Data$glucose_v1,Data$group,pool.sd=TRUE,p.adjust.method="hochberg")
# Bonferroni
pairwise.t.test(df$mmse, df$site,p.adjust.method = "bonf")




#=======================================================================================================================




# 10. Perform a t-test (and a non-parametric alternative) to check whether “glucose_v1” and “glucose_v3”
# are significantly different in the 3 groups (1 test per group). Provide 90% confidence intervals for the
# true difference in glucose at the first and third visit.

# H0 : The variables are significantly different in the 3 groups
# HA : The variables are significantly same in the 3 groups

# To check manually whether the Confidence interval true difference in two groups
k <- length(Data$glucose_v1)
gl <- (!is.na(Data$glucose_v1 - Data$glucose_v3))
alpha <- 0.1  # 90% CI 
mean(gl) - sd(gl)/sqrt(n)*qt(1-alpha/2,n-1)  ### lower confidence point 0.5946179
mean(gl) - sd(gl)/sqrt(n)*qt(alpha/2,n-1)    ### upper confidence point 0.7335071


# To check using a t-test 
t.test(gl,alternative="two.sided",conf.level = 0.90)

# Confidence interval
# 90 percent confidence interval:
#  0.5946179    0.7335071

# Non - parametric alternative
gl <- as.numeric(gl)
wilcox.test(gl,alternative = "two.sided",conf.level = 0.90)



#==========================================================================================================



#11. Perform a 2-Way-ANOVA separately for the groups “Fitbit-only”, “Fitbit-Message”, “Fitbit-Message +
#  goal” to compare the measurements “glucose_v1”, “glucose_v2“, “glucose_v3”. Can they be assumed
#to be identical? Also perform a non-parametric alternative to the 2-Way-ANOVA.


# Script error:
# glucose_v2 is not exist in data set, so perform only to glucose_v1 and glucose_v2

# H0 : means of glucose_v1,glucose_v2,glucose_v3 are equal
# HA : means of glucose_v1,glucose_v2,glucose_v3 are not equal


anova(lm(Data$glucose_v1 ~ Data$glucose_v3 * Data$group)) #2.359e-10 < 0.05
anova(lm(Data$glucose_v3 ~ Data$glucose_v1 * Data$group)) #6.321e-13 < 0.05

# we can reject the null hypotheses, and conclude that means are different among
# the different groups


# a non-parametric alternative to the 2-Way-ANOVA is Friedman test

hist(Data$glucose_v1)
hist(Data$glucose_v3)

x1 <- !is.na(Data$group) # to remove the NA values

friedman.test(Data$glucose_v1, Data$glucose_v3 |x1 )

# since the p-value >0.05 we can conclude that means are unequal 



#=======================================================================================================================


# 12. Perform a 3-Way-ANOVA. Similar to the 2-Way-ANOVA but use the formula “glucose~ ID + visit + group”,
# where ID is the subject ID (you have to define it first), visit is the visit (v1, v2, v3) and group the respective
# study group.

Data$ID <- 1:128 # defining the ID

# Collecting the visit1 data
visit1 <- Data$weight_v1 + Data$glucose_v1+ Data$bmi_v1+ Data$dbp_v1+Data$sbp_v1+ Data$insulin_v1
visit1

# Collecting the visit 2 data
visit2 <- Data$weight_v2+ Data$bmi_v2+Data$sbp_v2+Data$dbp_v2
visit2

# Collecting the visit 3 data
visit3 <- Data$weight_v3+Data$bmi_v3 + Data$sbp_v3+Data$dbp_v3+ Data$glucose_v3
visit3

# removing the missing values
x<- !is.na(Data$glucose1) 
y<- !is.na(Data$glucose_v3)

Data$ID
id <- Data[complete.cases(Data$ID),]


# 3 - way Anova 

# tests which of three separate variables have an effect on an outcome,
# and the relationship between the three variables.

# H0 : Means of 4 variables is same
# H1 : means of 4 variables is different

anova(lm(x ~ id + visit1+ Data$group))
anova(lm(y ~ id+ visit2 + Data$group))
anova(lm(y ~ id+ visit3 + Data$group))

# p -value >0.05 which means we fail to reject the null hypothesis, so we can confirm that 
# means of 3 variables are different 



#=======================================================================================================================

# 13. Test whether “glucose_v1” is higher than “glucose_v3” in the “Fitbit-Message + goal” group. Perform
# an appropriate parametric and nonparametric test.


# Parametric test for distribution of glucose_v1, glucose_v3 on "Fitbit-Message+goal"
x1 <- Data$glucose_v1[Data$group=="Fitbit-Message+goal"]
x2 <- Data$glucose_v3[Data$group=="Fitbit-Message+goal"]


higher_yes <- Data[Data$group =="Fitbit-Message+goal" , ]  ## select all samples with booster =="Yes"
higher_yes
df_group <- subset(higher_yes, select = c(glucose_v1, glucose_v3))
df_group

df_group$higher_YES_Or_NO <- NA       
df_group

attach(df_group)
length(glucose_v1)
glucose_v1[1]

for (i in 1:length(glucose_v1))
{
  if(!is.na(glucose_v1[i] > glucose_v3[i])){
        higher_YES_Or_NO[i] <- "Yes"
  }else{
        higher_YES_Or_NO[i] <- "No"
   }
}

df_group

detach(df_group)

# parametric test for higher distribution
# H0: glucose_v1 is higher than glucose_v3
# HA: glucose_v3 is higher than glucose_v1

hist(x1)
hist(x2)

# since the data is not normally distributed, we can prefer to the non parametric test
# still we can check with t-test
t.test(x1,x2,alternative = "less") # p-value : 2.2e-16 < 0.05 

# here we can come to the conclusion that glucose_v3 has higher distribution with "Fitbit-Message+goal"

# non parametric test 

x1 <- as.numeric(x1)

wilcox.test(x1,x2, alternative = "less") # p-value < 2.2e-16
# even from the non parametric test, we can conclude that glucose_v3 has higher distribution with "Fit-Message+goal"


