# Computational-Statistics-
My Project consist of certain tasks, which are shown below

```
| Scales of Variables
|  |__Nominal (Categorical)
|  |___Ordinal (Categorical)
|  |___Metric 
| Factoring the variables
| Frequency Table (Absoulte & Relative), Marginal Table
| Summary statistics
|  |__ Boxplot
|  |__ Histogram
|  |__ Barplot
| Correlation coefficient
|  |__ Scatter plot
|  |__ Pearson
|  |__ Spearman
|  |__ Kendall
| Distribution among two variables
|  |__ One-sided test (One-sample)
|  |__ Two-sided test (One-sample)
|  |__ One-sided test (Two-sample)
|  |__ Two-sided test (Two-sample)
|  |__ Non-parametric test
|     |__ Wilcox test
| Distribution of one variable among three different groups
|  |__ Parametric test
|     |__ 1-Way ANOVA
|  |__ Non parametric test
|     |__ Kruskal-walli test
|        | Pairwise test
| True Difference
|  |__ 90% confidence interval
| 2-Way ANOVA
| Non-parametric test (friedman test)
| 3-Way ANOVA
```

### we use the following code for performing the above tests
```R
Data$male <- as.factor(Data$male)  # for factoring
levels(Data$male) <- c("Female","Male") # for lebeling the data
table(Data$obesity,Data$male) # absolute frequency
rf <- af/length(af) # relative frequency
margin.table(af,1) # marginal table
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("pearson")) # pearson correlation method
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("spearman")) # spearman correlation method
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("kendall")) # kendall correlation method
t.test(x1,x2, alternative = "less",var.equal = TRUE) # two-sided test
wilcox.test(x1,x2,alternative = "two.sided") # non-parametric test
oneway <- anova(lm(Data$glucose_v1 ~ Data$group)) # 1-Way ANOVA test
pairwise.t.test(glucose_v1,group,var.equal=TRUE,alternative = "two.sided") # paired sample test
t.test(gl,alternative="two.sided",conf.level = 0.90) # to check 90% confidence level
anova(lm(Data$glucose_v1 ~ Data$glucose_v3 * Data$group)) # 2-Way ANOVA test
anova(lm(x ~ id + visit1+ Data$group)) #3-Way ANOVA test
```






