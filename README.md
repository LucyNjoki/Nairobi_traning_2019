# Classification Model
European Union Data with Response variable as leave/stay EU  and different Explanatory Variables such as gender,religion
## Jupyterlab classification model|exploration|presenting tutorial

This project is a tutorial on classification model|exploration|presenting using Jupyter notebooks through the dpylr,ggplot2, package/library 


## Getting Started

### Prerequisites

Be running Jupyter. I use the anaconda installer: https://www.continuum.io/downloads

You'll also need to install the libraries listed below, if you don't have them.

```
dplyr, ggplot2, ggthemes, FSA
```


You can install the packages  using install.packages() like this

Windows: As admin open the anaconda prompt and run
```
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("FSA")
install.packages("magrittr)
```
#clearing the environment
rm(list = ls())

#loading library
library("plyr"); library("dplyr") #data manipulation
library("ggplot2")  #plot using ggplot()
library("ggthemes") #ggplot themes
library("FSA") #descriptive statistics

#setting work directory
setwd("D:\\NJUKI\\Nairobi training\\Data templates\\Classification - 1st week May")

#getting the work directory
getwd()


#importing data
mydata <- read.csv(file = "ess.csv",header = TRUE, sep = ",")
mydata

attach(mydata)

#variable names
colnames(mydata)

#dimenstions of data(how big the data is interms of variables and observations)
dim(mydata)

#structure of the data
str(mydata)
glimpse(mydata)
class(mydata)
#labelling levels of variables
leave <- mapvalues(leave, from = c(0,1), to = c("stay","leave"))
leave
trust_people <- mapvalues(trust_people, from = c())

#checking the first/last coloums
head(mydata)
tail(mydata)

#table variable in dataset
table(leave)
table(mydata[,1])
table(mydata[,"leave"])


#table two variable
table(leave,age)

#rename variable
colnames(mydata)[4] <- "res_age"
colnames(mydata)[1:2] <- c("response", "country")
colnames(mydata)[1] <- "leave"

View(mydata)

#subset data by selecting specific columns
subset.df <- mydata[,c(1,3)]
subset.df 

#descriptive statistics for age by gender

Summarize(res_age ~ gender, data = mydata)


#summary table 
age_sum <- mydata %>% 
  group_by(gender) %>% 
  summarise(average = mean(res_age))
age_sum



religion_sum <- mydata %>%
  group_by(religion,gender) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Percent = round(Count * 100 / sum(Count),2))
#mutate - generates new variable percentage
religion_sum

#R themes from theme_set()
#theme_economist()
#theme_stata()
#theme_tufte()
#theme_ecnomistwhite()
#vjust - positioning  the label on the bar
theme_set(theme_tufte())
ggplot(religion_sum, aes(x = religion, y = Percent, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "Religion",y = "Percentage",title = "Distribution of gender by religion",subtitle = "") +
  geom_text(aes(label = paste(Percent,"%",sep = "")), 
            size = 3, position = position_dodge(0.9),vjust = -0.25) +
scale_fill_manual(values = c("#EEAAEE","#DF5353"))  +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),#moving title to the centre
        text = element_text(family="Source Sans Pro", size = 12)) 
  
 
#size = font size

ggplot(age_sum, aes(x = gender, y = average)) +
  geom_bar(stat = "identity", fill = c("#FF0066", "#313131"),width = 0.6)+
  labs(x = "Gender",y = "Average",title = "Distribution of mean age by gender",subtitle = "") +
  geom_text(aes(label = paste(round(average,2),sep = "")), size = 4, position = position_dodge(0.9),vjust = -0.25) +
  theme(legend.position = "topleft",
        legend.title = element_blank(),
        plot.title= element_text(hjust = 0.5))

#data analysis
t.test(res_age ~ gender, data = mydata)
#No siginficant difference between mean age of male and female

t.test(res_age ~ leave, data = mydata)
#There is significant difference between mean age of those voted to leave EU and those who voted to leave EU 

t.test(years_education ~ leave)
#there is  significant difference in mean years of schooling those who voted to leave EU and Staying in EU 

t.test(news_consumption ~ leave)
#there is no  significant difference in mean time for those who voted to leave EU and Staying in EU


#Assumption #1: Your dependent variable should consist of two categorical, independent (unrelated) groups (i.e., a dichotomous variable). 
#Examples of dichotomous variables include gender (2 groups: male or female).
#The two categories of the dependent variable need to be mutually exclusive and exhaustive.

#Assumption #2: You have two or more independent variables, which should be measured at the continuous or nominal level. 
#Examples of continuous variables include height (measured in feet and inches), temperature (measured in °C), 
#salary (measured in US dollars), revision time (measured in hours), intelligence (measured using IQ score), 
#reaction time (measured in milliseconds), test performance (measured from 0 to 100), sales (measured in number of transactions per month), and so forth.				

#Assumption #3: You should have independence of observations, which means that there is no relationship between the observations. 
#If you do not have independence of observations, you most likely have repeated measures, and you will need another type of statistical test.


#Assumption #4: Your data must not show multicollinearity, which occurs when you have two or more independent variables that are highly correlated with each other.

#Assumption #5: There needs to be a linear relationship between any continuous independent variables and the logit transformation of the dependent variable. 


#Assumption #6: There should be no significant outliers, high leverage points or highly influential points, which represent observations in your data set 
#that are in some way unusual. These can have a very negative effect on the binomial logistic regression equation that is used to predict 
#the value of the dependent variable based on the independent variables. You can check for outliers, leverage points and influential points using Stata.

mydata$leave <- factor(mydata$leave)

mylogit <- glm(leave ~ country + gender + res_age + years_education + news_consumption + 
               trust_people + trust_politicians + past_vote + immig_econ  + immig_culture + country_attach +   
               religion + climate_change + imp_tradition  + imp_equality + income + eu_integration  + 
               trade_union + unemployed, data = mydata, family = "binomial")

summary(mylogit)

#log(p/1-p) = b0 + b1*var1 + b2*var2 + b3*var3


#These estimates tell you about the relationship between the independent variables and the dependent variable


#Note: For the independent variables which are not significant, the coefficients are not significantly different from 0, 
#which should be taken into account when interpreting the coefficients.

#Std. Err. – These are the standard errors associated with the coefficients.  
#The standard error is used for testing whether the parameter is significantly different from 0; 
#by dividing the parameter estimate by the standard error you obtain a z-value (see the column with z-values and p-values).  
#The standard errors can also be used to form a confidence interval for the parameter, as shown in the last two columns of this table.

#z and P>|z| – These columns provide the z-value and 2-tailed p-value used in testing the null hypothesis that the coefficient (parameter) is 0.   
#If you use a 2-tailed test, then you would compare each p-value to your preselected value of alpha.  Coefficients having p-values less than alpha are
#statistically significant.	




## Author

* **Lucy Njoki** - [LucyNjoki](https://github.com/LucyNjoki) - [LinkedIn](https://www.linkedin.com/in/lucy-njoki-b34b44135/)

## Acknowledgments

* To Cyrus Murithii  my R/Python mentor.
