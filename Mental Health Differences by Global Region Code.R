## ---- message=FALSE-------------------------------------------------------------------------------------------
library(tidyverse) #Used for data manipulation, organization and visualization
library(ggiraphExtra) #Used for data visualization
library(rstatix) #Used for parametric statistical analysis functions
library(car) #Used for recoding variables and parametric assumptions testing
library(countrycode) #Used to get country/region names from ISO codes
library(knitr) #Used to create HTML style descrptive tables
library(FSA) #Used to conduct dunn post hoc test
library(summarytools)# Used to get frequency results


## -------------------------------------------------------------------------------------------------------------
##Import and attach initial dataset
data = read.csv("data.csv")



## -------------------------------------------------------------------------------------------------------------
#Fixing the scoring of the DASS items into their original values
data[,1] = Recode(data[,1], "1=0; 2=1; 3=2; 4=3")
data[,4] = Recode(data[,4], "1=0; 2=1; 3=2; 4=3")
data[,7] = Recode(data[,7], "1=0; 2=1; 3=2; 4=3")
data[,10] = Recode(data[,10], "1=0; 2=1; 3=2; 4=3")
data[,13] = Recode(data[,13], "1=0; 2=1; 3=2; 4=3")
data[,16] = Recode(data[,16], "1=0; 2=1; 3=2; 4=3")
data[,19] = Recode(data[,19], "1=0; 2=1; 3=2; 4=3")
data[,22] = Recode(data[,22], "1=0; 2=1; 3=2; 4=3")
data[,25] = Recode(data[,25], "1=0; 2=1; 3=2; 4=3")
data[,28] = Recode(data[,28], "1=0; 2=1; 3=2; 4=3")
data[,31] = Recode(data[,31], "1=0; 2=1; 3=2; 4=3")
data[,34] = Recode(data[,34], "1=0; 2=1; 3=2; 4=3")
data[,37] = Recode(data[,37], "1=0; 2=1; 3=2; 4=3")
data[,40] = Recode(data[,40], "1=0; 2=1; 3=2; 4=3")
data[,43] = Recode(data[,43], "1=0; 2=1; 3=2; 4=3")
data[,46] = Recode(data[,46], "1=0; 2=1; 3=2; 4=3")
data[,49] = Recode(data[,49], "1=0; 2=1; 3=2; 4=3")
data[,52] = Recode(data[,52], "1=0; 2=1; 3=2; 4=3")
data[,55] = Recode(data[,55], "1=0; 2=1; 3=2; 4=3")
data[,58] = Recode(data[,58], "1=0; 2=1; 3=2; 4=3")
data[,61] = Recode(data[,61], "1=0; 2=1; 3=2; 4=3")
data[,64] = Recode(data[,64], "1=0; 2=1; 3=2; 4=3")
data[,67] = Recode(data[,67], "1=0; 2=1; 3=2; 4=3")
data[,70] = Recode(data[,70], "1=0; 2=1; 3=2; 4=3")
data[,73] = Recode(data[,73], "1=0; 2=1; 3=2; 4=3")
data[,76] = Recode(data[,76], "1=0; 2=1; 3=2; 4=3")
data[,79] = Recode(data[,79], "1=0; 2=1; 3=2; 4=3")
data[,82] = Recode(data[,82], "1=0; 2=1; 3=2; 4=3")
data[,85] = Recode(data[,85], "1=0; 2=1; 3=2; 4=3")
data[,88] = Recode(data[,88], "1=0; 2=1; 3=2; 4=3")
data[,91] = Recode(data[,91], "1=0; 2=1; 3=2; 4=3")
data[,94] = Recode(data[,94], "1=0; 2=1; 3=2; 4=3")
data[,97] = Recode(data[,97], "1=0; 2=1; 3=2; 4=3")
data[,100] = Recode(data[,100], "1=0; 2=1; 3=2; 4=3")
data[,103] = Recode(data[,103], "1=0; 2=1; 3=2; 4=3")
data[,106] = Recode(data[,106], "1=0; 2=1; 3=2; 4=3")
data[,109] = Recode(data[,109], "1=0; 2=1; 3=2; 4=3")
data[,112] = Recode(data[,112], "1=0; 2=1; 3=2; 4=3")
data[,115] = Recode(data[,115], "1=0; 2=1; 3=2; 4=3")
data[,118] = Recode(data[,118], "1=0; 2=1; 3=2; 4=3")
data[,121] = Recode(data[,121], "1=0; 2=1; 3=2; 4=3")
data[,124] = Recode(data[,124], "1=0; 2=1; 3=2; 4=3")


## -------------------------------------------------------------------------------------------------------------
#Creating total scores for the DASS-21 sub-scales (Stress, Anxiety and Depression

data$stress = rowSums(data[,c(1,16,22,31,34,40,52)])*2
data$depression = rowSums(data[c(7,13,28,37,46,49,61)])*2
data$anxiety = rowSums(data[, c(4,10,19,25,43,55,58)])*2



## -------------------------------------------------------------------------------------------------------------
attach(data)

#extract master dataset for analysis
df = data.frame(age,country,engnat,familysize,education, gender,major,married,orientation,race,religion,urban,voted,depression,anxiety,stress)

detach(data)


## -------------------------------------------------------------------------------------------------------------
#recode 0 as NA
df[,c(3,5:6,8:13)] = na_if(df[,c(3,5:6,8:13)],0)


## -------------------------------------------------------------------------------------------------------------
#Relabeling categorical values
df$engnat = Recode(df$engnat, '1="Yes"; 2="No"', as.factor = TRUE)
df$education = Recode(df$education, '1="Less than high school"; 2="High school"; 3="University degree"; 4="Graduate degree"', as.factor = TRUE)
df$urban = Recode(df$urban, '1="Rural(country side)"; 2="Suburban"; 3="Urban(town, city)"', as.factor = TRUE)
df$gender = Recode(df$gender, '1="Male"; 2="Female"; 3="Other"', as.factor =TRUE)
df$religion = Recode(df$religion, '1="Agnostic"; 2="Atheist"; 3="Buddhist"; 4="Christian (Catholic)"; 5="Christian (Mormon)"; 6="Christian (Protestant)"; 7="Christian (Other)"; 8="Hindu"; 9="Jewish"; 10="Muslim"; 11="Sikh"; 12="Other"', as.factor =TRUE)
df$orientation = Recode(df$orientation, '1="Heterosexual"; 2="Bisexual"; 3="Homosexual"; 4="Asexual"; 5="Other"', as.factor =TRUE)
df$race = Recode(df$race, '10="Asian"; 20="Arab"; 30="Black"; 40="Indigenous Australian"; 50="Native American"; 60="White"; 70="Other"', as.factor = TRUE)
df$voted = Recode(df$voted, '1="Yes"; 2="No"', as.factor = TRUE)
df$married = Recode(df$married, '1="Never married"; 2="Currently married"; 3="Previously married"', as.factor = TRUE)


## -------------------------------------------------------------------------------------------------------------
#Recode Country ISO Codes to Country Names, Continent Names and Region Names
df$country = as.character(df$country)

df$country_name = countrycode(df$country, "iso2c", "country.name")
df$continent = countrycode(df$country, "iso2c", "continent")
df$region = countrycode(df$country, "iso2c", "region")


## -------------------------------------------------------------------------------------------------------------
#Re-ordering variable labels
df$education = factor(df$education, levels = c("Less than high school","High school","University degree","Graduate degree"))


## -------------------------------------------------------------------------------------------------------------
#Attach Dataset
attach(df)


## -------------------------------------------------------------------------------------------------------------
#Sample size for each global region
kable(freq(region))

#Descriptive results of DASS Variables
kable(df %>%
        get_summary_stats(depression,anxiety,stress, type = "mean_sd"))

#Descriptive results of DASS V=variables by global region
kable(df %>%
        drop_na(region)%>%
        group_by(region)%>%
        get_summary_stats(depression,anxiety,stress, type = "mean_sd"))


## -------------------------------------------------------------------------------------------------------------
#ANOVA Model
model1 = aov(depression~region, df)
summary(model1)


## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis: Bonferroni Correction
kable(df%>%
        emmeans_test(depression~region, p.adjust.method = "BH"))


## -------------------------------------------------------------------------------------------------------------
kable(df %>%
        drop_na(region)%>%
        group_by(region) %>%
        get_summary_stats(depression, type = "mean_sd"))


## -------------------------------------------------------------------------------------------------------------
df %>%
  drop_na(region)%>%
  ggCatepillar(aes(region,depression))+ggtitle("Differences in Depression Scores by Global Region")+scale_x_discrete(labels = c("1","2","3","4","5","6","7"))+labs(y="Depression Scores")


## -------------------------------------------------------------------------------------------------------------
#Check for outliers/
df %>%
  drop_na(region)%>%
  identify_outliers(depression)


#Check for Normality of residuals
model1b = lm(depression~as.factor(region), df)

plot(model1b,2)

#check for homogenity of variance
levene_test(df, depression~region)


## -------------------------------------------------------------------------------------------------------------
#Non-parametric analysis
kruskal.test(depression~region, df)

#Group Medians
df %>% 
  group_by(region)%>%
  summarise(Md = median(depression), N = length(depression))


## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis
kable(dunn_test(depression~region, data = df, p.adjust.method = "BH"))


## -------------------------------------------------------------------------------------------------------------
#ANOVA Model
model2 = aov(anxiety~region, df)
summary(model2)



## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis: Bonferroni Correction
kable(df%>%
        emmeans_test(anxiety~region, p.adjust.method = "BH"))


## -------------------------------------------------------------------------------------------------------------
kable(df %>%
        drop_na(region)%>%
        group_by(region) %>%
        get_summary_stats(anxiety, type = "mean_sd"))



## -------------------------------------------------------------------------------------------------------------

df %>%
  drop_na(region)%>%
  ggCatepillar(aes(region,anxiety))+ggtitle("Differences in Anxiety Scores by Global Region")+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7"))+labs(y="Anxiety Scores")



## -------------------------------------------------------------------------------------------------------------
#Check for outliers/
df %>%
  drop_na(region)%>%
  identify_outliers(anxiety)


#Check for Normality of residuals
model2b = lm(anxiety~as.factor(region), df)

plot(model2b,2)

#check for homogenity of variance
levene_test(df, anxiety~region)


## -------------------------------------------------------------------------------------------------------------
#Non-parametric analysis
kruskal.test(anxiety~region, df)

#Group Medians
df %>% 
  group_by(region)%>%
  summarise(Md = median(anxiety), N = length(anxiety))


## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis
kable(dunn_test(anxiety~region, data = df, p.adjust.method = "BH"))


## -------------------------------------------------------------------------------------------------------------
#ANOVA Model
model3= aov(stress~region, df)
summary(model3)



## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis: Bonferroni Correction
kable(df%>%
        emmeans_test(stress~region, p.adjust.method = "BH"))


## -------------------------------------------------------------------------------------------------------------
kable(df %>%
        drop_na(region)%>%
        group_by(region) %>%
        get_summary_stats(stress, type = "mean_sd"))



## -------------------------------------------------------------------------------------------------------------
df %>%
  drop_na(region)%>%
  ggCatepillar(aes(region,stress))+ggtitle("Differences in Stress Scores by Global Region")+scale_x_discrete(labels = c("1","2","3","4","5","6","7"))+labs(y="Stress Scores")


## -------------------------------------------------------------------------------------------------------------
#Check for outliers/
df %>%
  drop_na(region)%>%
  identify_outliers(stress)


#Check for Normality of residuals
model3b = lm(stress~as.factor(region), df)

plot(model3b,2)

#check for homogenity of variance
levene_test(df, stress~region)


## -------------------------------------------------------------------------------------------------------------
#Non-parametric analysis
kruskal.test(stress~region, df)

#Group Medians
df %>% 
  group_by(region)%>%
  summarise(Md = median(stress), N = length(stress))


## -------------------------------------------------------------------------------------------------------------
#Post Hoc Analysis
kable(dunn_test(stress~region, data = df, p.adjust.method = "BH"))

