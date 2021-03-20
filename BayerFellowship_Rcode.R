##
##
##
## Question 18


library(readxl)
library(openxlsx)
library(tidyverse)
library(janitor)
install.packages("vtree")
library(vtree)
library("dplyr")

library ("ggpubr")

# Import dataset. Use the "FinalDataFile_Survey_results_12_7_2020" dataset
all_data <- read.xlsx("FinalDataFile_Survey_results_12_7_2020.xlsx")
data <- all_data[-c(1, 599),c(19:27, 60, 61, 63:65)]

data <- transform(data, Q9_1 = as.numeric(Q9_1))
data <- transform(data, Q9_2 = as.numeric(Q9_2))
data <- transform(data, Q18_1 = as.numeric(Q18_1))
data <- transform(data, Q18_2 = as.numeric(Q18_2))
data <- transform(data, Q21_1 = as.numeric(Q21_1))
data <- transform(data, Q21_2 = as.numeric(Q21_2))

# create new column to store broader categories for Q8: Instead of urban, suburban
# and rural, create categories for rural and urban only (e.g. combine suburb and urban).
# Do the same for Q20 (two categories of consumer of ag affiliated)
data_2 <- cbind(data, matrix(data=NA, ncol=2, nrow = nrow(data))) 
names(data_2)[15] <- "rural_v_urban"
names(data_2)[16] <- "ag_v_consumer"
data_2$rural_v_urban<- ifelse(grepl("urban", data_2$Q8), "urban", 
                                  ifelse(grepl("Urban", data_2$Q8), "urban", "rural"))

data_2$ag_v_consumer <- ifelse(grepl("Producer/Farmer", data_2$Q20), "ag affiliated",  
                               ifelse(grepl("Academic", data_2$Q20), "ag affiliated", 
                                      ifelse(grepl("Agricultural", data_2$Q20), "ag affiliated",
                                             ifelse(grepl("Student", data_2$Q20), "ag affiliated",
                                                    ifelse(grepl("Government", data_2$Q20), "ag affiliated", "consumer only")))))

data_2 <- transform(data_2, Q9_1 = as.numeric(Q9_1))
data_2 <- transform(data_2, Q9_2 = as.numeric(Q9_2))
data_2 <- transform(data_2, Q18_2 = as.numeric(Q18_2))
data_2 <- transform(data_2, Q21_1 = as.numeric(Q21_1))
data_2 <- transform(data_2, Q21_2 = as.numeric(Q21_2))

#Counts and percentages by  for WTP-household (Q18_1)
tabyl(data_2, rural_v_urban, Q18_1)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, rural_v_urban, Q18_2)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, rural_v_urban, Q21_1)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, rural_v_urban, Q21_12)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, ag_v_consumer, Q18_1)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, ag_v_consumer, Q18_2)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, ag_v_consumer, Q21_1)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

tabyl(data_2, ag_v_consumer, Q21_2)%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)




ggplot(test , aes(x= 1, color = ag_v_consumer, fill = ag_v_consumer)) +
  geom_bar() +
  #scale_color_manual(values=c('dodgerblue2'))+
  #ylab("Percent RTLDG") +
  #xlab("Percent Change in UAV Height") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text=element_text(size=13, colour = "black"),
        axis.title=element_text(size=14),
        legend.text = element_text(size=14, colour = "black"))






########################################################################################
# Q18_1: Willingness to pay, household
########################################################################################

#Counts and percentages by age (Q2) for WTP-household (Q18_1)
Q18_1_Q2_count <- tabyl(data, Q2, Q18_1)
Q18_1_Q2_perc <- tabyl(data, Q2, Q18_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by education level (Q5) for WTP-household (Q18_1)
Q18_1_Q5_count <- tabyl(data, Q5, Q18_1)
Q18_1_Q5_perc <- tabyl(data, Q5, Q18_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by location(urban vs rural; Q8) for WTP-household (Q18_1)
Q18_1_Q8_count <- tabyl(data, Q8, Q18_1)
Q18_1_Q8_perc <- tabyl(data, Q8, Q18_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by ag affiliation (Q20) for WTP-household (Q18_1)
Q18_1_Q20_count <- tabyl(data, Q20, Q18_1)
Q18_1_Q20_perc <- tabyl(data, Q20, Q18_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

########################################################################################
# Q18_2: Willingness to pay, household
########################################################################################

#Counts and percentages by age (Q2) for WTP-household (Q18_2)
Q18_2_Q2_count <- tabyl(data, Q2, Q18_2)
Q18_2_Q2_perc <- tabyl(data, Q2, Q18_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by education level (Q5) for WTP-household (Q18_2)
Q18_2_Q5_count <- tabyl(data, Q5, Q18_2)
Q18_2_Q5_perc <- tabyl(data, Q5, Q18_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by location(urban vs rural; Q8) for WTP-household (Q18_2)
Q18_2_Q8_count <- tabyl(data, Q8, Q18_2)
Q18_2_Q8_perc <- tabyl(data, Q8, Q18_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by ag affiliation (Q20) for WTP-household (Q18_2)
Q18_2_Q20_count <- tabyl(data, Q20, Q18_2)
Q18_2_Q20_perc <- tabyl(data, Q20, Q18_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

########################################################################################
# Q21_1: Willingness to pay, household
########################################################################################

#Counts and percentages by age (Q2) for WTP-household (Q21_1)
Q21_1_Q2_count <- tabyl(data, Q2, Q21_1)
Q21_1_Q2_perc <- tabyl(data, Q2, Q21_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by education level (Q5) for WTP-household (Q21_1)
Q21_1_Q5_count <- tabyl(data, Q5, Q21_1)
Q21_1_Q5_perc <- tabyl(data, Q5, Q21_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by location(urban vs rural; Q8) for WTP-household (Q21_1)
Q21_1_Q8_count <- tabyl(data, Q8, Q21_1)
Q21_1_Q8_perc <- tabyl(data, Q8, Q21_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by ag affiliation (Q20) for WTP-household (Q21_1)
Q21_1_Q20_count <- tabyl(data, Q20, Q21_1)
Q21_1_Q20_perc <- tabyl(data, Q20, Q21_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

########################################################################################
# Q21_2: Willingness to pay, household
########################################################################################

#Counts and percentages by age (Q2) for WTP-household (Q21_2)
Q21_2_Q2_count <- tabyl(data, Q2, Q21_2)
Q21_2_Q2_perc <- tabyl(data, Q2, Q21_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by education level (Q5) for WTP-household (Q21_2)
Q21_2_Q5_count <- tabyl(data, Q5, Q21_2)
Q21_2_Q5_perc <- tabyl(data, Q5, Q21_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by location(urban vs rural; Q8) for WTP-household (Q21_2)
Q21_2_Q8_count <- tabyl(data, Q8, Q21_2)
Q21_2_Q8_perc <- tabyl(data, Q8, Q21_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

#Counts and percentages by ag affiliation (Q20) for WTP-household (Q21_2)
Q21_2_Q20_count <- tabyl(data, Q20, Q21_2)
Q21_2_Q20_perc <- tabyl(data, Q20, Q21_2)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

######################################################################################
perc_sheets <- list("Q18_1_Q2_perc" = Q18_1_Q2_perc,
                    "Q18_1_Q5_perc" = Q18_1_Q5_perc,
                    "Q18_1_Q8_perc" = Q18_1_Q8_perc,
                    "Q18_1_Q20_perc" = Q18_1_Q20_perc,
                    "Q18_2_Q2_perc" = Q18_2_Q2_perc,
                    "Q18_2_Q5_perc" = Q18_2_Q5_perc,
                    "Q18_2_Q8_perc" = Q18_2_Q8_perc,
                    "Q18_2_Q20_perc" = Q18_2_Q20_perc,
                    "Q21_1_Q2_perc" = Q21_1_Q2_perc,
                    "Q21_1_Q5_perc" = Q21_1_Q5_perc,
                    "Q21_1_Q8_perc" = Q21_1_Q8_perc,
                    "Q21_1_Q20_perc" = Q21_1_Q20_perc,
                    "Q21_2_Q2_perc" = Q21_2_Q2_perc,
                    "Q21_2_Q5_perc" = Q21_2_Q5_perc,
                    "Q21_2_Q8_perc" = Q21_2_Q8_perc,
                    "Q21_2_Q20_perc" = Q21_2_Q20_perc)

write.xlsx(perc_sheets, file = "BayerFellowship_Percents.xlsx", rownames = TRUE)
                
######################################################################################
count_sheets <- list("Q18_1_Q2_count" = Q18_1_Q2_count,
                    "Q18_1_Q5_count" = Q18_1_Q5_count,
                    "Q18_1_Q8_count" = Q18_1_Q8_count,
                    "Q18_1_Q20_count" = Q18_1_Q20_count,
                    "Q18_2_Q2_count" = Q18_2_Q2_count,
                    "Q18_2_Q5_count" = Q18_2_Q5_count,
                    "Q18_2_Q8_count" = Q18_2_Q8_count,
                    "Q18_2_Q20_count" = Q18_2_Q20_count,
                    "Q21_1_Q2_count" = Q21_1_Q2_count,
                    "Q21_1_Q5_count" = Q21_1_Q5_count,
                    "Q21_1_Q8_count" = Q21_1_Q8_count,
                    "Q21_1_Q20_count" = Q21_1_Q20_count,
                    "Q21_2_Q2_count" = Q21_2_Q2_count,
                    "Q21_2_Q5_count" = Q21_2_Q5_count,
                    "Q21_2_Q8_count" = Q21_2_Q8_count,
                    "Q21_2_Q20_count" = Q21_2_Q20_count)

write.xlsx(count_sheets, file = "BayerFellowship_counts.xlsx", rownames = TRUE)

#######################################################################################
ggplot(data , aes(x= Q18_1, color = Q8, fill = Q8)) +
  geom_bar() +
  #scale_color_manual(values=c('dodgerblue2'))+
  #ylab("Percent RTLDG") +
  #xlab("Percent Change in UAV Height") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text=element_text(size=13, colour = "black"),
        axis.title=element_text(size=14),
        legend.text = element_text(size=14, colour = "black"))

ggplot(data , aes(x= Q18_2, color = Q8, fill = Q2)) +
  geom_bar() +
  #scale_color_manual(values=c('dodgerblue2'))+
  #ylab("Percent RTLDG") +
  #xlab("Percent Change in UAV Height") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text=element_text(size=13, colour = "black"),
        axis.title=element_text(size=14),
        legend.text = element_text(size=14, colour = "black"))


#########################################################################################
#Q18_1 (Location, Q8)

#Check ANOVA assumptions
hist(data_2$Q18_1)

res.aov <- aov(Q18_1 ~ rural_v_urban, data = data_2)
plot(res.aov, 1)
leveneTest(Q18_1 ~ rural_v_urban, data = data_2)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )


# ANOVA normality assumptions were not met

#Wilcoxon test for 2-group non-parametric analysis (average of all levels)
rural <- subset(data_2, rural_v_urban=='rural',na.action = "omit") 
rural <- rural %>% drop_na("Q18_1")

urban <- subset(data_2, rural_v_urban=='urban',na.action = "omit")
urban <- urban %>% drop_na("Q18_1")

mean(rural$Q18_1, na.rm = TRUE)
#n = 106, mean = 2.875
mean(urban$Q18_1, na.rm = TRUE)
#n = 486 (5 NA removed), mean = 3.15

RvUWilcox<- wilcox.test(rural$Q18_1,urban$Q18_1)
RvUWilcox

#Mean not significantly different; W = 22845, p = 0.1134


#Wilcoxon test for 2-group non-parametric analysis (mean of two levels: < $1 or </= $1)
rural_Q18_1 <- rural[, c(10,15)] 
rural_Q18_1 <- cbind(rural_Q18_1, matrix(data=NA, ncol=1, nrow = nrow(rural_Q18_1))) 
names(rural_Q18_1)[3] <- "WTP_class"
rural_Q18_1 <- rural_Q18_1 %>% drop_na("Q18_1")
rural_Q18_1$WTP_class<- ifelse(grepl("0", rural_Q18_1$Q18_1), "1", 
                               ifelse(grepl("1", rural_Q18_1$Q18_1), "1", 
                                      ifelse(grepl("2", rural_Q18_1$Q18_1), "1","2")))
rural_Q18_1 <- transform(rural_Q18_1, WTP_class = as.numeric(WTP_class))

urban_Q18_1 <- urban[, c(10,15)] 
urban_Q18_1 <- cbind(urban_Q18_1, matrix(data=NA, ncol=1, nrow = nrow(urban_Q18_1))) 
names(urban_Q18_1)[3] <- "WTP_class"
urban_Q18_1 <- urban_Q18_1 %>% drop_na("Q18_1")
urban_Q18_1$WTP_class<- ifelse(grepl("0", urban_Q18_1$Q18_1), "1", 
                               ifelse(grepl("1", urban_Q18_1$Q18_1), "1", 
                                      ifelse(grepl("2", urban_Q18_1$Q18_1), "1","2")))
urban_Q18_1 <- transform(urban_Q18_1, WTP_class = as.numeric(WTP_class))

mean(rural_Q18_1$WTP_class)
#n = 104, mean = 1.63
mean(urban_Q18_1$WTP_class)
#n= 486 (5 NA removed), 1.72

RvUWilcox<- wilcox.test(rural_Q18_1$WTP_class,urban_Q18_1$WTP_class)
RvUWilcox
#Means are significantly different; W = 23058, p=0.0752


#Three-group: rural, suburban, urban (mean across all levels)
data_2_Q18a <- cbind(data_2, matrix(data=NA, ncol=1, nrow = nrow(data_2))) 
names(data_2_Q18a)[3] <- "WTP_class"
data_2_Q18a <- data_2_Q18a %>% drop_na("Q18_1")
data_2_Q18a$WTP_class<- ifelse(grepl("0", data_2_Q18a$Q18_1), "1", 
                               ifelse(grepl("1", data_2_Q18a$Q18_1), "1", 
                                      ifelse(grepl("2", data_2_Q18a$Q18_1), "1","2")))
data_2_Q18a <- transform(data_2_Q18a, WTP_class = as.numeric(WTP_class))



kruskal.test(WTP_class~Q8, data = data_2_Q18a)
pairwise.wilcox.test(data_2_Q18a$Q18_1, data_2_Q18a$Q8,
                     p.adjust.method = "BH")
#Difference between means is not significant, p=0.1307


#Three-group: rural, suburban, urban (mean across WTP levels)
kruskal.test(Q18_1~Q8, data = data_2)
pairwise.wilcox.test(data_2$Q18_1, data_2$Q8,
                     p.adjust.method = "BH")
#Rural-Sub, p=0.077; rural-urb, 0.562; sub-urb, 0.077

##########################################################################################
##########################################################################################
#Q18_2 (WTP rec; Location, Q8)

#Check ANOVA assumptions
hist(data_2$Q18_2)

res.aov <- aov(Q18_2 ~ rural_v_urban, data = data_2)
plot(res.aov, 1)
leveneTest(Q18_2 ~ rural_v_urban, data = data_2)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )


# ANOVA normality assumptions were not met

#Wilcoxon test for 2-group non-parametric analysis (average of all levels)
rural <- subset(data_2, rural_v_urban=='rural',na.action = "omit") 
rural <- rural %>% drop_na("Q18_2")
#omitted 5 NA

urban <- subset(data_2, rural_v_urban=='urban',na.action = "omit")
urban <- urban %>% drop_na("Q18_2")
#9 NA omitted

mean(rural$Q18_2, na.rm = TRUE)
#n = 99, mean = 2.68
mean(urban$Q18_2, na.rm = TRUE)
#n = 482, mean = 2.92

RvUWilcox<- wilcox.test(rural$Q18_2,urban$Q18_2)
RvUWilcox
#Mean not significantly different; W = 21788, p = 0.1625


#Wilcoxon test for 2-group non-parametric analysis (mean of two levels: < $1 or </= $1)
rural_Q18_2 <- rural[, c(11,15)] 
rural_Q18_2 <- cbind(rural_Q18_2, matrix(data=NA, ncol=1, nrow = nrow(rural_Q18_2))) 
names(rural_Q18_2)[3] <- "WTP_class"
rural_Q18_2 <- rural_Q18_2 %>% drop_na("Q18_2")
rural_Q18_2$WTP_class<- ifelse(grepl("0", rural_Q18_2$Q18_2), "1", 
                               ifelse(grepl("1", rural_Q18_2$Q18_2), "1", 
                                      ifelse(grepl("2", rural_Q18_2$Q18_2), "1","2")))
rural_Q18_2 <- transform(rural_Q18_2, WTP_class = as.numeric(WTP_class))

urban_Q18_2 <- urban[, c(11,15)] 
urban_Q18_2 <- cbind(urban_Q18_2, matrix(data=NA, ncol=1, nrow = nrow(urban_Q18_2))) 
names(urban_Q18_2)[3] <- "WTP_class"
urban_Q18_2 <- urban_Q18_2 %>% drop_na("Q18_2")
urban_Q18_2$WTP_class<- ifelse(grepl("0", urban_Q18_2$Q18_2), "1", 
                               ifelse(grepl("1", urban_Q18_2$Q18_2), "1", 
                                      ifelse(grepl("2", urban_Q18_2$Q18_2), "1","2")))
urban_Q18_2 <- transform(urban_Q18_2, WTP_class = as.numeric(WTP_class))

mean(rural_Q18_2$WTP_class)
#n = 99, mean = 1.56
mean(urban_Q18_2$WTP_class)
#n= 482, 1.63

RvUWilcox<- wilcox.test(rural_Q18_2$WTP_class,urban_Q18_2$WTP_class)
RvUWilcox
#Means are not significantly different; W = 23058, p=0.1738


#Three-group: rural, suburban, urban (mean across WTP levels)
data_2_Q18a <- cbind(data_2, matrix(data=NA, ncol=1, nrow = nrow(data_2))) 
names(data_2_Q18a)[3] <- "WTP_class"
data_2_Q18a <- data_2_Q18a %>% drop_na("Q18_2")
data_2_Q18a$WTP_class<- ifelse(grepl("0", data_2_Q18a$Q18_2), "1", 
                               ifelse(grepl("1", data_2_Q18a$Q18_2), "1", 
                                      ifelse(grepl("2", data_2_Q18a$Q18_2), "1","2")))
data_2_Q18a <- transform(data_2_Q18a, WTP_class = as.numeric(WTP_class))



kruskal.test(WTP_class~Q8, data = data_2_Q18a)
pairwise.wilcox.test(data_2_Q18a$Q18_2, data_2_Q18a$Q8,
                     p.adjust.method = "BH")
#Difference between means is not significant, p=0.1307


#Three-group: rural, suburban, urban (mean across all levels)
data_2_Q18b <- data_2 %>% drop_na("Q18_2")
kruskal.test(Q18_2~Q8, data = data_2_Q18b)
pairwise.wilcox.test(data_2_Q18b$Q18_2, data_2_Q18b$Q8,
                     p.adjust.method = "BH")
#Rural-Sub, p=0.077; rural-urb, 0.562; sub-urb, 0.077

########################################################################
########################################################################

##########################################################################################
##########################################################################################
#Q21_1 (WTP rec; Location, Q8)

#Check ANOVA assumptions
hist(data_2$Q21_1)

res.aov <- aov(Q21_1 ~ rural_v_urban, data = data_2)
plot(res.aov, 1)
leveneTest(Q21_1 ~ rural_v_urban, data = data_2)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )


# ANOVA normality assumptions were not met

#Wilcoxon test for 2-group non-parametric analysis (average of all levels)
rural <- subset(data_2, rural_v_urban=='rural',na.action = "omit") 
rural <- rural %>% drop_na("Q21_1")
#omitted 5 NA

urban <- subset(data_2, rural_v_urban=='urban',na.action = "omit")
urban <- urban %>% drop_na("Q21_1")
#13 NA omitted

mean(rural$Q21_1, na.rm = TRUE)
#n = 101, mean = 2.60
mean(urban$Q21_1, na.rm = TRUE)
#n = 478, mean = 2.65

RvUWilcox<- wilcox.test(rural$Q21_1,urban$Q21_1)
RvUWilcox
#Mean not significantly different; W = 23299, p = 0.5676


#Wilcoxon test for 2-group non-parametric analysis (mean of two levels: < $1 or </= $1)
rural_Q21_1 <- rural[, c(13,15)] 
rural_Q21_1 <- cbind(rural_Q21_1, matrix(data=NA, ncol=1, nrow = nrow(rural_Q21_1))) 
names(rural_Q21_1)[3] <- "Risk_class"
rural_Q21_1 <- rural_Q21_1 %>% drop_na("Q21_1")
rural_Q21_1$Risk_class<- ifelse(grepl("1", rural_Q21_1$Q21_1), "1", 
                               ifelse(grepl("2", rural_Q21_1$Q21_1), "1", "2"))
rural_Q21_1 <- transform(rural_Q21_1, Risk_class = as.numeric(Risk_class))

urban_Q21_1 <- urban[, c(13,15)] 
urban_Q21_1 <- cbind(urban_Q21_1, matrix(data=NA, ncol=1, nrow = nrow(urban_Q21_1))) 
names(urban_Q21_1)[3] <- "Risk_class"
urban_Q21_1 <- urban_Q21_1 %>% drop_na("Q21_1")
urban_Q21_1$Risk_class<- ifelse(grepl("1", urban_Q21_1$Q21_1), "1", 
                               ifelse(grepl("2", urban_Q21_1$Q21_1), "1","2"))
urban_Q21_1 <- transform(urban_Q21_1, Risk_class = as.numeric(Risk_class))

mean(rural_Q21_1$Risk_class)
#n = 101, mean = 1.54
mean(urban_Q21_1$Risk_class)
#n= 478, 1.61

RvUWilcox<- wilcox.test(rural_Q21_1$Risk_class,urban_Q21_1$Risk_class)
RvUWilcox
#Means are not significantly different; W = 22639, p=0.2481


#Three-group: rural, suburban, urban (mean across risk levels)
data_2_Q18a <- cbind(data_2, matrix(data=NA, ncol=1, nrow = nrow(data_2))) 
names(data_2_Q18a)[3] <- "WTP_class"
data_2_Q18a <- data_2_Q18a %>% drop_na("Q21_1")
data_2_Q18a$WTP_class<- ifelse(grepl("0", data_2_Q18a$Q21_1), "1", 
                               ifelse(grepl("1", data_2_Q18a$Q21_1), "1", 
                                      ifelse(grepl("2", data_2_Q18a$Q21_1), "1","2")))
data_2_Q18a <- transform(data_2_Q18a, WTP_class = as.numeric(WTP_class))



kruskal.test(WTP_class~Q8, data = data_2_Q18a)
pairwise.wilcox.test(data_2_Q18a$Q21_1, data_2_Q18a$Q8,
                     p.adjust.method = "BH")
#Difference between means is not significant, p=0.1307


#Three-group: rural, suburban, urban (mean across all levels)
data_2_Q21_1 <- data_2 %>% drop_na("Q21_1")
kruskal.test(Q21_1~Q8, data = data_2_Q21_1)
pairwise.wilcox.test(data_2_Q21_1$Q21_1, data_2_Q21_1$Q8,
                     p.adjust.method = "BH")
#Rural-Sub, p=0.57; rural-urb, 0.98; sub-urb, 0.57





##########################################################################################
##########################################################################################
#Q21_2 (WTP rec; Location, Q8)

#Check ANOVA assumptions
hist(data_2$Q21_2)

res.aov <- aov(Q21_2 ~ rural_v_urban, data = data_2)
plot(res.aov, 1)
leveneTest(Q21_2 ~ rural_v_urban, data = data_2)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )


# ANOVA normality assumptions were not met

#Wilcoxon test for 2-group non-parametric analysis (average of all levels)
rural <- subset(data_2, rural_v_urban=='rural',na.action = "omit") 
rural <- rural %>% drop_na("Q21_2")
#omitted 1 NA

urban <- subset(data_2, rural_v_urban=='urban',na.action = "omit")
urban <- urban %>% drop_na("Q21_2")
#9 NA omitted

mean(rural$Q21_2, na.rm = TRUE)
#n = 105, mean = 2.99
mean(urban$Q21_2, na.rm = TRUE)
#n = 482, mean = 3.08

RvUWilcox<- wilcox.test(rural$Q21_2,urban$Q21_2)
RvUWilcox
#Mean not significantly different; W = 24139, p = 0.4438


#Wilcoxon test for 2-group non-parametric analysis (mean of two levels: < $1 or </= $1)
rural_Q21_2 <- rural[, c(14,15)] 
rural_Q21_2 <- cbind(rural_Q21_2, matrix(data=NA, ncol=1, nrow = nrow(rural_Q21_2))) 
names(rural_Q21_2)[3] <- "Risk_class"
rural_Q21_2 <- rural_Q21_2 %>% drop_na("Q21_2")
rural_Q21_2$Risk_class<- ifelse(grepl("1", rural_Q21_2$Q21_2), "1", 
                                ifelse(grepl("2", rural_Q21_2$Q21_2), "1", "2"))
rural_Q21_2 <- transform(rural_Q21_2, Risk_class = as.numeric(Risk_class))

urban_Q21_2 <- urban[, c(14,15)] 
urban_Q21_2 <- cbind(urban_Q21_2, matrix(data=NA, ncol=1, nrow = nrow(urban_Q21_2))) 
names(urban_Q21_2)[3] <- "Risk_class"
urban_Q21_2 <- urban_Q21_2 %>% drop_na("Q21_2")
urban_Q21_2$Risk_class<- ifelse(grepl("1", urban_Q21_2$Q21_2), "1", 
                                ifelse(grepl("2", urban_Q21_2$Q21_2), "1","2"))
urban_Q21_2 <- transform(urban_Q21_2, Risk_class = as.numeric(Risk_class))

mean(rural_Q21_2$Risk_class)
#n = 105, mean = 1.71
mean(urban_Q21_2$Risk_class)
#n= 478, 1.61

RvUWilcox<- wilcox.test(rural_Q21_2$Risk_class,urban_Q21_2$Risk_class)
RvUWilcox
#Means are not significantly different; W = 25268, p=0.976


#Three-group: rural, suburban, urban (mean across risk levels)
data_2_Q18a <- cbind(data_2, matrix(data=NA, ncol=1, nrow = nrow(data_2))) 
names(data_2_Q18a)[3] <- "WTP_class"
data_2_Q18a <- data_2_Q18a %>% drop_na("Q21_2")
data_2_Q18a$WTP_class<- ifelse(grepl("0", data_2_Q18a$Q21_2), "1", 
                               ifelse(grepl("1", data_2_Q18a$Q21_2), "1", 
                                      ifelse(grepl("2", data_2_Q18a$Q21_2), "1","2")))
data_2_Q18a <- transform(data_2_Q18a, WTP_class = as.numeric(WTP_class))



kruskal.test(WTP_class~Q8, data = data_2_Q18a)
pairwise.wilcox.test(data_2_Q18a$Q21_2, data_2_Q18a$Q8,
                     p.adjust.method = "BH")
#Difference between means is not significant, p=0.1307


#Three-group: rural, suburban, urban (mean across all levels)
data_2_Q21_2 <- data_2 %>% drop_na("Q21_2")
kruskal.test(Q21_2~Q8, data = data_2_Q21_2)
pairwise.wilcox.test(data_2_Q21_2$Q21_2, data_2_Q21_2$Q8,
                     p.adjust.method = "BH")
#Rural-Sub, p=0.57; rural-urb, 0.98; sub-urb, 0.57












ggboxplot(data_2, x = "Q8", y = "Q18_1", color = "Q8", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
          ylab = "Response", xlab = "Location")









#Q18_2
res.aov <- aov(Q18_2 ~ Q8, data = data)
plot(res.aov, 1)
leveneTest(Q18_2 ~ Q8, data = data)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

# ANOVA normality assumptions were not met; Kruskal-Wallis for 3-group non-parametric analysis
kruskal.test(Q18_2~Q8, data = data)
pairwise.wilcox.test(data$Q18_2,data$Q8,
                     p.adjust.method = "BH")

ggboxplot(data, x = "Q8", y = "Q18_1", color = "Q8", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
          ylab = "Response", xlab = "Location")




#Q18_2
res.aov <- aov(Q18_2 ~ Q8, data = data)
plot(res.aov, 1)
leveneTest(Q18_2 ~ Q8, data = data)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

# ANOVA normality assumptions were not met; Kruskal-Wallis for 3-group non-parametric analysis
kruskal.test(Q18_2~Q8, data = data)
pairwise.wilcox.test(data$Q18_2,data$Q8,
                     p.adjust.method = "BH")

ggboxplot(data, x = "Q8", y = "Q18_1", color = "Q8", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
          ylab = "Response", xlab = "Location")

#############################################################################################################










Q18_1_Q2 <-xtabs(~Q2 + Q18_1, data = data)
Q18_1_Q5 <- xtabs(~Q5 + Q18_1, data = data)
Q18_1_Q8 <- xtabs(~Q8 + Q18_1, data = data)
Q18_1_Q20 <- xtabs(~Q20 + Q18_1, data = data)


vtree(data, c("Q18_1", "Q2"), horiz = FALSE)
vtree(data, c("Q18_1", "Q5"), horiz = FALSE)
vtree(data, c("Q18_1", "Q8"), horiz = FALSE)
vtree(data, c("Q18_1", "Q20"), horiz = FALSE)


vtree(data, c("Q18_2", "Q8"), horiz = FALSE)





WTP_house_urb_c <- 

WTP_house_urb_p <- 

#Counts and percentages of age (Q2) for WTP-recreation (Q18_1)
WTP_rec_urb_c <- tabyl(data, Q2, Q18_1)

WTP_rec_urb_p <- tabyl(data, Q2, Q18_1)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)


urban <- my_q[,c(7, 10, 11)]
sum_urban <- urban %>%
  group_by(Q8) 



q18_aff_ag
q18_age
q18_edu <- 

