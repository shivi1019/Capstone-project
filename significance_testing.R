
# loading library 
library(dplyr)

setwd("~/Documents/UW MSBA Course work/Capstone/")

# reading data 
data <- read.csv("FinalAdsv1.csv")

# overall 

# Multitasking_Meeting_hours_External 
df <- data[,c("Account","Month","Location","Multitasking_Meeting_hours_External","X..change")]

df_agg <- df %>% group_by(Account, Month, Location) %>% summarise( avg_multitasking_meeting_hrs_external = 
                                                             mean(Multitasking_Meeting_hours_External),
                                                           rev_change = round(mean(X..change, na.rm = T),3) * 100)


# revenue change will repeat for few onshore offshore location because multiple sellers one account 
# but i assumed it will still give out the difference by equally attributing  

ondf <- df_agg %>% filter(Location == "Onshore")
ofdf <- df_agg %>% filter(Location == "Offshore")

# testing correlation 
# onshore 
cor(ondf$avg_multitasking_meeting_hrs_external, ondf$rev_change)
# correlation value too close to 0 not significant 
# offshore 
cor(ofdf$avg_multitasking_meeting_hrs_external, ofdf$rev_change)
# correlation value too close to 0 not significant 

# regression 
summary(lm(rev_change~avg_multitasking_meeting_hrs_external, data = ondf))

# p-value more than 0.05 hence not significant 

# check for core

# filter for core 


# for categorical variables filter the data sets and do tests - annova 














## ploting industy vs revenue 
install.packages("hrbrthemes")

library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

rev_data = read_excel("UWCapstone_Revenue_4.0.xlsx")

t <- rev_data %>% group_by(Industry,Month) %>% summarise( rev = sum(`% change`), 
                                                          acc_cnt = n())

t <-  t %>% filter(Industry != "NA")

p <- ggplot(t) + geom_line(aes(x = Month, y = rev), color = "dark blue") + facet_wrap( ~Industry , nrow = 6, ncol = 6) + theme_ipsum()


q <- ggplot(t) + geom_line(aes(x = Month, y = acc_cnt), color = "dark red") + facet_wrap( ~Industry , nrow = 6, ncol = 6) + theme_ipsum()

q

