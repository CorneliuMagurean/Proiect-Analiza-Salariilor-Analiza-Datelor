# Loading libraries 
install.packages("moments")
install.packages("gridExtra")
install.packages("Metrics")

library(dplyr)  # data wrangling
library(ggplot2) # plotting
library(tidyverse) # cleaning data
library(moments) # calculating statistics
library(forcats)  # editing factors
library(gridExtra) # plotting
library(RColorBrewer) # plotting
library(rsample)     # data splitting       
library(Metrics)

#Loading data

salaries_data = read.csv('/Users/cornel/Univer/Analiza datelor/Proiect/ds_salaries.csv')

glimpse(salaries_data)

# Droping unused columns and creating factors

salaries_data <- salaries_data%>% 
  mutate_if(sapply(salaries_data, is.character), as.factor)%>% 
  select(-c(X,salary,salary_currency)) 

glimpse(salaries_data)


# Renaming and reordering factors 
salaries_data <- salaries_data %>% 
  mutate(experience_level = recode(experience_level,  EN = "Entry/Junior", 
                                   MI = "Mid-level", 
                                   SE = "Senior/Expert", 
                                   EX = "Executive"))%>% 
  mutate(employment_type = recode(employment_type,   PT = "Part Time", 
                                  FT = "Full Time", 
                                  CT = "Contract", 
                                  FL = "Freelance"))%>%  
  mutate(remote_ratio = recode_factor(remote_ratio,'0'= "Stationary", 
                                      '50' = "Partially remote", 
                                      '100' = "Remote")) 

glimpse(salaries_data)

#Summarizing data 

salaries_data %>% 
  summary()

bw <- 2 * IQR(salaries_data$salary_in_usd) / length(salaries_data$salary_in_usd)^(1/3)

hist_salaries <- salaries_data%>% 
  ggplot( mapping= aes(x = salary_in_usd)) +   
  geom_histogram(binwidth = bw,color = "#000000", fill = "#0099F8", alpha = 0.6) +
  theme_bw()

box_salaries <- salaries_data%>% 
  ggplot(mapping= aes(x = salary_in_usd)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_bw()

grid.arrange(hist_salaries, box_salaries, ncol = 2)

#Skewness of salary
print(paste0("Skewness of salary: " , skewness(salaries_data$salary_in_usd)))


#Plot of salary year by year
salaries_data%>% 
  group_by(work_year)%>% 
  summarize( 
    median_salary = median(salary_in_usd, na.rm = TRUE))%>% 
  ggplot(mapping = aes(x=work_year, y =median_salary ))+ 
  geom_line()+ 
  theme_bw()+ labs(y = "Salary Median (USD)", x = "Year", title = "Median of salary in Data related jobs in last three years")


# Salary median by company size
salaries_data$company_size <- factor(salaries_data$company_size, levels=c("S","M","L"))
p1 <- salaries_data %>% 
  group_by(company_size)%>% 
  summarize( 
    median_salary = median(salary_in_usd, na.rm = TRUE))%>% 
  ggplot(mapping = aes(x=company_size, y =median_salary, fill=company_size ))+ 
  geom_bar(stat = 'Identity')+     
  theme_bw()+ labs(y = "Salary Median (USD)", x = "Company Size", title = "Median of salary in Data related jobs by company size") 

p1 +  scale_fill_brewer(palette = "Pastel1")


#Salary by experience level 

p2 <- salaries_data %>% 
  group_by(experience_level)%>% 
  summarize( 
    median_salary = median(salary_in_usd, na.rm = TRUE))%>% 
  ggplot(mapping = aes(x=fct_reorder(experience_level,median_salary), y =median_salary,fill = experience_level ))+ 
  geom_bar(stat = 'Identity')+ 
  geom_hline(yintercept = median(salaries_data$salary_in_usd, na.rm=TRUE), color = 'red')+     
  theme_bw()+ labs(y = "Salary Median (USD)", x = "Experience Level", title = "Median of salary in Data related jobs by experience level")  

p2 + scale_fill_brewer(palette = "Pastel1")


#Salary by company location 
nb.cols <- 50
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(nb.cols)
p3 <-salaries_data %>% 
  group_by(company_location)%>% 
  summarize( 
    median_salary = median(salary_in_usd, na.rm = TRUE))%>% 
  ggplot(mapping = aes(x=company_location, y =median_salary, fill = company_location ))+ 
  geom_bar(stat = 'Identity')+ 
  geom_hline(yintercept = median(salaries_data$salary_in_usd, na.rm=TRUE), color = 'red')+     
  theme_bw()+ labs(y = "Salary Median (USD)", x = "Company Location", title = "Median of salary in Data related jobs by company location")
p3+    scale_fill_manual(values = mycolors)


# Number of workers in companies by size and worker experience
salaries_data$experience_level <- factor(salaries_data$experience_level, levels=c("Entry/Junior", "Mid-level", "Senior/Expert","Executive"))

p4 <- salaries_data%>% 
  group_by(experience_level, company_size)%>% 
  count()%>% 
  ggplot(mapping = aes(y = experience_level, x = n, fill = experience_level))+ 
  geom_bar(stat = 'Identity')+ 
  facet_wrap(~company_size)+ 
  theme_bw()+ labs(y = "Experience Level", x = "Number of positions", title = "Number of positions by experience level and company size")  

p4  + scale_fill_brewer(palette = "Pastel1")

#Data Modeling

salaries_model_data <- salaries_data %>%
  group_by(job_title)%>% 
  filter(n()>40) 

salaries_model_data <-salaries_model_data %>% 
  group_by(company_location)%>% 
  filter(n()>10)  
salaries_model_data <-salaries_model_data %>% 
  group_by(employee_residence)%>% 
  filter(n()>10) 
salaries_model_data <-salaries_model_data %>% 
  select(-employment_type,) 
salaries_model_data%>% 
  summary()

#

set.seed(123) 

salary_split <- initial_split(salaries_model_data, prop = .7)
salary_train <- training(salary_split)
salary_test  <- testing(salary_split) 

model1 <- lm(salary_in_usd~., salary_train) 
summary(model1)

#
predicted <- predict(model1, newdata = salary_test) 

linear_prediction <- data.frame(Predicted = predicted,Observed = salary_test$salary_in_usd) 

ggplot(linear_prediction,                                    
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 1)+ 
  theme_bw()

#
ggplot(linear_prediction, aes(x = Observed - Predicted)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Reziduuri", y = "Frecvență") +
  theme_bw()
#
ggplot(linear_prediction, aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) + 
  theme_bw() +
  labs(x = "Valori Prezise", y = "Valori Observate")
#

plot(model1)
ggplot(salary_train, aes(x = company_location, y = salary_in_usd)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Locația Companiei", y = "Salariu în USD")


print(paste0("RMSE: " , rmse(linear_prediction$Observed, linear_prediction$Predicted)))
print(paste0("Median: " ,median(linear_prediction$Observed)))

write.csv(salaries_data, "salaries_data.csv", row.names = FALSE)

