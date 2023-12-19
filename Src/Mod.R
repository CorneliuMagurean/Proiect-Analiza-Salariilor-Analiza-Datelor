# Instalarea pachetelor necesare
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("rsample")
install.packages("lattice")
install.packages("ROCR")



# Încărcarea pachetelor
library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(lattice)
library(pROC)



# Citirea datelor
salaries <- read.csv("/Users/cornel/Univer/Analiza datelor/Proiect/salaries.csv")

# Verificăm primele rânduri din setul de date
head(salaries)

# Împărțirea datelor
set.seed(123)
split <- initial_split(salaries, prop = 0.7)
salaries_train <- training(split)
salaries_test <- testing(split)

### Part 1 ###

# Model de regresie liniară simplă
model1 <- lm(salary_in_usd ~ experience_level, data = salaries_train)
summary(model1)

# Extrage rezumatul modelului cv_model1
summary(cv_model1)

# Pentru a obține RMSE și R-squared pentru cv_model1
cv_model1$results


# Model de regresie liniară multiplă
model2 <- lm(salary_in_usd ~ experience_level + remote_ratio + work_year, data = salaries_train)
summary(model2)

# Evaluarea modelului
set.seed(123)
cv_model1 <- train(
  salary_in_usd ~ experience_level,
  data = salaries_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

cv_model2 <- train(
  salary_in_usd ~ experience_level + remote_ratio + work_year,
  data = salaries_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)
#Pentru model 1
#Grafic al Reziduurilor:

ggplot(data = salaries_train, aes(x = model1$fitted.values, y = model1$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Valori Prezise") +
  ylab("Reziduuri") +
  ggtitle("Grafic al Reziduurilor")


#Grafic QQ al Reziduurilor
qqnorm(model1$residuals)
qqline(model1$residuals)

#Grafic al Valorilor Prezise vs. Observate
ggplot(data = salaries_train, aes(x = model1$fitted.values, y = salary_in_usd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("Valori Prezise") +
  ylab("Salariu Observat") +
  ggtitle("Valori Prezise vs. Observate")

#Pentru model 2

# Rezumatul modelului 2
summary(model2)

# Rezultatele cross-validation pentru modelul 2
cv_model2$results

#Grafic al Reziduurilor
ggplot(data = salaries_train, aes(x = model2$fitted.values, y = model2$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Valori Prezise") +
  ylab("Reziduuri") +
  ggtitle("Grafic al Reziduurilor pentru Modelul 2")

#Grafic QQ al Reziduurilor
qqnorm(model2$residuals)
qqline(model2$residuals)

#Grafic al Valorilor Prezise vs. Observate
ggplot(data = salaries_train, aes(x = model2$fitted.values, y = salary_in_usd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("Valori Prezise") +
  ylab("Salariu Observat") +
  ggtitle("Valori Prezise vs. Observate pentru Modelul 2")

### Part 2 ###

# Citirea și pregătirea datelor

salaries <- read.csv("/Users/cornel/Univer/Analiza datelor/Proiect/salaries.csv")
salaries$high_salary <- ifelse(salaries$salary_in_usd > 100000, "High", "Low")
salaries$high_salary <- as.factor(salaries$high_salary) # Convertim în factor


# Împărțirea în seturi de antrenament și testare
set.seed(123)
split <- initial_split(salaries, prop = 0.7)
salaries_train <- training(split)
salaries_test <- testing(split)

# Construirea modelului de regresie logistică
model_logistic1 <- glm(high_salary ~ work_year + company_size, 
                      data = salaries_train, family = binomial())

# Cross-validation pentru modelul logistic
cv_model1 <- train(
  salary_in_usd ~ experience_level,
  data = salaries_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10),
  metric = "RMSE"  
)


# Rezultatele cross-validation
cv_model_logistic1$results


### Model Logistic 2
model_logistic2 <- glm(high_salary ~ work_year + company_size, 
                       data = salaries_train, family = binomial())

# Rezumatul modelului
summary(model_logistic2)

# Cross-validation pentru modelul logistic 2
cv_model2 <- train(
  salary_in_usd ~ experience_level + remote_ratio + work_year,
  data = salaries_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10),
  metric = "RMSE"  # Specifică metrica corectă pentru regresie
)

# Rezultatele cross-validation
cv_model_logistic2$results



# Curba ROC pentru Modelul Logistic 2
probabilitati2 <- predict(cv_model_logistic2, salaries_test, type = "prob")
roc_curve2 <- roc(response = salaries_test$high_salary, predictor = probabilitati2$Yes)
plot(roc_curve2, main = "Curba ROC pentru Modelul Logistic 2")


### Test

# Obțineți probabilitățile prezise pentru modelul logistic 1
probabilitati_model_logistic1 <- predict(model_logistic1, newdata = salaries_test, type = "response")

# Obțineți probabilitățile prezise pentru modelul logistic 2
probabilitati_model_logistic2 <- predict(model_logistic2, newdata = salaries_test, type = "response")


# Curba ROC pentru modelul logistic 1
plot(roc_curve_model_logistic1, main = "Curba ROC pentru Modelul Logistic 1")

# Curba ROC pentru modelul logistic 2
plot(roc_curve_model_logistic2, main = "Curba ROC pentru Modelul Logistic 2")
