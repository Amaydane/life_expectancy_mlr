library('e1071')
library('corrplot')
library('DescTools')
library('car')
library('forecast')
library('caTools')
library('lmtest')
library('nortest')
library('leaps')
library(tseries)

########################################################
tmp <- subset(na.omit(Life_Expectancy_Data), Year == 2014)
tmp
tmp <- tmp[c('Life expectancy', 'Alcohol', 'Total expenditure', 'Hepatitis B', 'BMI', 'Polio', 'HIV/AIDS', 'GDP', 'thinness 5-9 years')]
tmp

for(name in colnames(tmp)){
    print(name)
    print(mean(tmp[[name]]))
    print(median(tmp[[name]]))
    print(var(tmp[[name]]))
    print(skewness(tmp[[name]]))
    print(kurtosis(tmp[[name]]))
    print(summary(tmp[[name]]))
    print(mean(tmp[[name]])/sd(mean(tmp[[name]]))) * 100
}

tmp_corr_matrix <- cor(tmp)
corrplot(tmp_corr_matrix, method = 'pie')

# Multiple Linear Regression Model
set.seed(123)
split <- sample.split(tmp$`Life expectancy`, SplitRatio = 0.8)
tmp_training_set <- subset(tmp, split == TRUE)
tmp_test_set <- subset(tmp, split == FALSE)
tmp_training_set

tmp_mlr <- lm(`Life expectancy` ~ ., tmp_training_set)
tmp_mlr$coefficients
# Basic life expectancy is 62.41 years, economic factors and immunization have positive impact, thinness of children and hiv/aids negative
# results for alcohol and bmi are quite unexpectable 

summary(tmp_mlr)
# At the significance level 0.05 coefficients Total expenditure, vaccinations against Hep.B, Polio, thinness 5-9 years are not statistical significance.
# SE of regression is equal to 4.661

confint(tmp_mlr, level = 0.95)
#                         2.5 %        97.5 %
# (Intercept)           5.672981e+01 68.0951202299
# Alcohol               2.544536e-01  0.8350037351
# `Total expenditure`  -1.225267e-01  0.6919756200 <- Another one evidence of insignificance of this coef.(different signs)
# `Hepatitis B`        -4.994408e-02  0.0499735943 <- Another one evidence of insignificance of this coef.(different signs)
# BMI                   1.307260e-02  0.1299259198
# Polio                -8.804888e-03  0.1112626255 
# `HIV/AIDS`           -3.422764e+00 -1.8462441462
# GDP                   6.942272e-06  0.0001513993
# `thinness 5-9 years` -4.215730e-01  0.0641457691 <- Another one evidence of insignificance of this coef.(different signs)

# R-squared:  0.7083,	Adjusted R-squared:  0.6837 - not bad. 
# Difference between them consist in the fact that R^2_{adj} adapted for increase of number of independent variables(R^2 only increase).
# F-statistic: 28.83 on 8 and 95 DF,  p-value: < 2.2e-16 => regression equation is statistical significant

vif(tmp_mlr) # all vif_j < 2 => there is no significant dependence between factors

for(name in colnames(tmp)){
  if(name != 'Life expectancy'){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.38 with 'HIV/AIDS'
adj_model <- lm(`Life expectancy` ~ `HIV/AIDS`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']]+ tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.53 with + 'Alcohol'
adj_model <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS', 'Alcohol')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']] + tmp[['Alcohol']] + tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.60 with + 'BMI'
adj_model <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol` + `BMI`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS', 'Alcohol', 'BMI')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']] + tmp[['Alcohol']] + tmp[['BMI']] + tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.63 with + 'GDP'
adj_model <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol` + `BMI` + `GDP`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS', 'Alcohol', 'BMI', 'GDP')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']] + tmp[['Alcohol']] + tmp[['BMI']] + tmp[['GDP']] + tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.65 with + 'Total expenditure'
adj_model <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol` + `BMI` + `GDP` + `Total expenditure`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS', 'Alcohol', 'BMI', 'GDP', 'Total expenditure')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']] + tmp[['Alcohol']] + tmp[['BMI']] + tmp[['GDP']] + tmp[['Total expenditure']] + tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.66 with + 'Polio'

adj_model <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol` + `BMI` + `GDP` + `Total expenditure` + `Polio`, data = tmp)
model_labels <- c('Life expectancy', 'HIV/AIDS', 'Alcohol', 'BMI', 'GDP', 'Total expenditure', 'Polio')
for(name in colnames(tmp)){
  if(!name %in% model_labels){
    step_model <- lm(tmp[['Life expectancy']] ~ tmp[['HIV/AIDS']] + tmp[['Alcohol']] + tmp[['BMI']] + tmp[['GDP']] + tmp[['Total expenditure']] + tmp[['Polio']] + tmp[[name]])
    print(name)
    print(summary(step_model)$adj.r.squared)
  }
}
# max(R_{adj}^2) = 0.667 with + 'thinness 5-9 years' => there is no sign. improve from previous one

summary(adj_model)
# All coefs and equation are statistical significant at level 0.05. Interpretation of the coefs is the same.

plot(adj_model$residuals)
checkresiduals(adj_model) # Graphically looks like there are some outliers

bptest(adj_model, studentize = F) # p-value = 6.794e-06 => heteroskedasticity of residuals

dwtest(adj_model, alternative = "two.sided") # p-value = 0.2915 => absence of autocorrelation of residues
bgtest(model, order = 1) # p-value = 0.9399 => absence of autocorrelation of residues

lillie.test(adj_model$residuals) # p-value = 0.1081 => residuals are normally distributed
t.test(adj_model$residuals, mu = 0, alternative = "two.sided") # same result

hist(adj_model$residuals)
plot(adj_model)
resettest(adj_model)

# Delete residuals and Total expenditure from variables

tmp_without_outliers <- tmp[-c(4, 99, 108, 126),]
model_without_total_expenditure <- lm(`Life expectancy` ~ `HIV/AIDS` + `Alcohol` + `BMI` + `GDP` + `Polio`, data = tmp_without_outliers)

summary(model_without_total_expenditure)
# All coefs and equation are statistical significant at level 0.05. Interpretation of the coefs is the same.

plot(model_without_total_expenditure$residuals)
checkresiduals(model_without_total_expenditure) # Graphically looks like same variance

bptest(model_without_total_expenditure, studentize = F) # p-value = 0.5392 => heteroskedasticity of residuals

dwtest(model_without_total_expenditure, alternative = "two.sided") # p-value = 0.07353 => absence of autocorrelation of residues
bgtest(model_without_total_expenditure, order = 1) # p-value = 0.05439 => same result

lillie.test(model_without_total_expenditure$residuals) # p-value = 0.5838 => residuals are normally distributed
t.test(model_without_total_expenditure$residuals, mu = 0, alternative = "two.sided") # same result

means <- list("Alcohol" = 2.950394,"Hepatitis B"=82.1811,"BMI"=41.27717,"Polio"=83.81102,"HIV/AIDS"=0.8125984,"GDP"=7460.606,"thinness 5-9 years"=4.893701)
means_10 <- list("Alcohol" = 3.245433,"Hepatitis B"=90.39921,"BMI"=45.40489,"Polio"=92.19212,"HIV/AIDS"=0.8938582,"GDP"=8206.667,"thinness 5-9 years"=5.383071)
means_20 <- list("Alcohol" = 3.540473,"Hepatitis B"=98.61732,"BMI"=49.5326,"Polio"=100,"HIV/AIDS"=0.9751181,"GDP"=8952.727,"thinness 5-9 years"=5.872441)

point_forecasts_10 <- predict(model_without_total_expenditure, newdata = means_10)
point_forecasts_10 # 71.77649
point_forecasts_20 <- predict(model_without_total_expenditure, newdata = means_20)
point_forecasts_20 # 72.87664

intervals_forecasts_10 <- predict(model_without_total_expenditure, newdata = means_10, interval = "prediction", level = 0.95)
intervals_forecasts_10
#       fit      lwr      upr
# 1 71.77649 63.28694 80.26605
intervals_forecasts_20 <- predict(model_without_total_expenditure, newdata = means_20, interval = "prediction", level = 0.95)
intervals_forecasts_20
#       fit     lwr      upr
# 1 72.87664 64.3636 81.38967