library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(plotly)
library(leaps)
library(MASS)
library(sjPlot)
library(rstatix)
library(ggpubr)
library(gplots)
library(multcomp)
library(sna)
library(kableExtra)
#full data
tfp_all <- read_excel("~/EDS222/EDS222Final/AgTFPInternational2020_long.xlsx", sheet = 2) |>
  clean_names()


#data dictionary 
tfp_dict <- read_excel("~/EDS222/EDS222Final/AgTFPInternational2020_long.xlsx", sheet = 1, skip = 2)

#world tfp visualization
tfp_world <- tfp_all |> 
  filter(level == 'World')

tfp_world_plot <- ggplot(data = tfp_world) + 
  geom_line(aes(x=year, y=tfp_index, group = 1)) + 
  theme_minimal() +
  labs(y = 'TFP Index', x = 'Year', 
       title = 'Global Total Factor Production Indices (1961-2020)') +
  scale_x_discrete(breaks = scales::pretty_breaks(n=10)) 
#ggplotly(tfp_world_plot)

tfp_country_p <- tfp_all |> 
  filter(level == 'Country',
         income %in% c('LI', 'MI-L', 'MI-U', 'HI')) |> 
  select(year, country_territory, tfp_index, income)
#View(tfp_country)

tfp_country_plot <- ggplot(data=tfp_country_p) +
  geom_line(aes(x=year, y=tfp_index, group=country_territory, color = income), alpha = 0.5) + 
  theme_minimal() +
  labs(y = 'TFP Index', x = 'Year', title = 'Total Factor Production Growth') +
  scale_x_discrete(breaks = scales::pretty_breaks(n=10))
#ggplotly(tfp_country_plot)

tfp_country <- tfp_all |> 
  filter(level == 'Country') |> 
  select(country_territory, income, year, tfp_index, c(13:16))

y <- tfp_country$tfp_index
x1 <- tfp_country$land_index
x2 <- tfp_country$labor_index
x3 <- tfp_country$capital_index
x4 <- tfp_country$materials_index

mod0 <- lm(y~1)
mod_upper <- lm(y~x1+x2+x3+x4)
step(mod0, scope=list(lower = mod0, upper=mod_upper))

mod <- regsubsets(cbind(x1,x2,x3),y)
summary_mod <- summary(mod)
summary_mod$which
summary_mod$rsq
summary_mod$adjr2

pairs(cbind(x1,x2,x3))

mod_upper1 <- lm(y~x1+x2+x3+x1*x2+x1*x3+x1*x3)
step(mod0, scope=list(lower = mod0, upper=mod_upper1))
model1 <- lm(y~x1+x2+x3+x2:x1)
plot(fitted(model1),residuals(model1))
abline(h=0)
qqnorm(residuals(model1))
qqline(residuals(model1))
hist(y)

hist(log(y))
model2 <- lm(log(y) ~ x1+x2+x3+x2:x1)
plot(fitted(model2),residuals(model2))
abline(h=0)
qqnorm(residuals(model2))
qqline(residuals(model2))

summary <- summary(model2)
summary
#response variable
response <- (exp(summary$coefficients[1]))
#land index
land <- (exp(summary$coefficients[2])-1)*100
#labor index 
labor <- (exp(summary$coefficients[3])-1)*100
#capital index 
capital <- (exp(summary$coefficients[4])-1)*100
#land index * capital index 
land_capital <- (exp(summary$coefficients[5])-1)*100

tab_model(model2, 
          pred.labels = c('Intercept', 'Land',
                          'Labor', 'Capital', 'Land * Capital'),
          dv.labels = 'Log Total Factor Production',
          string.ci = '95% Conf. Interval',
          string.p = 'P-value',
          title = 'Transformed Linear Model Results for TFP Regression',
          digits = 7)

tfp_income <- tfp_country |> 
  select(income, tfp_index) |>
  filter(income %in% c('LI','MI-U','HI','MI-L')) |> 
  mutate(income = as.factor(income))

ggviolin(tfp_income, x='income', y='tfp_index', color = 'income', 
         order = c('LI', 'MI-L', 'MI-U', 'HI'), 
         ylab = 'TFP Index', xlab = 'Income Class',
         draw_quantiles = 0.5, add = 'boxplot')

# income_k <- kruskal.test(tfp_index ~ income, data = tfp_income)
# table(income_k)

# income_pairs_d <- dunn_test(tfp_index ~ income, data = tfp_income, p.adjust.method = 'bonferroni')
# income_pairs_d <- income_pairs_d |> 
#   add_xy_position(x = 'income') 
# ggboxplot(tfp_income, x = 'income', y = 'tfp_index',
#           order = c('LI', 'MI-L', 'MI-U', 'HI')) +
#   stat_pvalue_manual(income_pairs_d, hide.ns = TRUE) +
#   labs(subtitle = get_test_label(income_k, detailed = TRUE),
#        caption = get_pwc_label(income_pairs_d),
#        y = 'TFP Index', x = 'Income Level') 

library(lessR)
library(tsibble)
library(feasts)
library(xts)
library(forecast)

tfp_time_li <- tfp_country |> 
  filter(income == 'LI') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_li <- xts(tfp_time_li$tfp_index_mean, tfp_time_li$date)
plot(ts_li)

fit_li <- auto.arima(ts_li)
checkresiduals(fit_li)
fit_forecast_li <- forecast(fit_li)
autoplot(fit_forecast_li)

tfp_time_mil <- tfp_country |> 
  filter(income == 'MI-L') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_mil <- xts(tfp_time_mil$tfp_index_mean, tfp_time_mil$date)
plot(ts_mil)

fit_mil <- auto.arima(ts_mil)
checkresiduals(fit_mil)
fit_forecast_mil <- forecast(fit_mil)
autoplot(fit_forecast_mil)

tfp_time_mi <- tfp_country |> 
  filter(income == 'MI-U') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_mi <- xts(tfp_time_mi$tfp_index_mean, tfp_time_mi$date)
plot(ts_mi)

fit_mi <- auto.arima(ts_mi)
checkresiduals(fit_mi)
fit_forecast_mi <- forecast(fit_mi)
autoplot(fit_forecast_mi)

tfp_time_hi <- tfp_country |> 
  filter(income == 'HI') |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts_hi <- xts(tfp_time_hi$tfp_index_mean, tfp_time_hi$date)
plot(ts_hi)

fit_hi <- auto.arima(ts_hi)
checkresiduals(fit_hi)
fit_forecast_hi <- forecast(fit_hi)
autoplot(fit_forecast_hi)

par(mfrow = c(2,2))
plot(fit_forecast_li, main = 'Low Income Class')
plot(fit_forecast_mil, main = 'Lower-Middle Income Class')
plot(fit_forecast_mi, main = 'Upper-Middle Income Class')
plot(fit_forecast_hi, main = 'High Income Class')

tfp_time <- tfp_country |> 
  mutate(date = paste0(year, '-01-01'),
         date = as.Date(date, format = '%Y-%m-%d')) |> 
  group_by(date) |> 
  summarize(tfp_index_mean = mean(tfp_index))

ts <- xts(tfp_time$tfp_index_mean, tfp_time$date)
plot(ts)

fit <- auto.arima(ts)
checkresiduals(fit)
fit_forecast <- forecast(fit)
autoplot(fit_forecast)

pred.tr <- predict(fit, n.ahead=10)
pred.tr$pred
U.tr <- pred.tr$pred + 2*pred.tr$se
L.tr <- pred.tr$pred - 2*pred.tr$se
ts.plot(ts, xlim=c(55,length(ts)+12), ylim=c(min(ts),max(ts)+20))
lines(U.tr, col='blue', lty='dashed')
lines(L.tr, col='blue', lty='dashed')
points((length(ts)+1):(length(ts)+10), pred.tr$pred, col='red')

pred.hi <- predict(fit_hi, n.ahead=10)
pred.li <- predict(fit_li, n.ahead=10)
pred.mi <- predict(fit_mi, n.ahead=10)
pred.mil <- predict(fit_mil, n.ahead=10)
year <- c(2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)
predictions <- data.frame(year, pred.li$pred, pred.mil$pred, pred.mi$pred, pred.hi$pred, pred.tr$pred) 
colnames(predictions) <- c('Year', 'Low Income', 'Lower-Middle Income', 'Upper-Middle Income', 'High Income', 'World')
predictions |> 
  knitr::kable(caption = 'Predicted TFP Indices')
