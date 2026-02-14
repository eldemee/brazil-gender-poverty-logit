library(readxl)
library(tidyverse)
library(car)
library(msm)
library(sandwich)
library(stargazer)

df <- read_xlsx('data/poverty_brazil.xlsx')

# 1 - female 0 - male
# 1 - was poor in 2020 interview 0 - not poor in 2020 interview

#EDUCATION
#1 - No education and less than 1 year of study 
#2 - Incomplete elementary or equivalent 
#3 - Complete fundamental or equivalent 
#4 - Incomplete audio or equivalent 
#5 - Complete audio or equivalent 
#6 - Incomplete higher or equivalent 
#7 - Superior complete
df <- df %>% arrange(education)
df <- df%>% mutate(education = as.factor(education),
                   work = as.factor(work),
                   work_permit = as.factor(work_permit))
df <- df %>% mutate('Level of Education' = factor(case_when(
  education ==1 ~ '1-No education and less than 1 year of study', 
  education ==2 ~ '2-Incomplete elementary or equivalent', 
  education ==3 ~ '3-Complete fundamental or equivalent', 
  education ==4 ~ '4-Incomplete audio or equivalent', 
  education ==5 ~ '5-Complete audio or equivalent', 
  education ==6 ~ '6-Incomplete higher or equivalent', 
  education ==7 ~ '7-Superior complete')))

attach(df)
df$Sex <- ifelse(df$woman == '1', 'Women', 'Men')
ggplot(df, aes(Sex, fill = `Level of Education`)) +
  geom_bar(position = 'dodge') +
  ggtitle('Level of Education by Gender') +
  theme_classic()

m1 <- glm(poverty ~ woman + education + non_white + urban + metropolitan_area + work_permit + age + work, family = 'binomial')
m2 <- glm(poverty ~ woman*education + non_white + urban + metropolitan_area + work_permit + age + work, family = 'binomial')

names(m1$coefficients) <- c(
  '(Intercept)',
  'Woman = 1',
  'Incomplete elementary or equivalent',
  'Complete fundamental or equivalent',
  'Incomplete high school or equivalent',
  'Complete high school or equivalent', 
  'Incomplete higher or equivalent',
  'Superior complete',
  'omit',
  'omit', #Urban
  'omit',
  'omit',
  'omit',
  'omit', #Age
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit'
)

names(m2$coefficients) <- c(
  '(Intercept)',
  'Woman = 1',
  'Incomplete elementary or equivalent',
  'Complete fundamental or equivalent',
  'Incomplete high school or equivalent',
  'Complete high school or equivalent', 
  'Incomplete higher or equivalent',
  'Superior complete',
  'omit',
  'omit', #Urban
  'omit',
  'omit',
  'omit',
  'omit', #Age
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'omit',
  'Incomplete elementary or equivalent x Woman',
  'Complete fundamental or equivalent x Woman',
  'Incomplete high school or equivalent x Woman',
  'Complete high school or equivalent x Woman', 
  'Incomplete higher or equivalent x Woman',
  'Superior complete x Woman'
)
cov_m1 <- vcovHC(m1, type = 'HC1')
se_m1 <- sqrt(diag(cov_m1))
cov_m2 <- vcovHC(m2, type = 'HC1')
se_m2 <- sqrt(diag(cov_m2))

or_ci <- function(model, vcov_mat) {
  beta <- coef(model)
  se   <- sqrt(diag(vcov_mat))
  
  OR <- exp(beta)
  LL <- exp(beta - 1.96 * se)
  UL <- exp(beta + 1.96 * se)
  
  list(OR = OR, LL = LL, UL = UL)
}

or_m1 <- or_ci(m1, cov_m1)
or_m2 <- or_ci(m2, cov_m2)

ci_m1 <- cbind(or_m1$LL, or_m1$UL)
ci_m2 <- cbind(or_m2$LL, or_m2$UL)


stargazer(
  m1, m2,
  title = 'Gender and Education Determinants of Poverty',
  type = 'html',
  out = 'regression.html',
  column.labels = c('Baseline Model', 'Education x Woman'),
  apply.coef = exp,
  dep.var.labels = 'Poverty (1 = was poor in 2020 interview)',
  t.auto = F,
  p.auto = T,
  se = list(se_m1, se_m2),
  omit = c('omit', '(Intercept)', 'Constant'),
  digits = 3,
  no.space = TRUE,
  notes = 'The estimates are all odds ratios (ORs). An OR > 1 indicates a higher probability of being poor, while an OR < 1 indicates a lower probability. Control variables were omitted to simplify the table. Education reference group = No education and less than 1 year of study. Data source: Determinants of Poverty in Brazil (Kaggle).',
  notes.align = "l",
  ci.custom = list(ci_m1, ci_m2)
)

stargazer(
  m2,
  title = 'Gender and Education Determinants of Poverty',
  type = 'html',
  out = 'model2.html',
  column.labels = 'Interaction effects model',
  apply.coef = exp,
  dep.var.labels = 'Poverty (1 = was poor in 2020 interview)',
  t.auto = F,
  p.auto = T,
  se = se_m2,
  omit = c('omit', '(Intercept)', 'Constant'),
  digits = 3,
  no.space = TRUE,
  notes = 'The estimates are all odds ratios (ORs). An OR > 1 indicates a higher probability of being poor, while an OR < 1 indicates a lower probability. Control variables were omitted to simplify the table. Education reference group = No education and less than 1 year of study. Data source: Determinants of Poverty in Brazil (Kaggle).',
  notes.align = "l",
  ci.custom = list(ci_m2)
)

mS <- glm(poverty ~ factor(woman) + education + non_white + urban + metropolitan_area + work_permit + age, family = 'binomial')
newdata <- with(df, data.frame(age = mean(age), education = factor(rep(1:7,2), levels = 1:7), woman = factor(rep(0:1, each = 7)), urban = 1, non_white = 1, metropolitan_area = 1, work_permit = factor(1, levels = 0:2)))
pred <- predict(mS, newdata, type = 'link', se.fit = T)
newdata <- cbind(newdata, pred)

newdata <- newdata %>% mutate(Prob = plogis(fit),
                                LL = plogis(fit - qnorm(0.975)*se.fit),
                                UL = plogis(fit + qnorm(0.975)*se.fit))




ggplot(newdata, aes(education, Prob, group = woman)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = woman), alpha = 0.2) +
  geom_line(aes(color = woman), size = 1) +
  coord_cartesian(ylim = c(0.05, 0.5)) +
  scale_color_manual(values = c("0"="orange","Women"="blue")) +
  scale_fill_manual(values = c("Men"="darkorange","Women"="darkblue")) +
  labs(
    x = "Education level",
    y = "Predicted probability of poverty",
    color = "Gender",
    fill  = "Gender"
  ) +
  ggtitle("Predicted Probability of Poverty by Education and Gender") +
  theme_classic()
  




