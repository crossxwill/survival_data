### Example taken from
### Moore, Dirk. Applied Survival Analysis Using R. 2016
### Example expanded to compare expected time to event
### Between Cox, OLS, and Poisson

library(survival)
library(tidyverse)
library(Metrics)

### Simulate some survival data (time to event)

set.seed(1)

nobs <- 3*3000

age <- runif(nobs, 25, 85)

loghazard <- -4.5 + 0.06*age + 
  c(rep(0, nobs/3), rep(1, nobs/3), rep(2, nobs/3)
    )

time_to_event <- round(rexp(nobs, rate=exp(loghazard)),0)

status <- rep(1, nobs) ## no censoring

df <- data.frame(time=time_to_event,
                 status=status,
                 age=age,
                 category = factor(c(rep("urban", nobs/3),
                                     rep("suburban", nobs/3),
                                     rep("rural", nobs/3)),
                                   levels=c("urban", "suburban", "rural")
                                   )
                )

rm(age, loghazard, time_to_event, status)

summary(df)

## split train-test

train <- sample(1:nrow(df), size=round(0.75*nobs))

### Cox Model

cox_mod <- coxph(Surv(time, status) ~ age + category, data=df[train,])

summary(cox_mod)


## OLS Model

lm_mod <- lm(time ~ age + category, data=df[train,])

summary(lm_mod)

## Log-OLS Model

lm_mod_log <- lm(log(time+1) ~ age + category, data=df[train,])

summary(lm_mod_log)

## Poisson Model

pois_mod <- glm(time ~ age + category, data=df[train,], family=poisson)

summary(pois_mod)

## Predict Expected Time to Event (with test set)

cox_surv_curves <- summary(survfit(cox_mod, newdata=df[-train,]))

tte_cox <- cox_surv_curves$table[,"rmean"]
tte_ols <- predict(lm_mod, newdata=df[-train,])
tte_ols_log <- exp(predict(lm_mod_log, newdata=df[-train,])) - 1
tte_pois <- predict(pois_mod, newdata=df[-train,], type="response")
tte_actuals <- df[-train, "time"]

df_plot <- data.frame(tte_actuals=tte_actuals,
                      tte_cox=tte_cox,
                      tte_ols=tte_ols,
                      tte_ols_log=tte_ols_log,
                      tte_pois=tte_pois)

df_plot_longer <- pivot_longer(df_plot, -tte_actuals, values_to="predicted",
                               names_to="model")

ggplot(df_plot_longer, aes(x=tte_actuals, y=predicted, color=model)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

## Compare MSE

lapply(list(tte_cox=tte_cox, 
            tte_ols=tte_ols, 
            tte_ols_log=tte_ols_log,
            tte_pois=tte_pois),
       function(x) {
          Metrics::mse(actual=tte_actuals, predicted=x)
})
