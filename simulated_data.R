### Example taken from
### Moore, Dirk. Applied Survival Analysis Using R. 2016
### Exampled expanded to compare expected event time
### Between Cox, OLS, and Poisson

library(survival)
library(tidyverse)
library(Metrics)

### Simulate some survival data (time to event)

set.seed(1)

nobs <- 3*60

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

### Cox Model

cox_mod <- coxph(Surv(time, status) ~ age + category, data=df)

summary(cox_mod)


## OLS Model

lm_mod <- lm(time ~ age + category, data=df)

summary(lm_mod)

## Poisson Model

pois_mod <- glm(time ~ age + category, data=df, family=poisson)

summary(pois_mod)

## Compare Time to Event

cox_surv_curves <- summary(survfit(cox_mod, newdata=df))

tte_cox <- cox_surv_curves$table[,"rmean"]
tte_ols <- predict(lm_mod)
tte_pois <- predict(pois_mod, type="response")
tte_actuals <- df$time

df_plot <- data.frame(tte_actuals=tte_actuals,
                      tte_cox=tte_cox,
                      tte_ols=tte_ols,
                      tte_pois=tte_pois)

df_plot_longer <- pivot_longer(df_plot, -tte_actuals, values_to="predicted",
                               names_to="model")

ggplot(df_plot_longer, aes(x=tte_actuals, y=predicted, color=model)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

## Compare MSE

lapply(list(tte_cox=tte_cox, tte_ols=tte_ols, tte_pois=tte_pois), function(x) {
  Metrics::mse(actual=tte_actuals, predicted=x)
})
