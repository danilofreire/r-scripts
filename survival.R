# How to use the survival package
# Manual: http://www.ms.uky.edu/~mai/Rsurv.pdf and 
# First,
library(survival)

# Cox Model
model <- coxph(Surv(duration, censor) ~ X1 + X2, data=Rossi)
summary(model)
plot(survfit(model))

# Weibull
model <- survreg(Surv(duration, censor) ~ invest + numst2 + crisis,
                 data = coalition, dist = "weibull")

# Kaplan-Meier
mfit <- survfit(Surv(days, status == 1) ~ sex, data = melanom)
