#library(stargazer)
library(plm)

#data <- read.csv("Users\PcLaptop\Documents\GitHub\APlanet\Pilot_2\Rearrange_datasets\Part2_long_format.csv")
form <- conflicts ~  TA + PA + DL #+ sum_disp #+ population_density

fe <- plm(form, 
              data = data, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")
 

fixefs <- fixef(fe, "twoways")
fitted_by_hand <- fixefs + fe$coefficients["TA"] * fe$model$TA +fe$coefficients["PA"] * fe$model$PA + fe$coefficients["DL"] * fe$model$DL

try <- plm(conflicts ~  as.numeric(fitted_by_hand), 
              data = data, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

yhat <- as.numeric(fe$model[ , 1] - fe$residuals) # reference
yhat1 <- as.numeric(predict(fe)) # fitted values
pred_beta <- as.numeric(tcrossprod(coef(fe), as.matrix(fe$model[ , -1])))
pred_effs <- as.numeric(fixef(fe, "twoways")) # sum of ind and time effects

# ok they are all the same
plot(data_d2$conflicts, yhat, asp = 1)
all.equal(yhat1, yhat)

stargazer(fe, type = "text")

# coefficients
# fe_all <- data.frame(coef(fe), coef(fe_lag1), coef(fe_lag2), coef(fe_lag3), coef(fe_lag4), coef(fe_lag5), coef(fe_lag6))
# write.csv(fe_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_all.csv")

# # r squared
# fe_r_all <- data.frame(summary(fe)$r.squared, summary(fe_lag1)$r.squared, summary(fe_lag2)$r.squared, summary(fe_lag3)$r.squared, summary(fe_lag4)$r.squared, summary(fe_lag5)$r.squared, summary(fe_lag6)$r.squared)
# write.csv(fe_r_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_r_all.csv")

# # p values
# fe_p_all <- data.frame(summary(fe)$coef[,4], summary(fe_lag1)$coef[,4], summary(fe_lag2)$coef[,4], summary(fe_lag3)$coef[,4], summary(fe_lag4)$coef[,4], summary(fe_lag5)$coef[,4], summary(fe_lag6)$coef[,4])
# write.csv(fe_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_p_all.csv")
#plot(as.numeric(fe$model[ , 1]), as.numeric(fe$model[ , 1] - residuals(fe)), asp = 1)
