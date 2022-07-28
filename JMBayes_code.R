

## JMBayes
## ref: https://www.drizopoulos.com/vignettes/dynamic_predictions

library("JMbayes")
MixedModelFit <- mvglmer(list(prothrombin ~ year * sex + (year | id)), data = pbc2,
                         families = list(gaussian),
                         control=list(n.iter=1000,n.burnin=500,n.thin=1,
                                      n.chains=1))
pbc2.id$event <- as.numeric(pbc2.id$status != "alive")
CoxFit <- coxph(Surv(years, event) ~ drug + age, data = pbc2.id, model = TRUE)
JMFit <- mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "year")

ND <- pbc2[pbc2$id == 81, ]
sprobs <- survfitJM(JMFit, ND)
sprobs
plot(sprobs, split = c(2, 1), surv_in_all = TRUE, 
     lty_lines_CI = 3, col_lines = "blue", col_fill_CI = "pink2", 
     main = "Patient 81", ylab = c("Prothro", "Hepa"),
     col_points = "red", pch_points = 16, 
     cex_xlab = 0.8, cex_ylab = 0.8, cex_zlab = 0.8, 
     cex_main = 0.8, cex_axis = 0.7)

N <- nrow(ND)
dyn_sprobs <- vector("list", N)
for (i in seq_len(N)) {
  dyn_sprobs[[i]] <- survfitJM(JMFit, ND[1:i, ], 
                               survTimes = seq(0, 14, length.out = 85))
  plot(dyn_sprobs[[i]], split = c(2, 1), surv_in_all = TRUE)
}


pbc2$event <- as.numeric(pbc2$status != "alive")
roc <- rocJM(JMFit, newdata = pbc2, Tstart = 5, Dt = 3)
roc
plot(roc)
aucJM(JMFit, newdata = pbc2, Tstart = 5, Dt = 3)
prederrJM(JMFit, newdata = pbc2, Tstart = 5, Thoriz = 8)


## for AUS data 

f_long <- egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
  age*gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
  cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
  hlamismatchesdr + tr_yr*age + (monthcode_yr | id)

datLong_aus <- datLong[datLong$transplantcentrestate!=8,]
datSurv_aus <- datSurv[datSurv$transplantcentrestate!=8,]

## remove all attributes
datLong_aus[] <- lapply(datLong_aus, function(x) { attributes(x) <- NULL; x })
datSurv_aus[] <- lapply(datSurv_aus, function(x) { attributes(x) <- NULL; x })

## convert all cha into fator
datLong_aus$id <- as.factor(datLong_aus$id)
datSurv_aus$id <- as.factor(datSurv_aus$id)

#datLong_aus[sapply(datLong_aus, is.character)] <- lapply(datLong_aus[sapply(datLong_aus, is.character)], as.factor)
#datSurv_aus[sapply(datSurv_aus, is.character)] <- lapply(datSurv_aus[sapply(datSurv_aus, is.character)], as.factor)

library("JMbayes")
MixedModelFit <- mvglmer(list(egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
                                age*gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
                                cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
                                hlamismatchesdr + tr_yr*age + (monthcode_yr | id)), 
                         data = datLong_aus, families = list(gaussian),
                         control=list(n.iter=1000,n.burnin=500,n.thin=1,n.chains=1))
CoxFit <- coxph(Surv(ts_firstgfloss, transplantstatus) ~ donorage + 
                  donorgendercode + donor_deathcategorycode + gendercode + bmi + smokingcode + racialorigincode +
                  coronaryarterycode + cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + 
                  timeondialysis + hlamismatchesdr + tr_yr, 
                data = datSurv_aus, model = TRUE)
JMFit <- mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "monthcode_yr")

save.image(file="JMbayes_output_aus.RData")


## for NZ data 

f_long <- egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
  age*gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
  cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
  hlamismatchesdr + tr_yr*age + (monthcode_yr | id)

datLong_nz <- datLong[datLong$transplantcentrestate==8,]
datSurv_nz <- datSurv[datSurv$transplantcentrestate==8,]

## convert all cha into fator
datLong_nz[sapply(datLong_nz, is.character)] <- lapply(datLong_nz[sapply(datLong_nz, is.character)], as.factor)
datSurv_nz[sapply(datSurv_nz, is.character)] <- lapply(datSurv_nz[sapply(datSurv_nz, is.character)], as.factor)
## remove all attributes
datLong_nz[] <- lapply(datLong_nz, function(x) { attributes(x) <- NULL; x })
datSurv_nz[] <- lapply(datSurv_nz, function(x) { attributes(x) <- NULL; x })


library("JMbayes")
MixedModelFit <- mvglmer(list(egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
                                age*gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
                                cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
                                hlamismatchesdr + tr_yr*age + (monthcode_yr | id)), 
                         data = datLong_nz, families = list(gaussian),
                         control=list(n.iter=1000,n.burnin=500,n.thin=1,n.chains=1))
CoxFit <- coxph(Surv(ts_firstgfloss, transplantstatus) ~ donorage + 
                  donorgendercode + donor_deathcategorycode + gendercode + bmi + smokingcode + racialorigincode +
                  coronaryarterycode + cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + 
                  timeondialysis + hlamismatchesdr + tr_yr, 
                data = datSurv_nz, model = TRUE)
JMFit <- mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "monthcode_yr")

#save.image(file="JMbayes_output_nz.RData")

## prediction to AUS data 

datLong_aus <- datLong[datLong$transplantcentrestate!=8,]
datSurv_aus <- datSurv[datSurv$transplantcentrestate!=8,]

## convert all cha into fator
datLong_nz[sapply(datLong_nz, is.character)] <- lapply(datLong_nz[sapply(datLong_nz, is.character)], as.factor)
datSurv_nz[sapply(datSurv_nz, is.character)] <- lapply(datSurv_nz[sapply(datSurv_nz, is.character)], as.factor)
## remove all attributes
datLong_nz[] <- lapply(datLong_nz, function(x) { attributes(x) <- NULL; x })
datSurv_nz[] <- lapply(datSurv_nz, function(x) { attributes(x) <- NULL; x })


set.seed(123)
rand_id <- sample(datSurv_nz$id,1)
ND <- datLong_aus[datLong_aus$id%in%rand_id,]

sprobs <- survfitJM(JMFit, ND)
plot(sprobs, split = c(1, 1), surv_in_all = TRUE, 
     lty_lines_CI = 3, col_lines = "blue", col_fill_CI = "pink2", 
     main = "Patient", ylab = c("eGFR"),
     col_points = "red", pch_points = 16, 
     cex_xlab = 0.8, cex_ylab = 0.8, cex_zlab = 0.8, 
     cex_main = 0.8, cex_axis = 0.7)
N <- nrow(ND)
dyn_sprobs <- vector("list", N)
for (i in seq_len(N)) {
  dyn_sprobs[[i]] <- survfitJM(JMFit, ND[1:i, ], 
                               survTimes = seq(0, 14, length.out = 85))
  plot(dyn_sprobs[[i]], split = c(1, 1), surv_in_all = TRUE)
}

roc <- rocJM(JMFit, newdata = datLong_nz, Tstart = 0, Dt = 5) # Tstart=0, to 10, Dt = max 10 years
roc
plot(roc)
aucJM(JMFit, newdata = datLong_nz, Tstart = 0, Dt = 5)


