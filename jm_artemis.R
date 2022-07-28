
##
rm(list = ls())
##
## load saved data and packages
library(haven); library(dplyr); library(tidyverse); library(data.table);
library(rstanarm);
options(mc.cores = parallel::detectCores())
##
##
datSurv <- readRDS(file="datSurv_202207.rds")
datLong <- readRDS(file="datLong_202207.rds")

nrow(datSurv)
length(unique(datLong$id))
length(unique(datSurv$id))

## create transplant year variable 

datSurv$tr_yr <- as.numeric(substr(datSurv$transplantdate,1,4))
datLong$tr_yr <- as.numeric(substr(datLong$transplantdate,1,4))
## convert into 4 categories
datSurv$tr_yr <- cut(datSurv$tr_yr,4)
datLong$tr_yr <- cut(datLong$tr_yr,4)
levels(datSurv$tr_yr) <- c("(1980-1990]","(1990-2000]","(2000-2010]","(2010-2017]")
levels(datLong$tr_yr) <- c("(1980-1990]","(1990-2000]","(2000-2010]","(2010-2017]")
##

#datSurv$censored_gf <- NA
#datSurv[is.na(datSurv$graftfailuredate), "censored_gf"] <- 0
#datSurv[!is.na(datSurv$graftfailuredate), "censored_gf"] <- 1


##
## longitude formula

f_long <- egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
  age*gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
  cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
  hlamismatchesdr + tr_yr*age + (monthcode_yr | id)

##
## Australia model 

datLong_aus <- datLong[datLong$transplantcentrestate!=8,]
datSurv_aus <- datSurv[datSurv$transplantcentrestate!=8,]

mod <- stan_jm(formulaLong = f_long,
               dataLong = datLong_aus,
               formulaEvent = survival::Surv(ts_firstgfloss, transplantstatus) ~ donorage + 
                 donorgendercode + donor_deathcategorycode + gendercode + bmi + smokingcode + racialorigincode +
                 coronaryarterycode + cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + 
                 timeondialysis + hlamismatchesdr + tr_yr, 
               dataEvent = datSurv_aus,
               time_var = "monthcode_yr",
               assoc = c("etaauc"),
               chains = 1, refresh = 100, iter = 500, seed = 12345)
saveRDS(mod, "mod_AUS_20220725.rds")

saveRDS(mod, "mod_AUS_etavalue_20220728.rds")

##
## NZ model

datLong_nz <- datLong[datLong$transplantcentrestate==8,]
datSurv_nz <- datSurv[datSurv$transplantcentrestate==8,]

mod <- stan_jm(formulaLong = f_long,
               dataLong = datLong_nz,
               formulaEvent = survival::Surv(ts_firstgfloss, transplantstatus) ~ donorage + 
                 donorgendercode + donor_deathcategorycode + gendercode + bmi + smokingcode + racialorigincode +
                 coronaryarterycode + cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + 
                 timeondialysis + hlamismatchesdr + tr_yr, 
               dataEvent = datSurv_nz,
               time_var = "monthcode_yr",
               assoc = c("etaauc"),
               chains = 1, refresh = 100, iter = 500, seed = 12345)
saveRDS(mod, "mod_NZ_20220725.rds")

saveRDS(mod, "mod_NZ_etavalue_20220728.rds")

##
## All model 


## randomly subset the model based on the id - toy example

id_var <- datSurv$id
set.seed(10001)
id_sample_test <- sort(sample(x=id_var,size=round(length(id_var)*0.05),replace = FALSE))
id_sample <- id_var[!id_var%in%id_sample_test]
length(id_sample)
length(id_sample_test)

id_sample <- id_var # full model 

## Model:
f_long <- egfr_ckdepi ~ donorage + donorgendercode + donor_deathcategorycode + 
  age + gendercode + bmi + smokingcode + racialorigincode + coronaryarterycode + 
  cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + timeondialysis + 
  hlamismatchesdr + tr_yr + (monthcode_yr | id)

egFR = time + age*sex + age + sex+ weight+BMI+ DM+ time*age



# ts_firstgfloss = datSurv$transplantperiod/365

mod <- stan_jm(formulaLong = f_long,
               dataLong = datLong[datLong$id%in%id_sample,],
               formulaEvent = survival::Surv(ts_firstgfloss, transplantstatus) ~ donorage + 
                 donorgendercode + donor_deathcategorycode + gendercode + bmi + smokingcode + racialorigincode +
                 coronaryarterycode + cerebrovasularcode + diabetescode_text + primaryrenaldiseasecode + 
                 timeondialysis + hlamismatchesdr + tr_yr, 
               dataEvent = datSurv[datSurv$id%in%id_sample,],
               time_var = "monthcode_yr",
               chains = 1, refresh = 500, iter = 5000, seed = 12345)
#saveRDS(mod, "mod_full_20220702.rds")
mod <- readRDS("mod_full_20220702.rds")
##
summary(mod)
##
## Predicted individual-specific trajectory for in-sample individuals
p1 <- posterior_traj(mod, m = 1, ids = id_sample[1:3], extrapolate = TRUE)
#pp1 <- plot(p1, plot_observed = TRUE, vline = TRUE); #pp1
pp1 <- plot(p1, plot_observed = TRUE, vline = FALSE); #pp1 
# vertical dashed line at the timing of the individual’s event or censoring time
p2 <- posterior_survfit(mod, ids = id_sample[1:3])
pp2 <- plot(p2); #pp2
plot_stack_jm(yplot = list(pp1), survplot = pp2)
##
## Predicted individual-specific trajectory for in-sample individuals
p1 <- posterior_traj(mod, m = 1, ids = id_sample[1001:1003], extrapolate = TRUE)
pp1 <- plot(p1, plot_observed = TRUE, vline = FALSE); #pp1 
# vertical dashed line at the timing of the individual’s event or censoring time
p2 <- posterior_survfit(mod, ids = id_sample[1001:1003])
pp2 <- plot(p2); #pp2
plot_stack_jm(yplot = list(pp1), survplot = pp2)
##
## Predicted individual-specific trajectory for in-sample individuals
p1 <- posterior_traj(mod, m = 1, ids = id_sample[10001:10003], extrapolate = TRUE)
pp1 <- plot(p1, plot_observed = TRUE, vline = FALSE); #pp1 
# vertical dashed line at the timing of the individual’s event or censoring time
p2 <- posterior_survfit(mod, ids = id_sample[10001:10003])
pp2 <- plot(p2); #pp2
plot_stack_jm(yplot = list(pp1), survplot = pp2)
##


## dynamic prediction: 1

newdatLong <- datLong[datLong$id%in%id_var[3], , drop = FALSE]
newdatLong <- newdatLong[-1,]
newdatLong$id <- paste0("Time: ",1:nrow(newdatLong))
rows <- list(c(1),c(1:2),c(1:3),c(1:4),c(1:5),c(1:6))
xx <- NULL
for(i in 1:nrow(newdatLong)){
  tmp <- newdatLong %>% slice(rep(rows[[i]],1))
  tmp$id <- paste0("Time: ",i) 
  xx <- rbind(xx,tmp)
}
newdatLong <- xx
newdatSurv <- datSurv[datSurv$id%in%id_var[3], , drop = FALSE]
newdatSurv <- newdatSurv %>% slice(rep(1:n(), length(unique(newdatLong$id))))
newdatSurv$id <- paste0("Time: ",1:length(unique(newdatLong$id)))
tmp <- unique(newdatLong$monthcode_yr)
newdatSurv$ts_firstgfloss <- tmp
##
p1o <- posterior_traj(mod, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
#saveRDS(p1o, "dyn_pred_p1o.rds")
p1o <-  readRDS("dyn_pred_p1o.rds")
pp1o <- plot(p1o, plot_observed = TRUE, vline = TRUE)
p2o <- posterior_survfit(mod,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")
#saveRDS(p2o, "dyn_pred_p2o.rds")
p2o <-  readRDS("dyn_pred_p2o.rds")
pp2o <- plot(p2o); #pp2o
plot_stack_jm(yplot = list(pp1o), survplot = pp2o)
##

## dynamic prediction: 2

newdatLong <- datLong[datLong$id%in%id_var[10001], , drop = FALSE]
newdatLong <- newdatLong[-1,]
newdatLong$id <- paste0("Time: ",1:nrow(newdatLong))
rows <- list(c(1),c(1:2),c(1:3),c(1:4),c(1:5),c(1:6))
xx <- NULL
for(i in 1:nrow(newdatLong)){
  tmp <- newdatLong %>% slice(rep(rows[[i]],1))
  tmp$id <- paste0("Time: ",i) 
  xx <- rbind(xx,tmp)
}
newdatLong <- xx
newdatSurv <- datSurv[datSurv$id%in%id_var[10001], , drop = FALSE]
newdatSurv <- newdatSurv %>% slice(rep(1:n(), length(unique(newdatLong$id))))
newdatSurv$id <- paste0("Time: ",1:length(unique(newdatLong$id)))
tmp <- unique(newdatLong$monthcode_yr)
newdatSurv$ts_firstgfloss <- tmp
##
p1oo <- posterior_traj(mod, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
#saveRDS(p1oo, "dyn_pred_p1oo.rds")
p1oo <-  readRDS("dyn_pred_p1oo.rds")
pp1oo <- plot(p1oo, plot_observed = TRUE, vline = TRUE)
p2oo <- posterior_survfit(mod,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")
#saveRDS(p2oo, "dyn_pred_p2oo.rds")
p2oo <-  readRDS("dyn_pred_p2oo.rds")
pp2oo <- plot(p2oo); #pp2o
plot_stack_jm(yplot = list(pp1oo), survplot = pp2oo)
##



##
## AUS and NZ models
##

datLong_aus <- datLong[datLong$transplantcentrestate!=8,]
datSurv_aus <- datSurv[datSurv$transplantcentrestate!=8,]

datLong_nz <- datLong[datLong$transplantcentrestate==8,]
datSurv_nz <- datSurv[datSurv$transplantcentrestate==8,]


## load saved models

mod <- readRDS("mod_AUS_etavalue_20220728.rds")

## static prediction 

newdatLong1 <- datLong_nz[datLong_nz$id%in%rand_id, , drop = FALSE]
newdatSurv1 <- datSurv_nz[datSurv_nz$id%in%rand_id, , drop = FALSE]

newdatLong1[,c("id","egfr_ckdepi","monthcode_yr","ts_firstgfloss","transplantstatus")]
newdatSurv1[,c("id","ts_firstgfloss","transplantstatus")]

newdatSurv1[,c("ts_firstgfloss")] = 0.247
newdatLong1[,c("ts_firstgfloss")] = 0.247

p1oo <- posterior_traj(mod, m = 1,
                      newdataLong = newdatLong1,
                      newdataEvent = newdatSurv1,
                      last_time = "ts_firstgfloss")
p2oo <- posterior_survfit(mod,
                         newdataLong = newdatLong1,
                         newdataEvent = newdatSurv1,
                         last_time = "ts_firstgfloss")



## dynamic prediction

rand_id <- sample(datSurv_nz$id,1)
newdatLong <- datLong_nz[datLong_nz$id%in%rand_id, , drop = FALSE]
newdatLong <- newdatLong[-1,]
newdatLong$id <- paste0("Time: ",1:nrow(newdatLong))
rows <- list(c(1),c(1:2),c(1:3),c(1:4),c(1:5),c(1:6))
xx <- NULL
for(i in 1:nrow(newdatLong)){
  tmp <- newdatLong %>% slice(rep(rows[[i]],1))
  tmp$id <- paste0("Time: ",i) 
  xx <- rbind(xx,tmp)
}
newdatLong <- xx
newdatSurv <- datSurv_nz[datSurv_nz$id%in%rand_id, , drop = FALSE]
newdatSurv <- newdatSurv %>% slice(rep(1:n(), length(unique(newdatLong$id))))
newdatSurv$id <- paste0("Time: ",1:length(unique(newdatLong$id)))
tmp <- unique(newdatLong$monthcode_yr)
newdatSurv$ts_firstgfloss <- tmp

p1o <- posterior_traj(mod, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
p2o <- posterior_survfit(mod,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")






### old code ###


##########################################################
## define the censored variable 
datSurv$censored_gf <- NA
datSurv[is.na(datSurv$graftfailuredate), "censored_gf"] <- 0
datSurv[!is.na(datSurv$graftfailuredate), "censored_gf"] <- 1

## censored variable for death 
#datSurv$censored_d <- NA
#datSurv[is.na(datSurv$deathdate), "censored_d"] <- 0
#datSurv[!is.na(datSurv$deathdate), "censored_d"] <- 1

## drop "Unknown" from the smoking variable
aa=datSurv[datSurv$smokingcode=="Unknown","id"]
datSurv = datSurv[!datSurv$id%in%aa,]
datLong = datLong[!datLong$id%in%aa,]
datSurv$smokingcode=droplevels(datSurv$smokingcode)
datLong$smokingcode=droplevels(datLong$smokingcode)
length(unique(datLong$id))
length(unique(datSurv$id))
## merge current and former smoking into one 
levels(datSurv$smokingcode) = c("yes","yes","no")
table(datSurv$smokingcode)
levels(datLong$smokingcode) = c("yes","yes","no")
table(datLong$smokingcode)

length(unique(datLong$id))
length(unique(datSurv$id))

##

datLong <- datLong %>% distinct(egfr_ckdepi,age,gendercode,bmi,smokingcode,monthcode_yr,id)

length(unique(datLong$id))
length(unique(datSurv$id))

## randomly subset the model based on the id - toy example

id_var <- datSurv$id
set.seed(10001)
id_sample_test <- sort(sample(x=id_var,size=round(length(id_var)*0.0005),replace = FALSE))
id_sample <- id_var[!id_var%in%id_sample_test]
length(id_sample)
length(id_sample_test)

## Model 2:
f_long <- egfr_ckdepi ~ age + gendercode + bmi + smokingcode + (monthcode_yr | id)
mod <- stan_jm(formulaLong = f_long,
                    dataLong = datLong[datLong$id%in%id_sample,],
                    formulaEvent = survival::Surv(ts_firstgfloss, censored_gf) ~ age_ave + gendercode + bmi + smokingcode,
                    dataEvent = datSurv[datSurv$id%in%id_sample,],
                    time_var = "monthcode_yr",
                    chains = 1, refresh = 500, iter = 3000, seed = 12345)

##
#saveRDS(mod, "mod_gf_mod2.rds")
mod <- readRDS(file="mod_gf_mod2.rds")
##
newdatLong <- datLong[datLong$id%in%id_sample_test[1], c("id","age","gendercode","bmi","smokingcode","monthcode_yr","egfr_ckdepi")]
newdatSurv <- datSurv[datSurv$id%in%id_sample_test[1], , drop = FALSE]
p1o <- posterior_traj(mod, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
pp1o <- plot(p1o, plot_observed = TRUE, vline = TRUE)
p2o <- posterior_survfit(mod,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")
pp2o <- plot(p2o); #pp2o
plot_stack_jm(yplot = list(pp1o), survplot = pp2o)
##

## Null model:
out <- readRDS(file="mod_gf_null.rds")
p1o <- posterior_traj(out, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
pp1o <- plot(p1o, plot_observed = TRUE, vline = TRUE)
p2o <- posterior_survfit(out,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")
pp2o <- plot(p2o); #pp2o
plot_stack_jm(yplot = list(pp1o), survplot = pp2o)
##



## Model for time to graft loss
f_long <- egfr_ckdepi ~ 1 + (monthcode_yr | id)
mod_gf_null <- stan_jm(formulaLong = f_long,
                  dataLong = datLong[datLong$id%in%id_sample,],
                  formulaEvent = survival::Surv(ts_firstgfloss, censored_gf) ~ 1,
                  dataEvent = datSurv[datSurv$id%in%id_sample,],
                  time_var = "monthcode_yr",
                  chains = 1, refresh = 500, iter = 2500, seed = 12345)
##
saveRDS(mod_gf_null, "mod_gf_null.rds")

##
f_long <- egfr_ckdepi ~ age + gendercode + bmi +  donorage + donorgendercode + smokingcode + serumcreatinine + (monthcode_yr | id)
mod_gf_3 <- stan_jm(formulaLong = f_long,
                    dataLong = datLong[datLong$id%in%id_sample,],
                    formulaEvent = survival::Surv(ts_firstgfloss, censored_gf) ~ age_ave + gendercode + bmi,
                    dataEvent = datSurv[datSurv$id%in%id_sample,],
                    time_var = "monthcode_yr",
                    chains = 2, refresh = 500, iter = 2500, seed = 12345)
##
saveRDS(mod_gf_3, "mod_gf_3.rds")
##

##

out <- readRDS(file="mod_gf_null.rds")
loo_null <- loo(out)
loo_2 <- loo(womensrole_bglm_2)


## Predicted individual-specific trajectory for in-sample individuals
out2 <- readRDS(file="mod_gf_2.rds")
out <- readRDS(file="mod_gf_null.rds")
p1 <- posterior_traj(out, ids = id_sample[100], extrapolate = TRUE)
pp1 <- plot(p1, plot_observed = TRUE, vline = TRUE); #pp1 
# vertical dashed line at the timing of the individual’s event or censoring time
p2 <- posterior_survfit(out, ids = id_sample[100])
pp2 <- plot(p2); #pp2
plot_stack_jm(yplot = list(pp1), survplot = pp2)




##
## out of sample prediction - working 
## made up data
newdatLong <- datLong[datLong$id%in%id_var[1], c("id","age","gendercode","bmi","monthcode_yr","egfr_ckdepi")]

newdatSurv <- datSurv[datSurv$id%in%id_var[1], , drop = FALSE]


p1o <- posterior_traj(out, m = 1,
                      newdataLong = newdatLong,
                      newdataEvent = newdatSurv,
                      last_time = "ts_firstgfloss")
pp1o <- plot(p1o, plot_observed = TRUE, vline = TRUE)
p2o <- posterior_survfit(out,
                         newdataLong = newdatLong,
                         newdataEvent = newdatSurv,
                         last_time = "ts_firstgfloss")
pp2o <- plot(p2o); #pp2o
plot_stack_jm(yplot = list(pp1o), survplot = pp2o)




################################# old 


id_sample <- sort(sample(x=id_var,size=round(length(id_var)*0.1),replace = FALSE))
length(id_sample)

a=id_sample
b= id_sample

id_sample <- id_sample[!id_sample%in%"P7985377"]

id_sample <- c("P10000015","P10000027","P10000038",
               "P7985377","P7986953","P7988044","P7989258","P7990452","P7990531","P7991486",
               "P7992938","P7993591","P7996426","P7998404","P7999079","P8000058","P8001251", 
               "P8002419","P8002658","P8002804","P8003816","P8004220","P8005267","P8006472",
               "P8006977","P8008889","P8008990","P8010858","P8011994","P8014437","P8015460",
               "P8016596","P8017744","P8018677","P8018857","P8023335","P8025473","P8026024",
               "P8030962","P8034146","P8035463","P8036384","P8041806")
c(1:3,21:22)
