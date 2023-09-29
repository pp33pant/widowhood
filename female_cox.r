rm(list = ls())
library(ggplot2)
library(tidyverse)
library(survival)
library(survminer)
library(haven)
library(xgboost)
library(pROC)
library(caret)
library(BART)
library(naniar)
set.seed(0726)
expit <- function(x) {
  exp(x) / (1 + exp(x))
}
ps.barplot <- function(ps, group){
  
  data <- data.frame(ps = ps, 
                     group = group,
                     ps.strata <- floor(ps*10)+1)
  
  table <- as.data.frame(table(ps.strata, group))
  table.summarise <- table %>%
    group_by(ps.strata) %>%
    summarise(sum.freq = sum(Freq)) %>%
    ungroup()
  
  table <- left_join(table, table.summarise, by = "ps.strata") %>%
    mutate(prop = Freq/sum.freq)
  ggplot(data = table) +
    geom_bar(aes(x = ps.strata, y = prop, fill = group), position="dodge", stat="identity") +
    scale_x_discrete("Propensity", labels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
                                            "0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9","0.9-1.0"))
  
}

# attach data 
female <- read_dta("../female.dta")

female.propensity.prediction.health <- female %>%
  select(., s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
         ,s_max_hosp_visit,s_mean_bmi 
         ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
         ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
         ,s_max_psych)

cor_matrix <- cor(female.propensity.prediction.health, use = "complete.obs")
colSums(is.na(female.propensity.prediction.health))
female.propensity.prediction.health <- female %>% drop_na(., s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
          ,s_max_hosp_visit,s_mean_bmi 
          ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
          ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
          ,s_max_psych)

ns.logit.func2 <- glm(widow  ~  s_mean_drink_days + s_mean_drink_num + s_mean_hosp_time 
                      + s_max_hosp_visit + s_mean_bmi 
                      + s_max_nurs_home + s_mean_cesd + s_mean_expend + s_max_high_bp 
                      + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 
                      + s_max_psych , data = female.propensity.prediction.health, family = binomial(link = "logit"))
summary(ns.logit.func2)
ns.logit.pred2 <- predict(ns.logit.func2, type = "response")

plot(pROC::roc(response = female.propensity.prediction.health$widow,
               predictor = ns.logit.func2$fitted.values,
               levels=c(0, 1)),
     lwd=1.5) 

confusionMatrix(factor(ifelse(ns.logit.func2$fitted.values <= 0.5, 0, 1)), factor(female.propensity.prediction.health$widow))

param <- list(max_depth = 5, eta = 0.5,  nthread = 8,  eval_metric = "auc",
              lambda = 0, gamma = 0, booster = "gbtree")
ps.xgb.input.female.health <- female.propensity.prediction.health  %>% select (s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time, 
                                                                           s_max_hosp_visit,s_mean_bmi, 
                                                                           s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp, 
                                                                           s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke,
                                                                           s_max_psych) %>% as.matrix()

ps.xgb.output.female.health <- female.propensity.prediction.health$widow %>% as.vector()

ns.xgb.func2 <- xgboost(params = param, data = ps.xgb.input.female.health, label = ps.xgb.output.female.health,  objective = "binary:logistic", nrounds = 200, verbose = 0)
ns.xgb.pred2 <- predict(ns.xgb.func2, ps.xgb.input.female.health)
plot(pROC::roc(response = female.propensity.prediction.health$widow,
               predictor = ns.xgb.pred2,
               levels=c(0, 1)),
     lwd=1.5) 

confusionMatrix(factor(ifelse(ns.xgb.pred2  <= 0.5, 0, 1)), factor(female.propensity.prediction.health$widow))

# describe the distributions of the propensity scores predicted by logit and xgboost models

hist(ns.logit.pred2)
hist(ns.xgb.pred2)
summary(ns.logit.pred2)
summary(ns.xgb.pred2)

female.propensity.prediction.full <- female %>%
  drop_na(.,s_wave_age,s_eduyrs,num_shared_child,age_diff 
          ,s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
          ,s_max_hosp_visit,s_max_pension,s_mean_bmi 
          ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
          ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
          ,s_max_psych,child_liv,s_ever_retired,s_white,s_black 
          ,s_protestant,s_catholic,s_migration,s_year_since_retire)
ns.logit.func1 <- glm(widow  ~ s_wave_age+ s_eduyrs + num_shared_child + + age_diff 
                      + s_mean_drink_days + s_mean_drink_num + s_mean_hosp_time 
                      + s_max_hosp_visit +  s_max_pension + s_mean_bmi 
                      + s_max_nurs_home + s_mean_cesd + s_mean_expend + s_max_high_bp 
                      + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 
                      + s_max_psych + child_liv + s_ever_retired + s_white + s_black 
                      + s_protestant + s_catholic + s_migration + s_year_since_retire, data = female.propensity.prediction.full, family = binomial(link = "logit"))
summary(ns.logit.func1)
ns.logit.pred1 <- predict(ns.logit.func1, type = "response")
summary(ns.logit.pred1)

ps.xgb.input.female.full <- female.propensity.prediction.full  %>% dplyr::select (s_wave_age,s_eduyrs,num_shared_child,age_diff 
                                                                       ,s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
                                                                       ,s_max_hosp_visit,s_max_pension,s_mean_bmi 
                                                                       ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
                                                                       ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
                                                                       ,s_max_psych,child_liv,s_ever_retired,s_white,s_black 
                                                                       ,s_protestant,s_catholic,s_migration,s_year_since_retire) %>% as.matrix()

ps.xgb.output.female.full <- female.propensity.prediction.full$widow %>% as.vector()

ns.xgb.func1 <- xgboost(params = param, data = ps.xgb.input.female.full, label = ps.xgb.output.female.full,  objective = "binary:logistic", nrounds = 200, verbose = 0)
ns.xgb.pred1 <- predict(ns.xgb.func1, ps.xgb.input.female.full)

female.propensity.prediction.output.full <- female.propensity.prediction.full%>% 
  cbind(ns.logit.pred1) %>%
  cbind(ns.xgb.pred1) %>%
  select(hhidpn, wave, ns.logit.pred1, ns.xgb.pred1)

female.full <- full_join(female, female.propensity.prediction.output.full, by = c("hhidpn","wave")) %>%
  arrange(hhidpn, wave)

female.full <- female.full%>%
  mutate(ps.logit = ifelse(is.na(ns.logit.pred1) == TRUE, ifelse(widow == 0, 999, ifelse(lag(widow, 1) == 1, 
                                                                                         ifelse(hhidpn == lag(hhidpn, 1), lag(ns.logit.pred1, 1), 999), 999)),
                           ns.logit.pred1)) %>%
  fill(ps.logit) %>%
  mutate(ps.xgb = ifelse(is.na(ns.xgb.pred1) == TRUE, ifelse(widow == 0, 999, ifelse(lag(widow, 1) == 1, 
                                                                                     ifelse(hhidpn == lag(hhidpn, 1), lag(ns.xgb.pred1, 1), 999), 999)),
                         ns.xgb.pred1)) %>%
  fill(ps.xgb) %>%
  replace_with_na(replace = list(ps.logit = 999, ps.xgb = 999)) %>%
  mutate(start.age = ifelse(is.na(wave_start_age), wave_end_age - 2, wave_start_age),
         stop.age = wave_end_age, 
         event = dead) 

female_trt_0 <- female.full %>% as_tibble() %>%
  filter(widow == 0) %>%
  select(-widow, -hhidpn)
  
female_trt_1 <- female.full %>% as_tibble() %>%
  filter(widow == 1) %>%
  select(-widow, -hhidpn)
  
  # survival: based on full model
female_drop_na <-
drop_na(female.full,start.age,stop.age,event,widow,ps.logit,birth_year,child_liv,migration,num_shared_child,child_dead,
        college_homo,racial_homo,reli_homo,inc_homo,geo_homo,age_diff,mean_drink_days,mean_drink_num,
          mean_hosp_time,max_hosp_visit,mean_log_fam_income,mean_bmi,max_nurs_home, 
          mean_cesd,mean_expend,max_high_bp,max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych,eduyrs,white,black, 
          catholic,protestant,ever_retired,labor_force, year_since_widow, year_since_child_dead, year_since_retire)

coxph.full <- coxph(Surv(start.age, stop.age, event) ~ widow  + birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire + ps.logit, data = female.full)
summary(coxph.full)
summary(coxph.full)
ph_test <- cox.zph(coxph.full)
print(ph_test)
png("full/female_ph_test.png")
plot(ph_test, xlab = "Age", ylab = "Scaled Schoenfeld Residuals")
dev.off()
# directly calculate the hazard ratios for different propensity
newdata <- expand.grid(widow = 1, ps.logit = seq(0, 1, by = 0.01))

covariates <- female.full %>% select(birth_year , child_liv , migration , num_shared_child , child_dead
                                   ,  age_diff , mean_drink_days , mean_drink_num
                                   , mean_hosp_time , max_hosp_visit , mean_log_fam_income , max_pension , mean_bmi , max_nurs_home 
                                   , mean_cesd , mean_expend , max_high_bp , max_diabetes , max_cancer , max_lung , max_heart , max_stroke , max_psych ,eduyrs , white , black 
                                   , catholic , protestant , year_since_widow , year_since_child_dead ,year_since_retire)
covmeans <- colMeans(covariates, na.rm = TRUE)
new_data_cov <- data.frame(matrix(rep(covmeans, each = nrow(newdata)), nrow = nrow(newdata), byrow = FALSE))
names(new_data_cov) <- names(covmeans)
new_data <- cbind(newdata, new_data_cov)

new_data$survival <- predict(coxph.full, new_data, type = "risk")
new_data$mean_survival <- mean(new_data$survival)
library(ggplot2)

ggplot(new_data, aes(x = ps.logit)) +
  geom_line(aes(y = survival), color = "blue") +
  geom_line(aes(y = mean_survival), color = "red") +
  labs(x = "Propensity Score", y = "Predicted Hazard Ratio", 
       title = "Predicted Hazard Ratio vs Propensity Score") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("HTE", "ATE"), 
                     name = "Survival Type") +
  theme_minimal()


# bootstrap for the confidence interval 
# number of bootstrap samples
B <- 1000

# store results
boot_results <- list()

# bootstrap loop
for (i in 1:B) {
  # create a bootstrap sample
  boot_sample <- female.full[sample(nrow(female.full), replace = TRUE), ]
  
  # fit model on the bootstrap sample
  boot_fit <-coxph(Surv(start.age, stop.age, event) ~ widow + ps.logit + birth_year + child_liv + migration + num_shared_child + child_dead
                   +  age_diff + mean_drink_days + mean_drink_num
                   + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                   + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                   + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = boot_sample)
  newdata <- expand.grid(widow = 1, ps.logit = seq(0, 1, by = 0.01))
  
  covariates <- boot_sample %>% select(birth_year, child_liv , migration , num_shared_child , child_dead
                                     ,  age_diff , mean_drink_days , mean_drink_num
                                     , mean_hosp_time , max_hosp_visit , mean_log_fam_income , max_pension , mean_bmi , max_nurs_home 
                                     , mean_cesd , mean_expend , max_high_bp , max_diabetes , max_cancer , max_lung , max_heart , max_stroke , max_psych ,eduyrs , white , black 
                                     , catholic , protestant , year_since_widow , year_since_child_dead ,year_since_retire)
  covmeans <- colMeans(covariates, na.rm = TRUE)
  new_data_cov <- data.frame(matrix(rep(covmeans, each = nrow(newdata)), nrow = nrow(newdata), byrow = FALSE))
  names(new_data_cov) <- names(covmeans)
  new_data <- cbind(newdata, new_data_cov)
  
  # store results
  boot_results[[i]] <- predict(boot_fit, new_data, type = "risk")
}
# convert results into a matrix
boot_results_matrix <- do.call(cbind, boot_results)

# compute 2.5% and 97.5% percentiles along the columns of the bootstrap results matrix
ci_lower <- apply(boot_results_matrix, 1, function(x) quantile(x, probs = 0.025))
ci_upper <- apply(boot_results_matrix, 1, function(x) quantile(x, probs = 0.975))
survival <- apply(boot_results_matrix, 1, function(x) quantile(x, probs = 0.5))

# add to newdata
new_data$ci_lower <- ci_lower
new_data$ci_upper <- ci_upper
new_data$survival <- survival
new_data$mean_survival <- 1

ggplot(new_data, aes(x = ps.logit)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2, fill = "blue") +
  geom_line(aes(y = survival), color = "blue") +
  geom_line(aes(y = mean_survival), color = "red") +
  labs(x = "Propensity Score", y = "Predicted Hazard Ratio", 
       title = "Results from Marginal Hazard Model") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Individual Survival", "Cutoff Line"), 
                     name = "Survival Type") 
# save the plot
ggsave("female_survival_cox.png", width = 8, height = 6, units = "in")
