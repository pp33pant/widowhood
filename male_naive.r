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
male <- read_dta("../male.dta")

male.propensity.prediction.health <- male %>%
  select(., s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
         ,s_max_hosp_visit,s_mean_bmi 
         ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
         ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
         ,s_max_psych)

cor_matrix <- cor(male.propensity.prediction.health, use = "complete.obs")
colSums(is.na(male.propensity.prediction.health))
male.propensity.prediction.health <- male %>% drop_na(., s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
          ,s_max_hosp_visit,s_mean_bmi 
          ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
          ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
          ,s_max_psych)

ns.logit.func2 <- glm(widow  ~  s_mean_drink_days + s_mean_drink_num + s_mean_hosp_time 
                      + s_max_hosp_visit + s_mean_bmi 
                      + s_max_nurs_home + s_mean_cesd + s_mean_expend + s_max_high_bp 
                      + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 
                      + s_max_psych , data = male.propensity.prediction.health, family = binomial(link = "logit"))
summary(ns.logit.func2)
ns.logit.pred2 <- predict(ns.logit.func2, type = "response")

plot(pROC::roc(response = male.propensity.prediction.health$widow,
               predictor = ns.logit.func2$fitted.values,
               levels=c(0, 1)),
     lwd=1.5) 

confusionMatrix(factor(ifelse(ns.logit.func2$fitted.values <= 0.5, 0, 1)), factor(male.propensity.prediction.health$widow))

param <- list(max_depth = 5, eta = 0.5,  nthread = 8,  eval_metric = "auc",
              lambda = 0, gamma = 0, booster = "gbtree")
ps.xgb.input.male.health <- male.propensity.prediction.health  %>% select (s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time, 
                                                                           s_max_hosp_visit,s_mean_bmi, 
                                                                           s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp, 
                                                                           s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke,
                                                                           s_max_psych) %>% as.matrix()

ps.xgb.output.male.health <- male.propensity.prediction.health$widow %>% as.vector()

ns.xgb.func2 <- xgboost(params = param, data = ps.xgb.input.male.health, label = ps.xgb.output.male.health,  objective = "binary:logistic", nrounds = 200, verbose = 0)
ns.xgb.pred2 <- predict(ns.xgb.func2, ps.xgb.input.male.health)
plot(pROC::roc(response = male.propensity.prediction.health$widow,
               predictor = ns.xgb.pred2,
               levels=c(0, 1)),
     lwd=1.5) 

confusionMatrix(factor(ifelse(ns.xgb.pred2  <= 0.5, 0, 1)), factor(male.propensity.prediction.health$widow))

# describe the distributions of the propensity scores predicted by logit and xgboost models

hist(ns.logit.pred2)
hist(ns.xgb.pred2)
summary(ns.logit.pred2)
summary(ns.xgb.pred2)

male.propensity.prediction.full <- male %>%
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
                      + s_protestant + s_catholic + s_migration + s_year_since_retire, data = male.propensity.prediction.full, family = binomial(link = "logit"))
summary(ns.logit.func1)
ns.logit.pred1 <- predict(ns.logit.func1, type = "response")
summary(ns.logit.pred1)

ps.xgb.input.male.full <- male.propensity.prediction.full  %>% dplyr::select (s_wave_age,s_eduyrs,num_shared_child,age_diff 
                                                                       ,s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
                                                                       ,s_max_hosp_visit,s_max_pension,s_mean_bmi 
                                                                       ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
                                                                       ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
                                                                       ,s_max_psych,child_liv,s_ever_retired,s_white,s_black 
                                                                       ,s_protestant,s_catholic,s_migration,s_year_since_retire) %>% as.matrix()

ps.xgb.output.male.full <- male.propensity.prediction.full$widow %>% as.vector()

ns.xgb.func1 <- xgboost(params = param, data = ps.xgb.input.male.full, label = ps.xgb.output.male.full,  objective = "binary:logistic", nrounds = 200, verbose = 0)
ns.xgb.pred1 <- predict(ns.xgb.func1, ps.xgb.input.male.full)

male.propensity.prediction.output.full <- male.propensity.prediction.full%>% 
  cbind(ns.logit.pred1) %>%
  cbind(ns.xgb.pred1) %>%
  select(hhidpn, wave, ns.logit.pred1, ns.xgb.pred1)

male.full <- full_join(male, male.propensity.prediction.output.full, by = c("hhidpn","wave")) %>%
  arrange(hhidpn, wave)

male.full <- male.full%>%
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



male_drop_na <-
drop_na(male.full,start.age,stop.age,event,widow,ps.logit,birth_year,child_liv,migration,num_shared_child,child_dead,
        college_homo,racial_homo,reli_homo,inc_homo,geo_homo,age_diff,mean_drink_days,mean_drink_num,
          mean_hosp_time,max_hosp_visit,mean_log_fam_income,mean_bmi,max_nurs_home, 
          mean_cesd,mean_expend,max_high_bp,max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych,eduyrs,white,black, 
          catholic,protestant,ever_retired,labor_force, year_since_widow, year_since_child_dead, year_since_retire)

male_trt_0 <- male_drop_na %>% as_tibble() %>%
  filter(widow == 0) %>%
  select(-widow, -hhidpn)
  
male_trt_1 <- male_drop_na %>% as_tibble() %>%
  filter(widow == 1) %>%
  select(-widow, -hhidpn)
  
# survival: control model
coxph.male_trt_0 <- coxph(Surv(start.age, stop.age, event) ~  birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire + ps.logit, data = male_trt_0)
summary(coxph.male_trt_0)
ph_test <- cox.zph(coxph.male_trt_0)
print(ph_test)
png("full/male_ph_test_control.png")
plot(ph_test, xlab = "Age", ylab = "Scaled Schoenfeld Residuals")
dev.off()
# survival: treatment model
coxph.male_trt_1 <- coxph(Surv(start.age, stop.age, event) ~  birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire + ps.logit, data = male_trt_1)
summary(coxph.male_trt_1)
ph_test <- cox.zph(coxph.male_trt_1)
print(ph_test)
png("full/male_ph_test_treated.png")
plot(ph_test, xlab = "Age", ylab = "Scaled Schoenfeld Residuals")
dev.off()
# predict the outcome under control with trt_0:

male_coxph_full_trt_1_trt_1 <- summary(survfit(coxph.male_trt_1, newdata = male_trt_1))$table %>% as_tibble()
male_coxph_full_trt_1_trt_0 <- summary(survfit(coxph.male_trt_1, newdata = male_trt_0))$table %>% as_tibble()
male_coxph_full_trt_0_trt_0 <- summary(survfit(coxph.male_trt_0, newdata = male_trt_0))$table %>% as_tibble()
male_coxph_full_trt_0_trt_1 <- summary(survfit(coxph.male_trt_0, newdata = male_trt_1))$table %>% as_tibble()
ps.logit.category <- cut(male_drop_na$ps.logit, breaks = c(quantile(male_drop_na$ps.logit, probs = seq(0, 1, by = 0.02))), include.lowest = T, lables = 1:50)
ps.logit.category.levels <- levels(ps.logit.category)
lower_bound <- sapply(strsplit(ps.logit.category.levels, split = ','), `[`, 2)
lower_bound_numeric <- as.numeric(gsub("]", "", lower_bound))

male_coxph_full_trt_1 <- male_coxph_full_trt_1_trt_1 %>% 
bind_rows(male_coxph_full_trt_1_trt_0) %>% 
bind_cols(ps.logit = ps.logit.category) %>% 
select(median, ps.logit) %>% 
rename(median_trt1 = median,
        ps.logit_trt1 = ps.logit)


male_coxph_full_trt_0 <- male_coxph_full_trt_0_trt_1 %>% 
  bind_rows(male_coxph_full_trt_0_trt_0) %>% 
  bind_cols(ps.logit = ps.logit.category) %>% 
  select(median, ps.logit) %>% 
  rename(median_trt0 = median,
          ps.logit_trt0 = ps.logit)

cox_hte <- male_coxph_full_trt_1 %>% 
  bind_cols(male_coxph_full_trt_0) %>% 
  select(-ps.logit_trt1) %>% 
  rename(ps.logit = ps.logit_trt0) %>% 
  group_by(ps.logit) %>%
  mutate(hte = median_trt1 -  median_trt0) %>%
  drop_na() %>%
  # pull(hte) %>% mean
  summarise(lower_hte = quantile(hte, .025), upper_hte = quantile (hte, .975),hte = mean(hte), treated = mean(median_trt1), control = mean(median_trt0)) # for full model 

  #calculating ate
male_coxph_full_trt_1_avg <- summary(survfit(coxph.male_trt_1, newdata = male_drop_na))$table %>% as_tibble() %>%
    select(median) %>%
    rename(median_trt1 = median)
male_coxph_full_trt_0_avg <- summary(survfit(coxph.male_trt_0, newdata = male_drop_na))$table %>% as_tibble()%>%
    select(median) %>%
    rename(median_trt0 = median)
cox_ate <- male_coxph_full_trt_1_avg %>% 
  bind_cols(male_coxph_full_trt_0_avg) %>% 
  mutate(ate = median_trt1 -  median_trt0) %>%
  drop_na() %>%
  # pull(hte) %>% mean
  summarise(lower_ate = quantile(ate, .025), upper_ate = quantile (ate, .975),ate = mean(ate),treated = mean(median_trt1), control = mean(median_trt0)) # for full model 

table_for_graph <- as.data.frame(lapply(cox_ate, rep, 50)) %>% cbind(ps = seq(.02,1,.02), object = "ate") %>%
    rename(lower = lower_ate, upper = upper_ate, mean = ate) %>%
    rbind(as.data.frame(cox_hte) %>% cbind(ps = seq(.02,1,.02), object = "hte") %>%
            rename(lower = lower_hte, upper = upper_hte, mean = hte) %>% select(-ps.logit)) 

ggplot(table_for_graph, aes(x = ps, fill = object, group = object, linetype = object, color = object))+
  geom_smooth(aes(y = mean))+ xlab("Propensity Scores for Widowhood") + ylab("Age Differences between Treatment and Control")

# generate the standard error for the results 

# resample from male_drop_na

B = 100
hte_bootstrap_result <- matrix(ncol = B, nrow = 50)
for (i in 1:B){
  male_resample <- male_drop_na[sample(nrow(male_drop_na), replace = TRUE), ]
  male_trt_0 <- male_resample %>% as_tibble() %>%
  filter(widow == 0) %>%
  select(-widow, -hhidpn)
  
  male_trt_1 <- male_resample %>% as_tibble() %>%
  filter(widow == 1) %>%
  select(-widow, -hhidpn)
  
  coxph.male_trt_0 <- coxph(Surv(start.age, stop.age, event) ~  ps.logit + birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = male_trt_0)


# survival: treatment model
  coxph.male_trt_1 <- coxph(Surv(start.age, stop.age, event) ~  ps.logit + birth_year + child_liv + migration + num_shared_child + child_dead
                      +  age_diff + mean_drink_days + mean_drink_num
                      + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                      + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                      + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = male_trt_1)


# predict the outcome under control with trt_0:

  male_coxph_full_trt_1_trt_1 <- summary(survfit(coxph.male_trt_1, newdata = male_trt_1))$table %>% as_tibble()
  male_coxph_full_trt_1_trt_0 <- summary(survfit(coxph.male_trt_1, newdata = male_trt_0))$table %>% as_tibble()
  male_coxph_full_trt_0_trt_0 <- summary(survfit(coxph.male_trt_0, newdata = male_trt_0))$table %>% as_tibble()
  male_coxph_full_trt_0_trt_1 <- summary(survfit(coxph.male_trt_0, newdata = male_trt_1))$table %>% as_tibble()
  ps.logit.category <- cut(male_drop_na$ps.logit, breaks = c(quantile(male_drop_na$ps.logit, probs = seq(0, 1, by = 0.02))), include.lowest = T, lables = 1:50)

  male_coxph_full_trt_1 <- male_coxph_full_trt_1_trt_1 %>% 
  bind_rows(male_coxph_full_trt_1_trt_0) %>% 
  bind_cols(ps.logit = ps.logit.category) %>% 
  select(median, ps.logit) %>% 
  rename(median_trt1 = median,
          ps.logit_trt1 = ps.logit)


  male_coxph_full_trt_0 <- male_coxph_full_trt_0_trt_1 %>% 
    bind_rows(male_coxph_full_trt_0_trt_0) %>% 
    bind_cols(ps.logit = ps.logit.category) %>% 
    select(median, ps.logit) %>% 
    rename(median_trt0 = median,
            ps.logit_trt0 = ps.logit)

  cox_hte_boot <- male_coxph_full_trt_1 %>% 
    bind_cols(male_coxph_full_trt_0) %>% 
    select(-ps.logit_trt1) %>% 
    rename(ps.logit = ps.logit_trt0) %>% 
    group_by(ps.logit) %>%
    mutate(hte = median_trt1 -  median_trt0) %>%
    drop_na() %>%
    # pull(hte) %>% mean
    summarise(lower_hte = quantile(hte, .025), upper_hte = quantile (hte, .975),hte = mean(hte), treated = mean(median_trt1), control = mean(median_trt0)) # for full model 

  hte_bootstrap_result[,i] <- cox_hte_boot$hte
}

# get the standard deviation of the bootstrap results
sd_cox_hte <- apply(hte_bootstrap_result, 1, sd)
se_cox_hte <- sd_cox_hte / sqrt(nrow(hte_bootstrap_result))
ci_lower <- cox_hte$hte - 1.96 * se_cox_hte
ci_upper <- cox_hte$hte + 1.96 * se_cox_hte
ci_mean <- cox_hte$hte
ps.logit <- lower_bound_numeric
ate <- cox_ate$ate

# generate the new data for plotting 
new_data <- data.frame(ps.logit, ci_lower, ci_upper, ci_mean, ate)


# plot the results
ggplot(new_data, aes(x = ps.logit, y = ci_mean)) +
  geom_smooth(color = "blue") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = ate, color = "red") +
  xlab("Propensity Scores for Widowhood") + ylab("Age Differences between Treatment and Control")

# smooth the results
smooth_data <- new_data %>% 
  mutate(ci_lower = smooth.spline(ci_lower, spar = 0.5)$y,
         ci_upper = smooth.spline(ci_upper, spar = 0.5)$y,
         ci_mean = smooth.spline(ci_mean, spar = 0.5)$y,
         ate = smooth.spline(ate, spar = 0.5)$y)

# plot the results based on the smoothed data
 ggplot(smooth_data, aes(x = ps.logit, y = ci_mean)) +
  geom_line(color = "blue", se = FALSE) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = ate, color = "red") +
  xlab("Propensity Scores for Widowhood") + ylab("Age Differences between Treatment and Control")

ggsave("male_naive_1.png", width = 8, height = 6, units = "in")





