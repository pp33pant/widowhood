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

nuisance_function <- function(data = male, covariates = "full", modeling = "logit"){
  param <- list(max_depth = 5, eta = 0.5,  nthread = 8,  eval_metric = "auc",
                lambda = 0, gamma = 0, booster = "gbtree")
  if (covariates == "full"){
    data.propensity.prediction <- data %>%
      drop_na(.,s_wave_age,s_eduyrs,num_shared_child,age_diff 
          ,s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
          ,s_max_hosp_visit,s_max_pension,s_mean_bmi 
          ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
          ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
          ,s_max_psych,child_liv,s_ever_retired,s_white,s_black 
          ,s_protestant,s_catholic,s_migration,s_year_since_retire)
    ps.xgb.input.data <- data.propensity.prediction  %>% dplyr::select (s_wave_age,s_eduyrs,num_shared_child,age_diff 
                                                                       ,s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
                                                                       ,s_max_hosp_visit,s_max_pension,s_mean_bmi 
                                                                       ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
                                                                       ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
                                                                       ,s_max_psych,child_liv,s_ever_retired,s_white,s_black 
                                                                       ,s_protestant,s_catholic,s_migration,s_year_since_retire) %>% as.matrix()
    ps.xgb.output.data <- data.propensity.prediction$widow %>% as.vector()
    if (modeling == "logit"){
      ns.func <- glm(widow  ~  s_eduyrs + num_shared_child + educ_homo + racial_homo + reli_homo + inc_homo + age_diff 
                            + geo_homo + s_mean_drink_days + s_mean_drink_num + s_mean_hosp_time 
                            + s_max_hosp_visit + mean_log_asset + s_mean_log_income + s_max_pension + s_mean_bmi 
                            + s_max_nurs_home + s_mean_cesd + s_mean_expend + s_max_high_bp 
                            + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 
                            + s_max_psych + child_liv + s_ever_retired + s_labor_force + s_white + s_black 
                            + s_protestant + s_catholic + s_migration, data = data.propensity.prediction, family = binomial(link = "logit"))
      ns.pred <- predict(ns.func, type = "response")
    }
  }
  
  else if (covariates == "health"){
    data.propensity.prediction <- data %>%
      drop_na(., s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time 
              ,s_max_hosp_visit,s_mean_bmi 
              ,s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp 
              ,s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke 
              ,s_max_psych)
    ps.xgb.input.data<- data.propensity.prediction  %>% dplyr::select (s_mean_drink_days,s_mean_drink_num,s_mean_hosp_time, 
                                                                               s_max_hosp_visit,s_mean_bmi, 
                                                                               s_max_nurs_home,s_mean_cesd,s_mean_expend,s_max_high_bp, 
                                                                               s_max_diabetes,s_max_cancer,s_max_lung,s_max_heart,s_max_stroke,
                                                                               s_max_psych) %>% as.matrix()
    ps.xgb.output.data <- data.propensity.prediction$widow %>% as.vector()
    if (modeling == "logit"){
      ns.func <- glm(widow  ~  s_mean_drink_days + s_mean_drink_num + s_mean_hosp_time 
                            + s_max_hosp_visit + s_mean_bmi 
                            + s_max_nurs_home + s_mean_cesd + s_mean_expend + s_max_high_bp 
                            + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 
                            + s_max_psych , data = male.propensity.prediction, family = binomial(link = "logit"))
      ns.pred <- predict(ns.func, type = "response")
    }
  }
  else if (covariates == "ses"){
    data.propensity.prediction <- data %>%
      drop_na(.,  s_eduyrs,num_shared_child,educ_homo,racial_homo,reli_homo,inc_homo,age_diff 
              ,geo_homo,mean_log_asset,s_mean_log_income,s_max_pension,child_liv 
              ,s_ever_retired,s_labor_force,s_white,s_black,s_protestant,s_catholic 
              ,s_migration)
    
    ps.xgb.input.data <- data.propensity.prediction %>% dplyr::select (s_eduyrs,num_shared_child,educ_homo,racial_homo,reli_homo,inc_homo,age_diff,
                                                                         geo_homo,mean_log_asset,s_mean_log_income,s_max_pension,child_liv,s_ever_retired,
                                                                         s_labor_force,s_white,s_black,s_protestant,s_catholic,s_migration) %>% as.matrix()
    
  ps.xgb.output.data <- data.propensity.prediction$widow %>% as.vector()
  if (modeling == "logit"){
    ns.func<- glm (widow  ~  s_eduyrs + num_shared_child + educ_homo + racial_homo + reli_homo + inc_homo + age_diff 
                           + geo_homo + mean_log_asset + s_mean_log_income + s_max_pension + child_liv 
                           + s_ever_retired + s_labor_force + s_white + s_black + s_protestant + s_catholic 
                           + s_migration, data = male.propensity.prediction, family = binomial(link = "logit"))
    ns.pred <- predict(ns.func, type = "response")
  }
  }
  
  if (modeling == "xgboost"){
    ns.func <- xgboost(params = param, data = ps.xgb.input.data, label = ps.xgb.output.data,  objective = "binary:logistic", nrounds = 200, verbose = 0)
    ns.pred <- predict(ns.func, ps.xgb.input.data)
  }
  else if (modeling == "rf"){
    ns.func <- lbart(x.train = ps.xgb.input.data, y.train = ps.xgb.output.data,ntree=200, numcut=10,
                         ndpost=100, nskip=100)
    
    ns.pred <- predict(ns.func, newdata = ps.xgb.input.data)$prob.test.mean
  }

  cm <-confusionMatrix(factor(ifelse(ns.pred  <= 0.5, 0, 1)), factor(data.propensity.prediction$widow))
  data.propensity.prediction.output <- data.propensity.prediction %>% 
    cbind(ns.pred) %>%
    dplyr::select(hhidpn, wave, ns.pred)
  data <- full_join(data, data.propensity.prediction.output, by = c("hhidpn","wave")) %>%
    arrange(hhidpn, wave) 
  
  data.output<- data %>%
    mutate(ps = ifelse(is.na(ns.pred) == TRUE, ifelse(widow == 0, 999, ifelse(lag(widow, 1) == 1, 
                                                                                       ifelse(hhidpn == lag(hhidpn, 1), lag(ns.pred, 1), 999), 999)),
                           ns.pred)) %>%
    fill(ps) %>%
    replace_with_na(replace = list(ps = 999)) %>%
    mutate(start.age = ifelse(is.na(wave_start_age), wave_end_age - 2, wave_start_age),
           stop.age = wave_end_age, 
           event = dead) 
  list(data = data.output, cm = cm)
}

hte_ate_model <- function(data, ns_covariates, ns_model, surv_covariates = "full", surv_model = "coxph", subsample = NULL){
  if (is.null(subsample) == TRUE){
    data <- nuisance_function(data, ns_covariates, ns_model)$data
  }
  else {
    data <- nuisance_function(data, ns_covariates, ns_model)$data %>%
      dplyr::select(hhidpn, wave, ps, start.age, stop.age, event) %>%
      right_join(subsample, by = c("hhidpn", "wave"))
  }
  
  data_trt_0 <- data %>% as_tibble() %>%
    filter(widow == 0) %>%
    dplyr::select(-widow, -hhidpn)
  
  data_trt_1 <- data %>% as_tibble() %>%
    filter(widow == 1) %>%
    dplyr::select(-widow, -hhidpn)
  if (surv_covariates == "full"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps,birth_year,child_liv,migration,num_shared_child,child_dead,
        college_homo,racial_homo,reli_homo,inc_homo,geo_homo,age_diff,mean_drink_days,mean_drink_num,
          mean_hosp_time,max_hosp_visit,mean_log_fam_income,mean_bmi,max_nurs_home, 
          mean_cesd,mean_expend,max_high_bp,max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych,eduyrs,white,black, 
          catholic,protestant,ever_retired,labor_force, year_since_widow, year_since_child_dead, year_since_retire)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~ ps + birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = data_trt_1)
    
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~ ps +birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = data_trt_0)

  }
  else if (surv_covariates == "null"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~ ps, data = data_trt_1)
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~ ps, data = data_trt_0)
  }
  else if (surv_covariates == "health"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps,mean_drink_days,mean_drink_num
              ,mean_hosp_time,max_hosp_visit,mean_bmi,max_nurs_home,mean_cesd,mean_expend,max_high_bp,
              max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~ ps + mean_drink_days + mean_drink_num
                                          + mean_hosp_time + max_hosp_visit + + mean_bmi + max_nurs_home 
                                          + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart 
                                          + max_stroke + max_psych, data = data_trt_1)
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~ ps + mean_drink_days + mean_drink_num
                                          + mean_hosp_time + max_hosp_visit + + mean_bmi + max_nurs_home 
                                          + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart 
                                          + max_stroke + max_psych, data = data_trt_0)
    
  }
  ps.category <- cut(data_drop_na$ps, breaks = c(quantile(data_drop_na$ps, probs = seq(0, 1, by = 0.02))), include.lowest = T, lables = 1:50)
  data_coxph_full_trt_1_trt_1 <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data_trt_1))$table %>% as_tibble()
  data_coxph_full_trt_1_trt_0 <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data_trt_0))$table %>% as_tibble()
  data_coxph_full_trt_0_trt_0 <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data_trt_0))$table %>% as_tibble()
  data_coxph_full_trt_0_trt_1 <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data_trt_1))$table %>% as_tibble()
  
  data_coxph_full_trt_1 <- data_coxph_full_trt_1_trt_1 %>% 
    bind_rows(data_coxph_full_trt_1_trt_0) %>% 
    bind_cols(ps = ps.category) %>% 
    dplyr::select(median, ps) %>% 
    rename(median_trt1 = median,
           ps_trt1 = ps)
  
  data_coxph_full_trt_0 <- data_coxph_full_trt_0_trt_1 %>% 
    bind_rows(data_coxph_full_trt_0_trt_0) %>% 
    bind_cols(ps = ps.category) %>% 
    dplyr::select(median, ps) %>% 
    rename(median_trt0 = median,
           ps_trt0 = ps)
  
  cox_hte <- data_coxph_full_trt_1 %>% 
    bind_cols(data_coxph_full_trt_0) %>% 
    dplyr::select(-ps_trt1) %>% 
    rename(ps = ps_trt0) %>% 
    group_by(ps) %>%
    mutate(hte = median_trt1 -  median_trt0) %>%
    drop_na() %>%
    # pull(hte) %>% mean
    summarise(lower_hte = quantile(hte, .025), upper_hte = quantile (hte, .975),hte = mean(hte), treated = mean(median_trt1), control = mean(median_trt0)) # for full model 
  
  #calculating ate
  data_coxph_full_trt_1_avg <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data))$table %>% as_tibble() %>%
    dplyr::select(median) %>%
    rename(median_trt1 = median)
  data_coxph_full_trt_0_avg <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data))$table %>% as_tibble()%>%
    dplyr::select(median) %>%
    rename(median_trt0 = median)
  
  cox_ate <- data_coxph_full_trt_1_avg %>% 
    bind_cols(data_coxph_full_trt_0_avg) %>% 
    mutate(ate = median_trt1 -  median_trt0) %>%
    drop_na() %>%
    # pull(hte) %>% mean
    summarise(lower_ate = quantile(ate, .025), upper_ate = quantile (ate, .975),ate = mean(ate),treated = mean(median_trt1), control = mean(median_trt0)) # for full model 
  
  table_for_graph <- as.data.frame(lapply(cox_ate, rep, 50)) %>% cbind(propensity = seq(.02,1,.02), object = "ATE") %>%
    rename(lower = lower_ate, upper = upper_ate, mean = ate) %>%
    rbind(as.data.frame(cox_hte) %>% cbind(propensity = seq(.02,1,.02), object = "CATE") %>%
            rename(lower = lower_hte, upper = upper_hte, mean = hte) %>% dplyr::select(-ps)) 
  
  ggplot(table_for_graph, aes(x = propensity, fill = object, group = object, linetype = object, color = object))+
    geom_smooth(aes(y = mean))+ xlab("Propensity scores for widowhood") + ylab("Lifespan differences")
}

hte_ate_model.2 <- function(data, ns_covariates, ns_model, surv_covariates, surv_model = "coxph", subsample = NULL){
  data <- nuisance_function(data, ns_covariates, ns_model)$data
  data_trt_0 <- data %>% as_tibble() %>%
    filter(widow == 0) %>%
    dplyr::select(-widow, -hhidpn)
  
  data_trt_1 <- data %>% as_tibble() %>%
    filter(widow == 1) %>%
    dplyr::select(-widow, -hhidpn)
  if (is.null(subsample) == TRUE){
    data_trt_0.fit <- data_trt_0
    data_trt_1.fit <- data_trt_1
  }
  else {
    data_trt.fit <- subsample %>%
      left_join(data %>% dplyr::select(hhidpn, wave, ps), by = c("hhidpn", "wave")) %>%
      as_tibble() 
    data_trt_0.fit <- subsample %>%
      left_join(data %>% dplyr::select(hhidpn, wave, ps), by = c("hhidpn", "wave")) %>%
      as_tibble() %>%
      filter(widow == 0) %>%
      dplyr::select(-widow, -hhidpn)
    data_trt_1.fit <- subsample %>% 
      left_join(data %>% dplyr::select(hhidpn, wave, ps), by = c("hhidpn", "wave")) %>%
      as_tibble() %>%
      filter(widow == 1) %>%
      dplyr::select(-widow, -hhidpn)
  }
  if (surv_covariates == "full"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps,birth_year,child_liv,migration,num_shared_child,child_dead,
        college_homo,racial_homo,reli_homo,inc_homo,geo_homo,age_diff,mean_drink_days,mean_drink_num,
          mean_hosp_time,max_hosp_visit,mean_log_fam_income,mean_bmi,max_nurs_home, 
          mean_cesd,mean_expend,max_high_bp,max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych,eduyrs,white,black, 
          catholic,protestant,ever_retired,labor_force, year_since_widow, year_since_child_dead, year_since_retire)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~  ps +birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = data_trt_1)
    
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~  ps +birth_year + child_liv + migration + num_shared_child + child_dead
                    +  age_diff + mean_drink_days + mean_drink_num
                    + mean_hosp_time + max_hosp_visit + mean_log_fam_income + max_pension + mean_bmi + max_nurs_home 
                    + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke + max_psych +eduyrs + white + black 
                    + catholic + protestant + year_since_widow + year_since_child_dead +year_since_retire, data = data_trt_0)
    
  }
  else if (surv_covariates == "null"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~ ps, data = data_trt_1)
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~ ps, data = data_trt_0)
  }
  else if (surv_covariates == "health"){
    data_drop_na <-
      drop_na(data,start.age,stop.age,event,widow,ps,mean_drink_days,mean_drink_num
              ,mean_hosp_time,max_hosp_visit,mean_bmi,max_nurs_home,mean_cesd,mean_expend,max_high_bp,
              max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych)
    data_coxph.full_linear_trt_1 <- coxph(Surv(start.age, stop.age, event) ~ ps + mean_drink_days + mean_drink_num
                                          + mean_hosp_time + max_hosp_visit + + mean_bmi + max_nurs_home 
                                          + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart 
                                          + max_stroke + max_psych, data = data_trt_1)
    data_coxph.full_linear_trt_0 <- coxph(Surv(start.age, stop.age, event) ~ ps + mean_drink_days + mean_drink_num
                                          + mean_hosp_time + max_hosp_visit + + mean_bmi + max_nurs_home 
                                          + mean_cesd + mean_expend + max_high_bp + max_diabetes + max_cancer + max_lung + max_heart 
                                          + max_stroke + max_psych, data = data_trt_0)
    
  }
  if (is.null(subsample) == TRUE){
    ps.category <- cut(data_drop_na$ps, breaks = c(quantile(data_drop_na$ps, probs = seq(0, 1, by = 0.02))), include.lowest = T, lables = 1:50)
  }
  else{
    data_trt.fit <- data_trt.fit %>%drop_na(.,ps,mean_drink_days,mean_drink_num
                                            ,mean_hosp_time,max_hosp_visit,mean_bmi,max_nurs_home,mean_cesd,mean_expend,max_high_bp,
                                            max_diabetes,max_cancer,max_lung,max_heart,max_stroke,max_psych)
    ps.category <- cut(data_trt.fit$ps, breaks = c(quantile(data_trt.fit$ps, probs = seq(0, 1, by = 0.02))), include.lowest = T, lables = 1:50)
    
  }
  data_coxph_full_trt_1_trt_1 <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data_trt_1.fit))$table %>% as_tibble()
  data_coxph_full_trt_1_trt_0 <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data_trt_0.fit))$table %>% as_tibble()
  data_coxph_full_trt_0_trt_0 <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data_trt_0.fit))$table %>% as_tibble()
  data_coxph_full_trt_0_trt_1 <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data_trt_1.fit))$table %>% as_tibble()
  
  data_coxph_full_trt_1 <- data_coxph_full_trt_1_trt_1 %>% 
    bind_rows(data_coxph_full_trt_1_trt_0) %>% 
    bind_cols(ps = ps.category) %>% 
    dplyr::select(median, ps) %>% 
    rename(median_trt1 = median,
           ps_trt1 = ps)
  
  data_coxph_full_trt_0 <- data_coxph_full_trt_0_trt_1 %>% 
    bind_rows(data_coxph_full_trt_0_trt_0) %>% 
    bind_cols(ps = ps.category) %>% 
    dplyr::select(median, ps) %>% 
    rename(median_trt0 = median,
           ps_trt0 = ps)
  
  cox_hte <- data_coxph_full_trt_1 %>% 
    bind_cols(data_coxph_full_trt_0) %>% 
    dplyr::select(-ps_trt1) %>% 
    rename(ps = ps_trt0) %>% 
    group_by(ps) %>%
    mutate(hte = median_trt1 -  median_trt0) %>%
    drop_na() %>%
    # pull(hte) %>% mean
    summarise(lower_hte = quantile(hte, .025), upper_hte = quantile (hte, .975),hte = mean(hte), treated = mean(median_trt1), control = mean(median_trt0)) # for full model 
  
  #calculating ate
  if (is.null(subsample) == TRUE){
    data_coxph_full_trt_1_avg <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data))$table %>% as_tibble() %>%
      dplyr::select(median) %>%
      rename(median_trt1 = median)
    data_coxph_full_trt_0_avg <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data))$table %>% as_tibble()%>%
      dplyr::select(median) %>%
      rename(median_trt0 = median)
  }
  else{
    data_coxph_full_trt_1_avg <- summary(survfit(data_coxph.full_linear_trt_1, newdata = data_trt.fit))$table %>% as_tibble() %>%
      dplyr::select(median) %>%
      rename(median_trt1 = median)
    data_coxph_full_trt_0_avg <- summary(survfit(data_coxph.full_linear_trt_0, newdata = data_trt.fit))$table %>% as_tibble()%>%
      dplyr::select(median) %>%
      rename(median_trt0 = median)
  }
  
  cox_ate <- data_coxph_full_trt_1_avg %>% 
    bind_cols(data_coxph_full_trt_0_avg) %>% 
    mutate(ate = median_trt1 -  median_trt0) %>%
    drop_na() %>%
    # pull(hte) %>% mean
    summarise(lower_ate = quantile(ate, .025), upper_ate = quantile (ate, .975),ate = mean(ate),treated = mean(median_trt1), control = mean(median_trt0)) # for full model 
  
  table_for_graph <- as.data.frame(lapply(cox_ate, rep, 50)) %>% cbind(propensity = seq(.02,1,.02), object = "ATE") %>%
    rename(lower = lower_ate, upper = upper_ate, mean = ate) %>%
    rbind(as.data.frame(cox_hte) %>% cbind(propensity = seq(.02,1,.02), object = "CATE") %>%
            rename(lower = lower_hte, upper = upper_hte, mean = hte) %>% dplyr::select(-ps)) 
  
  ggplot(table_for_graph, aes(x = propensity, fill = object, group = object, linetype = object, color = object))+
    geom_smooth(aes(y = mean))+ xlab("Propensity scores for widowhood") + ylab("Lifespan differences")
}


dress_plot <- function(plot, ggtitle){
    plot <- plot + ggtitle(ggtitle) + theme_bw() + 
      theme(legend.position="center", 
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) 
}

age_cuts <- function(age.cut){
  male.young <- male %>% filter(.,wave_start_age <= age.cut & wave_start_age >=55)
  male.old <- male %>% filter(.,wave_start_age > age.cut)
  
  female.young <- female %>% filter(.,wave_start_age <= age.cut & wave_start_age >=55)
  female.old <- female %>% filter(.,wave_start_age > age.cut)
  
  plot3 <- dress_plot(hte_ate_model(male, "health", "xgboost", "health","coxph",male.young), paste0("Men, Younger than ", age.cut))
  plot4 <- dress_plot(hte_ate_model(male, "health", "xgboost", "health","coxph",male.old),paste0("Men, Older than ", age.cut))
  
  plot5 <- dress_plot(hte_ate_model(female, "health", "xgboost", "health","coxph",female.young), paste0("Women, Younger than ", age.cut))
  plot6 <- dress_plot(hte_ate_model(female, "health", "xgboost", "health","coxph",female.old), paste0("Women, Older than ", age.cut))
  
  ggarrange(plot3, plot4, plot5, plot6, ncol = 2, nrow = 2, common.legend = T,  legend = "bottom")
  
}

age_cuts.2 <- function(age.cut){
  male.young <- male %>% filter(.,wave_start_age <= age.cut)
  male.old <- male %>% filter(.,wave_start_age > age.cut)
  
  female.young <- female %>% filter(.,wave_start_age <= age.cut)
  female.old <- female %>% filter(.,wave_start_age > age.cut)
  
  plot3 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.young), paste0("Men, Younger than ", age.cut))
  plot4 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.old),paste0("Men, Older than ", age.cut))
  
  plot5 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.young), paste0("Women, Younger than ", age.cut))
  plot6 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.old), paste0("Women, Older than ", age.cut))
  
  ggarrange(plot3, plot4, plot5, plot6, ncol = 2, nrow = 2, common.legend = T,  legend = "bottom")
  
}

# only household ses measurement 
ses.vars.hh <- c("mean_log_asset", "mean_log_capital_income")

# Household + self economic 
ses.vars.self.econ <- c("mean_log_asset", "mean_log_income", "mean_log_capital_income","max_pension"
                        ,"ever_retired","labor_force")

# Household + self economic + spouse economic 
ses.vars.econ <- c("mean_log_asset","s_mean_log_income","max_pension","s_max_pension"
                   ,"ever_retired", "s_ever_retired","labor_force", "s_labor_force")

# Household + self
ses.vars.self <- c("eduyrs","num_shared_child","educ_homo","racial_homo","reli_homo","inc_homo","age_diff" 
                   ,"geo_homo","mean_log_asset","mean_log_capital_income", "mean_log_income","max_pension","child_liv" 
                   ,"ever_retired","labor_force","white","black","protestant","catholic","migration")


# Household + self + spouse
ses.vars.all <- c("eduyrs","s_eduyrs","num_shared_child","educ_homo","racial_homo","reli_homo","inc_homo","age_diff" 
                  ,"geo_homo","mean_log_asset","mean_log_capital_income","mean_log_income","s_mean_log_income","max_pension","s_max_pension","child_liv" 
                  ,"ever_retired", "s_ever_retired","labor_force", "s_labor_force","white","black","s_white","s_black","protestant","catholic"
                  ,"s_protestant","s_catholic","migration","s_migration")

ses_cut <- function(ses.selection){
  male.ses <- dplyr::select(male, all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  #get the PCA factor
  male.factor <- prcomp(male.ses %>%dplyr::select(-c(hhidpn, wave)), center = T, scale. = T)$x[,1]
  male.new <- male.ses %>% cbind(male.factor) %>%left_join(male, by =  c("hhidpn", "wave"))
  male.ses.low <- male.new %>% filter(male.factor <= 0)
  male.ses.high <- male.new %>% filter(male.factor >=0)
  
  plot1 <- dress_plot(hte_ate_model(male, "health", "xgboost", "health","coxph",male.ses.low), paste0("Men, Low SES"))
  plot2 <- dress_plot(hte_ate_model(male, "health", "xgboost", "health","coxph",male.ses.high),paste0("Men, High SES"))
  female.ses <- dplyr::select(female,all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  #get the PCA factor
  female.factor <- prcomp(female.ses %>%dplyr::select(-c(hhidpn, wave)), center = T, scale. = T)$x[,1]
  female.new <- female.ses %>% cbind(female.factor)%>%left_join(female, by =  c("hhidpn", "wave"))
  female.ses.low <- female.new %>% filter(female.factor <= 0)
  female.ses.high <- female.new %>% filter(female.factor >=0)
  
  plot3 <- dress_plot(hte_ate_model(female, "health", "xgboost", "health","coxph",female.ses.low), paste0("Women, Low SES"))
  plot4 <- dress_plot(hte_ate_model(female, "health", "xgboost", "health","coxph",female.ses.high),paste0("Women, High SES"))
  ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, common.legend = T,  legend = "bottom")
}

evaluation_ses <- function(ses.selection){
  male.ses <- dplyr::select(male, all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  crobach.male <- cronbach.alpha(male.ses %>% dplyr::select(-c(hhidpn, wave)), CI = TRUE)
  cases.dropped.male <- n_distinct(male$hhidpn) - n_distinct(male.ses$hhidpn) 
  samples.dropped.male <- nrow(male) - nrow(male.ses)
  female.ses <- dplyr::select(female, all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  crobach.female <- cronbach.alpha(female.ses %>% dplyr::select(-c(hhidpn, wave)), CI = TRUE)
  cases.dropped.female <-n_distinct(female$hhidpn) - n_distinct(female.ses$hhidpn) 
  samples.dropped.female <- nrow(female) - nrow(female.ses)
  
  list(crobach.male = crobach.male, cases.dropped.male = cases.dropped.male, samples.dropped.male = samples.dropped.male, 
       crobach.female = crobach.female, cases.dropped.female = cases.dropped.female, samples.dropped.female = samples.dropped.female)
}
ses_cut.2 <- function(ses.selection){
  male.ses <- dplyr::select(male, all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  #get the PCA factor
  male.factor <- prcomp(male.ses %>%dplyr::select(-c(hhidpn, wave)), center = T, scale. = T)$x[,1]
  male.new <- male.ses %>% cbind(male.factor) %>%left_join(male, by =  c("hhidpn", "wave"))
  male.ses.low <- male.new %>% filter(male.factor <= 0)
  male.ses.high <- male.new %>% filter(male.factor >=0)
  
  plot1 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.ses.low), paste0("Men, Low SES"))
  plot2 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.ses.high),paste0("Men, High SES"))
  female.ses <- dplyr::select(female,all_of(ses.selection),"hhidpn","wave") %>%drop_na()
  #get the PCA factor
  female.factor <- prcomp(female.ses %>%dplyr::select(-c(hhidpn, wave)), center = T, scale. = T)$x[,1]
  female.new <- female.ses %>% cbind(female.factor)%>%left_join(female, by =  c("hhidpn", "wave"))
  female.ses.low <- female.new %>% filter(female.factor <= 0)
  female.ses.high <- female.new %>% filter(female.factor >=0)
  
  plot3 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.ses.low), paste0("Women, Low SES"))
  plot4 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.ses.high),paste0("Women, High SES"))
  ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, common.legend = T,  legend = "bottom")
}
gaps_cut <- function(gap.cut){
  male.short <- male %>% filter(.,year_diff <= gap.cut)
  male.long <- male %>% filter(., year_diff > gap.cut)
  male.others <- male %>% filter(., is.na(year_diff) == TRUE)
  
  female.short <- female %>% filter(.,year_diff <= gap.cut)
  female.long <- female %>% filter(., year_diff > gap.cut)
  female.others <- female %>% filter(., is.na(year_diff) == TRUE)
  
  plot3 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.short), paste0("Men, Short Widowhood"))
  plot4 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.long),paste0("Men, Long Widowhood"))
  plot5 <- dress_plot(hte_ate_model.2(male, "health", "xgboost", "health","coxph",male.others), paste0("Men, Others"))

  
  plot6 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.short), paste0("Women, Short Widowhood"))
  plot7 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.long),paste0("Women, Long Widowhood"))
  plot8 <- dress_plot(hte_ate_model.2(female, "health", "xgboost", "health","coxph",female.others), paste0("Women, Others"))
  
  plot <- ggarrange(plot3, plot4, plot5, plot6, plot7, plot8, ncol = 3, nrow = 2, common.legend = T,  legend = "bottom")
  annotate_figure(plot, top = text_grob(paste0("Gap Between Widowhood and Mortaility = ", gap.cut))) 
                  
}

# educational status 
edu_status <- function(){
  male.college <- male %>% filter(.,college == 1)
  male.noncollege <- male %>% filter(.,college == 0)
  
  female.college <- female %>% filter(.,college == 1)
  female.noncollege <- female %>% filter(.,college == 0)
  
  plot3 <- dress_plot(hte_ate_model.2(male, "full", "logit", "full","coxph",male.college), paste0("Men, College"))
  plot4 <- dress_plot(hte_ate_model.2(male, "full", "logit", "full", "coxph", male.noncollege), paste0("Men, Non-College"))
  
  plot5 <- dress_plot(hte_ate_model.2(female, "full", "logit", "full","coxph",female.college), paste0("Women, College"))
  plot6 <- dress_plot(hte_ate_model.2(female, "full", "logit", "full","coxph",female.noncollege), paste0("Women, Non-College"))
  ggarrange(plot3, plot4, plot5, plot6, ncol = 2, nrow = 2, common.legend = T,  legend = "bottom")
  
}
