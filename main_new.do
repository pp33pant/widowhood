


/*************************     HRS Covariates Data        ******************************************
**************************  Widowed and Mortality Project ******************************************
**************************       Guanghui Pan             ******************************************
**************************      January 30th, 2022        ******************************************
************************** guanghui.pan@sociology.ox.ac.uk******************************************/ 

capture log close
log using widowhood_new.log , replace

timer on 1

clear 
clear matrix
clear mata 
set maxvar 32767
 use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\Data\rand_longitudinal\randhrs1992_2018v1_STATA\randhrs1992_2018v1.dta", clear 

*use "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/Data/rand_longitudinal/randhrs1992_2018v1_STATA/randhrs1992_2018v1.dta", clear 

/********************************** STEP I: INITIAL DATA CLEANING **********************************************************************/
/********** respondent's own demographic information *********/
* interview year
forvalues i = 1(1)14{
	rename  r`i'iwendy int_year_wave_`i'
}
gen int_year_1998 = int_year_wave_4

	
recode ragender(2=0), gen(male) /* we will establish models separately for male and female */ 

rename rabyear birth_year /* birth year */
rename raracem race /*race category */
rename raeduc educ /* education category */

* place of birth 
rename rabplace birth_place 
gen migration = 1 if birth_place == 11
replace migration = 0 if migration != 1

*religion 
rename rarelig religion 

/*total income in every wave */
forvalues i =1(1)14{
	foreach j in r`i'iearn  r`i'ipena r`i'issdi r`i'isret r`i'iunwc  r`i'igxfr{
	replace `j'=. if `j'==.u
	replace `j'=0 if `j'==.
	}
}
forvalues i =1(1)14{
	gen income_`i' = r`i'iearn + r`i'ipena+ r`i'issdi+ r`i'isret+ r`i'iunwc + r`i'igxfr
	gen log_income_`i' = log(income_`i'+10)
}
* whether have pension
gen r1peninc=.
forvalues i = 1(1)14{
	gen pension_`i' = 1 if r`i'peninc == 1
	replace pension_`i' = 0 if pension_`i'!=1
}


/******* spouse information *******************/
* the information for spouse was updated en every wave 
/* birthyear for spouse */ 
forvalues i = 1(1)14{
	rename s`i'byear s_birth_year_`i'  
}
/*race for spouse */
forvalues i =1(1)14{
	rename s`i'racem s_race_`i'
	g s_white_`i' = 1 if s_race_`i' == 1
	replace s_white_`i' = 0 if s_white_`i' ! = 1
	g s_black_`i' = 1 if s_race_`i' == 2
	replace s_black_`i' = 0 if s_black_`i' ! = 1
}

/*education categories for spouse */
forvalues i=1(1)14{
	rename s`i'educ s_educ_`i'
}

* place of birth
forvalues i =1(1)14{
	rename s`i'bplace s_birth_place_`i'
	gen s_migration_`i' = 1 if s_birth_place_`i' == 11
	replace s_migration_`i' = 0 if s_migration_`i' != 1

}

*religion 
forvalues i =1(1)14{
	rename s`i'relig s_religion_`i'
	g s_protestant_`i' = 1 if s_religion_`i' == 1
	replace s_protestant_`i' = 0 if s_protestant_`i' ! = 1
	g s_catholic_`i' = 1 if s_religion_`i' == 2
	replace s_catholic_`i' = 0 if s_catholic_`i' ! = 1

}

/*total income for spouse */
forvalues i =1(1)14{
	foreach j in s`i'iearn  s`i'ipena s`i'issdi s`i'isret s`i'iunwc  s`i'igxfr {
	replace `j'=. if `j'==.u
	replace `j'=0 if `j'==.
	}
}
forvalues i =1(1)14{
	gen s_income_`i' = s`i'iearn + s`i'ipena+ s`i'issdi+ s`i'isret+ s`i'iunwc + s`i'igxfr
	gen s_log_income_`i' = log(s_income_`i'+10)
}

* whether have pension
gen s1peninc=.
forvalues i = 1(1)14{
	gen s_pension_`i' = 1 if s`i'peninc == 1
	replace s_pension_`i' = 0 if s_pension_`i'!=1
}



/******************** Module for respondent's own health ****************/

* drink 
* information on drinking starts from wave 3
forvalues i =3(1)14{
	rename r`i'drinkd drink_days_`i'
}
forvalues i= 3(1)14{
	rename r`i'drinkn drink_num_`i'
}

/* health */
* for the section of health, we extract three variables: self-reported health, BMI, and days of hospital stays 
* sum of conditions ever had
forvalues i = 1(1)14{
	rename r`i'conde cond_`i'
}

*self-reported health
forvalues i=1(1)14{
	rename r`i'shlt health_`i'
}

*hospital stays 
/* note that the hospital stays in wave 1 was asked for the previous 12 months, while in the other waves were asked for the previous 2 years, meanwhile, the information of # of hospital stays starts from wave 2, therefore, we clean the data from wave 2 */

forvalues i = 2(1)14{
	rename r`i'hosp hosp_`i'
	rename r`i'hsptim hosp_time_`i'
}

* BMI
* after wave 8, we have the physical measured BMI, thus, we use the measured one to replace the self-reported one. 
forvalues i = 1(1)7{
	rename r`i'bmi bmi_`i'
}
forvalues i = 8(1)14{
	rename r`i'pmbmi bmi_`i'
}

* number of doctor visits (as in wave 1, the variable is measured by the numbers in the previous 12 months, therefore, we start the measurement from wave 2)

forvalues i = 2(1)14{
	rename r`i'doctim doct_time_`i'
}

*probability of living over 75 (life expectancy; scale of 1-100) (dropped)

forvalues i = 1(1)14{
	rename r`i'liv75 liv75_`i'
}

* living in the nurse home (asked from wave 3)
forvalues i = 3(1)14{
	rename r`i'nhmliv nurs_home_`i'
}


/* mental health */
* we apply CESD scores imputed in RAND data to measure mental health 
/* note: the cesd scores starts from wave 2 */
forvalues i = 2(1)14{
	rename r`i'cesd cesd_`i'
}

/*health expenditures (out of pocket) (wave 3 - 14) */
forvalues i = 3(1)14{
	rename r`i'oopmd expend_`i'
}



/* several specific health conditions */
* we take the variable of whether the respondent ever had such symptom 

* high blood pressure (note if someone ever had high BP, which might lead her to take the antihypertentive drugs, and therefore, reports no high BP symptom in the wave, but we regard that the respondent ever had high BP)

* diabetes 

* cancer 

* lung disease 

* heart problem 

* stroke 

* psychological problem 

* arthritis 
forvalues i = 1(1)14{
	rename r`i'hibpe high_bp_`i' 
	rename r`i'diabe diabetes_`i'
	rename r`i'cancre cancer_`i'
	rename r`i'lunge lung_`i'
	rename r`i'hearte heart_`i'
	rename r`i'stroke stroke_`i'
	rename r`i'psyche psych_`i'
	recode psych_`i' (1 3 5 =1)(2 4 = 0)
	rename r`i'arthre arthritis_`i'
}

/* ever smoking */

forvalues i = 1(1)14{
	rename r`i'smokev smoke_`i'
}

* proxy rating of respondent memory (ordered variable)
forvalues i = 2(1)14{
	rename r`i'prmem proxy_memory_`i'
	replace proxy_memory_`i' =. if proxy_memory_`i'<1 | proxy_memory_`i' > 5
}
/*ADL */
forvalues i = 2(1)14{
	rename r`i'adla adl_`i'
}
/***************** health for spouse ********************/ 

/*drink */
forvalues i =3(1)14{
	rename s`i'drinkd s_drink_days_`i'
}
forvalues i= 3(1)14{
	rename s`i'drinkn s_drink_num_`i'
}

* sum of conditions ever had 
forvalues i = 1(1)14{
	rename s`i'conde s_cond_`i'
}

*self-reported health
forvalues i=1(1)14{
	rename s`i'shlt s_health_`i'
}

*hospital stays 
/* note that the hospital stays in wave 1 was asked for the previous 12 months, while in the other waves were asked for the previous 2 years, meanwhile, the information of # of hospital stays starts from wave 2, therefore, we clean the data from wave 2 */

forvalues i = 2(1)14{
	rename s`i'hosp s_hosp_`i'
	rename s`i'hsptim s_hosp_time_`i'
}

* BMI
* after wave 8, we have the physical measured BMI, thus, we use the measured one to replace the self-reported one. 
forvalues i = 1(1)7{
	rename s`i'bmi s_bmi_`i'
}
forvalues i = 8(1)14{
	rename s`i'pmbmi s_bmi_`i'
}


* number of doctor visits (as in wave 1, the variable is measured by the numbers in the previous 12 months, therefore, we start the measurement from wave 2)

forvalues i = 2(1)14{
	rename s`i'doctim s_doct_time_`i'
}

*probability of living over 75 (life expectancy; scale of 1-100)
forvalues i = 1(1)14{
	rename s`i'liv75 s_liv75_`i'
}

* living in the nurse home (asked from wave 3)
forvalues i = 3(1)14{
	rename s`i'nhmliv s_nurs_home_`i'
}

/* mental health */
* we apply CESD scores imputed in RAND data to measure mental health 
/* note: the cesd scores starts from wave 2 */
forvalues i = 2(1)14{
	rename s`i'cesd s_cesd_`i'
}


/*health expenditures (out of pocket) (wave 3 - 14) */
forvalues i = 3(1)14{
	rename s`i'oopmd s_expend_`i'
}


/* several specific health conditions */
* we take the variable of whether the respondent ever had such symptom 

* high blood pressure (note if someone ever had high BP, which might lead her to take the antihypertentive drugs, and therefore, reports no high BP symptom in the wave, but we regard that the respondent ever had high BP)

* diabetes 

* cancer 

* lung disease 

* heart problem 

* stroke 

* psychological problem 

* arthritis 
forvalues i = 1(1)14{
	rename s`i'hibpe s_high_bp_`i' 
	rename s`i'diabe s_diabetes_`i'
	rename s`i'cancre s_cancer_`i'
	rename s`i'lunge s_lung_`i'
	rename s`i'hearte s_heart_`i'
	rename s`i'stroke s_stroke_`i'
	rename s`i'psyche s_psych_`i'
	recode s_psych_`i' (1 3 5 =1)(2 4 = 0)
	rename s`i'arthre s_arthritis_`i'
}


/* ever smoking */

forvalues i = 1(1)14{
	rename s`i'smokev s_smoke_`i'
}

* proxy rating of respondent memory (ordered variable)
forvalues i = 2(1)14{
	rename s`i'prmem s_proxy_memory_`i'
	replace s_proxy_memory_`i' =. if s_proxy_memory_`i'<1 | s_proxy_memory_`i' > 5
}

/*ADL */
forvalues i = 2(1)14{
	rename s`i'adla s_adl_`i'
}


****** Household Information *************************************
/* asset in every wave */
gen h3atotb = h3atota /* in wave 3, there's no total wealth including second residence, so we use the wealth without second residence in replace of it. However, as our calculation starts from wave 4, this doesn't affect our results. */
forvalues i=1(1)14{
	rename h`i'atotb asset_`i'
}

gen h3ahoub =.
gen h3amrtb =.
forvalues i=1(1)14{
foreach w in h`i'ahous  h`i'ahoub  h`i'arles  h`i'atran  h`i'absns  h`i'aira  h`i'astck  h`i'achck  h`i'acd  h`i'abond  h`i'aothr  h`i'amort h`i'ahmln  h`i'adebt  h`i'amrtb{
replace `w' = 0 if `w' ==.
}
}
forvalues i=1(1)14{
	replace asset_`i' = h`i'ahous + h`i'ahoub + h`i'arles + h`i'atran + h`i'absns + h`i'aira + h`i'astck + h`i'achck + h`i'acd + h`i'abond + h`i'aothr - h`i'amort - h`i'ahmln - h`i'adebt - h`i'amrtb if asset_`i'==.
	*replace asset_`i'=0 if asset_`i'<0
}

/* number of living children */
forvalues i = 1(1)14{
	rename h`i'child child_liv_`i'
	recode child_liv_`i' (.=0)
}

* household capital income 
forvalues i = 1(1)14{
	rename h`i'icap capital_income_`i'
	recode capital_income_`i' (. .u=0)

}

* total household income 
forvalues i = 1(1)14{
	gen fam_inc_`i' = income_`i' + s_income_`i' + capital_income_`i'
	gen log_fam_inc_`i' = log(fam_inc_`i' + 10)
}

* tabulate 
preserve 

 use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\Data\randhrsfam1992_2014v1_STATA\randhrsfamk1992_2014v1.dta" , clear 

* use "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/Data/randhrsfam1992_2014v1_STATA/randhrsfamk1992_2014v1.dta", clear 

/* the data structure is: an individual stands for a kid */
/* information on kids: number of shared children */
* first, we need to clean the variable indicating whether the kid is alive at every wave: 
forvalues i = 1(1)12{
	rename k`i'alive alive_`i'
}
gen child_dead = 0
forvalues i = 1(1)12{
	replace child_dead = 1 if alive_`i'==0 
}

gen child_dead_wave = .
forvalues i = 1(1)12{
	replace child_dead_wave= `i' if alive_`i'==0 & child_dead_wave == . 
}


* the relationship between respondent and the children (best guess value)
recode karel (2 6 7 12 = 0), gen(child) /* own child */ 
/* in the dynamic model, the number of shared children would vary in different waves, as some kids might not be alive in the waves */
keep hhidpn child  child_dead child_dead_wave
/*transform from long form to wide form */
bysort hhidpn: gen kid_id = _n

* ever experienced kids' death 


reshape wide child child_dead child_dead_wave, i(hhidpn) j(kid_id)
* calculate the number of shared children 
gen child0 =0
forvalues i = 1(1)33{
	replace child0 = child0 + child`i' if child`i' != .
}
rename child0 num_shared_child
gen child_dead = 0
gen child_dead_wave = .
forvalues i = 1(1)33{
	replace child_dead = 1 if child_dead`i' == 1
	replace child_dead_wave = child_dead_wave`i' if child_dead_wave`i'!=. & child_dead_wave`i'< child_dead_wave
}


keep hhidpn num_shared_child child_dead child_dead_wave

 save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\rand_num_child.dta", replace 
* save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/rand_num_child.dta", replace

restore
 merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\rand_num_child.dta" 
* merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/rand_num_child.dta"
 
 /* 7822 cases from the master data doesn't contain information on kids, we assume they do not have shared kids */
replace num_shared_child = 0 if _merge==1
drop if _m == 2 /* 2 cases dropped */ 
drop _m 

/* merge with the marital history data */ 
 merge 1:1 hhid pn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\mar_history_revised.dta" 
*merge 1:1 hhid pn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/mar_history_revised.dta" 


drop if _m == 1 | _m == 2 /*1403 cases dropped */
drop _m 



/* marital related variables */ 
* times of divorces
forvalues i =1(1)14{
	rename r`i'mdiv times_div_`i'
}

* times of divorces for spouse 
forvalues i =1(1)14{
	rename s`i'mdiv s_times_div_`i'
}

*length of longest marriage 

forvalues i = 1(1)14{
	rename r`i'mlen marriage_len_`i'
}

*length of longest marriage for spouse 
forvalues i = 1(1)14{
	rename s`i'mlen s_marriage_len_`i'
}

*length of current marriage 
forvalues i = 1(1)14{
	rename r`i'mcurln cur_mar_len_`i'
}

*length of current marriage for spouse 
forvalues i = 1(1)14{
	rename s`i'mcurln s_cur_mar_len_`i'
}

* total times of marriage 
forvalues i = 1(1)14{
	rename r`i'mrct times_mar_`i'
}

forvalues i = 1(1)14{
	rename s`i'mrct s_times_mar_`i'
}

* times widowed 
forvalues i =1(1)14{
	rename r`i'mwid times_wid_`i'
}

* times widowed for spouse
forvalues i =1(1)14{
	rename s`i'mwid s_times_wid_`i'
}


/* covariates */ 

*respondents' information 

forvalues i=1(1)14{
	gen log_asset_`i' = log(asset_`i'+100)
	gen log_caital_income_`i' = log(capital_income_`i' + 10)
}
 *sum survival_year divorce ragender birth_year race educ birth_place migration religion rank_income_4 drink_days_4 drink_num_4 cond_4 health_4 hosp_4 hosp_time_4 cesd_4 s_birth_year_4 s_race_4 s_educ_4 s_birth_place_4 s_migration_4 s_religion_4 s_rank_income_4 s_drink_days_4 s_drink_num_4 s_cond_4 s_health_4 s_hosp_4 s_hosp_time_4 s_cesd_4  rank_asset_4 child_liv_4 num_shared_child times_div_4 s_times_div_4 times_mar_4 s_times_mar_4 times_wid_4 s_times_wid_4 marriage_len_4 s_marriage_len_4 cur_mar_len_4 rawtsamp

/* find the information on wave 4 (wave of 1998) */ 
* locate wave 4
/* generating time-varying variables with the three-previous wave average */
gen drink_days_1 =.
gen drink_days_2 =.
gen drink_num_1 = .
gen drink_num_2 = .
gen hosp_time_1 = .
gen hosp_1 = .
gen doct_time_1 = . 
gen nurs_home_1 = .
gen nurs_home_2 = .
gen cesd_1 = .
gen proxy_memory_1 = .
gen expend_1 = .
gen expend_2 = . 
gen adl_1 = .
gen s_drink_days_1 =.
gen s_drink_days_2 =.
gen s_drink_num_1 = .
gen s_drink_num_2 = .
gen s_hosp_time_1 = .
gen s_hosp_1 = .
gen s_doct_time_1 = . 
gen s_nurs_home_1 = .
gen s_nurs_home_2 = .
gen s_cesd_1 = .
gen s_proxy_memory_1 = .
gen s_expend_1 = .
gen s_expend_2 = . 
gen s_adl_1 = .

forvalues i = 4(1)14{
	local j = `i' - 1
	local k = `i' - 2
	local m = `i' - 3
	egen mean_drink_days_`i' = rowmean(drink_days_`j' drink_days_`k' drink_days_`m') 
	egen mean_drink_num_`i' = rowmean(drink_num_`j' drink_num_`k' drink_num_`m') 
	egen max_cond_`i' = rowmax(cond_`j' cond_`k' cond_`m') 
	egen mean_health_`i' = rowmean(health_`j' health_`k' health_`m') 
	egen mean_hosp_time_`i' = rowmean(hosp_time_`j' hosp_time_`k' hosp_time_`m') 
	egen max_hosp_visit_`i' = rowmax(hosp_`j' hosp_`k' hosp_`m') 
	egen mean_bmi_`i' =rowmean(bmi_`j' bmi_`k' bmi_`m')
	egen max_nurs_home_`i'= rowmax(nurs_home_`j' nurs_home_`k' nurs_home_`m')
	egen mean_cesd_`i' = rowmean(cesd_`j' cesd_`k' cesd_`m')
	egen mean_expend_`i'= rowmean(expend_`j' expend_`k' expend_`m')
	egen max_high_bp_`i' = rowmax(high_bp_`j' high_bp_`k' high_bp_`m')
	egen max_diabetes_`i' = rowmax(diabetes_`j' diabetes_`k' diabetes_`m')
	egen max_cancer_`i' = rowmax(cancer_`j' cancer_`k' cancer_`m')
	egen max_lung_`i' = rowmax(lung_`j' lung_`k' lung_`m')
	egen max_heart_`i'= rowmax(heart_`j' heart_`k' heart_`m')
	egen max_stroke_`i' = rowmax(stroke_`j' stroke_`k' stroke_`m')
	egen max_psych_`i' = rowmax(psych_`j' psych_`k' psych_`m')
	egen max_arthritis_`i' = rowmax(arthritis_`j' arthritis_`k' arthritis_`m')
	egen max_smoke_`i' = rowmax(smoke_`j' smoke_`k' smoke_`m')
	egen min_proxy_memory_`i' = rowmin(proxy_memory_`j' proxy_memory_`k' proxy_memory_`m')
	egen max_adl_`i'= rowmax(adl_`j' adl_`k' adl_`m')
	egen s_mean_drink_days_`i' = rowmean(s_drink_days_`j' s_drink_days_`k' s_drink_days_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_drink_num_`i' = rowmean(s_drink_num_`j' s_drink_num_`k' s_drink_num_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_cond_`i' = rowmax(s_cond_`j' s_cond_`k' s_cond_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_health_`i' = rowmean(s_health_`j' s_health_`k' health_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_hosp_time_`i' = rowmean(s_hosp_time_`j' s_hosp_time_`k' s_hosp_time_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_hosp_visit_`i' = rowmax(s_hosp_`j' s_hosp_`k' s_hosp_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_bmi_`i' =rowmean(s_bmi_`j' s_bmi_`k' s_bmi_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_nurs_home_`i'= rowmax(s_nurs_home_`j' s_nurs_home_`k' s_nurs_home_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_cesd_`i' = rowmean(s_cesd_`j' s_cesd_`k' s_cesd_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_mean_expend_`i'= rowmean(s_expend_`j' s_expend_`k' s_expend_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_high_bp_`i' = rowmax(s_high_bp_`j' s_high_bp_`k' s_high_bp_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_diabetes_`i' = rowmax(s_diabetes_`j' s_diabetes_`k' s_diabetes_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_cancer_`i' = rowmax(s_cancer_`j' s_cancer_`k' s_cancer_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_lung_`i' = rowmax(s_lung_`j' s_lung_`k' s_lung_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_heart_`i'= rowmax(s_heart_`j' s_heart_`k' s_heart_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_stroke_`i' = rowmax(s_stroke_`j' s_stroke_`k' s_stroke_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_psych_`i' = rowmax(s_psych_`j' s_psych_`k' s_psych_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_arthritis_`i' = rowmax(s_arthritis_`j' s_arthritis_`k' s_arthritis_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_smoke_`i' = rowmax(s_smoke_`j' s_smoke_`k' s_smoke_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_min_proxy_memory_`i' = rowmin(s_proxy_memory_`j' s_proxy_memory_`k' s_proxy_memory_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen s_max_adl_`i'= rowmax(s_adl_`j' s_adl_`k' s_adl_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen mean_log_fam_inc_`i' = rowmean(log_fam_inc_`i' log_fam_inc_`j' log_fam_inc_`k')
}


/* cases selection */ 

keep if r4mstat == 1 /* 27913 cases deleted, 13819 cases remaining */ 
drop if birth_year >= 1948 /* set as older than 50 */
forvalues i = 4(1)14{
	drop if r`i'mstat ==8
} /* drop the individuals with never married */

forvalues i = 4(1)14{
	drop if s`i'gender == ragender 
} /* drop same-sex marriage */ 


/* treatment variable: whether widowed after the observed year*/
forvalues i = 4(1)14{
	gen widow_`i' = 0 
}

replace widow_4 = 1 if r4mstat == 7
/* calculate the widowhood age */
gen year_widow = 1998 if widow_4 == 1
 
forvalues i = 5(1)14{
	local j =  `i' - 1
	replace widow_`i' = 1 if widow_`j' == 1 
	replace widow_`i'  = 1 if r`i'mstat == 7
}

*find the newly-widowed 
forvalues i = 5(1)14{
	local j = `i' - 1
	replace year_widow = `i'* 2 + 1990 if widow_`i' == 1 & widow_`j' == 0
}


/* find out the cases widowed and again remarried */ 
forvalues i = 4(1)14{
	gen wid_remar_`i' = 0
}


forvalues i = 5(1)14{
	local j = `i' - 1
	replace wid_remar_`i' = 1 if wid_remar_`j' == 1
	replace wid_remar_`i' = 1 if widow_`i' == 1 & (r`i'mstat == 1 | r`i'mstat == 2 | r`i'mstat == 3) 
}
*drop all the cases which widowed and then remarried 
forvalues i = 5(1)14{
	drop if wid_remar_`i' == 1
}

/* status of divorce */ 


forvalues i = 4(1)14{
	gen divorce_`i' = 0
}
replace divorce_4 = 1 if r4mstat== 4 | r4mstat == 5 | r4mstat == 6 

forvalues i = 5(1)14{
	local j = `i' - 1
	replace divorce_`i' = 1 if divorce_`j' == 1
	replace divorce_`i' = 1 if r`i'mstat == 4 | r`i'mstat == 5| r`i'mstat == 6 
}

/* generate the variables for time */ 

gen failure_year = death_year - int_year_1998 /* we define the survival time as the year interval between the observation start year (1998) and the death year*/ 

gen censor_year =  rexityr - int_year_1998 if death_year == . 
replace censor_year = int_year_2018 - int_year_1998 if rexityr == . 
replace censor_year = 2018 - int_year_1998 if censor_year == . /* right censored */ 

gen censor_age =  rexityr - birth_year if death_year == . 
replace censor_age = int_year_2018 - birth_year if rexityr == . 
replace censor_age = 2018 - birth_year if censor_age == . /* right censored */ 

gen survival_year = min(failure_year, censor_year)
gen right_censor = 0 if failure_year <= censor_year
replace right_censor = 1 if right_censor != 0
drop if survival_year< 0

gen right_censor_alive = 1 if right_censor == 1 & rexityr == .
replace right_censor_alive = 0 if right_censor_alive != 1

gen right_censor_exit = 1 if right_censor == 1 & rexityr != .
replace right_censor_exit = 0 if right_censor_exit != 1

gen death = 1 if death_year != .

gen year_gap = survival_year - year_widow + int_year_1998 /* robustness check for shorter/longer period effects of widowhood */

* generate a wave-specific gap between widowhood and interview year 
forvalues i = 4(1)14{
	gen year_since_widow_`i' = int_year_wave_`i' - year_widow if int_year_wave_`i'>= year_widow
}
replace death = 0 if death != 1

gen wid = widow_14
tab wid death if male == 1
tab wid death if male == 0 

* tabulate for each subgroups 

/* generate the variables indicating homogamy */ 
/*educational homogamy */ 
gen educ_homo = 1 if educ == s_educ_4
replace educ_homo = 0 if educ_homo != 1


/* religion homogamy */ 
gen reli_homo = 1 if religion == s_religion_4
replace reli_homo = 0 if reli_homo != 1


/* geographical homogamy */ 
gen geo_homo = 1 if birth_place == s_birth_place_4
replace geo_homo = 0 if geo_homo != 1

/* asset missing */ 
gen asset_miss = 1 if asset_4 ==.
replace asset_miss = 0 if asset_miss != 1
replace asset_4 = 0 if asset_4 == . 

/* age difference */ 
gen age_diff = abs(birth_year - s_birth_year_4)

* education 
recode educ (1 2 3 .m = 0)(4 5 = 1), gen(college)
recode s_educ_4 (1 2 3 .m= 0)(4 5 = 1), gen(s_college)

* college homogamy
gen college_homo = 1 if college == 1 & s_college == 1
replace college_homo = 2 if college == 1 & s_college == 0
replace college_homo = 3 if college == 0 & s_college == 1
replace college_homo = 4 if college == 0 & s_college == 0

* race 
gen white = 1 if race == 1
replace white = 0 if white != 1

gen black = 1 if race == 2
replace black = 0 if black != 1

* racial homogamy 
gen racial_homo = 1 if white == 1 & s_white_4 == 1
replace racial_homo = 2 if white == 1 & s_white_4 != 1
replace racial_homo = 3 if white == 0 & s_white_4 == 1
replace racial_homo = 4 if white == 0 & s_white_4 == 0
* after selecting the cases, we can generate the percentile variables
/* transfer income to income rank */ 
forvalues i = 1(1)14{
	xtile rank_income_`i' = income_`i', nq(5)
	xtile s_rank_income_`i' = s_income_`i', nq(5)
	xtile rank_fam_inc_`i' = fam_inc_`i', nq(3)
	xtile rank_asset_`i' = asset_`i', nq(3)
}

forvalues i = 4(1)14{
	local j = `i' - 1
	local k = `i' - 2
	local m = `i' - 3
	egen min_income_rank_`i' = rowmin(rank_income_`m' rank_income_`j' rank_income_`k') 
	egen min_asset_rank_`i' = rowmin(rank_asset_`m' rank_asset_`j' rank_asset_`k')
	egen s_min_income_rank_`i' = rowmin(s_rank_income_`m' s_rank_income_`j' s_rank_income_`k') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen mean_log_fam_income_`i' = rowmean(log_fam_inc_`m' log_fam_inc_`j' log_fam_inc_`k')
	egen mean_log_asset_`i' = rowmean(log_asset_`m' log_asset_`j' log_asset_`k')
	egen mean_log_income_`i' = rowmean(log_income_`m' log_income_`j' log_income_`k')
	egen s_mean_log_income_`i' = rowmean(s_log_income_`m' s_log_income_`j' s_log_income_`k') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
	egen max_pension_`i' = rowmax(pension_`j' pension_`k' pension_`m')
	egen s_max_pension_`i' = rowmax(s_pension_`j' s_pension_`k' s_pension_`m') if s`j'hhidpn == s`k'hhidpn & s`j'hhidpn == s`m'hhidpn
}
/* income homogamy */
gen inc_homo = 1 if rank_income_4 == s_rank_income_4
replace inc_homo = 0 if inc_homo != 1
gen inc_miss = 1 if rank_income_4 == . 
replace inc_miss = 0 if inc_miss != 1

*asset distribution 
gen asset_class = 1 if rank_asset_4 == 1
replace asset_class = 2 if rank_asset_4 == 2
replace asset_class = 3 if rank_asset_4 == 3


* descriptive table 
*education level
foreach i in college college_homo white black racial_homo asset_class{
	sort `i'
	by `i': tab death wid if male == 1
	by `i': tab death wid if male == 0 
}


/* if survival year = ., it means right censored */ 
/* transfer death year into death wave */
recode death_year (1998 1999 = 4)(2000 2001 = 5)(2002 2003 = 6)(2004 2005 = 7)(2006 2007 =8)(2008 2009 = 9)(2010 2011 = 10)(2012 2013 = 11)(2014 2015 = 12)(2016 2017 = 13)(2018 2019 =14), gen(death_wave)
recode rexityr (1998 1999 = 4)(2000 2001 = 5)(2002 2003 = 6)(2004 2005 = 7)(2006 2007 =8)(2008 2009 = 9)(2010 2011 = 10)(2012 2013 = 11)(2014 2015 = 12)(2016 2017 = 13)(2018 2019 =14), gen(exit_wave)
replace death_wave = exit_wave if death_wave == . & exit_wave != .y & exit_wave != .z
replace death_wave = 14 if right_censor == 1

forvalues i = 4(1)14{
	gen wave_start_`i' = 2*`i'+ 1990
	gen wave_end_`i' = 2*`i' + 1991 
}


forvalues i = 4(1)14{
	gen wave_start_age_`i' = wave_start_`i' - birth_year
	gen wave_end_age_`i' = wave_end_`i' - birth_year
}
* age at current wave 
forvalues i = 4(1)14{
	gen wave_age_`i' = int_year_wave_`i' - birth_year
	gen s_wave_age_`i' = int_year_wave_`i' - s_birth_year_4
}

gen divorce_censor = 0
forvalues i = 4(1)14{
	replace divorce_censor = 1 if divorce_`i'== 1
}

save "C:\Users\pangu\OneDrive - Nexus365\thesis\survival\widowhood\main_all_variables_wide.dta", replace




clear 
clear matrix
clear mata 
set maxvar 32767
use "C:\Users\pangu\OneDrive - Nexus365\thesis\survival\widowhood\main_all_variables_wide.dta" , clear

* tabulate the variables for description 
* demographic information

recode raedyrs (.m = 0), gen(eduyrs)

recode s4edyrs (.m .v = 0), gen(s_eduyrs)


g s_race = s_race_4
gen s_white = 1 if s_race == 1
replace s_white = 0 if s_race != 1
gen s_black = 1 if s_race == 2
replace s_black = 0 if s_race != 2 

gen protestant = 1 if religion == 1
replace protestant= 0 if religion != 1
g catholic = 1 if religion == 2
replace catholic = 0 if religion != 2

gen s_religion = s_religion_4
gen s_protestant = 1 if s_religion == 1
replace s_protestant= 0 if s_religion != 1
g s_catholic = 1 if s_religion == 2
replace s_catholic = 0 if s_religion != 2

g s_migration = s_migration_4

* generate widowhood age and death age 

gen widow_age = year_widow - birthyear 

gen death_age = death_year - birthyear 


* generate widow wave
gen widow_dummy = widow_14 
gen widow_wave = .
forvalues i = 5(1)14{
	local j = `i' - 1
	replace widow_wave = `i' if widow_`i' == 1 & widow_`j' == 0
}
* the dummy for death exists 

* years between widowhood and death 
gen year_diff = death_age - widow_age

* year of retirement 
*check the first retirement status 

* we regard both partial retirement and complete retirement as retired. 

forvalues i =1(1)14{
	recode r`i'sayret (1 2 = 1), gen(retire_`i')
}

*ever retired
gen ever_retired = 0
forvalues i = 1(1)14{
	replace ever_retired = 1 if retire_`i' == 1 & ever_retired!=1
}
*check the retirement year (for the first retirement)
gen retire_year = .
forvalues i = 1(1)14{
	replace retire_year = r`i'retyr if retire_`i'== 1 & r`i'retyr != .
}

*generate year diff between retirement year

gen yrs_diff_retire = death_year - retire_year 
* generate the year difference between current year and retirement year*/

forvalues i = 4(1)14{
	gen year_since_retire_`i' = int_year_wave_`i' - retire_year if int_year_wave_`i'>= retire_year
	replace year_since_retire_`i' = 0 if int_year_wave_`i'< retire_year
}
*labor force participation 
forvalues i = 1(1)14{
	gen labor_force_`i' = r`i'inlbrf
	replace labor_force_`i'= 0 if labor_force_`i'==.
}


*ever retired
forvalues i =1(1)14{
	recode s`i'sayret (1 2 = 1), gen(s_retire_`i')
}


gen s_ever_retired = 0
forvalues i = 1(1)14{
	replace s_ever_retired = 1 if s_retire_`i' == 1 & s_ever_retired!=1
}

*check the retirement year (for the first retirement)
gen s_retire_year = .
forvalues i = 1(1)14{
	replace s_retire_year = s`i'retyr if s_retire_`i'== 1 & s_retire_year==.
}
gen s_yrs_diff_retire = year_widow - s_retire_year 
replace s_yrs_diff_retire = . if s_yrs_diff_retire<0
forvalues i = 4(1)14{
	gen s_year_since_retire_`i' = int_year_wave_`i' - s_retire_year if int_year_wave_`i'>= s_retire_year
	replace s_year_since_retire_`i' = 0 if int_year_wave_`i'< s_retire_year
}
*labor force participation 
forvalues i = 1(1)14{
	gen s_labor_force_`i' = s`i'inlbrf
	replace s_labor_force_`i'= 0 if s_labor_force_`i'==.
}


* health status 
* drink days 
egen mean_drink_days = rowmean(mean_drink_days_4 mean_drink_days_5 mean_drink_days_6 mean_drink_days_7 mean_drink_days_8 mean_drink_days_9 mean_drink_days_10 mean_drink_days_11 mean_drink_days_12 mean_drink_days_13 mean_drink_days_14)

gen mean_drink_days_before_death = .
forvalues i = 4(1)14{
	replace mean_drink_days_before_death = mean_drink_days_`i' if death_wave == `i'
}

egen s_mean_drink_days = rowmean(s_mean_drink_days_4 s_mean_drink_days_5 s_mean_drink_days_6 s_mean_drink_days_7 s_mean_drink_days_8 s_mean_drink_days_9 s_mean_drink_days_10 s_mean_drink_days_11 s_mean_drink_days_12 s_mean_drink_days_13 s_mean_drink_days_14)

gen s_mean_drink_days_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_drink_days_before_widow = s_mean_drink_days_`i' if widow_wave == `i'
}

egen mean_drink_num = rowmean(mean_drink_num_4 mean_drink_num_5 mean_drink_num_6 mean_drink_num_7 mean_drink_num_8 mean_drink_num_9 mean_drink_num_10 mean_drink_num_11 mean_drink_num_12 mean_drink_num_13 mean_drink_num_14)

gen mean_drink_num_before_death = .
forvalues i = 4(1)14{
	replace mean_drink_num_before_death = mean_drink_num_`i' if death_wave == `i'
}

egen s_mean_drink_num = rowmean(s_mean_drink_num_4 s_mean_drink_num_5 s_mean_drink_num_6 s_mean_drink_num_7 s_mean_drink_num_8 s_mean_drink_num_9 s_mean_drink_num_10 s_mean_drink_num_11 s_mean_drink_num_12 s_mean_drink_num_13 s_mean_drink_num_14)

gen s_mean_drink_num_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_drink_num_before_widow = s_mean_drink_num_`i' if widow_wave == `i'
}

egen max_cond = rowmax(max_cond_4 max_cond_5 max_cond_6 max_cond_7 max_cond_8 max_cond_9 max_cond_10 max_cond_11 max_cond_12 max_cond_13 max_cond_14)

gen max_cond_before_death = .
forvalues i = 4(1)14{
	replace max_cond_before_death = max_cond_`i' if death_wave == `i'
}

egen s_max_cond = rowmax(s_max_cond_4 s_max_cond_5 s_max_cond_6 s_max_cond_7 s_max_cond_8 s_max_cond_9 s_max_cond_10 s_max_cond_11 s_max_cond_12 s_max_cond_13 s_max_cond_14)

gen s_max_cond_before_widow = .
forvalues i = 4(1)14{
	replace s_max_cond_before_widow = s_max_cond_`i' if widow_wave == `i'
}

egen mean_health = rowmean(mean_health_4 mean_health_5 mean_health_6 mean_health_7 mean_health_8 mean_health_9 mean_health_10 mean_health_11 mean_health_12 mean_health_13 mean_health_14)

gen mean_health_before_death = .
forvalues i = 4(1)14{
	replace mean_health_before_death = mean_health_`i' if death_wave == `i'
}

egen s_mean_health = rowmean(s_mean_health_4 s_mean_health_5 s_mean_health_6 s_mean_health_7 s_mean_health_8 s_mean_health_9 s_mean_health_10 s_mean_health_11 s_mean_health_12 s_mean_health_13 s_mean_health_14)

gen s_mean_health_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_health_before_widow = s_mean_health_`i' if widow_wave == `i'
}

egen mean_hosp_time = rowmean(mean_hosp_time_4 mean_hosp_time_5 mean_hosp_time_6 mean_hosp_time_7 mean_hosp_time_8 mean_hosp_time_9 mean_hosp_time_10 mean_hosp_time_11 mean_hosp_time_12 mean_hosp_time_13 mean_hosp_time_14)

gen mean_hosp_time_before_death = .
forvalues i = 4(1)14{
	replace mean_hosp_time_before_death = mean_hosp_time_`i' if death_wave == `i'
}

egen s_mean_hosp_time = rowmean(s_mean_hosp_time_4 s_mean_hosp_time_5 s_mean_hosp_time_6 s_mean_hosp_time_7 s_mean_hosp_time_8 s_mean_hosp_time_9 s_mean_hosp_time_10 s_mean_hosp_time_11 s_mean_hosp_time_12 s_mean_hosp_time_13 s_mean_hosp_time_14)

gen s_mean_hosp_time_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_hosp_time_before_widow = s_mean_hosp_time_`i' if widow_wave == `i'
}

egen max_hosp_visit = rowmax(max_hosp_visit_4 max_hosp_visit_5 max_hosp_visit_6 max_hosp_visit_7 max_hosp_visit_8 max_hosp_visit_9 max_hosp_visit_10 max_hosp_visit_11 max_hosp_visit_12 max_hosp_visit_13 max_hosp_visit_14)

gen max_hosp_visit_before_death = .
forvalues i = 4(1)14{
	replace max_hosp_visit_before_death = max_hosp_visit_`i' if death_wave == `i'
}

egen s_max_hosp_visit = rowmax(s_max_hosp_visit_4 s_max_hosp_visit_5 s_max_hosp_visit_6 s_max_hosp_visit_7 s_max_hosp_visit_8 s_max_hosp_visit_9 s_max_hosp_visit_10 s_max_hosp_visit_11 s_max_hosp_visit_12 s_max_hosp_visit_13 s_max_hosp_visit_14)

gen s_max_hosp_visit_before_widow = .
forvalues i = 4(1)14{
	replace s_max_hosp_visit_before_widow = s_max_hosp_visit_`i' if widow_wave == `i'
}

egen max_nurs_home = rowmax(max_nurs_home_4 max_nurs_home_5 max_nurs_home_6 max_nurs_home_7 max_nurs_home_8 max_nurs_home_9 max_nurs_home_10 max_nurs_home_11 max_nurs_home_12 max_nurs_home_13 max_nurs_home_14)

gen max_nurs_home_before_death = .
forvalues i = 4(1)14{
	replace max_nurs_home_before_death = max_nurs_home_`i' if death_wave == `i'
}

egen s_max_nurs_home = rowmax(s_max_nurs_home_4 s_max_nurs_home_5 s_max_nurs_home_6 s_max_nurs_home_7 s_max_nurs_home_8 s_max_nurs_home_9 s_max_nurs_home_10 s_max_nurs_home_11 s_max_nurs_home_12 s_max_nurs_home_13 s_max_nurs_home_14)

gen s_max_nurs_home_before_widow = .
forvalues i = 4(1)14{
	replace s_max_nurs_home_before_widow = s_max_nurs_home_`i' if widow_wave == `i'
}

egen mean_bmi = rowmean(mean_bmi_4 mean_bmi_5 mean_bmi_6 mean_bmi_7 mean_bmi_8 mean_bmi_9 mean_bmi_10 mean_bmi_11 mean_bmi_12 mean_bmi_13 mean_bmi_14)

gen mean_bmi_before_death = .
forvalues i = 4(1)14{
	replace mean_bmi_before_death = mean_bmi_`i' if death_wave == `i'
}

egen s_mean_bmi = rowmean(s_mean_bmi_4 s_mean_bmi_5 s_mean_bmi_6 s_mean_bmi_7 s_mean_bmi_8 s_mean_bmi_9 s_mean_bmi_10 s_mean_bmi_11 s_mean_bmi_12 s_mean_bmi_13 s_mean_bmi_14)

gen s_mean_bmi_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_bmi_before_widow = s_mean_bmi_`i' if widow_wave == `i'
}

egen mean_cesd = rowmean(mean_cesd_4 mean_cesd_5 mean_cesd_6 mean_cesd_7 mean_cesd_8 mean_cesd_9 mean_cesd_10 mean_cesd_11 mean_cesd_12 mean_cesd_13 mean_cesd_14)

gen mean_cesd_before_death = .
forvalues i = 4(1)14{
	replace mean_cesd_before_death = mean_cesd_`i' if death_wave == `i'
}

egen s_mean_cesd = rowmean(s_mean_cesd_4 s_mean_cesd_5 s_mean_cesd_6 s_mean_cesd_7 s_mean_cesd_8 s_mean_cesd_9 s_mean_cesd_10 s_mean_cesd_11 s_mean_cesd_12 s_mean_cesd_13 s_mean_cesd_14)

gen s_mean_cesd_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_cesd_before_widow = s_mean_cesd_`i' if widow_wave == `i'
}

egen mean_expend = rowmean(mean_expend_4 mean_expend_5 mean_expend_6 mean_expend_7 mean_expend_8 mean_expend_9 mean_expend_10 mean_expend_11 mean_expend_12 mean_expend_13 mean_expend_14)

gen mean_expend_before_death = .
forvalues i = 4(1)14{
	replace mean_expend_before_death = mean_expend_`i' if death_wave == `i'
}

egen s_mean_expend = rowmean(s_mean_expend_4 s_mean_expend_5 s_mean_expend_6 s_mean_expend_7 s_mean_expend_8 s_mean_expend_9 s_mean_expend_10 s_mean_expend_11 s_mean_expend_12 s_mean_expend_13 s_mean_expend_14)

gen s_mean_expend_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_expend_before_widow = s_mean_expend_`i' if widow_wave == `i'
}

egen max_high_bp = rowmax(max_high_bp_4 max_high_bp_5 max_high_bp_6 max_high_bp_7 max_high_bp_8 max_high_bp_9 max_high_bp_10 max_high_bp_11 max_high_bp_12 max_high_bp_13 max_high_bp_14)

gen max_high_bp_before_death = .
forvalues i = 4(1)14{
	replace max_high_bp_before_death = max_high_bp_`i' if death_wave == `i'
}

egen s_max_high_bp = rowmax(s_max_high_bp_4 s_max_high_bp_5 s_max_high_bp_6 s_max_high_bp_7 s_max_high_bp_8 s_max_high_bp_9 s_max_high_bp_10 s_max_high_bp_11 s_max_high_bp_12 s_max_high_bp_13 s_max_high_bp_14)

gen s_max_high_bp_before_widow = .
forvalues i = 4(1)14{
	replace s_max_high_bp_before_widow = s_max_high_bp_`i' if widow_wave == `i'
}

egen max_diabetes = rowmax(max_diabetes_4 max_diabetes_5 max_diabetes_6 max_diabetes_7 max_diabetes_8 max_diabetes_9 max_diabetes_10 max_diabetes_11 max_diabetes_12 max_diabetes_13 max_diabetes_14)

gen max_diabetes_before_death = .
forvalues i = 4(1)14{
	replace max_diabetes_before_death = max_diabetes_`i' if death_wave == `i'
}

egen s_max_diabetes = rowmax(s_max_diabetes_4 s_max_diabetes_5 s_max_diabetes_6 s_max_diabetes_7 s_max_diabetes_8 s_max_diabetes_9 s_max_diabetes_10 s_max_diabetes_11 s_max_diabetes_12 s_max_diabetes_13 s_max_diabetes_14)

gen s_max_diabetes_before_widow = .
forvalues i = 4(1)14{
	replace s_max_diabetes_before_widow = s_max_diabetes_`i' if widow_wave == `i'
}

egen max_cancer = rowmax(max_cancer_4 max_cancer_5 max_cancer_6 max_cancer_7 max_cancer_8 max_cancer_9 max_cancer_10 max_cancer_11 max_cancer_12 max_cancer_13 max_cancer_14)

gen max_cancer_before_death = .
forvalues i = 4(1)14{
	replace max_cancer_before_death = max_cancer_`i' if death_wave == `i'
}

egen s_max_cancer = rowmax(s_max_cancer_4 s_max_cancer_5 s_max_cancer_6 s_max_cancer_7 s_max_cancer_8 s_max_cancer_9 s_max_cancer_10 s_max_cancer_11 s_max_cancer_12 s_max_cancer_13 s_max_cancer_14)

gen s_max_cancer_before_widow = .
forvalues i = 4(1)14{
	replace s_max_cancer_before_widow = s_max_cancer_`i' if widow_wave == `i'
}

egen max_lung = rowmax(max_lung_4 max_lung_5 max_lung_6 max_lung_7 max_lung_8 max_lung_9 max_lung_10 max_lung_11 max_lung_12 max_lung_13 max_lung_14)

gen max_lung_before_death = .
forvalues i = 4(1)14{
	replace max_lung_before_death = max_lung_`i' if death_wave == `i'
}

egen s_max_lung = rowmax(s_max_lung_4 s_max_lung_5 s_max_lung_6 s_max_lung_7 s_max_lung_8 s_max_lung_9 s_max_lung_10 s_max_lung_11 s_max_lung_12 s_max_lung_13 s_max_lung_14)

gen s_max_lung_before_widow = .
forvalues i = 4(1)14{
	replace s_max_lung_before_widow = s_max_lung_`i' if widow_wave == `i'
}

egen max_heart = rowmax(max_heart_4 max_heart_5 max_heart_6 max_heart_7 max_heart_8 max_heart_9 max_heart_10 max_heart_11 max_heart_12 max_heart_13 max_heart_14)

gen max_heart_before_death = .
forvalues i = 4(1)14{
	replace max_heart_before_death = max_heart_`i' if death_wave == `i'
}

egen s_max_heart = rowmax(s_max_heart_4 s_max_heart_5 s_max_heart_6 s_max_heart_7 s_max_heart_8 s_max_heart_9 s_max_heart_10 s_max_heart_11 s_max_heart_12 s_max_heart_13 s_max_heart_14)

gen s_max_heart_before_widow = .
forvalues i = 4(1)14{
	replace s_max_heart_before_widow = s_max_heart_`i' if widow_wave == `i'
}

egen max_stroke = rowmax(max_stroke_4 max_stroke_5 max_stroke_6 max_stroke_7 max_stroke_8 max_stroke_9 max_stroke_10 max_stroke_11 max_stroke_12 max_stroke_13 max_stroke_14)

gen max_stroke_before_death = .
forvalues i = 4(1)14{
	replace max_stroke_before_death = max_stroke_`i' if death_wave == `i'
}

egen s_max_stroke = rowmax(s_max_stroke_4 s_max_stroke_5 s_max_stroke_6 s_max_stroke_7 s_max_stroke_8 s_max_stroke_9 s_max_stroke_10 s_max_stroke_11 s_max_stroke_12 s_max_stroke_13 s_max_stroke_14)

gen s_max_stroke_before_widow = .
forvalues i = 4(1)14{
	replace s_max_stroke_before_widow = s_max_stroke_`i' if widow_wave == `i'
}


* calculate total number of symptoms 
gen num_symptoms = max_high_bp + max_diabetes + max_cancer + max_lung + max_heart + max_stroke 
gen s_num_symptoms = s_max_high_bp + s_max_diabetes + s_max_cancer + s_max_lung + s_max_heart + s_max_stroke 

gen num_symptoms_before_death = max_high_bp_before_death + max_diabetes_before_death + max_cancer_before_death + max_lung_before_death + max_heart_before_death + max_stroke_before_death
 
gen s_num_symptoms_before_widow = s_max_high_bp_before_widow + s_max_diabetes_before_widow + s_max_cancer_before_widow + s_max_lung_before_widow + s_max_heart_before_widow + s_max_stroke_before_widow

egen max_psych = rowmax(max_psych_4 max_psych_5 max_psych_6 max_psych_7 max_psych_8 max_psych_9 max_psych_10 max_psych_11 max_psych_12 max_psych_13 max_psych_14)

gen max_psych_before_death = .
forvalues i = 4(1)14{
	replace max_psych_before_death = max_psych_`i' if death_wave == `i'
}

egen s_max_psych = rowmax(s_max_psych_4 s_max_psych_5 s_max_psych_6 s_max_psych_7 s_max_psych_8 s_max_psych_9 s_max_psych_10 s_max_psych_11 s_max_psych_12 s_max_psych_13 s_max_psych_14)

gen s_max_psych_before_widow = .
forvalues i = 4(1)14{
	replace s_max_psych_before_widow = s_max_psych_`i' if widow_wave == `i'
}

egen max_smoke = rowmax(max_smoke_4 max_smoke_5 max_smoke_6 max_smoke_7 max_smoke_8 max_smoke_9 max_smoke_10 max_smoke_11 max_smoke_12 max_smoke_13 max_smoke_14)

gen max_smoke_before_death = .
forvalues i = 4(1)14{
	replace max_smoke_before_death = max_smoke_`i' if death_wave == `i'
}

egen s_max_smoke = rowmax(s_max_smoke_4 s_max_smoke_5 s_max_smoke_6 s_max_smoke_7 s_max_smoke_8 s_max_smoke_9 s_max_smoke_10 s_max_smoke_11 s_max_smoke_12 s_max_smoke_13 s_max_smoke_14)

gen s_max_smoke_before_widow = .
forvalues i = 4(1)14{
	replace s_max_smoke_before_widow = s_max_smoke_`i' if widow_wave == `i'
}

egen mean_proxy_memory = rowmean(min_proxy_memory_4 min_proxy_memory_5 min_proxy_memory_6 min_proxy_memory_7 min_proxy_memory_8 min_proxy_memory_9 min_proxy_memory_10 min_proxy_memory_11 min_proxy_memory_12 min_proxy_memory_13 min_proxy_memory_14)

gen min_proxy_memory_before_death = .
forvalues i = 4(1)14{
	replace min_proxy_memory_before_death = min_proxy_memory_`i' if death_wave == `i'
}

egen s_mean_proxy_memory = rowmean(s_min_proxy_memory_4 s_min_proxy_memory_5 s_min_proxy_memory_6 s_min_proxy_memory_7 s_min_proxy_memory_8 s_min_proxy_memory_9 s_min_proxy_memory_10 s_min_proxy_memory_11 s_min_proxy_memory_12 s_min_proxy_memory_13 s_min_proxy_memory_14)

gen s_min_proxy_memory_before_widow = .
forvalues i = 4(1)14{
	replace s_min_proxy_memory_before_widow = s_min_proxy_memory_`i' if widow_wave == `i'
}

* SES
egen min_income_rank = rowmin(min_income_rank_4 min_income_rank_5 min_income_rank_6 min_income_rank_7 min_income_rank_8 min_income_rank_9 min_income_rank_10 min_income_rank_11 min_income_rank_12 min_income_rank_13 min_income_rank_14)

gen min_income_rank_before_death = .
forvalues i = 4(1)14{
	replace min_income_rank_before_death = min_income_rank_`i' if death_wave == `i'
}

egen s_min_income_rank = rowmin(s_min_income_rank_4 s_min_income_rank_5 s_min_income_rank_6 s_min_income_rank_7 s_min_income_rank_8 s_min_income_rank_9 s_min_income_rank_10 s_min_income_rank_11 s_min_income_rank_12 s_min_income_rank_13 s_min_income_rank_14)

gen s_min_income_rank_before_widow = .
forvalues i = 4(1)14{
	replace s_min_income_rank_before_widow = s_min_income_rank_`i' if widow_wave == `i'
}

egen min_asset_rank = rowmin(min_asset_rank_4 min_asset_rank_5 min_asset_rank_6 min_asset_rank_7 min_asset_rank_8 min_asset_rank_9 min_asset_rank_10 min_asset_rank_11 min_asset_rank_12 min_asset_rank_13 min_asset_rank_14)

gen min_asset_rank_before_death = .
forvalues i = 4(1)14{
	replace min_asset_rank_before_death = min_asset_rank_`i' if death_wave == `i'
}

egen mean_log_asset = rowmean(mean_log_asset_4 mean_log_asset_5 mean_log_asset_6 mean_log_asset_7 mean_log_asset_8 mean_log_asset_9 mean_log_asset_10 mean_log_asset_11 mean_log_asset_12 mean_log_asset_13 mean_log_asset_14)

gen mean_log_asset_before_death = .
forvalues i = 4(1)14{
	replace mean_log_asset_before_death = mean_log_asset_`i' if death_wave == `i'
}

egen mean_log_family_inc = rowmean(mean_log_fam_income_4 mean_log_fam_income_5 mean_log_fam_income_6 mean_log_fam_income_7 mean_log_fam_income_8 mean_log_fam_income_9 mean_log_fam_income_10 mean_log_fam_income_11 mean_log_fam_income_12 mean_log_fam_income_13 mean_log_fam_income_14)

gen mean_log_family_inc_before_death = .
forvalues i = 4(1)14{
	replace mean_log_family_inc_before_death = mean_log_fam_income_`i' if death_wave == `i'
}


egen mean_log_income = rowmean(mean_log_income_4 mean_log_income_5 mean_log_income_6 mean_log_income_7 mean_log_income_8 mean_log_income_9 mean_log_income_10 mean_log_income_11 mean_log_income_12 mean_log_income_13 mean_log_income_14)

gen mean_log_income_before_death = .
forvalues i = 4(1)14{
	replace mean_log_income_before_death = mean_log_income_`i' if death_wave == `i'
}

egen s_mean_log_income = rowmean(s_mean_log_income_4 s_mean_log_income_5 s_mean_log_income_6 s_mean_log_income_7 s_mean_log_income_8 s_mean_log_income_9 s_mean_log_income_10 s_mean_log_income_11 s_mean_log_income_12 s_mean_log_income_13 s_mean_log_income_14)

gen s_mean_log_income_before_widow = .
forvalues i = 4(1)14{
	replace s_mean_log_income_before_widow = s_mean_log_income_`i' if widow_wave == `i'
}

egen max_pension = rowmax(max_pension_4 max_pension_5 max_pension_6 max_pension_7 max_pension_8 max_pension_9 max_pension_10 max_pension_11 max_pension_12 max_pension_13 max_pension_14)

gen max_pension_before_death = .
forvalues i = 4(1)14{
	replace max_pension_before_death = max_pension_`i' if death_wave == `i'
}

egen s_max_pension = rowmax(s_max_pension_4 s_max_pension_5 s_max_pension_6 s_max_pension_7 s_max_pension_8 s_max_pension_9 s_max_pension_10 s_max_pension_11 s_max_pension_12 s_max_pension_13 s_max_pension_14)

gen s_max_pension_before_widow = .
forvalues i = 4(1)14{
	replace s_max_pension_before_widow = s_max_pension_`i' if widow_wave == `i'
}

egen mean_labor_force = rowmean(labor_force_4 labor_force_5 labor_force_6 labor_force_7 labor_force_8 labor_force_9 labor_force_10 labor_force_11 labor_force_12 labor_force_13 labor_force_14)

gen labor_force_before_death = .
forvalues i = 4(1)14{
	replace labor_force_before_death = labor_force_`i' if death_wave == `i'
}

egen s_mean_labor_force = rowmean(s_labor_force_4 s_labor_force_5 s_labor_force_6 s_labor_force_7 s_labor_force_8 s_labor_force_9 s_labor_force_10 s_labor_force_11 s_labor_force_12 s_labor_force_13 s_labor_force_14)

gen s_labor_force_before_death = .
forvalues i = 4(1)14{
	replace s_labor_force_before_death = s_labor_force_`i' if widow_wave == `i'
}
**** information on children 
gen child_dead_year = .

forvalues i = 1(1)12{
	replace child_dead_year = `i'*2 + 1990 if child_dead_wave == `i'
}
* calculate the year between child death and mortaility, child death and widowhood 

gen yrs_diff_child_dead = death_year - child_dead_year
gen s_yrs_diff_child_dead = year_widow - child_dead_year

*years since lossing children 
forvalues i = 4(1)14{
	gen year_since_child_dead_`i' = int_year_wave_`i' - child_dead_year if int_year_wave_`i'>= retire_year
	replace year_since_child_dead_`i' = 0 if int_year_wave_`i'< child_dead_year
}


* average number of living children 

egen child_liv = rowmean(child_liv_4 child_liv_5 child_liv_6 child_liv_7 child_liv_8 child_liv_9 child_liv_10 child_liv_11 child_liv_12 child_liv_13 child_liv_14)

gen child_liv_before_death = .
forvalues i = 4(1)14{
	replace child_liv_before_death = child_liv_`i' if death_wave == `i'
}

gen s_child_liv_before_death = .
forvalues i = 4(1)14{
	replace s_child_liv_before_death = child_liv_`i' if widow_wave == `i'
}

* weight 
forvalues i = 1(1)13{
	rename r`i'wtresp wt_`i'
}
g wt_14 = wt_13

gen group = 1 if male==1 & widow_dummy == 1
replace group = 2 if male == 1 & widow_dummy == 0
replace group = 3 if male == 0 & widow_dummy== 1
replace group = 4 if male == 0 & widow_dummy == 0

recode racial_homo (2 3 4 . = 0)

sort group
by group: sum eduyrs s_eduyrs white black s_white s_black migration s_migration protestant catholic s_protestant s_catholic death death_age widow_age year_diff ever_retired yrs_diff_retire s_ever_retired s_yrs_diff_retire child_dead yrs_diff_child_dead s_yrs_diff_child_dead child_liv child_liv_before_death s_child_liv_before_death mean_drink_days mean_drink_days_before_death s_mean_drink_days s_mean_drink_days_before_widow mean_drink_num mean_drink_num_before_death s_mean_drink_num s_mean_drink_num_before_widow max_hosp_visit max_hosp_visit_before_death s_max_hosp_visit s_max_hosp_visit_before_widow mean_hosp_time mean_hosp_time_before_death s_mean_hosp_time s_mean_hosp_time_before_widow max_nurs_home max_nurs_home_before_death s_max_nurs_home s_max_nurs_home_before_widow mean_bmi mean_bmi_before_death s_mean_bmi s_mean_bmi_before_widow mean_cesd mean_cesd_before_death s_mean_cesd s_mean_cesd_before_widow mean_expend mean_expend_before_death s_mean_expend s_mean_expend_before_widow num_symptoms num_symptoms_before_death s_num_symptoms s_num_symptoms_before_widow max_psych max_psych_before_death s_max_psych s_max_psych_before_widow mean_proxy_memory min_proxy_memory_before_death s_mean_proxy_memory s_min_proxy_memory_before_widow mean_log_asset mean_log_asset_before_death mean_log_family_inc mean_log_family_inc_before_death mean_log_income mean_log_income_before_death s_mean_log_income s_mean_log_income_before_widow max_pension max_pension_before_death s_max_pension s_max_pension_before_widow mean_labor_force labor_force_before_death s_mean_labor_force s_labor_force_before_death num_shared_child age_diff educ_homo racial_homo reli_homo geo_homo inc_homo 

/* select the variables and prepare for transferring into long form */
g s_birth_year = s_birth_year_4 

*before transferring to the long form, we need to first drop the generated average values for time-varying variables when conducting the descriptive statistics. 
drop child_liv mean_drink_days s_mean_drink_days mean_drink_num s_mean_drink_num max_hosp_visit s_max_hosp_visit mean_hosp_time s_mean_hosp_time max_nurs_home s_max_nurs_home mean_bmi s_mean_bmi mean_cesd s_mean_cesd mean_expend s_mean_expend max_high_bp s_max_high_bp max_diabetes s_max_diabetes max_cancer s_max_cancer max_lung s_max_lung max_heart s_max_heart max_stroke s_max_stroke max_psych s_max_psych mean_proxy_memory s_mean_proxy_memory mean_log_asset mean_log_family_inc mean_log_income s_mean_log_income max_pension s_max_pension mean_labor_force s_mean_labor_force 

g child_dead_bf_widow = 1 if (child_dead_year <= year_widow & year_widow != .)
replace child_dead_bf_widow = 0 if child_dead_bf_widow != 1

keep hhidpn group widow* male birth_year white black protestant catholic migration eduyrs s_eduyrs s_white s_black s_protestant s_catholic s_migration s_birth_year year_gap year_diff death death_age death_year death_wave exit_wave failure_year censor_year censor_age age_diff right_censor  right_censor_alive right_censor_exit wave_start* wave_end* ever_retired s_ever_retired child_dead child_dead_year year_widow child_dead_bf_widow child_liv* mean_drink_days* s_mean_drink_days* mean_drink_num* s_mean_drink_num* max_hosp_visit* s_max_hosp_visit* mean_hosp_time* s_mean_hosp_time* max_nurs_home* s_max_nurs_home* mean_bmi* s_mean_bmi* mean_cesd* s_mean_cesd* mean_expend* s_mean_expend* max_high_bp* s_max_high_bp* max_diabetes* s_max_diabetes* max_cancer* s_max_cancer* max_lung* s_max_lung* max_heart* s_max_heart* max_stroke* s_max_stroke* max_psych* s_max_psych* min_proxy_memory* s_min_proxy_memory* mean_log_asset* mean_log_fam_income* mean_log_income* s_mean_log_income* max_pension* s_max_pension* labor_force* s_labor_force* num_shared_child  educ_homo racial_homo reli_homo geo_homo inc_homo wt* wid_remar* divorce* widow_wave year_since_widow* year_since_retire* year_since_child_dead* college_homo college s_college s_year_since_retire* wave_age* s_wave_age* asset_class


reshape long widow_ wave_start_ wave_end_ child_liv_ mean_drink_days_ s_mean_drink_days_ mean_drink_num_ s_mean_drink_num_ max_hosp_visit_ s_max_hosp_visit_ mean_hosp_time_ s_mean_hosp_time_ max_nurs_home_ s_max_nurs_home_ mean_bmi_ s_mean_bmi_ mean_cesd_ s_mean_cesd_ mean_expend_ s_mean_expend_ max_high_bp_ s_max_high_bp_ max_diabetes_ s_max_diabetes_ max_cancer_ s_max_cancer_ max_lung_ s_max_lung_ max_heart_ s_max_heart_ max_stroke_ s_max_stroke_ max_psych_ s_max_psych_ min_proxy_memory_ s_min_proxy_memory_ mean_log_asset_ mean_log_fam_income_ mean_log_income_ s_mean_log_income_ max_pension_ s_max_pension_ labor_force_ s_labor_force_ wt_ wave_start_age_ wave_end_age_ wid_remar_  divorce_ year_since_widow_ year_since_retire_ year_since_child_dead_ s_year_since_retire_ wave_age_ s_wave_age_, i(hhidpn) j(wave)



foreach i in widow wave_start wave_end child_liv mean_drink_days s_mean_drink_days mean_drink_num s_mean_drink_num max_hosp_visit s_max_hosp_visit mean_hosp_time s_mean_hosp_time max_nurs_home s_max_nurs_home mean_bmi s_mean_bmi mean_cesd s_mean_cesd mean_expend s_mean_expend max_high_bp s_max_high_bp max_diabetes s_max_diabetes max_cancer s_max_cancer max_lung s_max_lung max_heart s_max_heart max_stroke s_max_stroke max_psych s_max_psych min_proxy_memory s_min_proxy_memory mean_log_asset mean_log_fam_income mean_log_income s_mean_log_income max_pension s_max_pension labor_force s_labor_force wt wave_start_age wave_end_age wid_remar divorce year_since_widow year_since_retire year_since_child_dead s_year_since_retire wave_age s_wave_age{
	rename `i'_ `i'
	recode `i' (.u = .)
}
drop if wave < 4
drop if divorce == 1
gen dead = 1 if death_wave <= wave 
replace dead = 0 if death_wave >wave 
drop if dead[_n] == 1 & dead[_n-1] == 1 & hhidpn[_n] == hhidpn[_n-1]
replace year_since_child_dead = 0 if year_since_child_dead == .
replace year_since_retire = 0 if year_since_retire == .
replace year_since_widow = 0 if year_since_widow == . 
replace s_year_since_retire = 0 if s_year_since_retire == .

save "C:\Users\pangu\OneDrive - Nexus365\thesis\survival\widowhood\stata_preparation.dta", replace

preserve
keep if male == 1
save "C:\Users\pangu\OneDrive - Nexus365\thesis\survival\widowhood\male.dta", replace

restore

preserve
keep if male == 0
save "C:\Users\pangu\OneDrive - Nexus365\thesis\survival\widowhood\female.dta", replace

restore

/*
/* select the variables, prepare for transferring into long form 
keep hhidpn male birth_year race educ birth_place migration year_gap religion s_race* s_educ* s_birth_place* s_migration* s_religion* s_birth_year* mean_drink_days*  mean_drink_num* max_cond* mean_health* mean_hosp_time* max_hosp_visit* min_income_rank* min_asset_rank* mean_bmi* max_nurs_home* mean_cesd* mean_expend* max_high_bp* max_diabetes* max_cancer* max_lung* max_heart* max_stroke* max_psych* max_arthritis* max_smoke* min_proxy_memory* max_adl* s_mean_drink_days*  s_mean_drink_num* s_max_cond* s_mean_health* s_mean_hosp_time* s_max_hosp_visit* s_min_income_rank*  s_mean_bmi* s_max_nurs_home* s_mean_cesd* s_mean_expend* s_max_high_bp* s_max_diabetes* s_max_cancer* s_max_lung* s_max_heart* s_max_stroke* s_max_psych* s_max_arthritis* s_max_smoke* s_min_proxy_memory* s_max_adl* widow_* wid_remar_* divorce_* wave_start_age* wave_end_age* educ_homo reli_homo racial_homo inc_homo geo_homo death_year death_wave exit_wave failure_year censor_year censor_age age_diff right_censor divorce_censor right_censor_alive right_censor_exit wave_start* wave_end* cesd* adl* health* drink_days* drink_num* 

drop s_birth_place_1 s_birth_place_2 s_birth_place_3 s_birth_year_1 s_birth_year_2 s_birth_year_3 s_race_1 s_race_2 s_race_3 s_educ_1 s_educ_2 s_educ_3 s_migration_1 s_migration_2 s_migration_3 s_religion_1 s_religion_2 s_religion_3 

reshape long s_birth_year_ s_race_ s_educ_ s_birth_place_ s_migration_ s_religion_  mean_drink_days_  mean_drink_num_ max_cond_ mean_health_ mean_hosp_time_ max_hosp_visit_ min_income_rank_ min_asset_rank_ mean_bmi_ max_nurs_home_ mean_cesd_ mean_expend_ max_high_bp_ max_diabetes_ max_cancer_ max_lung_ max_heart_ max_stroke_ max_psych_ max_arthritis_ max_smoke_ min_proxy_memory_ max_adl_ s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_ s_min_income_rank_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_min_proxy_memory_ s_max_adl_ mean_drink_days1_  mean_drink_num1_ max_cond1_ mean_health1_ mean_hosp_time1_ max_hosp_visit1_ min_income_rank1_ min_asset_rank1_ mean_bmi1_ max_nurs_home1_ mean_cesd1_ mean_expend1_ max_high_bp1_ max_diabetes1_ max_cancer1_ max_lung1_ max_heart1_ max_stroke1_ max_psych1_ max_arthritis1_ max_smoke1_ min_proxy_memory1_ max_adl1_ s_mean_drink_days1_  s_mean_drink_num1_ s_max_cond1_ s_mean_health1_ s_mean_hosp_time1_ s_max_hosp_visit1_ s_min_income_rank1_  s_mean_bmi1_ s_max_nurs_home1_ s_mean_cesd1_ s_mean_expend1_ s_max_high_bp1_ s_max_diabetes1_ s_max_cancer1_ s_max_lung1_ s_max_heart1_ s_max_stroke1_ s_max_psych1_ s_max_arthritis1_ s_max_smoke1_ s_min_proxy_memory1_ s_max_adl1_ mean_drink_days2_  mean_drink_num2_ max_cond2_ mean_health2_ mean_hosp_time2_ max_hosp_visit2_ min_income_rank2_ min_asset_rank2_ mean_bmi2_ max_nurs_home2_ mean_cesd2_ mean_expend2_ max_high_bp2_ max_diabetes2_ max_cancer2_ max_lung2_ max_heart2_ max_stroke2_ max_psych2_ max_arthritis2_ max_smoke2_ min_proxy_memory2_ max_adl2_ s_mean_drink_days2_  s_mean_drink_num2_ s_max_cond2_ s_mean_health2_ s_mean_hosp_time2_ s_max_hosp_visit2_ s_min_income_rank2_  s_mean_bmi2_ s_max_nurs_home2_ s_mean_cesd2_ s_mean_expend2_ s_max_high_bp2_ s_max_diabetes2_ s_max_cancer2_ s_max_lung2_ s_max_heart2_ s_max_stroke2_ s_max_psych2_ s_max_arthritis2_ s_max_smoke2_ s_min_proxy_memory2_ s_max_adl2_ widow_ wid_remar_ divorce_ wave_start_age_ wave_end_age_ wave_start_ wave_end_ cesd_ adl_ health_ drink_days_ drink_num_ , i(hhidpn) j(wave)

drop if wave < 4
rename wid_remar_ wid_remar 
rename divorce_ divorce
drop if divorce == 1
gen dead = 1 if death_wave <= wave 
replace dead = 0 if death_wave >wave 
drop if dead[_n] == 1 & dead[_n-1] == 1 & hhidpn[_n] == hhidpn[_n-1]


/************************ STEP II: INITIAL LOGIT MODEL TO PREDICT PROPENSITY OF WIDOWHOOD ******************************************************************/
/* generate the ordered variables */ 
gen white = 1 if race == 1
replace white = 0 if race!= 1
gen black = 1 if race == 2
replace black = 0 if race != 2

gen s_white = 1 if s_race == 1
replace s_white = 0 if s_race != 1
gen s_black = 1 if s_race == 2
replace s_black = 0 if s_race != 2 

gen protestant = 1 if religion == 1
replace protestant= 0 if religion != 1
g catholic = 1 if religion == 2
replace catholic = 0 if religion != 2

gen s_protestant = 1 if s_religion == 1
replace s_protestant= 0 if s_religion != 1
g s_catholic = 1 if s_religion == 2
replace s_catholic = 0 if s_religion != 2

/* predict widowhood */
/* adding the category of missing */

replace s_race = s_race[_n-1] if (s_race==.|s_race==.u|s_race==.v|s_race ==.m)& hhidpn[_n] == hhidpn[_n-1]
replace s_educ = s_educ[_n-1] if (s_educ==.|s_educ==.u|s_educ==.v|s_educ ==.m)& hhidpn[_n] == hhidpn[_n-1]
replace s_migration = s_migration[_n-1] if (s_migration==.|s_migration==.u|s_migration==.v|s_migration ==.m)& hhidpn[_n] == hhidpn[_n-1]
replace s_birth_year = s_birth_year[_n-1] if (s_birth_year==.|s_birth_year==.u|s_birth_year==.v|s_birth_year ==.m)& hhidpn[_n] == hhidpn[_n-1]
rename wave_start_ wave_start
g s_wave_start_age = wave_start - s_birth_year 

/** ROBUSTNESS CHECKPOINT I: CASES WIDOWED AND REMARRIED ***/
tab wid_remar 
save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\main.dta", replace /* all the analysis below, except robustness check II for multiple imputation, uses this dataset */
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/main.dta", replace
preserve 

sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

xi: logit widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction.dta", replace
 save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction.dta", replace 
restore 
preserve 
keep if male == 1
sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male.dta", replace /* long form data */ 

/* save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male.dta", replace */

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male.dta", replace
keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity.dta", replace

restore

preserve 
keep if male == 0
sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female.dta", replace

 save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female.dta", replace 


keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity.dta", replace
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity.dta", replace 


restore 

/* save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female.dta", replace */



/**************************************** STEP III: ADJUSTING AGE WEIGHTS FOR TREATMENT　AND CONTROL FOR BOX PLOT***********************************/
preserve 


keep hhidpn wave widow_ wave_start_age_

reshape wide widow_  wave_start_age_, i(hhidpn) j(wave)

g ever_widowed = 0
g widow_wave = .
forvalues i = 4(1)14{
	replace ever_widowed = 1 if widow_`i'== 1
}

forvalues i =5(1)14{
	local j = `i' - 1
	replace widow_wave= `i' if widow_`i' == 1 & widow_`j' == 0
}



*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity.dta"
merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity.dta"
drop _m 

*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity.dta"
merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity.dta"
drop _m 

g widow_age = . 
g widow_ps = .

forvalues i = 5(1)14{
	replace widow_age = wave_start_age_`i' if widow_wave == `i'
	replace widow_ps = propensity_widow_`i' if widow_wave == `i'
}


/*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\ever_widowed.dta", replace
graph box widow_ps, title("weighted treatment group") ytitle("propensity scores")
graph save "Graph" "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot.gph", replace
graph export "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot.png", as(png) name("Graph") replace */

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/ever_widowed.dta", replace
graph box widow_ps, title("weighted treatment group") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot.png", as(png) name("Graph") replace 
collapse (count) hhidpn, by(widow_age)
rename hhidpn count_treatment
drop if widow_age == . 
egen sum_treatment = total(count_treatment)
gen percent_treatment = count_treatment/sum_treatment 

* save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment.dta" , replace 


save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment.dta", replace 
restore 

/* control group age adjustments */

 preserve 
clear
* use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male.dta", clear

 use "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male.dta", clear



* merge 1:1 hhidpn  wave using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female.dta"
merge 1:1 hhidpn  wave using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female.dta"
drop _m 

keep if widow_ == 0
 *save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long.dta" , replace 

  save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long.dta" , replace 

collapse (count) hhidpn, by(wave_start_age_)


rename wave_start_age_ widow_age 
drop if widow_age == .

rename hhidpn count_control

egen sum_control = total(count_control)
g percent_control = count_control/sum_control  

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_control.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_control.dta" , replace 

*merge 1:1 widow_age using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment.dta"

merge 1:1 widow_age using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment.dta"
drop _m 

/* generate the weights */ 

rename widow_age wave_start_age_
gen age_weights = percent_control/percent_treatment

replace age_weights = 0.00001 if age_weights == . 

/*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis.dta" , replace 

merge 1:n wave_start_age_ using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long.dta"  */

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis.dta" , replace 

merge 1:n wave_start_age_ using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long.dta" 


drop _m 
* calculating the propensity distribution for control group */ 
/*graph box propensity_widow [aw= age_weights], title("weighted control group") ytitle("propensity scores")
graph save "Graph" "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\control_propensity_box_plot.gph", replace
graph export "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\control_propensity_box_plot.png", as(png) name("Graph") replace *

graph box propensity_widow [aw= age_weights], title("weighted control group") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot.png", as(png) name("Graph") replace 





restore 

/******************************* ROBUSTNESS CHECK II: MISSING DATA TREATMENT *************************************/

* Method 1: exclude all data with missing values for logit regression 

preserve 

foreach i in widow male race educ migration protestant catholic s_race s_educ s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s | `i' == .u 

}



sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 


xi: logit widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_exclude_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_exclude_missing.dta", replace

restore 

preserve 
keep if male == 1

foreach i in widow male race educ migration protestant catholic s_race s_educ s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s | `i' == .u 

}


sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_exclude_missing.dta", replace /* long form data */ 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_exclude_missing.dta", replace /* long form data */
keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity_exclude_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity_exclude_missing.dta", replace

/* save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male.dta", replace */

restore

preserve 

keep if male == 0

foreach i in widow male race educ migration protestant catholic s_race s_educ s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s| `i' == .u 

}

sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age 


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_exclude_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_exclude_missing.dta", replace

keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity_exclude_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity_exclude_missing.dta", replace

restore 
preserve

foreach i in widow male race educ migration protestant catholic s_race s_educ s_migration s_protestant s_catholic s_mean_drink_days_  s_mean_drink_num_ s_max_cond_ s_mean_health_ s_mean_hosp_time_ s_max_hosp_visit_  s_mean_bmi_ s_max_nurs_home_ s_mean_cesd_ s_mean_expend_ s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo wave_start_age s_wave_start_age {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s | `i' == .u 

}


keep hhidpn wave widow_ wave_start_age_

reshape wide widow_  wave_start_age_, i(hhidpn) j(wave)

g ever_widowed = 0
g widow_wave = .
forvalues i = 4(1)14{
	replace ever_widowed = 1 if widow_`i'== 1
}

forvalues i =5(1)14{
	local j = `i' - 1
	replace widow_wave= `i' if widow_`i' == 1 & widow_`j' == 0
}



*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity_exclude_missing.dta"
merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity_exclude_missing.dta"

drop _m 

*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity_exclude_missing.dta"
merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity_exclude_missing.dta"
drop _m 

g widow_age = . 
g widow_ps = .

forvalues i = 5(1)14{
	replace widow_age = wave_start_age_`i' if widow_wave == `i'
	replace widow_ps = propensity_widow_`i' if widow_wave == `i'
}


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\ever_widowed_exclude_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/ever_widowed_exclude_missing.dta", replace

/*graph box widow_ps, title("weighted treatment group/exclude missing") ytitle("propensity scores")
graph save "Graph" "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot_exclude_missing.gph", replace
graph export "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot_exclude_missing.png", as(png) name("Graph") replace */

graph box widow_ps, title("weighted treatment group/exclude missing") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot_exclude_missing.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot_exclude_missing.png", as(png) name("Graph") replace 


collapse (count) hhidpn, by(widow_age)
rename hhidpn count_treatment
drop if widow_age == . 
egen sum_treatment = total(count_treatment)
gen percent_treatment = count_treatment/sum_treatment 

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment_exclude_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment_exclude_missing.dta" , replace 

/*save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/ever_widowed.dta", replace */
restore 

/* control group age adjustments */

 preserve 
clear
/*use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_exclude_missing.dta", clear
merge 1:1 hhidpn  wave using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_exclude_missing.dta"*/

use "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_exclude_missing.dta", clear
merge 1:1 hhidpn  wave using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_exclude_missing.dta"
drop _m 

keep if widow_ == 0
*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long_exclude_missing.dta" , replace 
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long_exclude_missing.dta" , replace 


collapse (count) hhidpn, by(wave_start_age_)


rename wave_start_age_ widow_age 
drop if widow_age == .

rename hhidpn count_control

egen sum_control = total(count_control)
g percent_control = count_control/sum_control  

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_control_exclude_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_control_exclude_missing.dta" , replace 

*merge 1:1 widow_age using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment_exclude_missing.dta"

merge 1:1 widow_age using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment_exclude_missing.dta"

drop _m 

/* generate the weights */ 

rename widow_age wave_start_age_
gen age_weights = percent_control/percent_treatment

replace age_weights = 0.00001 if age_weights == . 

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_exclude_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_exclude_missing.dta" , replace 

*merge 1:n wave_start_age_ using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long_exclude_missing.dta" 
merge 1:n wave_start_age_ using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long_exclude_missing.dta" 

drop _m 
* calculating the propensity distribution for control group */ 
/*graph box propensity_widow [aw= age_weights], title("weighted control group/exclude missing") ytitle("propensity scores")
graph save "Graph" "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\control_propensity_box_plot_exclude_missing.gph", replace
graph export "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\control_propensity_box_plot_exclude_missing.png", as(png) name("Graph") replace *

graph box propensity_widow [aw= age_weights], title("weighted control group/exclude missing") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot_exclude_missing.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot_exclude_missing.png", as(png) name("Graph") replace


restore 



* Method 2: Impute missing data 

* for categorical variables, we impute the missing data with a missing category 

* note: we could not impute all the cases, we only impute the cases who just widowed (widow[t] == 1 & widow[t-1] == 0) or not widowed (we can't impute those who widowed for a period) so that we could make predictions on their propensity. 

preserve 
keep if hhidpn[_n] == hhidpn[_n-1] & widow_[_n] == 1 & widow_[_n-1]== 1

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widowed_multiple_years.dta",replace 
*restore 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widowed_multiple_years.dta",replace 
restore 

* preserve 
drop if hhidpn[_n] == hhidpn[_n-1] & widow_[_n] == 1 & widow_[_n-1]== 1

foreach i of var wave_start_age s_wave_start_age {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s | `i' == .u 

}

foreach i of var male race educ migration protestant catholic s_race s_educ s_migration s_protestant s_catholic  s_max_hosp_visit_  s_max_nurs_home_  s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo {
	replace `i' = 999 if `i' ==. 
	replace `i' = 999 if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s |`i' == .u
}

recode  s_max_hosp_visit_  s_max_nurs_home_  s_max_high_bp_ s_max_diabetes_ s_max_cancer_ s_max_lung_ s_max_heart_ s_max_stroke_ s_max_psych_ s_max_arthritis_ s_max_smoke_ s_max_adl_ educ_homo reli_homo racial_homo inc_homo geo_homo (.=999)

replace s_mean_health_ = int(s_mean_health_)
/* for continous variabls: 
s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_
* multiple imputation for continuous variables */

foreach i in s_mean_health_ s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_{
	quietly: reg `i'   i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_ i.s_max_adl_ 
	predict `i'_pre
	replace `i' = `i'_pre if `i' == .
}
*merge 1:1 hhidpn wave using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widowed_multiple_years.dta"
merge 1:1 hhidpn wave using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widowed_multiple_years.dta"
drop _m 

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\multiple_years_imputations.dta", replace
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/multiple_years_imputations.dta", replace
preserve 
sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo

xi: logit widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_impute_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_impute_missing.dta", replace

restore 

preserve

keep if male == 1
sum widow male i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo

predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_impute_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_impute_missing.dta", replace

keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

* save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity_impute_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity_impute_missing.dta", replace

restore
preserve

keep if male == 0
sum widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo

xi: logit widow  i.race i.educ i.migration protestant catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic  s_mean_hosp_time_ s_mean_drink_days_  s_mean_drink_num_ s_mean_bmi_ s_mean_cesd_ s_mean_expend_ s_max_cond_ s_mean_health_ i.s_max_hosp_visit_  i.s_max_nurs_home_  i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_  i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo

predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_impute_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_impute_missing.dta", replace

keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity_impute_missing.dta", replace

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity_impute_missing.dta", replace

restore 

preserve 

keep hhidpn wave widow_ wave_start_age_

reshape wide widow_  wave_start_age_, i(hhidpn) j(wave)

g ever_widowed = 0
g widow_wave = .
forvalues i = 4(1)14{
	replace ever_widowed = 1 if widow_`i'== 1
}

forvalues i =5(1)14{
	local j = `i' - 1
	replace widow_wave= `i' if widow_`i' == 1 & widow_`j' == 0
}



*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_propensity_impute_missing.dta"

merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_propensity_impute_missing.dta"
drop _m 

*merge 1:1 hhidpn using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity_impute_missing.dta"
merge 1:1 hhidpn using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_propensity_impute_missing.dta"
drop _m 

g widow_age = . 
g widow_ps = .

forvalues i = 5(1)14{
	replace widow_age = wave_start_age_`i' if widow_wave == `i'
	replace widow_ps = propensity_widow_`i' if widow_wave == `i'
}


*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\ever_widowed_impute_missing.dta", replace
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/ever_widowed_impute_missing.dta", replace
/*
graph box widow_ps, title("weighted treatment group/imputed missing") ytitle("propensity scores")
graph save "Graph" "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot_impute_missing.gph", replace
graph export "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\widowed_and_mortality\output\treatment_propensity_box_plot_impute_missing.png", as(png) name("Graph") replace */

graph box widow_ps, title("weighted treatment group/imputed missing") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot_impute_missing.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/treatment_propensity_box_plot_impute_missing.png", as(png) name("Graph") replace 

collapse (count) hhidpn, by(widow_age)
rename hhidpn count_treatment
drop if widow_age == . 
egen sum_treatment = total(count_treatment)
gen percent_treatment = count_treatment/sum_treatment 

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment_impute_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment_impute_missing.dta" , replace 


/*save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/ever_widowed.dta", replace */
restore 

/* control group age adjustments */

 preserve 
clear
*use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_male_impute_missing.dta", clear
use "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_male_impute_missing.dta", clear
*merge 1:1 hhidpn  wave using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_impute_missing.dta"
merge 1:1 hhidpn  wave using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/widow_prediction_female_impute_missing.dta"
drop _m 

keep if widow_ == 0
*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long_impute_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long_impute_missing.dta" , replace 

collapse (count) hhidpn, by(wave_start_age_)


rename wave_start_age_ widow_age 
drop if widow_age == .

rename hhidpn count_control

egen sum_control = total(count_control)
g percent_control = count_control/sum_control  

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_control_impute_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_control_impute_missing.dta" , replace 

*merge 1:1 widow_age using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_treatment_impute_missing.dta"

merge 1:1 widow_age using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_treatment_impute_missing.dta"

drop _m 

/* generate the weights */ 

rename widow_age wave_start_age_
gen age_weights = percent_control/percent_treatment

replace age_weights = 0.00001 if age_weights == . 

*save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\age_dis_impute_missing.dta" , replace 

save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/age_dis_impute_missing.dta" , replace 

*merge 1:n wave_start_age_ using "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\never_widowed_long_impute_missing.dta" 

merge 1:n wave_start_age_ using "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/never_widowed_long_impute_missing.dta" 

drop _m 
* calculating the propensity distribution for control group */ 
graph box propensity_widow [aw= age_weights], title("weighted control group/imputed missing") ytitle("propensity scores")
graph save "Graph" "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot_impute_missing.gph", replace
graph export "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/widowed_and_mortality/output/control_propensity_box_plot_impute_missing.png", as(png) name("Graph") replace 



restore 

/*


sum widow male i.race i.educ i.migration i.protestant i.catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic i.s_mean_drink_days_  i.s_mean_drink_num_ i.s_max_cond_ i.s_mean_health_ i.s_mean_hosp_time_ i.s_max_hosp_visit_  i.s_mean_bmi_ i.s_max_nurs_home_ i.s_mean_cesd_ i.s_mean_expend_ i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_ i.s_max_adl_ i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo wave_start_age s_wave_start_age 

xi: logit widow male i.race i.educ i.migration i.protestant i.catholic i.s_race i.s_educ i.s_migration i.s_protestant i.s_catholic i.s_mean_drink_days_  i.s_mean_drink_num_ i.s_max_cond_ i.s_mean_health_ i.s_mean_hosp_time_ i.s_max_hosp_visit_  i.s_mean_bmi_ i.s_max_nurs_home_ i.s_mean_cesd_ i.s_mean_expend_ i.s_max_high_bp_ i.s_max_diabetes_ i.s_max_cancer_ i.s_max_lung_ i.s_max_heart_ i.s_max_stroke_ i.s_max_psych_ i.s_max_arthritis_ i.s_max_smoke_ i.s_max_adl_ i.educ_homo i.reli_homo i.racial_homo i.inc_homo i.geo_homo wave_start_age s_wave_start_age 


predict propensity_widow 
replace propensity_widow = propensity_widow[_n-1] if propensity_widow == . & hhidpn[_n]==hhidpn[_n-1] & widow[_n] == 1 & widow[_n-1]==1
drop if propensity_widow == .


save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_impute_missing.dta", replace

keep hhidpn propensity_widow wave
rename propensity_widow propensity_widow_
reshape wide propensity_widow_, i(hhidpn) j(wave)

save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\widow_prediction_female_propensity_impute_missing.dta", replace

*restore 
*preserve








/* outcome variable: survival years */ 

/*
/*gen divorce = 0 
forvalues i = 5(1)14{
	replace divorce  = 1 if r`i'mstat == 5| r`i'mstat == 6 |r`i'mstat == 4
}
 */
/* this is a broader definition, as we include the separated couples */ 


foreach i in divorce ragender birth_year race educ birth_place migration religion  drink_days_4 drink_num_4 cond_4 health_4 hosp_4 hosp_time_4 cesd_4 s_birth_year_4 s_race_4 s_educ_4 s_birth_place_4 s_migration_4 s_religion_4  s_drink_days_4 s_drink_num_4 s_cond_4 s_health_4 s_hosp_4 s_hosp_time_4 s_cesd_4  rank_asset_4 child_liv_4 num_shared_child times_div_4 s_times_div_4 times_mar_4 s_times_mar_4 times_wid_4 s_times_wid_4 marriage_len_4 s_marriage_len_4 cur_mar_len_4 r4wtresp educ_homo racial_homo reli_homo inc_homo inc_miss geo_homo asset_miss age_diff  {
	drop if `i' == . 
	drop if `i' == .m | `i' == .d | `i' == .v | `i' == .r | `i' == .s
}

sum survival_year divorce ragender birth_year race educ birth_place migration religion rank_income_4 drink_days_4 drink_num_4 cond_4 health_4 hosp_4 hosp_time_4 cesd_4 s_birth_year_4 s_race_4 s_educ_4 s_birth_place_4 s_migration_4 s_religion_4 s_rank_income_4 s_drink_days_4 s_drink_num_4 s_cond_4 s_health_4 s_hosp_4 s_hosp_time_4 s_cesd_4  rank_asset_4 child_liv_4 num_shared_child times_div_4 s_times_div_4 times_mar_4 s_times_mar_4 times_wid_4 s_times_wid_4 marriage_len_4 s_marriage_len_4 cur_mar_len_4  r4wtresp educ_homo racial_homo reli_homo inc_homo inc_miss geo_homo asset_miss age_diff 


/* covariates for survival time *
* to predict survival time, we need the covariates which are related to death itself 
* recode the death year with waves, and we need to obtain the information from the waves before the person's death 

foreach i in mean_drink_days mean_drink_num mean_hosp_time max_hosp_visit mean_income mean_asset mean_cesd mean_cond mean_health {
	egen `i' = rowmax(`i'_4 `i'_5 `i'_6 `i'_7 `i'_8 `i'_9 `i'_10 `i'_11 `i'_12 `i'_13 `i'_14)
}

gen mean_income_missing = 1 if mean_income ==.
replace mean_income_missing = 0 if mean_income_missing != 1
replace mean_income= 0 if mean_income== .

gen mean_asset_missing = 1 if mean_asset == .
replace mean_asset_missing = 0 if mean_asset_missing != 1
replace mean_asset = 0 if mean_asset == .

/* further research should impute the values 
foreach i in mean_drink_days mean_drink_num mean_hosp_time max_hosp_visit mean_income mean_asset mean_cesd mean_cond mean_health {
	drop if `i' == . 
}
*/
*/


/* censor individuals who widowed before divorce */
forvalues i = 4(1)14{
	gen wid_censor_`i' = 0
}

forvalues i = 4(1)14{
	gen divorce_new_`i' = 0 
}

forvalues i = 5(1)14{
	local j =  `i' - 1
	replace divorce_new_`i' = 1 if divorce_new_`j' == 1 
	replace divorce_new_`i'  = 1 if r`i'mstat == 5| r`i'mstat == 6 |r`i'mstat == 4
}
forvalues i = 5(1)14{
	local j = `i' - 1
	replace wid_censor_`i' = 1 if wid_censor_`j' == 1 
	replace wid_censor_`i' = 1 if  divorce_new_`i' == 0 & widow_`i' == 1 
}


egen wid_censor_time_sum = rowtotal(wid_censor_5 wid_censor_6 wid_censor_7 wid_censor_8 wid_censor_9 wid_censor_10 wid_censor_11 wid_censor_12 wid_censor_13 wid_censor_14)



gen wid_censor_wave = 11 - wid_censor_time_sum + 4
recode wid_censor_wave (4 = 1998)(5=2000)(6=2002)(7=2004)(8=2006)(9=2008)(10=2010)(11=2012)(12=2014)(13=2016)(14 = 2018), gen(wid_censor_year)
replace censor_year =  wid_censor_year - int_year_1998 if wid_censor_year <= min(rexityr, int_year_2018) 
replace survival_year = min(failure_year, censor_year)
replace right_censor = 0 if failure_year <= censor_year

/* divorce revise */ 
/* Below added to change divorce to 0 if widow censoredd (Jiaxin)*/
replace divorce = 0 if wid_censor_year <= min(rexityr, int_year_2018) 

gen age_death = death_year - birth_year

sum survival_year divorce ragender birth_year race educ birth_place migration religion  drink_days_4 drink_num_4 cond_4 health_4 hosp_4 hosp_time_4 cesd_4 s_birth_year_4 s_race_4 s_educ_4 s_birth_place_4 s_migration_4 s_religion_4  s_drink_days_4 s_drink_num_4 s_cond_4 s_health_4 s_hosp_4 s_hosp_time_4 s_cesd_4  rank_asset_4 child_liv_4 num_shared_child times_div_4 s_times_div_4 times_mar_4 s_times_mar_4 times_wid_4 s_times_wid_4 marriage_len_4 s_marriage_len_4 cur_mar_len_4  r4wtresp mean_drink_days mean_drink_num mean_hosp_time max_hosp_visit mean_income mean_asset mean_cesd mean_cond mean_health age_death educ_homo racial_homo reli_homo inc_homo inc_miss geo_homo asset_miss age_diff 


keep hhid pn right_censor survival_year divorce ragender birth_year race educ birth_place migration religion  drink_days_4 drink_num_4 cond_4 health_4 hosp_4 hosp_time_4 cesd_4 s_birth_year_4 s_race_4 s_educ_4 s_birth_place_4 s_migration_4 s_religion_4  s_drink_days_4 s_drink_num_4 s_cond_4 s_health_4 s_hosp_4 s_hosp_time_4 s_cesd_4  rank_asset_4 child_liv_4 num_shared_child times_div_4 s_times_div_4 times_mar_4 s_times_mar_4 times_wid_4 s_times_wid_4 marriage_len_4 s_marriage_len_4 cur_mar_len_4 r4wtresp mean_drink_days mean_drink_num mean_hosp_time max_hosp_visit mean_income mean_asset mean_cesd mean_cond mean_health age_death educ_homo racial_homo reli_homo inc_homo inc_miss geo_homo asset_miss age_diff censor_year failure_year censor_age male death

/* save "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\divorce_prediction.dta" , replace */
save "/Users/guanghuipan/OneDrive - Nexus365/projects/HRS_project/Data/Data_new/divorce_prediction.dta", replace

*/ */
*/
timer off 1
timer list 1

log close
