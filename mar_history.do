 use "C:\Users\pangu\OneDrive - Nexus365\projects\HRS_project\Data\Data_new\mar_history_revised.dta" , clear
 
 gen dead = 1 if death_year != .
 replace dead = 0 if death_year == .
 
 * marriage information 
 * ever married 
 gen married = 0 
 forvalues i = 1(1)10{
 	replace married = 1 if mar_bg_y_`i' != .
 }
 
 * ever widowed 
 gen widowed = 0
 forvalues i = 1(1)3{
 	replace widowed = 1 if mar_wid_y_`i' != .
 }
 
 * ever divorced 
 gen divorced = 0
 forvalues i = 1(1)3{
 	replace divorced = 1 if mar_div_y_`i' != .
 }
 
 * widowed as final marital status 
gen wid_now = 0
egen last_wid = rowmax(mar_wid_y_1 mar_wid_y_2 mar_wid_y_3)
egen last_mar = rowmax(mar_bg_y_1 mar_bg_y_2 mar_bg_y_3 mar_bg_y_4 mar_bg_y_5 mar_bg_y_6 mar_bg_y_7)
egen last_div = rowmax(mar_div_y_1 mar_div_y_2 mar_div_y_3)

replace wid_now = 1 if (last_wid>=last_mar | last_wid >= last_div ) & last_wid !=.