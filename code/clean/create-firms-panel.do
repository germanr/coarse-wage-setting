/*--------------------------------------------------------------------------
This code creates a database with a 5% random sample of firms and their workers, a panel with the new hires of those firms, and a firm-by-yr database 

Uses:	 	       RAIS raw data, census_firms.dta, muni_identifiers.csv
Produces: 		   firmspanel_chosenfirms.dta, firm_panel.dta, firm_variables.dta, firms_panel_merge.dta, hires_firms_panel.dta,
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;
set seed 20210221;

cap log close;
log using "${root}\1-create-firms-panel.log", replace;

*----------------------------------------------*;
*------ Select 5% of private-sector firms -----*;
*----------------------------------------------*;

#delimit;
use "${dat}\census_firms.dta", replace;
drop *19* *2000 *2001 *2002 yrs_insample;

#delimit;
egen yrs_insample  = rsum(in*)          
egen priv_firm     = rsum(priv_firm*)   ; replace priv_firm = priv_firm/yrs_insample;
egen hires_monthly = rsum(hire_monthly*); 

gen tokeep = (yrs_insample > 0) ; tab tokeep; keep if tokeep == 1; drop tokeep;
gen tokeep = (priv_firm == 1)   ; tab tokeep; keep if tokeep == 1; drop tokeep; 
gen tokeep = (hires_monthly > 0); tab tokeep; keep if tokeep == 1; drop tokeep;

sample 5;
 
keep firmid oldest_adm first_yr last_yr yrs_insample; 
compress;
save "${dat}\firmspanel_chosenfirms.dta", replace;

*----------------------------------*;
*------ Create panel of firms -----*;
*----------------------------------*;

#delimit;
forvalues yr = 2003(1)2017 {; di in red `yr';

	use "${raw}\rais`yr'.dta", replace; 
	
	include "${cod}\labels.do";
	include "${cod}\dropvars.do";

	fmerge m:1 firmid using "${dat}\firmspanel_chosenfirms.dta", keepusing(oldest_adm first_yr last_yr yrs_insample); tab _merge; keep if _merge == 3; drop _merge;
	
	* Generate a valid worker id;
	local invalid "0 9 99 191 949 11111111111 22222222222 33333333333 44444444444 55555555555 66666666666 77777777777 88888888888 99999999999 12345678909 98765432100";

	foreach i of local invalid {; replace cpf = . if cpf == `i' ; };
	foreach i of local invalid {; replace pis = . if pis == `i' ; };

	gen double id = pis; 

	* Remove duplicate records and workers with multiple employment at the same firm within a given year;
	gen random    = r(uniform);
	gen zero_earn = (earn_dec == 0);

	bys id firmid (zero_earn hired_hours earn_dec earn_mean random): gen tokeep = (_n == _N); tab tokeep; keep if tokeep == 1; drop tokeep zero_earn random;
	
	gen white = (race == 2) if race != .;
	
	recode schooling (-1 = .) (10/11 = 9);
	recode schooling (0/8 = 0) (9 = 1), gen(college_comp);
	recode schooling (0/6 = 0) (7/9 = 1), gen(hs_comp);

	gen firm_age = yr - min(first_yr, oldest_adm); drop first_yr oldest_adm;

	gen byte hire_anyjob = (inlist(adm_type,1,2) & earn_type == 1);

	if `yr' >= 2016 {; replace hired_wage = hired_wage*100 if hired_wage < 100 & ((earn_mean_mw >= 1 & earn_mean_mw != .) | (earn_dec_mw >= 1 & earn_dec_mw != .) ); }; 

	gen mth = adm_month;
	gen st = floor(muni/10000);

	merge m:1 muni   using "${dat}\microregion.dta"; tab _merge; drop if _merge == 2; drop _merge;
	merge m:1 yr     using "${dat}\cpi_yrly.dta"   ; tab _merge; keep if _merge == 3; drop _merge; replace cpi = cpi/100; ren cpi cpi_yrly;
	merge m:1 yr st  using "${dat}\state_mw.dta"   ; tab _merge; drop if _merge == 2; drop _merge; 
	
	* Get minimum wage;
	
	gen 	mw_fed = .                ;
	replace mw_fed = 998 if yr == 2019;
	replace mw_fed = 954 if yr == 2018;
	replace mw_fed = 937 if yr == 2017;
	replace mw_fed = 880 if yr == 2016;
	replace mw_fed = 788 if yr == 2015;
	replace mw_fed = 724 if yr == 2014;
	replace mw_fed = 678 if yr == 2013;
	replace mw_fed = 622 if yr == 2012;
	replace mw_fed = 545 if yr == 2011;
	replace mw_fed = 510 if yr == 2010;
	replace mw_fed = 465 if yr == 2009;
	replace mw_fed = 415 if yr == 2008;
	replace mw_fed = 380 if yr == 2007;
	replace mw_fed = 350 if yr == 2006;
	replace mw_fed = 300 if yr == 2005;
	replace mw_fed = 260 if yr == 2004;
	replace mw_fed = 240 if yr == 2003;

	gen is_fed_mw = (hired_wage == mw_fed);
	gen is_st_mw = 0; forvalues i = 1(1)12 {; replace is_st_mw = 1 if hired_wage == mw`i' & mw`i' != .; drop mw`i'; };
	
	 * Adjust for inflation;
	gen earn_mean_real  =  earn_mean/cpi_yrly;
	gen hired_wage_real = hired_wage/cpi_yrly;
	gen double wage_real = earn_mean_real/(hired_hours*4.348) if earn_type == 1 & hired_wage >= mw_fed;	
	
	keep yr firmid cpf pis id age hired_wage earn_mean hired_wage_real earn_mean_real wage_real yrs_edu schooling hs_comp college_comp race white male adm_date adm_type earn_type mw_fed is_fed_mw is_st_mw cbo02 cnae95 occ1d muni micro hire_anyjob active firm_size yrs_insample potential_exp hired_hours tenure sep_cause ind1d firm_age ind2d occ2d last_yr;

	compress; 
	save "${dat}\firmspanel_`yr'.dta", replace;

};

*--------------------------------------------------------------------------*;
*------ Append and create variables that require pooling across years -----*;
*--------------------------------------------------------------------------*;

#delimit;
clear; forvalues yr = 2003(1)2017 {; append using "${dat}\firmspanel_`yr'.dta"; erase "${dat}\firmspanel_`yr'.dta"; };

* CEOS, managers, supervisors, and HR ;
gen ceo 	 = (cbo02==121010);
gen manag 	 = (occ1d==1);
gen superv 	 = (real(substr(string(cbo02),3,1)) == 0);

gen hr_manag  = (inlist(cbo02,123205,123210,142210,142205));
gen hr_oth    = (inlist(cbo02,252105,252405,411030));
	
#delimit;
bys firmid: egen has_hr_manag = total(hr_manag); replace has_hr_manag = 1 if has_hr_manag > 0 & has_hr_manag != .;
by  firmid: egen has_hr_oth   = total(hr_oth)  ; replace has_hr_oth   = 1 if has_hr_oth   > 0 & has_hr_oth   != .; 
by  firmid:  gen byte has_hr = (has_hr_manag == 1 | has_hr_oth == 1);

by  firmid: egen double highest_wage = max(wage_real); gen has_highest_wage = (wage_real == highest_wage);

gen aux_ceo_edu    = schooling if ceo == 1                ; by firmid: egen ceo_ed u            = mean(aux_ceo_edu)  ; drop aux_ceo_edu;
gen aux_manag_edu  = schooling if manag == 1 | superv == 1; by firmid: egen manag_edu          = mean(aux_manag_edu); drop aux_manag_edu;
gen aux_earn_edu   = schooling if has_highest_wage == 1   ; by firmid: egen highest_earner_edu = mean(aux_earn_edu) ; drop aux_earn_edu;

gen only_ceo_edu = ceo_edu;
replace ceo_edu = manag_edu          if ceo_edu == .; 
replace ceo_edu = highest_earner_edu if ceo_edu == .;

drop manag ceo superv highest_wage has_highest_wage hr_manag hr_oth has_hr_manag has_hr_oth;

gen byte is_mw = (is_fed_mw == 1 | is_st_mw == 1);

* Rounding firms;
gen byte mult_10	  = !mod(hired_wage,10);
gen byte mult_100  	  = !mod(hired_wage,100);
gen byte mult_1000	  = !mod(hired_wage,1000);
gen byte mult_10_yr   = !mod(hired_wage*12,10);

gen byte aux_1stjob        = (adm_type == 1 & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1                   );
gen byte aux_1stjob_10     = (adm_type == 1 & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_10    == 1 );
gen byte aux_1stjob_10_yr  = (adm_type == 1 & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_10_yr == 1 );

gen byte aux_anyjob        = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed                                );
gen byte aux_anyjob_nomw   = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1                   );
gen byte aux_anyjob_10     = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_10    == 1 );
gen byte aux_anyjob_100    = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_100   == 1 );
gen byte aux_anyjob_1000   = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_1000  == 1 );
gen byte aux_anyjob_10_yr  = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & is_mw != 1 & mult_10_yr == 1 );

bys firmid: egen hires_1stjob       = sum(aux_1stjob); 
by  firmid: egen hires_1stjob_10    = sum(aux_1stjob_10); 
by  firmid: egen hires_1stjob_10_yr = sum(aux_1stjob_10_yr); 

by  firmid: egen hires       = sum(aux_anyjob); 
by  firmid: egen hires_nomw  = sum(aux_anyjob_nomw); 
by  firmid: egen hires_10    = sum(aux_anyjob_10); 
by  firmid: egen hires_10_yr = sum(aux_anyjob_10_yr);
by  firmid: egen hires_100   = sum(aux_anyjob_100); 
by  firmid: egen hires_1000  = sum(aux_anyjob_1000); 
drop aux*;

gen shr_hires_10    = hires_10/hires_nomw;
gen shr_hires_100   = hires_100/hires_nomw;
gen shr_hires_1000  = hires_1000/hires_nomw;
gen shr_hires_10_yr = hires_10_yr/hires_nomw;

gen byte buncher  	       = (shr_hires_10 == 1)    if shr_hires_10 != .;
gen byte buncher_yr 	   = (shr_hires_10_yr == 1) if shr_hires_10_yr != .;

gen shr_hires1stjob_10    = hires_1stjob_10/hires_1stjob;
gen shr_hires1stjob_10_yr = hires_1stjob_10_yr/hires_1stjob;

gen byte buncher_1stjob    = (shr_hires1stjob_10 == 1)    if shr_hires1stjob_10 != .;
gen byte buncher_1stjob_yr = (shr_hires1stjob_10_yr == 1) if shr_hires1stjob_10_yr != .;

gen byte zero_active    = (firm_size == 0) if firm_size != .;

* Education of active employees;
gen aux_hs    = (hs_comp      == 1 & active == 1);
gen aux_coll  = (college_comp == 1 & active == 1);
gen aux_yredu =  yrs_edu if active == 1;

gen aux_work = (schooling    != . & active == 1);
bys firmid: egen firm_size_school  = total(aux_work); 

by  firmid: egen shr_hs   = total(aux_hs);   replace shr_hs   = shr_hs/firm_size_school;   
by  firmid: egen shr_coll = total(aux_coll); replace shr_coll = shr_coll/firm_size_school;
by  firmid: egen mean_edu = mean(aux_yredu);
drop firm_size_school;

bys firmid yr: egen firm_size_school  = total(aux_work); 
bys firmid yr: egen firm_size_hs     = total(aux_hs);
	   	        gen firm_size_nohs   = firm_size_school-firm_size_hs;

drop aux_hs hs_comp aux_coll college_comp aux_yredu aux_work;

* Mean characteristics;
by  firmid: egen mean_white = mean(white);
by  firmid: egen mean_male  = mean(male);
by  firmid: egen mean_age   = mean(age);

* Average earnings of full-time active employees; 
gen earn_mean_act = earn_mean_real if active == 1 & earn_type == 1;

sort firmid yr;
by   firmid yr: egen hire_anyjob_yr = total(hire_anyjob);
by   firmid yr: egen mean_pay_yr    = mean(earn_mean_act);
by   firmid   : egen mean_pay       = mean(earn_mean_act);

* To calcualte the growth rate of high-pay employees;
#delimit;
bys         yr: egen p50_pay_yr     = median(earn_mean_real);
gen aux_pay     = (earn_mean_real    != . & active == 1);
gen aux_highpay = (earn_mean_real >= p50_pay_yr & active == 1 & earn_mean_real !=.);
gen aux_lowpay  = (earn_mean_real <  p50_pay_yr & active == 1 & earn_mean_real !=.);

bys firmid yr: egen firm_size_pay      = total(aux_pay); 
by  firmid yr: egen firm_size_highpay  = total(aux_highpay);
		        gen firm_size_lowpay   = firm_size_pay-firm_size_highpay;

drop aux*;
 
#delimit;
loc rais_wrkr "male schooling white muni micro potential_exp earn_mean* earn_type hired_wage* hired_hours tenure occ* cbo02 active adm_type adm_date sep_cause yrs_edu";
loc rais_firm "firmid firm_* cnae* ind* has_hr* *edu* shr_hs shr_coll mean_edu mean_pay* firm_age hire_anyjob_yr last_yr ind2d";
order id pis cpf firmid yr yrs_insample `rais_wrkr' `rais_firm'; 
sort  id pis cpf firmid yr; 
format firmid cpf pis %18.0g;
compress; 
save "${dat}\firms_panel.dta", replace;

*-------------------------------------------*;
*------ Calculate Gini among new hires -----*;
*-------------------------------------------*;

#delimit;
use "${dat}\firms_panel.dta" , replace;

keep if inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & hired_wage != . & is_mw != 1 & hires > 1;
keep firmid hired_wage_real;
sort firmid hired_wage_real;

gen wi = 1;
by firmid: egen mean_allhires = mean(hired_wage_real);
by firmid:  gen sumwi = _N;
gen double fi = 1 / sumwi;
by firmid:  gen double py = (2 * sum(wi))/(2 * sumwi);
by firmid: egen double gini = sum(fi*(2 / mean_allhires) * py * (hired_wage_real - mean_allhires));

foreach p in 10 50 90 {; bys firmid: egen p`p'_allhires = pctile(hired_wage), p(`p'); };

by firmid: keep if _n == 1;

keep firmid gini mean_allhires p*_allhires;

gen r_90_to_50 = p90_allhires/p50_allhires;
gen r_50_to_10 = p50_allhires/p10_allhires;
gen r_90_to_10 = p90_allhires/p10_allhires;

save "${dat}\gini_firms.dta", replace;


*------------------------------------------*;
*------ Firm-by-year level variables  -----*;
*------------------------------------------*;

#delimit
use "${dat}\firms_panel.dta", replace;

keep yr firmid firm_size* hire_anyjob zero_active cnae95 ind1d micro yrs_insample shr_hs shr_coll ceo_edu manag_edu highest_earner_edu has_hr* hires* buncher* shr_hires* mean_edu mean_pay* ind* firm_age hire_anyjob_yr last_yr;

bys firmid yr: keep if _n == 1; // Keep one observation per firm-year;
xtset firmid yr;

* Average firm size over time;
bys firmid: egen mean_size = mean(firm_size);
gen lmean_size = log(mean_size);
replace mean_size = round(mean_size);

gen  firm_size_no0 = firm_size; replace firm_size_no0 = mean_size if firm_size == 0;
gen lfirm_size_no0 = log(firm_size_no0);

* Quantiles of bunching;
gen buncher_over50 = (shr_hires_10 > 1/2) if shr_hires_10 != .;
gen buncher_over66 = (shr_hires_10 > 2/3) if shr_hires_10 != .;

bys firmid (yr): gen temp = shr_hires_10 if _n == 1;
xtile buncher_quintile = temp , nq(5); 

forvalues q = 1(1)5{; gen bunch_q`q' = buncher_quintile == `q' if shr_hires_10 != .; }; drop buncher_quintile temp;

* Alternative measure of exit: observed in yr t and not observed in yr t+1;
gen     left_rais = 0;
replace left_rais = 1 if yr == last_yr;

* Measure of job growth + windsorize to deal with outliers;
gen delta = yr - l.yr;
gen job_growth = (firm_size/l.firm_size) -1 if delta == 1;
qui sum job_growth, d; replace job_growth = r(p99) if job_growth >= r(p99) & job_growth != .;

* Measure of job growth based on worker characteristics;
#delimit;
gen job_growth_highpay = (firm_size_highpay/l.firm_size_highpay) -1 if delta == 1;
gen job_growth_lowpay  = (firm_size_lowpay/l.firm_size_lowpay)   -1 if delta == 1;
gen job_growth_hs      = (firm_size_hs/l.firm_size_hs)           -1 if delta == 1;
gen job_growth_nohs    = (firm_size_nohs/l.firm_size_nohs)       -1 if delta == 1;

#delimit;
* Add measure of experience hiring workers;
bys firmid (yr):  gen cdf_hiring =   sum(hire_anyjob_yr);
by  firmid (yr): egen hiring_exp = total(hire_anyjob_yr);
gen lhiring_exp = log(1+hiring_exp);

* Add gini + windsorize ratios;
fmerge m:1 firmid using "${dat}\gini_firms.dta", nogen;
foreach v of varlist r_* {; qui sum `v', d; replace `v' = r(p99) if `v' !=.  & `v' >= r(p99); };

replace ceo_edu = manag_edu          if ceo_edu == .; 
replace ceo_edu = highest_earner_edu if ceo_edu == .;

keep firmid yr lmean_size mean_size mean_pay firm_size_no0 lfirm_size_no0 firm_age buncher_* bunch* job_growth* cdf_hiring hiring_exp lhiring_exp has_hr ceo_edu gini mean_allhires r_90_to_50 r_50_to_10 r_90_to_10 left_rais;
order firmid yr; 
compress;
save "${dat}\firm_variables.dta", replace;


#delimit;
use "${dat}\firms_panel.dta", replace;
merge m:1 firmid yr using "${dat}\firm_variables.dta"; tab _merge; drop _merge;
save "${dat}\firms_panel_merge.dta", replace;

*----------------------------------------------------*;
*------ Get new workers hired in the firm panel -----*;
*----------------------------------------------------*;

#delimit;
use "${dat}\firms_panel.dta", replace;

#delimit;
bys id firmid  yr :  gen N = _N; tab N; drop N;
by  id firmid (yr):  gen N = _N;
by  id firmid (yr): egen last_yr_worker = max(yr);

* Keep new hires hired at a monthly earnings contract above the MW;
gen temp = (inlist(adm_type,1,2) & earn_type == 1 & hired_wage >= mw_fed & hired_wage != . & is_mw != 1);
bys id firmid: egen hired_insample = total(temp); replace hired_insample = 1 if hired_insample > 1; tab hired_insample; keep if hired_insample == 1;

* Exclude workers if there is no separation information in the last year they are observed and they stay for less than one year;
gen temp2 = yr if temp == 1;
by  id firmid: egen hired_yr = min(temp2); drop temp temp2;
gen temp = (N == 1 & sep_cause == . & last_yr_worker != 2017                        ); bys id firmid: egen todrop = total(temp); tab todrop; drop if todrop > 0; drop temp todrop;
gen temp = (N == 2 & sep_cause == . & last_yr_worker != 2017 & yr == last_yr_worker ); by  id firmid: egen todrop = total(temp); tab todrop; drop if todrop > 0; drop temp todrop;

drop if yr < hired_yr; // Drop data before worker joined the firm;

* Separated/resigned in the hiring year or next year;
#delimit;
gen temp = 0; replace temp = 1 if inlist(sep_cause,10, 11, 20, 21) & (yr == hired_yr | yr == hired_yr+1); bys id firmid: egen sep_1yr = total(temp); replace sep_1yr = 1 if sep_1yr > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,        20, 21) & (yr == hired_yr | yr == hired_yr+1); bys id firmid: egen res_1yr = total(temp); replace res_1yr = 1 if res_1yr > 1; drop temp;

gen temp = 0; replace temp = 1 if inlist(sep_cause,10, 11, 20, 21) & (yr == hired_yr | yr == hired_yr+1 | yr == hired_yr+2); bys id firmid: egen sep_2yr = total(temp); replace sep_2yr = 1 if sep_2yr > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,        20, 21) & (yr == hired_yr | yr == hired_yr+1 | yr == hired_yr+2); bys id firmid: egen res_2yr = total(temp); replace res_2yr = 1 if res_2yr > 1; drop temp;

gen temp = 0; replace temp = 1 if inlist(sep_cause,10, 11, 20, 21) & (yr == hired_yr | yr == hired_yr+1 | yr == hired_yr+2 | yr == hired_yr+3); bys id firmid: egen sep_3yr = total(temp); replace sep_3yr = 1 if sep_3yr > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,        20, 21) & (yr == hired_yr | yr == hired_yr+1 | yr == hired_yr+2 | yr == hired_yr+3); bys id firmid: egen res_3yr = total(temp); replace res_3yr = 1 if res_3yr > 1; drop temp;

* Separated/resigned by skill level;
#delimit;
recode schooling (0/6 = 0) (7/9 = 1), gen(hs_comp);

gen temp = 0; replace temp = 1 if inlist(sep_cause,10, 11, 20, 21) & (yr == hired_yr | yr == hired_yr+1) & hs_comp == 1; bys id firmid: egen sep_1yr_hs   = total(temp); replace sep_1yr_hs   = 1 if sep_1yr_hs   > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,        20, 21) & (yr == hired_yr | yr == hired_yr+1) & hs_comp == 1; bys id firmid: egen res_1yr_hs   = total(temp); replace res_1yr_hs   = 1 if res_1yr_hs   > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,10, 11, 20, 21) & (yr == hired_yr | yr == hired_yr+1) & hs_comp == 0; bys id firmid: egen sep_1yr_nohs = total(temp); replace sep_1yr_nohs = 1 if sep_1yr_nohs > 1; drop temp;
gen temp = 0; replace temp = 1 if inlist(sep_cause,        20, 21) & (yr == hired_yr | yr == hired_yr+1) & hs_comp == 0; bys id firmid: egen res_1yr_nohs = total(temp); replace res_1yr_nohs = 1 if res_1yr_nohs > 1; drop temp;

sort id firmid  yr;
by   id firmid (yr): gen double wage_grw_abs  = hired_wage - hired_wage[_n-1]           if yr == yr[_n-1]+1 & yr == hired_yr+1; // Wage growth of new hires;
by   id firmid (yr): gen double wage_grw_pct  = (hired_wage/hired_wage[_n-1] - 1)*100   if yr == yr[_n-1]+1 & yr == hired_yr+1;

#delimit;
gen byte wage_grw_pct_int = (abs(wage_grw_pct-int(wage_grw_pct)) < 0.001) if wage_grw_pct != .;
gen byte wage_grw_abs_rn  = !mod(wage_grw_abs,10) if wage_grw_abs != .;
gen byte wage_grw_zero    = (wage_grw_pct == 0)   if wage_grw_pct != .;
gen byte wage_grw_round   = (wage_grw_abs_rn == 1 | wage_grw_pct_int == 1) if wage_grw_abs_rn !=. & wage_grw_pct_int !=. ;

#delimit;
keep id firmid yr sep_*yr* res_*yr* wage_grw* hired_yr last_yr_worker;
sort id firmid yr;
compress;
format id %16.0g;
save "${dat}\hires_firms_panel.dta", replace;


cap log close;


/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
