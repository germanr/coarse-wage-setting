/*--------------------------------------------------------------------------
This code creates a database with the count of workers in $R1 bins, by groups. It also produces a database with estimated theta for each round number by group.

Uses: 	  hires_`yr'.dta, state_mw.dta, ipca_15.dta,
Produces: bins_by_yr.dta, bins_fig.dta, theta.dta, theta_samplesize.dta, theta_reg.dta
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;

*-----------------------------------------*;
*------ Create bins of round numbers -----*;
*-----------------------------------------*;

#delimit;
local groups "mmc all reg_mth_yr firm_size_group cdf_hiring_group hiring_exp_group ind1d occ1d cba_any cba_wageclause cba_firm_lvl"; 

forvalues yr = 2003(1)2017 {;
	
	use "${dat}\hires_`yr'.dta", replace;
	
	gen byte has_cents = (hired_wage != floor(hired_wage));
	gen byte is_rn     = !mod(hired_wage,10);
	
	replace hired_wage = 10100 if hired_wage > 10100 & has_cents == 0 & is_rn == 1; // RN wages over 10100 go here;
	replace hired_wage = 10101 if hired_wage > 10100                              ; // Other wages (non-RN) over 10100 go here;
	replace hired_wage = floor(hired_wage); 
	
	gen str_adm_mth = string(adm_month) if adm_month != .; replace str_adm_mth = "0" + str_adm_mth if length(str_adm_mth) == 1;

	gen double mth_yr      = real(string(adm_month) + string(yr)) if adm_month != .;
	gen double reg_mth_yr  = real(string(floor(muni/10000)) + str_adm_mth + string(yr)) if reg_met != "";
	gen obs               = 1;
	gen all               = 1;
	
	keep `groups' reg_mth_yr obs has_cents hired_wage fixed_sample yr;
	compress; 
	tempfile bins_db; save `bins_db', replace;

	loc i = 0;
	foreach group of local groups {; di in red "Group: `group'";
		if `yr' <= 2007 & regex("`group'", "cba") continue; // These years are not available in the CBA dataset;

		loc ++i; 
		use `bins_db', replace;

		gcollapse (sum) obs has_cents if `group' != ., by(hired_wage `group' fixed_sample) fast;
		
		rename `group' level; gen group = "`group'";
	
		tempfile working_db`i'; save `working_db`i'', replace;

	};

	clear; forvalues j = 1(1)`i' {; append using `working_db`j''; };

	gen yr = `yr'; 
	save "${dat}/bins_`yr'.dta", replace;

};

clear; forvalues yr = 2003(1)2017 {; append using "${dat}/bins_`yr'.dta"; erase "${dat}/bins_`yr'.dta"; };

* Bunching in the fixed sample;
preserve; 
	keep if fixed_sample == 1; 
	drop fixed_sample; 
	rename obs       obs_fixed_sample     ; 
	rename has_cents has_cents_fixed_sample; 
	save "${dat}/tomerge_fixed.dta", replace; 
restore;

gcollapse (sum) obs has_cents, by(hired_wage group level yr);

merge 1:1 hired_wage group level yr using "${dat}/tomerge_fixed.dta"; drop _merge; erase "${dat}/tomerge_fixed.dta";
replace obs_fixed_sample = 0 if obs_fixed_sample == .;
save "${dat}/bins_by_yr.dta", replace;

* Get state as a group;
use "${dat}/bins_by_yr.dta", replace; 
keep if group == "mmc"; 
replace group = "state"; 
replace level = floor(level/1000); 
gcollapse (sum) obs* has_cents*, by(hired_wage group level yr);
append using "${dat}/bins_by_yr.dta";
save "${dat}/bins_by_yr.dta", replace;

* Aggregate across all years;
#delimit;
use "${dat}/bins_by_yr.dta", replace;
gcollapse (sum) obs* has_cents*, by(hired_wage group level);
save "${dat}/bins.dta", replace;

* Dataset for figures;
#delimit;
use "${dat}/bins_by_yr.dta" if group == "state", replace; 
rename level st; 

merge m:1 yr st  using "${dat}\state_mw.dta"; tab _merge; drop if _merge == 2; drop _merge; 
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
	
gen is_mw = (is_fed_mw == 1 | is_st_mw == 1);
drop if is_mw == 1; 
drop is_fed_mw is_st_mw is_mw group;

gcollapse (sum) obs* has_cents*, by(hired_wage yr);

preserve;
	gcollapse (sum) obs* has_cents*, by(hired_wage);
	xtile pool_pctile = hired_wage [w=obs], nq(100);
	sum hired_wage if pool_pctile == 100; // Min & p100 = R$ 3,414 & 4,123;
	loc hi_wage = 3501;
	tempfile pctil; save `pctil', replace;
restore;

merge m:1 hired_wage using `pctil', nogen;
keep if inrange(hired_wage, 0, `hi_wage');

foreach rn in "10" "20" "50" "100" "250" "500" "1000" {; gen mult_`rn' = !mod(hired_wage,`rn'); };

gen fig_10   = mult_10  ; replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50  ; replace fig_50  = 0 if mult_100  == 1                                 ;
gen fig_100  = mult_100 ; replace fig_100 = 0 if mult_500  == 1                                 ;
gen fig_500  = mult_500 ; replace fig_500 = 0 if mult_1000 == 1                                 ;
gen fig_1000 = mult_1000;

compress;
save "${dat}/bins_fig.dta", replace;


*----------------------------------------------*;
*------ Use bins to calculate excess mass -----*;
*----------------------------------------------*;

#delimit ;

include "${cod}/bunching.do";

use "${dat}/bins.dta", replace;

drop if hired_wage >= 10100; // Drop censored tail;

gcollapse (sum) obs* has_cents*, by(group level hired_wage);

gen cnt = 1; 
bys group level: egen tot_obs = sum(cnt); drop if tot_obs < 200; drop cnt tot_obs;

tempfile working_db; save `working_db', replace;

levelsof group, local(groups); 
local groups "cba_any cba_firm_lvl cba_wageclause";
foreach grp of local groups {;	
	levelsof level if group == "`grp'", local(levels); loc cnt = 0; foreach lvl of local levels {; di in red "`grp': `lvl'"; loc ++cnt;
	
		use if group == "`grp'" & level == `lvl' using `working_db', replace; 

		* Exclude outliers;
		if "`grp'" != "all" {;
			qui sum hired_wage if group == "`grp'" & level == `lvl' [fw=obs], d; 
			drop if hired_wage >= r(p99);
		};

		* Counterfactual;
		bunching hired_wage obs_fixed, poly(7); rename (obs_fixed_sample obs_hat obs_hat_adj se_obs) (n_act_fixed_sample n_hat_fixed_sample n_hat_adj_fixed_sample se_obs_fixed_sample);
		bunching hired_wage obs      , poly(7); rename (obs              obs_hat obs_hat_adj       ) (n_act              n_hat              n_hat_adj                                 );

		* Observed totals;
		#delimit;
		sum hired_wage [fw = n_act]	       ; gen tot_obs_lvl     		  = r(sum_w); 	gen double tot_inc_lvl      	    = r(sum);
		sum hired_wage [fw = n_act_fixed]  ; gen tot_obs_lvl_fixed_sample = r(sum_w); 	gen double tot_inc_lvl_fixed_sample = r(sum);

		* Complete missings;
		replace group = "`grp'" if group == "";
		replace level = `lvl'   if level == . ;
		
		tempfile data`cnt'; save `data`cnt'', replace;	
		
	};

	clear; forvalues i = 1(1)`cnt' {; append using `data`i''; };
	save "${dat}\theta_`grp'.dta", replace;
	use `working_db', replace;
} ;


#delimit;
clear; foreach g of local groups {; append using "${dat}\theta_`g'.dta"; *erase "${dat}\theta_`g'.dta"; };

#delimit;
gen inc_act = hired_wage*n_act; 
gen inc_hat = hired_wage*n_hat;

foreach rn in 10 100 1000 {; gen mult_`rn' = !mod(hired_wage,`rn')	; };

* Add G_low (fraction optimizing firms near each RN);
gen mult_5 = (!mod(hired_wage ,5) & mult_10 == 0);
gen rn = round(hired_wage/10)*10; replace rn = . if mult_5 == 1; replace rn = . if n_hat == 0;
bys group level rn (hired_wage): egen tot_n	        = total(n_act)     if mult_10 != 1;
bys group level rn (hired_wage): egen tot_n_hat_adj = total(n_hat_adj) if mult_10 != 1;
gen aux_g_low = tot_n/tot_n_hat_adj;
bys group level rn: egen g_low = min(aux_g_low);
qui sum hired_wage if mult_10 == 1;
replace g_low = . if mult_10 != 1 | hired_wage == r(min) | hired_wage == r(max);
drop mult_5 rn aux_g_low tot_n tot_n_hat_adj;

foreach v of varlist _all {; label variable `v' ""; };
compress; 
save "${dat}\theta.dta", replace;

* Number of round numbers used in each group;
#delimit;
use  "${dat}\theta.dta", replace;

gen nr_round_numbers = (mult_10 == 1 & n_act > 0); 

gcollapse (sum) nr_round_numbers n_act n_hat n_act_fixed_sample n_hat_fixed_sample, by(group level); 
save "${dat}\theta_samplesize.dta", replace;


*** Create a database for figures ***;
#delimit;
use "${dat}\theta.dta", replace;

* Remove the minimum wages;
gen mw = (inlist(hired_wage, 240, 260, 300, 350, 380, 415, 465, 510, 545, 622, 678, 724, 788, 880, 937)) if !inlist(group,"year","mth_yr");
gen year = real(substr(string(level),-4,.)) if group == "mth_yr"; replace year = level if group == "year";
replace mw = 1 if year == 2017 & hired_wage == 937;
replace mw = 1 if year == 2016 & hired_wage == 880;
replace mw = 1 if year == 2015 & hired_wage == 788;
replace mw = 1 if year == 2014 & hired_wage == 724;
replace mw = 1 if year == 2013 & hired_wage == 678;
replace mw = 1 if year == 2012 & hired_wage == 622;
replace mw = 1 if year == 2011 & hired_wage == 545;
replace mw = 1 if year == 2010 & hired_wage == 510;
replace mw = 1 if year == 2009 & hired_wage == 465;
replace mw = 1 if year == 2008 & hired_wage == 415;
replace mw = 1 if year == 2007 & hired_wage == 380;
replace mw = 1 if year == 2006 & hired_wage == 350;
replace mw = 1 if year == 2005 & hired_wage == 300;
replace mw = 1 if year == 2004 & hired_wage == 260;
replace mw = 1 if year == 2003 & hired_wage == 240;

foreach s in "" "_fixed_sample" {;
	bys group level: egen aux_tot_obs_mw`s' = total(n_act`s') if mw == 1; 
	bys group level: egen tot_obs_mw`s' = mean(aux_tot_obs_mw`s');
	replace tot_obs_mw`s'  = 0 if tot_obs_mw`s' == .; 
	replace tot_obs_lvl`s' = tot_obs_lvl`s' - tot_obs_mw`s' ;
};

drop if mw      == 1; 
keep if mult_10 == 1;

gcollapse (sum) n_act* n_hat* inc_act* inc_hat* se_obs* (mean) tot_obs_lvl* tot_inc_lvl*, by(group level mult*);

gen     mult = 10; 
replace mult = 1000 if mult_1000 == 1; 
replace mult = 100  if mult_100  == 1 & mult_1000 == 0;

gen inc_ratio_act   = inc_act/tot_inc_lvl;
gen inc_ratio_hat   = inc_hat/tot_inc_lvl;

foreach s in "" "_fixed_sample" {;
	gen obs_ratio_act`s' = n_act`s'/tot_obs_lvl`s';
	gen obs_ratio_hat`s' = n_hat`s'/tot_obs_lvl`s';

	gen excess_noadj`s' = n_act`s' - n_hat`s';
	gen excess_adj`s'   = n_act`s' - n_hat_adj`s';

	gen theta_noadj`s' = excess_noadj`s'/tot_obs_lvl`s';
	gen theta`s' 	   = excess_adj`s'/tot_obs_lvl`s';
	gen excess_lo`s'   = n_act`s' - (n_hat_adj`s' - 1.96*se_obs`s'); 
	gen excess_hi`s'   = n_act`s' - (n_hat_adj`s' + 1.96*se_obs`s');
	gen theta_lo`s'    = excess_lo`s'/tot_obs_lvl`s'; 			     
	gen theta_hi`s'    = excess_hi`s'/tot_obs_lvl`s';
	gen se`s' = se_obs`s'/tot_obs_lvl`s';
};

drop mult_* excess* se_obs* tot_obs_lvl* obs_ratio* inc_ratio* n_* inc_* tot_inc_lvl* se* theta_noadj*;

reshape wide theta theta_lo theta_hi theta_fixed_sample theta_lo_fixed_sample theta_hi_fixed_sample, i(group level) j(mult);

gen theta              = theta10              + theta100              + theta1000             ;
gen theta_fixed_sample = theta_fixed_sample10 + theta_fixed_sample100 + theta_fixed_sample1000;
gen theta_lo           = theta_hi10           + theta_hi100           + theta_hi1000          ;
gen theta_hi           = theta_lo10           + theta_lo100           + theta_lo1000          ;
gen std_error = (theta_hi - theta)/1.96;

merge 1:1 group level using "${dat}\theta_samplesize.dta"; tab _merge; drop _merge;

save "${dat}/theta_figs.dta", replace;


*** Create database for regressions *** ;
#delimit;
use "${dat}/theta_figs.dta" , replace;
keep if group == "reg_mth_yr";

replace theta100 = theta100 + theta1000; // Add multiples of 1000 to theta100;

tostring level, gen(level_str) format(%15.0g);
gen reg     = real(substr(level_str,1,2));
gen mth     = real(substr(level_str,3,2)); 
gen yr      = real(substr(level_str,-4,.));

gen     reg_met = "";
replace reg_met = "BEL" if reg == 15;
replace reg_met = "FOR" if reg == 23;
replace reg_met = "REC" if reg == 26;
replace reg_met = "SAL" if reg == 29;
replace reg_met = "BH"  if reg == 31;
replace reg_met = "RJ"  if reg == 33;
replace reg_met = "SP"  if reg == 35;
replace reg_met = "CUR" if reg == 41;
replace reg_met = "POA" if reg == 43;
replace reg_met = "GOI" if reg == 52;
replace reg_met = "DF"  if reg == 53;

merge 1:1 reg_met yr mth using "${dat}/ipca_15.dta"; keep if _merge == 3; drop _merge;

gen date = mdy(mth, 1, yr); format date %td;

qui sum ipca if yr == 2006 & mth == 12; 
replace ipca = ipca/`r(mean)'*100; 
gen logcpi = log(ipca);

gen yr16_17 = (inlist(yr,2016,2017)); // Adjust for problems with DV in 2016 and 2017;
foreach v of varlist theta* logcpi {; qui reghdfe `v', absorb(yr16_17) residuals(`v'_res); drop `v'; rename `v'_res `v';}; 
foreach t of varlist theta* logcpi {; qui sum `t' ; replace `t' = (`t'-r(mean))/r(sd); };

keep group level theta theta10 theta100 theta1000 theta_fix* logcpi n_*;

gen reg_mth_yr = level;
tempfile cpi; save `cpi', replace;


#delimit;
use "${dat}/theta_figs.dta", clear;
keep if inlist(group,"firm_size_group", "cdf_hiring_group", "hiring_exp_group", "reg_mth_yr");

drop if group == "firm_size_group"  & inlist(level, 0, 100);
drop if group == "cdf_hiring_group" & inlist(level,    100);
drop if group == "hiring_exp_group" & inlist(level,    100);

keep group level theta theta10 theta100 theta1000 theta_fix* n_*;

replace theta100 = theta100 + theta1000; 

levelsof group, local(groups); foreach g of local groups {;	gen `g' = level if group == "`g'"; };

gen lfirm_size  = log(firm_size_group);
gen lcdf_hiring = log(cdf_hiring_group);
gen lhiring_exp = log(hiring_exp_group);

replace theta100              = theta100 + theta1000;
replace theta_fixed_sample100 = theta_fixed_sample100 + theta_fixed_sample1000;

#delimit;
foreach v in lfirm_size lcdf_hiring lhiring_exp  {; qui sum `v' ; replace `v' = (`v'-r(mean))/r(sd);  foreach t of varlist theta* {; qui sum `t' if `v' !=. ; replace `t' = (`t'-r(mean))/r(sd) if `v' !=. ; }; };
 
append using `cpi';

save "${dat}/theta_reg.dta", replace;

/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>