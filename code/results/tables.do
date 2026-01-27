/*--------------------------------------------------------------------------
This code generates the tables in the main text and some appendix tables

Uses:	    RAIS raw data, firms_panel_merge.dta, hires_firms_panel.dta, hires_`yr'.dta, firmspanel_`yr'.dta, theta_reg.dta, cpi_yrly.dta, muni_metreg.dta, ipca_15.dta
Produces:
			Table 1:  Summary statistics on workers in the RAIS, new-hires sample, and firm random sample;
			Table 2:  The outcomes of firms that tend to hire workers at round numbers;
			Table 3:  The use of round-numbered salaries across decision environments;
			Table 4:  Testing the predictions of the model;
			Table A1: Robustness of firm market outcomes regressions;
			Table A2: Robustness of the use of round-numbered salaries across decision environments;
			Table A4: Testing the predictions of the model using alternative measures of coarse salaries;
			Table B2: Over-time worker separation rate of firms that tend to hire workers at round numbers
			Table B3: Separation rates of new workers hired by bunching firms by worker skill level
			Table B4: Job growth rate of firms that tend to hire workers at round numbers

-------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;

*** Table 1: Summary statistics on workers in the RAIS, new-hires sample, and firm random sample;

#delimit;
foreach s in "new_hires" "RAIS" "firms_panel"  {;
	forvalues yr = 2003(1)2017 {; di in red "`s': `yr'";

		if "`s'" == "new_hires"   {; use "${dat}/hires_`yr'.dta"     , replace; gen byte earn_type = 1; gen white = (race == 2) if race != .; };
		if "`s'" == "firms_panel" {; use "${dat}/firmspanel_`yr'.dta", replace;                                                               };
		if "`s'" == "RAIS"        {; use "${raw}/rais`yr'"           , replace;                       ; gen white = (race == 2) if race != .; };
				
		#delimit;
		keep yr muni earn_type age male white schooling earn_mean hired_wage ind1d;
		keep if earn_type == 1;
		
		merge m:1 yr using "${dat}\cpi_yrly.dta"; keep if _merge == 3; drop _merge; replace cpi = cpi/100 ; ren cpi cpi_yrly;
				
		gen     mw = .                  ;
	    replace mw = 998 if yr == 2019;
	    replace mw = 954 if yr == 2018;
		replace mw = 937 if yr == 2017;
		replace mw = 880 if yr == 2016;
		replace mw = 788 if yr == 2015;
		replace mw = 724 if yr == 2014;
		replace mw = 678 if yr == 2013;
		replace mw = 622 if yr == 2012;
		replace mw = 545 if yr == 2011;
		replace mw = 510 if yr == 2010;
		replace mw = 465 if yr == 2009;
		replace mw = 415 if yr == 2008;
		replace mw = 380 if yr == 2007;
		replace mw = 350 if yr == 2006;
		replace mw = 300 if yr == 2005;
		replace mw = 260 if yr == 2004;
		replace mw = 240 if yr == 2003;
		
		
		glo panel_a "age male white school_elem school_hs school_coll";
				
		gen school_elem = (inrange(schooling, 1, 6 )) if schooling != .;
		gen school_hs   = (inrange(schooling, 7, 8 )) if schooling != .;
		gen school_coll = (inrange(schooling, 9, 11)) if schooling != .;	
		
		
		glo panel_b   "mean_monthly_salary earn_mean_mult_* hired_wage_mult_*";
		glo panel_b_2 "p50_monthly_salary";
		
		gen mean_monthly_salary = earn_mean/cpi_yrly if earn_mean >= mw;
		gen  p50_monthly_salary = earn_mean/cpi_yrly if earn_mean >= mw;
		
		foreach rn in "10" "100" "1000" {; gen earn_mean_mult_`rn'  = !mod(earn_mean,`rn')  if earn_mean  != . & earn_mean  > mw; };
		foreach rn in "10" "100" "1000" {; gen hired_wage_mult_`rn' = !mod(hired_wage,`rn') if hired_wage != . & hired_wage > mw; };
		
		glo panel_c "ind_1 ind_2 ind_3 ind_4 ind_5";
		
		gen ind_1 = (inlist(ind1d,1,2)                      ); // Primary;
		gen ind_2 = (inlist(ind1d,3)                        ); // Industry;
		gen ind_3 = (inlist(ind1d,4,5,8)                    ); // Construction & Utilities;
		gen ind_4 = (inlist(ind1d,6,7)                      ); // Retail;
		gen ind_5 = (inlist(ind1d,9,10,11,12,13,14,15,16,17)); // Services & Domestic Service;
		
		glo panel_d "reg_1 reg_2 reg_3 reg_4 reg_5";

		gen region = int(muni/100000);
		tab region, gen(reg_);
		
		gen wt = 1;

		gcollapse (mean) ${panel_a} ${panel_b} ${panel_c}  ${panel_d} (p50) ${panel_b_2} (sum) wt;
		
		gen yr = `yr';
		gen s = "`s'";
	
		compress;
		save "${dat}/toappend_`s'_`yr'.dta", replace;
	};
};

#delimit; 

use "${dat}/toappend_RAIS_2013.dta", replace; save "${dat}/summ_RAIS_2013.dta", replace;

clear; foreach s in "firms_panel" "new_hires" "RAIS" {; forvalues yr = 2003(1)2017 {; append using "${dat}/toappend_`s'_`yr'.dta"; *erase "${dat}/toappend_`s'_`yr'.dta"; }; };
order s yr wt;
gen cnt = 1;

collapse (sum) cnt (mean) age male white school_* mean_monthly_salary p50_monthly_salary earn_mean_mult_* hired_wage_mult_* ind_* reg_* [fw=wt], by(s);

glo vlist "age male white school_elem school_hs school_coll mean_monthly_salary p50_monthly_salary earn_mean_mult_10 earn_mean_mult_100 earn_mean_mult_1000 hired_wage_mult_10 hired_wage_mult_100 hired_wage_mult_1000 ind_1 ind_2 ind_3 ind_4 ind_5 reg_1 reg_2 reg_3 reg_4 reg_5";

#delimit;
foreach v in cnt    {; gen temp="&"+string(`v',"%25.0fc"); drop `v'; rename temp `v';};
foreach v in $vlist {; gen temp="&"+string(`v',"%15.3f" ); drop `v'; rename temp `v';};
foreach v in cnt $vlist {; rename `v' val`v';};
	
reshape long val, i(s)       j(varname) string;
reshape wide val, i(varname) j(s)       string; 

loc n = 0;
gen order = 0;

foreach v in $vlist cnt {; 
	loc ++n; 
	replace order = `n' if varname == "`v'";
};

sort order;

gen cat = "";
foreach v in age male white school_elem school_hs school_coll {; replace cat = "char" if varname == "`v'"; };
foreach v in mean_monthly_salary p50_monthly_salary earn_mean_mult_10 earn_mean_mult_100 earn_mean_mult_1000 hired_wage_mult_10 hired_wage_mult_100 hired_wage_mult_1000 {; replace cat = "earnings" if varname == "`v'"; };
foreach v in ind_1 ind_2 ind_3 ind_4 ind_5 {; replace cat = "industry" if varname == "`v'"; };
foreach v in reg_1 reg_2 reg_3 reg_4 reg_5 {; replace cat = "region"   if varname == "`v'"; };
foreach v in cnt                           {; replace cat = "n"        if varname == "`v'"; };

gen     label= "";

replace label="Average age"                                     if var=="age"                 ;
replace label="Male"                                            if var=="male"                ;
replace label="White"                                           if var=="white"               ;
replace label="Elementary school or less"                       if var=="school_elem"         ;
replace label="Completed high school"                           if var=="school_hs"           ;
replace label="Completed university"                            if var=="school_coll"         ;
	
replace label="Mean monthly salary (Reals)"	                    if var=="mean_monthly_salary" ;
replace label="Median monthly salary (Reals)"                   if var=="p50_monthly_salary"  ;
replace label="Share of mean earnings divisible by 10"          if var=="earn_mean_mult_10"   ;
replace label="Share of mean earnings divisible by 100"         if var=="earn_mean_mult_100"  ;
replace label="Share of mean earnings divisible by 1,000"       if var=="earn_mean_mult_1000" ;
replace label="Share of contracted earnings divisible by 10"    if var=="hired_wage_mult_10"  ;
replace label="Share of contracted earnings divisible by 100"   if var=="hired_wage_mult_100" ;
replace label="Share of contracted earnings divisible by 1,000" if var=="hired_wage_mult_1000";

replace label="Primary sector"                                  if var=="ind_1"              ;
replace label="Manufacturing"                                   if var=="ind_2"              ;
replace label="Construction and utilities"                      if var=="ind_3"              ;
replace label="Retail"                                          if var=="ind_4"              ;
replace label="Services"                                        if var=="ind_5"              ;

replace label="North"                                           if var=="reg_1"              ;
replace label="Northeast"                                       if var=="reg_2"              ;
replace label="Southeast"                                       if var=="reg_3"              ;
replace label="South"                                           if var=="reg_4"              ;
replace label="Midwest"                                         if var=="reg_5"              ;

replace label="Number of observations"                          if var=="cnt";

#delimit;
gen end="\\"; drop var; 

keep cat label val* end; 
order label valRAIS valnew_hires valfirms_panel end; 
levelsof cat, local(grps); foreach g of local grps {; 
	preserve; keep if cat=="`g'"; drop cat; outsheet using "${res}/summ-`g'.tex", replace noquote nonames delimiter(" "); restore; 
};


*** Table 2: The outcomes of firms that tend to hire workers at round numbers;
*** Table 3: The use of round-numbered salaries across decision environments;
*** Table A1: Robustness of firm market outcomes regressions;
*** Table A2: Robustness of the use of round-numbered salaries across decision environments;

#delimit ;

glo opt_tex `"replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&")"';

glo outcomes  "sep_1yr res_1yr job_growth zero_active" ;
glo heurist   "wage_grw_abs_rn wage_grw_pct_int wage_grw_round" ;
glo ineq      "gini r_90_to_10 r_90_to_50 r_50_to_10" ;
glo sticky    "wage_grw_zero";
glo xlist     "firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr miss_*";
glo felist    "ind_reg_yr mean_size hiring_exp";
glo wlist     "occ2d male race";
glo bunch     "buncher buncher_shr buncher_yr buncher_over66 buncher_over50 buncher_bigf buncher_100 buncher_shr100" ;

use "${dat}\firms_panel_merge.dta", replace;

fmerge m:1 id firmid yr using "${dat}\hires_firms_panel.dta"; tab _merge; gen in_hires_panel = (_merge == 3); drop _merge; 

drop if shr_hires_10 == . ;
replace mean_pay_yr = mean_pay if zero_active == 1;

foreach v of varlist firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr male race occ2d {; gen miss_`v' = (`v' == .); tab miss_`v'; if r(r) == 1 drop miss_`v'; replace `v' = 0 if `v' == .; }; drop miss_shr_coll;

egen ind_reg_yr = group(ind2d micro yr);

gen buncher_bigf = buncher if mean_size > 5;
gen buncher_100  = (shr_hires_100 == 1) if shr_hires_100 != .;

gen hired_wage_bin100 = floor(hired_wage/100)*100;

rename shr_hires_10  buncher_shr;
rename shr_hires_100 buncher_shr100;

gen heurist_samp = (wage_grw_abs_rn != . & wage_grw_pct_int != .);

keep firmid yr ${outcomes} ${heurist} ${ineq} ${sticky} ${xlist} ${felist} ${wlist} ${bunch} heurist_samp in_hires_panel; 
compress;

preserve; keep if in_hires_panel == 1   ; keep firmid yr ${outcomes} ${sticky}                     ${bunch} ${xlist} ${felist} ${wlist}; drop job_growth zero_active;  compress; save "${dat}\temp_worker.dta" , replace; restore;
preserve; bys firmid yr: keep if _n == 1; keep firmid yr ${outcomes}           ${ineq}             ${bunch} ${xlist} ${felist}         ; drop sep_1yr res_1yr       ;  compress; save "${dat}\temp_firm.dta"   , replace; restore;
preserve; keep if heurist_samp == 1     ; keep firmid yr             ${sticky}          ${heurist} ${bunch} ${xlist} ${felist} ${wlist};                            ;  compress; save "${dat}\temp_heu.dta"    , replace; restore;

tempname buncherbeta; tempfile buncher_file; postfile `buncherbeta' str70(dv bunchvar beta se mean_nonbuncher n) using `buncher_file', replace;

#delimit;
foreach b in $bunch {;
   
  if "`b'" == "buncher"        {; loc file_name "bunch_allf"    ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_100"    {; loc file_name "bunch100_allf" ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_yr"     {; loc file_name "bunchyr_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_over66" {; loc file_name "bunch66_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_over50" {; loc file_name "bunch50_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_bigf"   {; loc file_name "bunch_bigf"    ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_shr"    {; loc file_name "shr10_allf"    ; loc var_lab "Share~hired~at~a~round~number"; };
  if "`b'" == "buncher_shr100" {; loc file_name "shr100_allf"   ; loc var_lab "Share~hired~at~mult~100"      ; };
 
  * Firm performance regressions;
   eststo clear; 
   foreach dv of global outcomes {;   
	if  inlist("`dv'","res_1yr","sep_1yr") {; use "${dat}\temp_worker.dta", replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist} ${wlist}) cluster(firmid); };
   	if !inlist("`dv'","res_1yr","sep_1yr") {; use "${dat}\temp_firm.dta"  , replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist}         ) cluster(firmid); };
	           							      loc beta = _b[`b']; loc se = _se[`b']; loc N = e(N);
											  sum `dv' if e(sample) & `b' == 0, mean; loc mean = r(mean)         ; post `buncherbeta' ("`dv'") ("`b'") ("`beta'") ("`se'") ("`mean'") ("`N'");
	           							      sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
											  
								 };    
  	esttab using "${res}/perf_`file_name'.tex", keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
   
	 
  * Heuristic decisions regressions;
	eststo clear; 
	use "${dat}\temp_heu.dta" , replace;
	foreach dv of global heurist {; eststo: reghdfe `dv' `b' ${xlist}                      , absorb(${felist} ${wlist}) cluster(firmid); loc beta = _b[`b']; loc se = _se[`b']; loc N = e(N);
						            sum `dv' if e(sample) & `b' == 0, mean; loc mean = r(mean)         ; post `buncherbeta' ("`dv'") ("`b'") ("`beta'") ("`se'") ("`mean'") ("`N'");
						            sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
	
		                            eststo: reghdfe `dv' `b' ${xlist} if wage_grw_zero != 1, absorb(${felist} ${wlist}) cluster(firmid); loc beta = _b[`b']; loc se = _se[`b']; loc N = e(N);
								    sum `dv' if e(sample) & `b' == 0, mean; loc mean = r(mean)         ; post `buncherbeta' ("`dv'_nozero") ("`b'") ("`beta'") ("`se'") ("`mean'") ("`N'");
								    sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
	                             };
	esttab using "${res}/heurist_`file_name'.tex" , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
	
	  
  * Wage inequality regressions;  
    eststo clear; 
	use "${dat}\temp_firm.dta"  , replace;
	foreach dv of global ineq {;  eststo: reghdfe `dv' `b' ${xlist}                        , absorb(${felist}          ) cluster(firmid); loc beta = _b[`b']; loc se = _se[`b']; loc N = e(N);
							      sum `dv' if e(sample) & `b' == 0, mean; loc mean = r(mean)         ; post `buncherbeta' ("`dv'") ("`b'") ("`beta'") ("`se'") ("`mean'") ("`N'");
							      sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
	                          };
	
  	esttab using "${res}/ineq_`file_name'.tex"   , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
  
  
  * Wage stickiness;
	eststo clear; 
	use "${dat}\temp_heu.dta" , replace;
	foreach dv of global sticky {; eststo: reghdfe `dv' `b' ${xlist}                      , absorb(${felist} ${wlist}) cluster(firmid); loc beta = _b[`b']; loc se = _se[`b']; loc N = e(N);
								   sum `dv' if e(sample) & `b' == 0, mean; loc mean = r(mean)         ; post `buncherbeta' ("`dv'") ("`b'") ("`beta'") ("`se'") ("`mean'") ("`N'");
								   sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
	                            };
	
	esttab using "${res}/sticky_`file_name'.tex"  , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
	
};


postclose `buncherbeta'; use `buncher_file', clear; destring, replace; compress; save "${dat}\buncher_coefficients.dta", replace;



*** Specifications that include wage bins controls;

#delimit ;

glo outcomes  "sep_1yr res_1yr job_growth zero_active" ;
glo heurist   "wage_grw_abs_rn wage_grw_pct_int wage_grw_round" ;
glo ineq      "gini r_90_to_10 r_90_to_50 r_50_to_10" ;
glo sticky    "wage_grw_zero";
glo xlist     "firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr miss_*";
glo felist    "ind_reg_yr mean_size hiring_exp occ2d male race hired_wage_bin100";

glo bunch     "buncher buncher_shr buncher_yr buncher_over66 buncher_over50 buncher_bigf buncher_100 buncher_shr100" ;

use "${dat}\firms_panel_merge.dta", replace;

fmerge m:1 id firmid yr using "${dat}\hires_firms_panel.dta"; tab _merge; gen in_hires_panel = (_merge == 3); drop _merge; 

drop if shr_hires_10 == . ;
replace mean_pay_yr = mean_pay if zero_active == 1;

foreach v of varlist firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr male race occ2d {; gen miss_`v' = (`v' == .); tab miss_`v'; if r(r) == 1 drop miss_`v'; replace `v' = 0 if `v' == .; }; drop miss_shr_coll;

egen ind_reg_yr = group(ind2d micro yr);

gen buncher_bigf = buncher if mean_size > 5;
gen buncher_100  = (shr_hires_100 == 1) if shr_hires_100 != .;

gen hired_wage_bin100 = floor(hired_wage/100)*100;

rename shr_hires_10  buncher_shr;
rename shr_hires_100 buncher_shr100;

gen heurist_samp = (wage_grw_abs_rn != . & wage_grw_pct_int != .);

keep firmid yr ${outcomes} ${heurist} ${ineq} ${sticky} ${xlist} ${felist}          ${bunch} heurist_samp in_hires_panel;
compress;

#delimit;
foreach b in $bunch {;
   
  if "`b'" == "buncher"        {; loc file_name "bunch_allf"    ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_100"    {; loc file_name "bunch100_allf" ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_yr"     {; loc file_name "bunchyr_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_over66" {; loc file_name "bunch66_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_over50" {; loc file_name "bunch50_allf"  ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_bigf"   {; loc file_name "bunch_bigf"    ; loc var_lab "Bunching~firm"                ; };
  if "`b'" == "buncher_shr"    {; loc file_name "shr10_allf"    ; loc var_lab "Share~hired~at~a~round~number"; };
  if "`b'" == "buncher_shr100" {; loc file_name "shr100_allf"   ; loc var_lab "Share~hired~at~mult~100"      ; };
 
  * Firm performance regressions;
   eststo clear; 
   foreach dv of global outcomes {;   
	if  inlist("`dv'","res_1yr","sep_1yr") {; eststo: reghdfe `dv' `b' ${xlist} if in_hires_panel == 1                   , absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'";  };
   	if !inlist("`dv'","res_1yr","sep_1yr") {; eststo: reghdfe `dv' `b' ${xlist}                                          , absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'";  };
  
   };    
  	esttab using "${res}/perf_`file_name'_full.tex"    , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
   
	 
   * Heuristic decisions regressions;
	eststo clear; 
	foreach dv of global heurist {;          eststo: reghdfe `dv' `b' ${xlist} if heurist_samp == 1                     , absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'"; 
		                                     eststo: reghdfe `dv' `b' ${xlist} if heurist_samp == 1 & wage_grw_zero != 1, absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'"; 
	};
	esttab using "${res}/heurist_`file_name'_full.tex" , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
	
	  
  * Wage compression regressions;  
	eststo clear; 
	foreach dv of global ineq {;             eststo: reghdfe `dv' `b' ${xlist}                                         , absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'"; }; 
  	esttab using "${res}/ineq_`file_name'_full.tex"   , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
  
  
  * Wage stickiness;
	eststo clear; 
	foreach dv of global sticky {;           eststo: reghdfe `dv' `b' ${xlist} if in_hires_panel == 1                  , absorb(${felist}) cluster(firmid); sum `dv' if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean "`mean'"; };
	esttab using "${res}/sticky_`file_name'_full.tex"  , keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
	
};




*** Table B2: Over-time worker separation rate of firms that tend to hire workers at round numbers
*** Table B3: Separation rates of new workers hired by bunching firms by worker skill level
*** Table B4: Job growth rate of firms that tend to hire workers at round numbers


#delimit ;

glo outcomes   "sep_1yr res_1yr job_growth zero_active" ;
glo outcomes2  "sep_1yr sep_2yr sep_3yr res_1yr res_2yr res_3yr" ;
glo outcomes3  "sep_1yr_hs sep_1yr_nohs res_1yr_hs res_1yr_nohs" ;
glo outcomes4  "job_growth_highpay job_growth_lowpay job_growth_hs job_growth_nohs" ; 
 
glo heurist   "wage_grw_abs_rn wage_grw_pct_int wage_grw_round" ;
glo ineq      "gini r_90_to_10 r_90_to_50 r_50_to_10" ;
glo sticky    "wage_grw_zero";
glo xlist     "firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr miss_*";
glo felist    "ind_reg_yr mean_size hiring_exp occ2d male race hired_wage_bin100";
glo bunch     "buncher buncher_bigf " ;

use "${dat}\firms_panel_merge.dta", replace;


fmerge m:1 id firmid yr using "${dat}\hires_firms_panel.dta"; tab _merge; gen in_hires_panel = (_merge == 3); drop _merge; 

drop if shr_hires_10 == . ;
replace mean_pay_yr = mean_pay if zero_active == 1;

foreach v of varlist firm_age shr_hs shr_coll ceo_edu has_hr mean_pay_yr male race occ2d {; gen miss_`v' = (`v' == .); tab miss_`v'; if r(r) == 1 drop miss_`v'; replace `v' = 0 if `v' == .; }; drop miss_shr_coll;

egen ind_reg_yr = group(ind2d micro yr);

gen buncher_bigf = buncher if mean_size > 5;

keep firmid yr ${outcomes2} ${outcomes3} ${outcomes4} ${sticky} ${xlist} ${felist} ${wlist} ${bunch} in_hires_panel; 
compress;

#delimit;
preserve; keep if in_hires_panel == 1   ; keep firmid yr ${outcomes2} ${outcomes3} ${outcomes4} ${bunch} ${xlist} ${felist} ${wlist}; drop job_growth*      ;  compress; save "${dat}\temp_worker.dta" , replace; restore;
preserve; bys firmid yr: keep if _n == 1; keep firmid yr ${outcomes2} ${outcomes3} ${outcomes4} ${bunch} ${xlist} ${felist}         ; drop sep_*yr* res_*yr*;  compress; save "${dat}\temp_firm.dta"   , replace; restore;


#delimit;
foreach b in $bunch {;
   
   if "`b'" == "buncher"        {; loc file_name "bunch_allf"    ; loc var_lab "Bunching~firm"                ; };
   if "`b'" == "buncher_bigf"   {; loc file_name "bunch_bigf"    ; loc var_lab "Bunching~firm"                ; };

  * Firm performance regressions - separation rates over time;
   eststo clear; 
   foreach dv in $outcomes2 {;   
	if  regex("`dv'","sep") |  regex("`dv'","res") {; use "${dat}\temp_worker.dta", replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist} ${wlist}) cluster(firmid); };
   	if !regex("`dv'","sep") & !regex("`dv'","res") {; use "${dat}\temp_firm.dta"  , replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist}         ) cluster(firmid); };
	           							         sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
											  
								 };    
  	esttab using "${res}/perf2_`file_name'.tex", keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));

  * Firm performance regressions - separation rates by skill level ;
   eststo clear; 
   foreach dv in $outcomes3 {;   
	if   regex("`dv'","sep") |  regex("`dv'","res") {; use "${dat}\temp_worker.dta", replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist} ${wlist}) cluster(firmid); };
   	if  !regex("`dv'","sep") & !regex("`dv'","res") {; use "${dat}\temp_firm.dta"  , replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist}         ) cluster(firmid); };
	           							         sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
											  
								 };    
  	esttab using "${res}/perf3_`file_name'.tex", keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));

	
	  * Firm performance regressions - job growth rate by skill level ;
   eststo clear; 
   foreach dv in $outcomes4 {;   
	if   regex("`dv'","sep") |  regex("`dv'","res") {; use "${dat}\temp_worker.dta", replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist} ${wlist}) cluster(firmid); };
   	if  !regex("`dv'","sep") & !regex("`dv'","res") {; use "${dat}\temp_firm.dta"  , replace; eststo: reghdfe `dv' `b' ${xlist}, absorb(${felist}         ) cluster(firmid); };
	           							         sum `dv' if e(sample)           , mean; loc mean : di %4.3f r(mean); estadd local mean "`mean'"; 
											  
								 };    
  	esttab using "${res}/perf4_`file_name'.tex", keep(bunch*) coeflabels(`b' "`var_lab'") ${opt_tex} stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N"));
	
};

*** Table 4: Testing the predictions of the model;
*** Table A4: Testing the predictions of the model using alternative measures of coarse salaries;

#delimit;
use "${dat}/firms_panel_merge.dta", replace;

keep if inlist(adm_type,1,2) & earn_type == 1;
drop if is_fed_mw == 1 | is_st_mw == 1;

gen mth = month(adm_date);

merge m:1 muni           using "${dat}\muni_metreg.dta"; tab _merge; drop if _merge == 2; drop _merge; 
merge m:1 reg_met yr mth using "${dat}\ipca_15.dta"    ; tab _merge; drop if _merge != 3; drop _merge;

drop schooling;
encode reg_met, gen(reg);

egen mth_yr = group(mth yr);
gen time = mth_yr;
gen logcpi      = log(ipca);
gen lfirm_size  = log(1+firm_size);
gen lcdf_hiring = log(cdf_hiring);
gen schooling   = yrs_edu;

tab reg  , gen(regmet_); drop reg   ; drop regmet_1;
tab occ1d, gen(occup_) ;            ; drop occup_1 ;
tab race , gen(race_)   ;           ; drop race_1   ;
tab yr   , gen(yr_)    ;            ; drop yr_1    ;

keep pis mult_* logcpi potential_exp schooling lfirm_size lcdf_hiring lhiring_exp firmid hired_wage yrs_insample male race_* occup_* regmet_* mth_* yr_* yr;

glo xlist "male race_* occup_* regmet_* yr_*";
glo zlist "logcpi potential_exp schooling lfirm_size lcdf_hiring";

reghdfe mult_10 ${zlist} ${xlist}, absorb(firmid); gen insample = (e(sample)); tab insample; keep if insample == 1; drop insample;
	
#delimit;
append using "${dat}/theta_reg.dta";

#delimit;
foreach v in "logcpi" "lfirm_size" "lcdf_hiring" {;	di in red "`v'";
	
	if "`v'" == "logcpi"        glo ylab "Consumer~price~index~OPAREN logs CPAREN"  ;
	if "`v'" == "lfirm_size"    glo ylab "Firm~size~OPAREN logs CPAREN"             ;
	if "`v'" == "lcdf_hiring"   glo ylab "Firm~hiring~experience~OPAREN logs CPAREN";
	
	glo res_list = subinstr("${zlist}", "`v'", "", .);

	qui reghdfe mult_10   ${res_list} ${xlist}                       , absorb(firmid) residual(mult_10_res)     ; qui sum mult_10_res                             ; replace mult_10_res      = mult_10_res/r(sd);
	qui reghdfe mult_10   ${res_list} ${xlist} if yrs_insample  == 15, absorb(firmid) residual(mult_10_res_fs)  ; qui sum mult_10_res_fs    if yrs_insample  == 15; replace mult_10_res_fs   = mult_10_res_fs/r(sd);
	qui reghdfe `v'       ${res_list} ${xlist}                       , absorb(firmid) residual(`v'_res)         ; qui sum `v'_res                                 ; replace `v'_res          = `v'_res/r(sd);
	qui reghdfe `v'       ${res_list} ${xlist} if yrs_insample  == 15, absorb(firmid) residual(`v'_res_fs)      ; qui sum `v'_res_fs        if yrs_insample  == 15; replace `v'_res_fs       = `v'_res_fs/r(sd);
	
	eststo clear; 
	eststo: reg theta                   `v'                              , robust; 
	eststo: reg theta_fixed_sample      `v'                              , robust;
	eststo: reg mult_10_res             `v'_res                          , robust; 
	eststo: reg mult_10_res_fs          `v'_res_fs if yrs_insample  == 15, robust;

   	esttab using "${res}/reg_mult10_`v'.tex" , keep(`v'*) rename(`v'_res `v' `v'_res_fs `v') coeflabels(`v' "${ylab}") replace noobs nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");	

	drop *_res *_res_fs;

};

#delimit;
foreach v in "logcpi" "lfirm_size" "lcdf_hiring" {;	di in red "`v'";
	
	if "`v'" == "logcpi"        glo ylab "Consumer~price~index~OPAREN logs CPAREN"  ;
	if "`v'" == "lfirm_size"    glo ylab "Firm~size~OPAREN logs CPAREN"             ;
	if "`v'" == "lcdf_hiring"   glo ylab "Firm~hiring~experience~OPAREN logs CPAREN";
	
	glo res_list = subinstr("${zlist}", "`v'", "", .);

	qui reghdfe mult_10   ${res_list} ${xlist}, absorb(firmid) residual(mult_10_res)  ; qui sum mult_10_res  ; replace mult_10_res   = mult_10_res/r(sd);
	qui reghdfe mult_100  ${res_list} ${xlist}, absorb(firmid) residual(mult_100_res) ; qui sum mult_100_res ; replace mult_100_res  = mult_100_res/r(sd);
	qui reghdfe mult_1000 ${res_list} ${xlist}, absorb(firmid) residual(mult_1000_res); qui sum mult_1000_res; replace mult_1000_res = mult_1000_res/r(sd);
	qui reghdfe `v'       ${res_list} ${xlist}, absorb(firmid) residual(`v'_res)      ; qui sum `v'_res      ; replace `v'_res       = `v'_res/r(sd);
	
	eststo clear; 
	eststo: reg theta         `v'    , robust; 
	eststo: reg theta100      `v'    , robust; 
	eststo: reg theta1000     `v'    , robust; 
	eststo: reg mult_10_res   `v'_res, robust; 
	eststo: reg mult_100_res  `v'_res, robust; 
	eststo: reg mult_1000_res `v'_res, robust; 

	esttab using "${res}/reg_mult_`v'_rob.tex" , keep(`v'*) rename(`v'_res `v') coeflabels(`v' "${ylab}") replace noobs nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");	
	
	drop *_res ;

};


/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
