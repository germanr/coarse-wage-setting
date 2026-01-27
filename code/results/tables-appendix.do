/*--------------------------------------------------------------------------
This code generates the appendix tables 

Uses:	 	firm_variables.dta, pnad_2013.dta, cpi_yrly.dta, rais_sample_restrictions.xlsx, hires_firms_panel.dta  
Produces: 		   
			Table A1: The characteristics of bunching firms;
			Table B1: Summary statistics of workers in the RAIS and the PNAD during 2013;
			Table D1: Sample size after each restriction;
			Table G1: Federal minimum wages in Brazil: 2003–2017;
			Table G2: Fraction of workers earning a round salary in year t + 1 as a function of their initial salary;
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;


*** Table A1: The characteristics of bunching firms;

glo opt_tex `"replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&")"'

#delimit;
use "${dat}\firm_variables.dta", clear;
keep firmid buncher firm_age ceo_edu has_hr mean_pay lmean_size mean_size lhiring_exp;

sort firmid;
foreach v in firm_age ceo_edu    {; rename `v' tmp; by firmid: egen `v'=mean(tmp); drop tmp;};
foreach v in has_hr              {; rename `v' tmp; by firmid: egen `v'= max(tmp); drop tmp;};
foreach v in mean_pay            {; gen l`v' = log(1+`v'); };

by firmid: gen one = (_n == 1); 
keep if one == 1;

gen buncher_bigf = buncher if mean_size > 5;

foreach v of varlist lmean_size lmean_pay lhiring_exp firm_age has_hr ceo_edu {; gen miss_`v' = (`v' == .); tab miss_`v'; if r(r) == 1 drop miss_`v'; replace `v' = 0 if `v' == .; };

#delimit;
foreach v in lhiring_exp lmean_size firm_age has_hr ceo_edu lmean_pay {; di in red "`v'" ;
	eststo clear;

	if "`v'" == "ceo_edu"      loc label "Education~manager"; 
	if "`v'" == "has_hr"       loc label "Has~an~HR~department"; 
	if "`v'" == "lmean_pay"    loc label "Average~salary~OPAREN logs CPAREN";
	if "`v'" == "firm_age"     loc label "Firm~age~OPAREN years CPAREN";
	if "`v'" == "lmean_size"   loc label "Firm~size~OPAREN logs CPAREN";
	if "`v'" == "lhiring_exp"  loc label "Hiring~experience~OPAREN logs CPAREN";
		
            summ  `v'       if buncher == 1 & miss_`v' == 0        ; gen  temp=r(mean);
	eststo: reg temp one    if buncher == 1, nocons; drop temp;
			summ  `v'       if buncher == 0 & miss_`v' == 0        ; gen  temp=r(mean);
	eststo: reg temp one    if buncher == 0, nocons; drop temp;
	eststo: reg `v' buncher if                miss_`v' == 0         , cluster(firmid);
						    
	        summ  `v'       if buncher == 1 & mean_size >5 & miss_`v' == 0; gen  temp=r(mean);
	eststo: reg temp one    if buncher == 1 & mean_size >5, nocons; drop temp;
            summ  `v'       if buncher == 0 & mean_size >5 & miss_`v' == 0; gen  temp=r(mean);
	eststo: reg temp one    if buncher == 0 & mean_size >5, nocons; drop temp;
	eststo: reg `v' buncher if                mean_size >5 & miss_`v' == 0, cluster(firmid);	

						  esttab using "${res}/char_`v'.tex", rename(one buncher) keep(buncher) coeflabels(buncher "`label'") ${opt_tex} noobs;
	if "`v'"=="lmean_pay" esttab using "${res}/char_n.tex", keep(one) drop(one) ${opt_tex} noobs stats(N, fmt(%20.0fc) layout("\multicolumn{1}{c}{@}") labels("\(N\)"));
};


*** Table B1: Summary statistics of workers in the RAIS and the PNAD during 2013;


#delimit;
foreach s in "pnad" "formal_prod" "formal_legal" "informal_prod" "informal_legal" {;

	if "`s'" != "RAIS" {; 
	
	use "${dat}\pnad_2013.dta", clear;
		
	gen pnad           = 1               ;
	gen formal_prod    = (categ_lab == 1);
	gen formal_legal   = (djubila   == 1);
	gen informal_prod  = (categ_lab == 2);
	gen informal_legal = (djubila   == 0);
    keep if `s' == 1;	
	
	gen age           = edad   ;
	gen male          = hombre ;
	gen schooling     = aedu   ;
	gen earn_mean     = ila    ;
	gen earn_type     = 1      ;
	gen weight        = pondera;
	gen yr            = ano    ;
	gen st            = reg_uf ;
	
	replace earn_mean = earn_mean * p_reg; // Remove regional price adjustment;
	
	gen     white = .                          ;
	replace white = 1 if inlist(raza,1)        ;
	replace white = 0 if inlist(raza,2,3,4,5,6);
	
	gen school_elem = inlist(nivel, 0, 1, 2, 3) if nivel != .;
	gen school_hs   = inlist(nivel, 4, 5)       if nivel != .;
	gen school_coll	= inlist(nivel, 6)          if nivel != .;
		

	gen     ind = .                                             ; 
	replace ind = 1 if inlist(sector1d,1,2,3)                   ; // Primary
	replace ind = 2 if inlist(sector1d,4)                       ; // Industry
	replace ind = 3 if inlist(sector1d,5,6,9)                   ; // Construction & Utilities
	replace ind = 4 if inlist(sector1d,7,8)                     ; // Retail
	replace ind = 5 if inlist(sector1d,10,11,12,13,14,15,16,17) ; // Services & Domestic Service
		
	keep if inrange(edad,18,65) & ocupado == 1;
	*drop if relab == 1 | relab == 3 | relab == 5; // Drop employers, self-employed, and not salaried;
		
	};
	
	if "`s'" == "RAIS" {; 
	
	gen weight = 1;
	
	gen     ind = . ;
	replace ind = 1 if inlist(ind1d,1,2)                      ; // Primary;
	replace ind = 2 if inlist(ind1d,3)                        ; // Industry;
	replace ind = 3 if inlist(ind1d,4,5,8)                    ; // Construction & Utilities;
	replace ind = 4 if inlist(ind1d,6,7)                      ; // Retail;
	replace ind = 5 if inlist(ind1d,9,10,11,12,13,14,15,16,17); // Services & Domestic Service;
			
	gen school_elem = (inrange(schooling, 1, 6 )) if schooling != .;
	gen school_hs   = (inrange(schooling, 7, 8 )) if schooling != .;
	gen school_coll = (inrange(schooling, 9, 11)) if schooling != .;	// check;
	
	gen region = int(muni/100000);
    gen st = floor(muni/10000);

	};


	keep yr earn_type age male white schooling earn_mean school_elem school_hs school_coll ind region weight;
	
	merge m:1 yr using "${dat}\cpi_yrly.dta"; keep if _merge == 3; drop _merge; replace cpi = cpi/100 ; ren cpi cpi_yrly;
	
	keep if earn_type == 1;

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
	
	glo panel_b   "mean_monthly_salary";
	glo panel_b_2 "p50_monthly_salary";
	
	gen mean_monthly_salary = earn_mean/cpi_yrly if earn_mean >= mw;
	gen  p50_monthly_salary = earn_mean/cpi_yrly if earn_mean >= mw;

	glo panel_c "ind_1 ind_2 ind_3 ind_4 ind_5";
	
	gen ind_1 = (ind == 1) if ind != .;
	gen ind_2 = (ind == 2) if ind != .;
	gen ind_3 = (ind == 3) if ind != .;
	gen ind_4 = (ind == 4) if ind != .;
	gen ind_5 = (ind == 5) if ind != .;
	
	glo panel_d "reg_1 reg_2 reg_3 reg_4 reg_5";

	tab region, gen(reg_);
	
	gen wt = 1;
	
	gcollapse (mean) ${panel_a} ${panel_b} ${panel_c} ${panel_d} (p50) ${panel_b_2} (sum) wt (rawsum) cnt=wt [fw=weight];
	
	gen s = "`s'";

	compress;
	save "${dat}/toappend_`s'.dta", replace;
};


#delimit;
clear; foreach s in "toappend_pnad" "toappend_formal_prod" "toappend_formal_legal" "toappend_informal_prod" "toappend_informal_legal" "summ_RAIS_2013" {; append using "${dat}/`s'.dta"; erase "${dat}/`s'.dta"; };
order s;

replace cnt = wt if s == "RAIS";

#delimit;
drop earn_mean_mult_10 earn_mean_mult_100 earn_mean_mult_1000 hired_wage_mult_10 hired_wage_mult_100 hired_wage_mult_1000 yr;

glo vlist "age male white school_elem school_hs school_coll mean_monthly_salary p50_monthly_salary ind_1 ind_2 ind_3 ind_4 ind_5 reg_1 reg_2 reg_3 reg_4 reg_5";

#delimit;
foreach v in cnt wt        {; gen temp="&"+string(`v',"%25.0fc"); drop `v'; rename temp `v';};
foreach v in $vlist        {; gen temp="&"+string(`v',"%15.3f" ); drop `v'; rename temp `v';};
foreach v in cnt wt $vlist {; rename `v' val`v';};
	
reshape long val, i(s)       j(varname) string;
reshape wide val, i(varname) j(s)       string; 

loc n = 0;
gen order = 0;

foreach v in $vlist cnt {; loc ++n;  replace order = `n' if varname == "`v'"; };

sort order;

gen cat = "";
foreach v in age male white school_elem school_hs school_coll {; replace cat = "char"     if varname == "`v'"; };
foreach v in mean_monthly_salary p50_monthly_salary           {; replace cat = "earnings" if varname == "`v'"; };
foreach v in ind_1 ind_2 ind_3 ind_4 ind_5                    {; replace cat = "industry" if varname == "`v'"; };
foreach v in reg_1 reg_2 reg_3 reg_4 reg_5                    {; replace cat = "region"   if varname == "`v'"; };
foreach v in cnt wt                                           {; replace cat = "n"        if varname == "`v'"; };

gen     label= "";

replace label="Average age"                                    if var=="age"                 ;
replace label="Male"                                           if var=="male"                ;
replace label="White"                                          if var=="white"               ;
replace label="Elementary school or less"                      if var=="school_elem"         ;
replace label="Completed high school"                          if var=="school_hs"           ;
replace label="Completed university"                           if var=="school_coll"         ;
	
replace label="Mean monthly salary (Reals)"	                   if var=="mean_monthly_salary" ;
replace label="Median monthly salary (Reals)"                  if var=="p50_monthly_salary"  ;

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

replace label="Observations (unweighted)"                       if var=="cnt";
replace label="Observations (weighted)"                         if var=="wt";

gen end="\\"; drop var; 

gen b1 = "&";
gen b2 = "&";
gen b3 = "&";

keep cat label val* b1 b2 b3 end; 
order label valRAIS b1 valpnad b2 valformal_legal valinformal_legal b3 valformal_prod valinformal_prod end; 
levelsof cat, local(grps); foreach g of local grps {; 
	preserve; keep if cat=="`g'"; drop cat; outsheet using "${res}/pnad-`g'.tex", replace noquote nonames delimiter(" "); restore; 
};



*** Table D1: Sample size after each restriction;

#delimit;
import excel using "${dat}/rais_sample_restrictions.xlsx", firstrow clear;

#delimit;
gen     order =  .                                      ;
replace order = 1  if stage == "Initial sample size"    ;
replace order = 2  if stage == "Initial # workers"      ;
replace order = 3  if stage == "Initial # firms"        ;

replace order = 4  if stage == "New hire"               ;
replace order = 5  if stage == "Valid PIS"              ;
replace order = 6  if stage == "Private firms"          ;
replace order = 7  if stage == "Monthly contracts"      ;
replace order = 8  if stage == "Above MW"               ;
replace order = 9  if stage == "Multiple positions"     ;
replace order = 10 if stage == "Hired wage available"   ;

replace order = 11 if stage == "Final sample size"      ;
replace order = 12 if stage == "Final # workers"        ;
replace order = 13 if stage == "Final # firms"          ;
drop stage;

reshape wide obs, i(year) j(order);

preserve; collapse (sum) obs*; tempfile all;  gen year = 9999; save `all', replace; restore; append using `all';

forvalues i = 4(1)10 {; replace obs`i' = obs`i'/obs1; };

foreach v in obs1 obs2 obs3 obs11 obs12 obs13       {; gen temp="&"+string(`v',"%15.0fc"); drop `v'; rename temp `v';};
foreach v in obs4 obs5 obs6 obs7  obs8  obs9 obs10  {; gen temp="&"+string(`v',"%15.3f" ); drop `v'; rename temp `v';};

gen b1 = "&";
gen b2 = "&";
gen end="\\"; 

order year obs1 obs2 obs3 b1 obs4 obs5 obs6 obs7 obs8 obs9 obs10 b2 obs11 obs12 obs13 end;

preserve; drop if year == 9999; outsheet using "${res}/sample_rest.tex", replace noquote nonames delimiter(" "); restore;
preserve; keep if year == 9999; drop year; gen year = " " ;outsheet using "${res}/sample_rest_all.tex", replace noquote nonames delimiter(" "); restore;


*** Table G1: Federal minimum wages in Brazil: 2003–2017;

#delimit;
clear all;
set obs 20;
gen yr = _n; 
replace yr = yr+2002;
	
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

keep if inrange(yr, 2003, 2017);

merge 1:1 yr using "${dat}\cpi_yrly.dta"; keep if _merge == 3; drop _merge;

sum cpi if yr == 2003;
gen cpi_2003 = cpi/r(mean);
gen mw_fed_real = mw_fed/cpi_2003;
gen mult_10 = !mod(mw_fed,10);

foreach v in mw_fed      {; gen temp=string(`v',"%5.0f"); drop `v'; rename temp `v';};
foreach v in mw_fed_real {; gen temp=string(`v',"%5.2f"); drop `v'; rename temp `v';};
replace mw_fed = "\textbf{" + mw_fed + "}" if mult_10 == 1;
foreach v in mw_fed mw_fed_real {; replace `v' = "& " + `v'; };

gen end="\\"; 

keep yr mw_fed mw_fed_real end;
outsheet using "${res}/mw_fed.tex", replace noquote nonames delimiter(" ");


*** Table G2: Fraction of workers earning a round salary in year t + 1 as a function of their initial salary;

#delimit ;
use "${dat}\hires_firms_panel.dta", replace;

keep id firmid earn_type adm_type yr hired_wage hiring_1yr mw_fed sep_cause sep_1yr res_1yr N_years_firm;
format id %16.0g;

keep if hiring_1yr == 1; // Keep 2 observations per worker: year hired and next year;

sort id yr;
bys id (yr): gen N = _N; drop if N == 1; // Workers hired and fired in year t;

gen     temp = 0;
replace temp = 1 if inlist(adm_type,1 ,2) & !inlist(adm_type[_n+1], 1 ,2) & yr+1 == yr[_n+1] &  firmid == firmid[_n+1] & earn_type == 1 & earn_type[_n+1]== 1;

gen     tokeep = 0;
replace tokeep = 1 if temp==1 | temp[_n-1] == 1;

keep if tokeep == 1; 

bys id (yr): gen mw_p1   = mw_fed[_n+1];
by  id (yr): gen wage_p1 = hired_wage[_n+1];

drop if hired_wage < mw_fed | wage_p1 < mw_p1;

keep hired_wage wage_p1 mw_fed mw_p1; 

gen cnt = 1;


#delimit ;
tempfile mw_file; tempname mw; postfile `mw' str70(new_mw new_hired_wage ini_mw ini_hired_wage shr n) using `mw_file', replace;

foreach ini_wage_type in "non_rn" "rn" {;

	if "`ini_wage_type'" == "all"    loc if_ini_w " "; 			 		   // Columns 1,4;
	if "`ini_wage_type'" == "rn"     loc if_ini_w "& !mod(hired_wage,10)"; // Columns 2,5;
	if "`ini_wage_type'" == "non_rn" loc if_ini_w "&  mod(hired_wage,10)"; // Columns 3,6;

	qui sum cnt; loc N = r(N);

	* Workers hired at MW_t;
	qui sum cnt if hired_wage == mw_fed `if_ini_w'; loc shr = r(N)/`N';
	post `mw' ("Fraction of workers hired") ("Fraction of workers hired") ("`ini_wage_type'") ("MW_t") ("`shr'") ("`r(N)'");
	
	* Workers hired between MW_t and M_t+1;
	qui sum cnt if hired_wage > mw_fed & hired_wage < mw_p1 `if_ini_w'; loc shr = r(N)/`N';
	post `mw' ("Fraction of workers hired") ("Fraction of workers hired") ("`ini_wage_type'") ("MW_t and M_t+1") ("`shr'") ("`r(N)'");
	
	* Workers hired at or above M_t+1;
	qui sum cnt if hired_wage >= mw_p1 `if_ini_w'; loc shr = r(N)/`N';
	post `mw' ("Fraction of workers hired") ("Fraction of workers hired") ("`ini_wage_type'") (">=M_t+1") ("`shr'") ("`r(N)'");
	
	foreach new_mw_type in "all" "non_rn" "rn" {;
		if "`new_mw_type'" == "all"    loc if_new_mw " "; 			    
		if "`new_mw_type'" == "rn"     loc if_new_mw "& !mod(mw_p1,10)";
		if "`new_mw_type'" == "non_rn" loc if_new_mw "&  mod(mw_p1,10)";

		foreach wage_p1_type in "rn" "non_rn" "mw" {;
		    if "`wage_p1_type'" == "mw"     loc if_new_wage "& wage_p1 == mw_p1";
			if "`wage_p1_type'" == "rn"     loc if_new_wage "& !mod(wage_p1,10)";
			if "`wage_p1_type'" == "non_rn" loc if_new_wage "&  mod(wage_p1,10)";

			di in red "ini_wage_type = `ini_wage_type' | new_mw_type = `new_mw_type' | wage_p1_type = `wage_p1_type'";
	
			* Initial wage is the minimum wage;
			qui sum cnt if hired_wage == mw_fed `if_ini_w' `if_new_mw'               ; loc N = r(N);
			qui sum cnt if hired_wage == mw_fed `if_ini_w' `if_new_mw' `if_new_wage' ; loc shr = r(N)/`N';
			post `mw' ("`new_mw_type'") ("`wage_p1_type'") ("`ini_wage_type'") ("MW_t") ("`shr'") ("`r(N)'");


			* Initial wage is above minimum wage but below MW_t+1 ;
			qui sum cnt if hired_wage > mw_fed & hired_wage < mw_p1 `if_ini_w' `if_new_mw'               ; loc N = r(N);
			qui sum cnt if hired_wage > mw_fed & hired_wage < mw_p1 `if_ini_w' `if_new_mw' `if_new_wage' ; loc shr = r(N)/`N';
			post `mw' ("`new_mw_type'") ("`wage_p1_type'") ("`ini_wage_type'") ("MW_t and M_t+1") ("`shr'") ("`r(N)'");
			
			* Initial wage is at or above MW_t+1 ;
			qui sum cnt if hired_wage >= mw_p1 `if_ini_w' `if_new_mw'               ; loc N = r(N);
			qui sum cnt if hired_wage >= mw_p1 `if_ini_w' `if_new_mw' `if_new_wage' ; loc shr = r(N)/`N';
			post `mw' ("`new_mw_type'") ("`wage_p1_type'") ("`ini_wage_type'") (">=M_t+1") ("`shr'") ("`r(N)'");
				
	
		};
	};
};

postclose `mw'; use `mw_file', clear; destring, replace; compress;


#delimit;
replace new_mw    = "shr_wrk" if new_mw    == "Fraction of workers hired";
replace new_hired = "shr_wrk" if new_hired == "Fraction of workers hired";

gen     order = .;
replace order = 1 if new_mw == "shr_wrk" & new_hired_wage == "shr_wrk";
replace order = 2 if new_mw == "all"	 & new_hired_wage == "mw"     ;
replace order = 3 if new_mw == "all"	 & new_hired_wage == "rn"     ;
replace order = 4 if new_mw == "all"	 & new_hired_wage == "non_rn" ;
replace order = 5 if new_mw == "non_rn"  & new_hired_wage == "rn"     ;
replace order = 6 if new_mw == "rn"	     & new_hired_wage == "non_rn" ;
replace order = 7 if new_mw == "rn"	     & new_hired_wage == "mw"     ;
replace order = 8 if new_mw == "non_rn"	 & new_hired_wage == "mw"     ;
				
keep if order != .;
rename shr val;

keep ini_mw ini_hired_wage val n order;

reshape wide val n, i(ini_mw ini_hired_wage) j(order);

gen     order = .                                      ;
replace order = 1 if ini_hired_wage == "MW_t"          ;
replace order = 2 if ini_hired_wage == "MW_t and M_t+1";
replace order = 3 if ini_hired_wage == ">=M_t+1"       ;

gen     order2 = (ini_mw == "non_rn"); 

sort order order2; 
drop order order2;

drop val5; gen val5 = (n3 - n7)/n1;
drop val6; gen val6 = (n4 - n8)/n1;
drop n* val7 val8;

foreach v in val1 val2 val3 val4 val5 val6 {; gen temp="&"+string(`v',"%15.3f" ); drop `v'; rename temp `v';}; gen end="\\"; 

#delimit;
replace ini_mw = "Initial salary is a round number"     if ini_mw == "rn"    ;
replace ini_mw = "Initial salary is not a round number" if ini_mw == "non_rn";

preserve; keep if ini_hired_wage == "MW_t"          ; drop ini_hired_wage; outsheet using "${res}/mw_spil_a.tex", replace noquote nonames delimiter(" "); restore;
preserve; keep if ini_hired_wage == "MW_t and M_t+1"; drop ini_hired_wage; outsheet using "${res}/mw_spil_b.tex", replace noquote nonames delimiter(" "); restore;
preserve; keep if ini_hired_wage == ">=M_t+1"       ; drop ini_hired_wage; outsheet using "${res}/mw_spil_c.tex", replace noquote nonames delimiter(" "); restore;


/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
