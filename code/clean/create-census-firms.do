/*--------------------------------------------------------------------------
This code creates a census of all firms in the RAIS from 1999-2017. Note: Running from 1999 is required to create the first year each firm observed in data

Uses:	 	       RAIS raw data
Produces: 		   census_firms.dta
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;


forvalues yr = 1999(1)2017 {;

	use "${raw}\rais`yr'.dta", replace; 
	include "${cod}\labels.do";

	* Reasonable admission yrs;
	gen age_at_adm = age - (yr - adm_year);
	replace adm_year = . if adm_year < 1900 | adm_year > `yr';
	replace adm_year = . if age_at_adm < 15 | age_at_adm > 65;
	
	gen hire_anycontract_`yr' = 0; if `yr' >= 2003 {; replace hire_anycontract_`yr' = 1 if inlist(adm_type,1,2)                 ; };
	gen hire_monthly_`yr'     = 0; if `yr' >= 2003 {; replace hire_monthly_`yr'     = 1 if inlist(adm_type,1,2) & earn_type == 1; };
	

	* Firm type;
	gen    ownership_type = floor(legal_nature/1000);
	recode ownership_type (2=1) (nonmissing = 0), gen(priv_firm);
	replace priv_firm = 0 if inlist(legal_nature,2011,2038); // To exclude public firms;
	
	gcollapse (sum) workers_`yr' = active hire_any* hire_month* (min) oldest_wrk_`yr' = adm_year (firstnm) priv_firm_`yr' = priv_firm, by(firmid) fast;
	
	label var workers_`yr' 	        "Workers active at the end of `yr'";
	label var hire_anycontract_`yr' "Workers hired (any earnings contract) during `yr'";
	label var hire_monthly_`yr'     "Workers hired at monthly contracts during `yr'";
	label var oldest_wrk_`yr'       "Lenghtiest tenure among all workers in `yr'";
	label var priv_firm_`yr'        "=1 if privately owned during `yr'";
	
	de; compress;
	save "${dat}\firms_`yr'.dta", replace;

};


* Collect all different ID's across all yrs;
#delimit;
clear; forvalues yr = 1999(1)2017 {; append using "${dat}\firms_`yr'.dta"; keep firmid; bys firmid: keep if _n == 1; };

* Years available;
forvalues yr = 1999(1)2017 {;
	merge 1:1 firmid using "${dat}\firms_`yr'.dta";
	recode _merge (1 = 0 "No") (3 = 1 "Yes"), gen(in_`yr'); drop _merge;
	label var in_`yr' "=1 if firm is in `yr'";
	erase "${dat}\firms_`yr'.dta"; 
};

* First & last yr in sample;
gen first_yr = .; forvalues yr = 2017(-1)1999 {; replace first_yr = `yr' if in_`yr' == 1; };
gen last_yr  = .; forvalues yr = 1999(1)2017  {; replace last_yr  = `yr' if in_`yr' == 1; };

* Oldest admission date;
gen oldest_adm = first_yr; forvalues yr = 1999(1)2017 {;  replace oldest_adm = oldest_wrk_`yr' if (oldest_wrk_`yr' < oldest_adm) & (oldest_wrk_`yr' != .) & (oldest_adm != .); };

egen yrs_insample = rsum(in_*);

label var first_yr     "First yr the firm is observed during 1999-2017";
label var last_yr      "Last yr the firm is observed during 1999-2017";
label var oldest_adm   "Oldest admission date";
label var yrs_insample "Number of yrs the firm is in the sample";

format firmid %16.0g; 
compress;
order firmid first_yr last_yr oldest_adm yrs_insample in_* workers* hire_any* hire_month* oldest* priv*;
save "${dat}/census_firms.dta", replace;

