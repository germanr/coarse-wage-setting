/*--------------------------------------------------------------------------
This code creates a database with all the workers hired for the first time in the RAIS during 2003-2017.

Uses: 	  RAIS raw data, census_firms.dta, inpc_202003SerieHist.csv, muni_identifiers.csv, state_mw.xlsx, muni_metreg.dta, cba.dta, firm_variables.dta
Produces: tomerge_fixedsample.dta, microregion.dta, cpi_mthly.dta, rais_sample_restrictions.xlsx, hires_`yr'.dta
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;
set seed 20210221;

tempname rais_samp_rest; tempfile rais_samp_rest_file; postfile `rais_samp_rest' str100(year stage obs) using `rais_samp_rest_file', replace;

* Fixed sample of firms;
#delimit;
use "${dat}\census_firms.dta", replace;
drop *19* *2000 *2001 *2002 yrs_insample;

egen yrs_insample  = rsum(in*); tab yrs_insample;         
egen priv_firm     = rsum(priv_firm*)   ; replace priv_firm = priv_firm/yrs_insample;

gen tokeep = (yrs_insample == 15) ; tab tokeep; keep if tokeep == 1; drop tokeep;
gen tokeep = (priv_firm == 1)     ; tab tokeep; keep if tokeep == 1; drop tokeep; 

gen fixed_sample = 1 ; 
keep firmid fixed_sample;
save "${dat}\tomerge_fixedsample.dta", replace;

* Mapping municipalities and microregions;
insheet using "${dat}/muni_identifiers.csv", clear; 
keep ibge6 micro; 
rename (ibge6 micro) (muni micro);
save "${dat}/microregion.dta", replace;

* Monthly CPI;
import excel using "${dat}\inpc_202003SerieHist.xls", clear; 
drop in 1/7; 
keep A B C; 
ren (A B C) (yr mth_str cpi);
carryforward yr, replace;
drop if cpi == "" | yr == "ANO";
gen sort = _n; 
bys yr (sort): gen mth = _n;
destring cpi yr, replace; drop mth_str sort;
keep if inrange(yr, 2003, 2017);
gen cpi_jan = cpi[1]; replace cpi = (cpi/cpi_jan)*100; drop cpi_jan; // Jan 2003 = 100;
save "${dat}\cpi_mthly.dta", replace;

* State MW;
#delimit;
import excel using "${dat}\state_mw.xlsx", first clear;
drop G group source mth;
drop if mw == .;
bys st yr: gen n = _n;
reshape wide mw , i(st yr) j(n);
save "${dat}\state_mw.dta", replace;s

#delimit;
forvalues yr = 2003(1)2017 {; di in red `yr';

use "${raw}\rais`yr'.dta", replace; 

*-----------------------------*;
*------ Sample selection -----*;
*-----------------------------*;

local invalid "0 9 99 191 949 11111111111 22222222222 33333333333 44444444444 55555555555 66666666666 77777777777 88888888888 99999999999 12345678909 98765432100";

foreach i of local invalid {; replace cpf = . if cpf == `i' ; };
foreach i of local invalid {; replace pis = . if pis == `i' ; };

gen      double id = pis; 

* Initial sample size, # workers, # firms;
								         count       ; post `rais_samp_rest' ("`yr'") ("Initial sample size") ("`r(N)'") ;
bys pis:    gen aux_wrk  = 1 if _n == 1; sum aux_wrk ; post `rais_samp_rest' ("`yr'") ("Initial # workers") ("`r(sum)'") ;
bys firmid: gen aux_firm = 1 if _n == 1; sum aux_firm; post `rais_samp_rest' ("`yr'") ("Initial # firms") ("`r(sum)'")   ; drop aux*;

* Sample restrictions;

format pis %13.0g; tostring pis, gen(pis_str) format("%12.0f"); 
gen random    = r(uniform);
gen zero_earn = (earn_dec == 0);
gen ownership_type = floor(legal_nature/1000);
gen private_firm = (ownership_type == 2 & !inlist(legal_nature,2011,2038));
bys id firmid (zero_earn hired_hours earn_dec earn_mean random): gen one_job_firm = (_n == _N);
if `yr' >= 2016 {; replace hired_wage = hired_wage*100 if hired_wage < 100 & ((earn_mean_mw >= 1 & earn_mean_mw != .) | (earn_dec_mw >= 1 & earn_dec_mw != .) ); }; // After 2016, half of obs earn less than the MW. They divided it by 100;

if `yr' == 2017 loc mw = 937;
if `yr' == 2016 loc mw = 880;
if `yr' == 2015 loc mw = 788;
if `yr' == 2014 loc mw = 724;
if `yr' == 2013 loc mw = 678;
if `yr' == 2012 loc mw = 622;
if `yr' == 2011 loc mw = 545;
if `yr' == 2010 loc mw = 510;
if `yr' == 2009 loc mw = 465;
if `yr' == 2008 loc mw = 415;
if `yr' == 2007 loc mw = 380;
if `yr' == 2006 loc mw = 350;
if `yr' == 2005 loc mw = 300;
if `yr' == 2004 loc mw = 260;
if `yr' == 2003 loc mw = 240;

gen tokeep = (inlist(adm_type,1,2))       ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("New hire") ("`r(N)'")            ; // Only new hires                     ;
gen tokeep = (length(trim(pis_str)) == 11); tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Valid PIS") ("`r(N)'")           ; // Valid PIS                          ;
gen tokeep = (private_firm == 1)          ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Private firms") ("`r(N)'")       ; // Private-sector firms               ;
gen tokeep = (earn_type    == 1)          ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Monthly contracts") ("`r(N)'")   ; // Only monthly contracts             ;
gen tokeep = (hired_wage   >= `mw')       ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Above MW") ("`r(N)'")            ; // Earnings above federal minimum wage;
gen tokeep = (one_job_firm == 1)          ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Multiple positions") ("`r(N)'")  ; // Multiple empl. at a firm           ;
gen tokeep = (hired_wage !=.)             ; tab tokeep; keep if tokeep == 1; drop tokeep; count; post `rais_samp_rest' ("`yr'") ("Hired wage available") ("`r(N)'"); // Hired wage available               ;

drop random one_job_firm zero_earn private_firm; 

* Final sample size, # workers, # firms;
bys pis:    gen aux_wrk  = 1 if _n == 1;
bys firmid: gen aux_firm = 1 if _n == 1;

count;		  post `rais_samp_rest' ("`yr'") ("Final sample size") ("`r(N)'") ;
sum aux_wrk;  post `rais_samp_rest' ("`yr'") ("Final # workers") ("`r(sum)'") ; drop aux_wrk;
sum aux_firm; post `rais_samp_rest' ("`yr'") ("Final # firms") ("`r(sum)'")   ; drop aux_firm;

save "${dat}/hires_raw_`yr'.dta", replace;

};

* Sample restrictions;
postclose `rais_samp_rest'; 
use `rais_samp_rest_file', clear;
destring, replace;
export excel using "${dat}/rais_sample_restrictions.xlsx", first(var) sheetreplace;


*---------------------------------------*;
*------ Create groups for the bins -----*;
*---------------------------------------*;

#delimit;
forvalues yr = 2003(1)2017 {; di in red `yr';

use "${dat}/hires_raw_`yr'.dta", replace;

foreach rn in 10 100 1000 {; gen mult_`rn' = !mod(hired_wage,`rn'); };

gen mult_10_only  = mult_10 ; replace mult_10_only  = 0 if mult_100   == 1;
gen mult_100_only = mult_100; replace mult_100_only = 0 if mult_1000  == 1;

gen mth = adm_month; 
gen codemun = string(muni);	
gen st = floor(muni/10000);

merge m:1 yr mth    using "${dat}/cpi_mthly.dta"                                        ; tab _merge; drop if _merge == 2; drop _merge mth    ;
merge m:1 muni      using "${dat}/microregion.dta"                                      ; tab _merge; drop if _merge == 2; drop _merge        ;
merge m:1 muni      using "${dat}/muni_metreg.dta"                                      ; tab _merge; drop if _merge == 2; drop _merge        ;
merge m:1 codemun   using "${dat}/rais_codemun_to_mmc_1991_2010.dta"                    ; tab _merge; drop if _merge == 2; drop _merge codemun;
merge m:1 firmid    using "${dat}/tomerge_fixedsample.dta"                              ; tab _merge; drop if _merge == 2; drop _merge        ; 
merge m:1 yr st     using "${dat}/state_mw.dta"                                         ; tab _merge; drop if _merge == 2; drop _merge        ; 
merge m:1 yr firmid using "${dat}/cba.dta"                                              ; tab _merge; drop if _merge == 2; drop _merge        ;
merge m:1 yr firmid using "${dat}/firm_variables.dta", keepusing(cdf_hiring hiring_exp) ; tab _merge; drop if _merge == 2; drop _merge        ;

foreach v of varlist cba* {; replace `v' = 0 if `v' == .; };

if `yr' == 2017 loc mw = 937;
if `yr' == 2016 loc mw = 880;
if `yr' == 2015 loc mw = 788;
if `yr' == 2014 loc mw = 724;
if `yr' == 2013 loc mw = 678;
if `yr' == 2012 loc mw = 622;
if `yr' == 2011 loc mw = 545;
if `yr' == 2010 loc mw = 510;
if `yr' == 2009 loc mw = 465;
if `yr' == 2008 loc mw = 415;
if `yr' == 2007 loc mw = 380;
if `yr' == 2006 loc mw = 350;
if `yr' == 2005 loc mw = 300;
if `yr' == 2004 loc mw = 260;
if `yr' == 2003 loc mw = 240;

gen is_fed_mw = (hired_wage == `mw');
gen is_st_mw = 0; forvalues i = 1(1)12 {; replace is_st_mw = 1 if hired_wage == mw`i' & mw`i' != .; drop mw`i'; };

drop firm_size_group;
recode firm_size  (101/99999999 = 101 ">100"), gen(firm_size_group) ;
recode hiring_exp (101/99999999 = 101 ">100"), gen(hiring_exp_group);
recode cdf_hiring (101/99999999 = 101 ">100"), gen(cdf_hiring_group);
recode schooling (-1 = .) (10/11 = 9); 

replace fixed_sample = 0 if fixed_sample !=1;

destring mmc, replace;

keep id pis yr firmid hired_wage age male race earn_mean adm_month adm_type muni mmc firm_size_group hiring_exp_group cdf_hiring_group schooling potential_exp mult_* cpi ind1d occ1d micro reg_met is_fed_mw is_st_mw fixed_sample cba*;
order yr pis firmid; 
compress;
save "${dat}/hires_`yr'.dta", replace;

};


/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>