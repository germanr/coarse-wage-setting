/*--------------------------------------------------------------------------
This code generates the figures in the main text and some appendix figures

Uses:	 	    bins_fig.dta, hires_`yr'.dta, base_amostra_pessoa_201312_20190131.csv, pnad_2013.dta, pme_2013.dta, census2010.dta, buncher_coefficients.dta
Produces: 		 
				Figure 1: Bunching at round numbers in the salary distribution
				*** Panel A. Distribution of contracted earnings in R$1 bins
				*** Panel B. Fraction of salaries divisible by round numbers: observed vs. uniform
				Figure 2: Fraction of salaries divisible by round numbers in four Brazilian datasets
				Figure 3: Wage compression and wage stickiness in the salaries of new hires
				*** Panel A: Gini coefficient 
				*** Panel B: Percentiles ratios
				*** Panel C: =1 if wage remained constant
				Figure A2: Distribution of monthly earnings in four Brazilian datasets
				Figure A5: Wage compression and wage stickiness in the salaries of large firms' new hires
				*** Panel A: Gini coefficient 
				*** Panel B: Percentiles ratios
				*** Panel C: =1 if wage remained constant
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;


*** Figure 1: Bunching at round numbers in the salary distribution ***;

*** Panel A. Distribution of contracted earnings in R$1 bins;
#delimit;
use "${dat}/bins_fig.dta", replace;

gcollapse (sum) obs (mean) pool (max) mult*, by(hired_wage);

gen fig_10   = mult_10  ; replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50  ; replace fig_50  = 0 if mult_100  == 1 ;
gen fig_100  = mult_100 ; replace fig_100 = 0 if mult_500  == 1 ;
gen fig_500  = mult_500 ; replace fig_500 = 0 if mult_1000 == 1 ;
gen fig_1000 = mult_1000;

sort hired_wage obs;
twoway connect obs hired_wage                 , mcolor(gs12)         mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) || 
	   scatter obs hired_wage if fig_10   == 1, mcolor(navy)         mlwidth(vthin) msymbol(Oh) msize(vsmall) || 
	   scatter obs hired_wage if fig_50   == 1, mcolor(maroon)       mlwidth(vthin) msymbol(D)  msize(vsmall) || 
	   scatter obs hired_wage if fig_100  == 1, mcolor(forest_green)                msymbol(T)  msize(vsmall) || 
	   scatter obs hired_wage if fig_500  == 1, mcolor(dkorange)                    msymbol(S)  msize(vsmall) || 
	   scatter obs hired_wage if fig_1000 == 1, mcolor(purple)                      msymbol(O)  msize(small) 
	   xtitle("Contracted monthly earnings (in R$)") ytitle("Number of contracts") xlabel(500(500)3500) 
	   ylabel(0 "0M" 500000 "0.5M" 1000000 "1M" 1500000 "1.5M" 2000000 "2M" 2500000 "2.5M", angle(0)) yscale(titlegap(2)) 
	   legend(order(2 "10" 3 "50" 4 "100" 5 "500" 6 "1000" 1 "Other") col(1) ring(0) position(1) bmargin(0)) legend(subtitle("Salary divisible by:"))
	   graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_1bins.pdf", replace as(pdf);


*** Panel B. Fraction of salaries divisible by round numbers: observed vs. uniform;

#delimit;

clear all;
forvalues yr = 2003(1)2017 {; append using "${dat}/hires_`yr'.dta"; };
drop if yr == .;
order yr wt;
gen cnt = 1;

gcollapse (sum) cnt (mean) hired_wage_mult_* [fw=wt];

rename hired_wage_mult_* mult_*;
foreach rn in 10 100 1000 {; gen unif_`rn' = 1/`rn'; };

reshape long mult_ unif_, i(cnt) j(rn);
drop cnt;
rename (mult_ unif_) (observed uniform);

label define rn
	10 `""Salaries divisible" "by 10""' 
	100 `""Salaries divisible" "by 100""'  		
	1000 `""Salaries divisible" "by 1000""';
label values rn rn;

graph bar observed uniform, over(rn) 
	ytitle("Fraction of contracts") 
	legend(order(1 "Observed" 2 "Uniform (benchmark)") col(1) ring(0) position(1) bmargin(medium))
	ylabel(0(0.1).35, format(%4.2fc) angle(0)) blabel(total, format(%4.3fc) )
	graphregion(color(white)) bgcolor(white);
	graph export "${res}\fig_uniform_benchmark.pdf", replace as(pdf);



*** Figure 2: Fraction of salaries divisible by round numbers in four Brazilian datasets; 
*** Figure A2: Distribution of monthly earnings in four Brazilian datasets;

#delimit;
tempname round; tempfile rn_file; postfile `round' str70(dataset rn shr sd n) using `rn_file', replace;

* Cadastro Unico - link: https://www.gov.br/mds/pt-br/pt-br/servicos/sagi/microdados;

#delimit cr
insheet using "${dat}\base_amostra_pessoa_201312_20190131.csv", clear delim(;)
#delimit ;

rename val_remuner_emprego_memb earn_mth;
rename cod_principal_trab_memb work_type;
rename idade age;

keep if earn_mth > 0        ;
keep if earn_mth != .       ;
drop if inlist(work_type,2,7,8); // 2=Temp in rural areas, 7=unpaid workers, 8 = Militar/Gov't;
drop if earn_mth == 678; // Minimum wage in 2013;
keep if inrange(age,18,65);

#delimit;
foreach rn in "10" "100" "1000" {; gen mult_`rn'=!mod(earn_mth,`rn'); qui sum mult_`rn'; post `round'("cad") ("`rn'") ("`r(mean)'") ("`r(sd)'") ("`r(N)'"); };

keep if earn_mth < 3501;
keep earn_mth pesopes;

replace earn_mth = floor(earn_mth); 
replace pesopes = subinstr(pesopes, ",", ".", .); 
destring peso, replace;

gen obs =1;

gcollapse (sum) obs wt=pesopes, by(earn_mth);

foreach rn in "10" "50" "100" "500" "1000" {; gen mult_`rn' = !mod(earn_mth,`rn'); };

gen fig_10   = mult_10;   replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50;   replace fig_50  = 0 if mult_100  == 1;
gen fig_100  = mult_100;  replace fig_100 = 0 if mult_500  == 1;
gen fig_500  = mult_500;  replace fig_500 = 0 if mult_1000 == 1;
gen fig_1000 = mult_1000;

#delimit;
sort earn_mth wt;
twoway connect wt earn_mth, mcolor(gs12) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) ||
	   scatter wt earn_mth if fig_10 == 1, mcolor(navy) mlwidth(vthin) msymbol(Oh) msize(vsmall) ||
	   scatter wt earn_mth if fig_50 == 1, mcolor(maroon) mlwidth(vthin) msymbol(D) msize(vsmall) ||
	   scatter wt earn_mth if fig_100 == 1, mcolor(forest_green) msymbol(T) msize(vsmall) ||
	   scatter wt earn_mth if fig_500 == 1 , mcolor(dkorange) msymbol(S) msize(vsmall) ||
	   scatter wt earn_mth if fig_1000 == 1 , mcolor(purple) msymbol(O) msize(small)
	   ylabel(0 "0M" 500000 "0.5M" 1000000 "1M" 1500000 "1.5M", angle(0) gmax) yscale(titlegap(2))
	   xtitle("Monthly earnings (in R$)") ytitle("Number of workers") xlabel(0(500)3500) xscale(titlegap(2))
	   legend(off)
	   graphregion(color(white)) bgcolor(white);

graph export "${res}/fig_1bins_cad.pdf", replace as(pdf);


* Household survey (PNAD);
#delimit;
use "${dat}\pnad_2013.dta", clear;

keep if ocupado == 1;
keep if inrange(edad, 18, 65);
keep if ila !=. & ila != 0;
keep if hstrp >= 40;
drop if relab == 4; // Non-salaried
drop if empresa == 3; // Public firms;

replace ila = ila * p_reg;
replace ila = floor(ila);
drop if ila == 678; // Minimum wage 2013;

foreach rn in "10" "100" "1000" {; gen mult_`rn'=!mod(ila,`rn'); qui sum mult_`rn'; post `round'("pnad") ("`rn'") ("`r(mean)'") ("`r(sd)'") ("`r(N)'"); };

keep if ila < 3501;
gen obs =1;
collapse (sum) obs wt=pondera, by(ila);

#delimit;
foreach rn in "10" "50" "100" "500" "1000" {; gen mult_`rn' = !mod(ila,`rn'); };

gen fig_10   = mult_10;   replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50;   replace fig_50  = 0 if mult_100  == 1;
gen fig_100  = mult_100;  replace fig_100 = 0 if mult_500  == 1;
gen fig_500  = mult_500;  replace fig_500 = 0 if mult_1000 == 1;
gen fig_1000 = mult_1000;

#delimit;
sort ila wt;
twoway connect wt ila, mcolor(gs12) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) ||
	   scatter wt ila if fig_10 == 1, mcolor(navy) mlwidth(vthin) msymbol(Oh) msize(vsmall) ||
	   scatter wt ila if fig_50 == 1, mcolor(maroon) mlwidth(vthin) msymbol(D) msize(vsmall) ||
	   scatter wt ila if fig_100 == 1, mcolor(forest_green) msymbol(T) msize(vsmall) ||
	   scatter wt ila if fig_500 == 1 , mcolor(dkorange) msymbol(S) msize(vsmall) ||
	   scatter wt ila if fig_1000 == 1 , mcolor(purple) msymbol(O) msize(small)
	   ylabel(0 "0M" 1000000 "1M" 2000000 "2M" 3000000 "3M" 4000000 "4M" 5000000 "5M", angle(0) gmax) yscale(titlegap(2))
	   xtitle("Monthly earnings (in R$)") ytitle("Number of workers") xlabel(0(500)3500) xscale(titlegap(2))
	   legend(off)
	   graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_1bins_pnad.pdf", replace as(pdf);


* Brazilian Monthly Employment Survey (PME);
#delimit;
use "${dat}\pme_2013.dta", clear;

keep if ocupado == 1;
keep if inrange(edad, 18, 65);
keep if ila !=. & ila != 0;
keep if hstrp >= 40;
drop if relab == 4;
drop if empresa == 3; // Public firms;

preserve; keep ipc_mes trimestre; duplicates drop; bys trimestre: egen double ipc = total(ipc_mes); replace ipc = ipc/3; drop ipc_mes; duplicates drop;
tempfile ipc; save `ipc', replace; restore; merge m:1 trimestre using `ipc', nogen;

replace ila = ila*(ipc_mes/ipc);
replace ila = floor(ila);
drop if ila == 678; // Minimum wage in 2013;

foreach rn in "10" "100" "1000" {; gen mult_`rn'=!mod(ila,`rn'); qui sum mult_`rn'; post `round'("pme") ("`rn'") ("`r(mean)'") ("`r(sd)'") ("`r(N)'"); };

keep if ila < 3501;
gen obs =1;
collapse (sum) obs wt=pondera, by(ila);

#delimit;
foreach rn in "10" "50" "100" "500" "1000" {; gen mult_`rn' = !mod(ila,`rn'); };

gen fig_10   = mult_10;   replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50;   replace fig_50  = 0 if mult_100  == 1;
gen fig_100  = mult_100;  replace fig_100 = 0 if mult_500  == 1;
gen fig_500  = mult_500;  replace fig_500 = 0 if mult_1000 == 1;
gen fig_1000 = mult_1000;

#delimit;
sort ila wt;
twoway connect wt ila, mcolor(gs12) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) ||
	   scatter wt ila if fig_10 == 1, mcolor(navy) mlwidth(vthin) msymbol(Oh) msize(vsmall) ||
	   scatter wt ila if fig_50 == 1, mcolor(maroon) mlwidth(vthin) msymbol(D) msize(vsmall) ||
	   scatter wt ila if fig_100 == 1, mcolor(forest_green) msymbol(T) msize(vsmall) ||
	   scatter wt ila if fig_500 == 1 , mcolor(dkorange) msymbol(S) msize(vsmall) ||
	   scatter wt ila if fig_1000 == 1 , mcolor(purple) msymbol(O) msize(small)
	   ylabel(0 "0M" 2000000 "2M" 4000000 "4M" 6000000 "6M", angle(0) gmax) yscale(titlegap(2))
	   xtitle("Monthly earnings (in R$)") ytitle("Number of workers") xlabel(0(500)3500) xscale(titlegap(2))
	   legend(off)
	   graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_1bins_pme.pdf", replace as(pdf);

* Census;
#delimit;
use "${dat}\census2010.dta", clear;

keep if emp == 1;
keep if inrange(age, 18, 65);
keep if income_occupation !=. & income_occupation != 0;
keep if hours_occupation >= 40;
drop if inlist(position,2,3,7); // 2=Military, 3=civil servant, 7=unpaid workers;
drop if income_occupation == 510; // Minimum wage 2010;

foreach rn in "10" "100" "1000" {; gen mult_`rn'=!mod(income_occupation,`rn'); qui sum mult_`rn'; post `round'("census") ("`rn'") ("`r(mean)'") ("`r(sd)'") ("`r(N)'"); };
keep if income_occupation < 3501;
replace income_occupation = floor(income_occupation); replace income_occupation = 10101 if income_occupation > 10100;

gen obs =1;

collapse (sum) obs wt=weight, by(income_occupation);

#delimit;
foreach rn in "10" "50" "100" "500" "1000" {; gen mult_`rn' = !mod(income,`rn'); };

gen fig_10   = mult_10;   replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50   = mult_50;   replace fig_50  = 0 if mult_100  == 1;
gen fig_100  = mult_100;  replace fig_100 = 0 if mult_500  == 1;
gen fig_500  = mult_500;  replace fig_500 = 0 if mult_1000 == 1;
gen fig_1000 = mult_1000;

#delimit;
sort income_occupation wt;
twoway connect wt income, mcolor(gs12) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) ||
	   scatter wt income if fig_10 == 1, mcolor(navy) mlwidth(vthin) msymbol(Oh) msize(vsmall) ||
	   scatter wt income if fig_50 == 1, mcolor(maroon) mlwidth(vthin) msymbol(D) msize(vsmall) ||
	   scatter wt income if fig_100 == 1, mcolor(forest_green) msymbol(T) msize(vsmall) ||
	   scatter wt income if fig_500 == 1 , mcolor(dkorange) msymbol(S) msize(vsmall) ||
	   scatter wt income if fig_1000 == 1 , mcolor(purple) msymbol(O) msize(small)
	   ylabel(0 "0M" 1000000 "1M" 2000000 "2M" 3000000 "3M" 4000000 "4M", angle(0) gmax) yscale(titlegap(2))
	   xtitle("Monthly earnings (in R$)") ytitle("Number of workers") xlabel(0(500)3500) xscale(titlegap(2))
	   legend(off)
	   graphregion(color(white)) bgcolor(white);
graph export "${res}/fig_1bins_cen.pdf", replace as(pdf);


* All four datasets;

postclose `round'; use `rn_file', clear; destring, replace; compress;

#delimit;
 
gen     xaxis = (data == "pnad");
replace xaxis = xaxis+5  if data == "pme"   ;
replace xaxis = xaxis+9  if data == "cad"   ;
replace xaxis = xaxis+13  if data == "census";

replace xaxis = xaxis + 1 if rn == 100 ;
replace xaxis = xaxis + 2 if rn == 1000;

foreach v in shr {;
	generate hi_`v' = `v' + invttail(n-1,0.025)*(sd / sqrt(n));
	generate lo_`v' = `v' - invttail(n-1,0.025)*(sd / sqrt(n));
};


gen lab = shr;
format lab %4.3f;

#delimit;
twoway  bar shr xaxis if rn == 10 
     || bar shr xaxis if rn == 100
     || bar shr xaxis if rn == 1000,
     || scatter shr xaxis, msym(none) mlab(lab) mlabpos(12) mlabcolor(black) 
	    ylabel(0(.25)1, format(%4.2fc) angle(0) gmax gmin)  xscale(titlegap(2))
	    xlabel(2  `""Household" "survey""' 6 `""Labor force" "survey""' 10 `""Social programs" "registry""' 14 `""Population" "census""' , noticks) 
        xtitle("") ytitle("Fraction of workers")
	    legend(order(0 "Salary divisible by: " 1 "10" 2 "100" 3 "1000") row(1) span)
	    graphregion(color(white)) bgcolor(white);
	    graph export "${res}\fig_rn_datasets.pdf", replace as(pdf);


*** Figure 3: Wage compression and wage stickiness in the salaries of new hires ; 
*** Figure A5: Wage compression and wage stickiness in the salaries of large firms' new hires;

*** Panels A--B: Gini coefficient and Percentiles ratios;

#delimit;
use "${res}\buncher_coefficients.dta", replace;

keep if bunchvar == "buncher" | bunchvar == "buncher_bigf" ;

gen big = (bunchvar == "buncher_bigf"); drop bunchvar;
gen mean_buncher = mean_nonbuncher + beta;


reshape long mean, i(dv big beta se n) j(buncher) string;

gen bunching_firm = (buncher == "_buncher"); drop buncher;

gen hi = mean + 1.96*se if bunching_firm == 1;
gen lo = mean - 1.96*se if bunching_firm == 1;

sort big bunching_firm;

keep if inlist(dv, "r_90_to_10", "r_90_to_50", "r_50_to_10", "gini");

#delimit;
gen     level = .;
replace level = 1 if bunch == 0 & dv == "r_90_to_10";
replace level = 2 if bunch == 1 & dv == "r_90_to_10";
replace level = 4 if bunch == 0 & dv == "r_90_to_50";
replace level = 5 if bunch == 1 & dv == "r_90_to_50";
replace level = 7 if bunch == 0 & dv == "r_50_to_10";
replace level = 8 if bunch == 1 & dv == "r_50_to_10";

gen     gini_lev = .;
replace gini_lev = 1 if bunch == 0 & dv == "gini";
replace gini_lev = 2 if bunch == 1 & dv == "gini";


#delimit;
format mean %4.2f;
cap drop labpos;
gen     labpos = mean if dv != "gini";
replace labpos = mean + 0.01 if bunch == 1;

twoway bar     mean   level if bunch == 0 & big == 0 & dv != "gini", barwidth(1) || 
	   bar     mean   level if bunch == 1 & big == 0 & dv != "gini", barwidth(1) ||
	   scatter labpos level if              big == 0 & dv != "gini", msym(none) mlab(mean) mlabpos(12) mlabcolor(black)  ||
       rcap     hi lo level if              big == 0 & dv != "gini", lcolor(gs4)
       xlabel(1.5 "Ratio 90p to 10p" 4.5 "Ratio 90p to 50p" 7.5 "Ratio 50p to 10p", noticks) xscale(titlegap(2))
       xtitle("") ytitle("Ratio between salaries")  
	   ylabel(1(.2)1.8, format(%4.2fc) angle(0) gmax gmin)
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_comp_ratios.pdf", replace as(pdf);


#delimit;
cap drop labpos;
gen     labpos = mean if dv != "gini";
replace labpos = mean + 0.02 if bunch == 1                      ;
replace labpos = mean + 0.03 if bunch == 1 & dv == "r_90_to_50" ;
replace labpos = mean + 0.06 if bunch == 1 & dv == "r_90_to_10" ;

twoway bar     mean   level if bunch == 0 & big == 1 & dv != "gini", barwidth(1) || 
	   bar     mean   level if bunch == 1 & big == 1 & dv != "gini", barwidth(1) ||
	   scatter labpos level if              big == 1 & dv != "gini", msym(none) mlab(mean) mlabpos(12) mlabcolor(black)  ||
       rcap     hi lo level if              big == 1 & dv != "gini", lcolor(gs4)
       xlabel(1.5 "Ratio 90p to 10p" 4.5 "Ratio 90p to 50p" 7.5 "Ratio 50p to 10p", noticks) xscale(titlegap(2))
       xtitle("") ytitle("Ratio between salaries")  
	   ylabel(1(.25)2, format(%4.2fc) angle(0) gmax gmin)
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_comp_ratios_bigf.pdf", replace as(pdf);


#delimit;
cap drop labpos;
gen      labpos = mean if dv == "gini";

format mean %4.3f;

twoway bar     mean   gini_lev if bunch == 0 & big == 0 & dv == "gini", barwidth(.75) || 
	   bar     mean   gini_lev if bunch == 1 & big == 0 & dv == "gini", barwidth(.75) ||
	   scatter labpos gini_lev if              big == 0 & dv == "gini", msym(none) mlab(mean) mlabpos(12) mlabcolor(black)  ||
       rcap     hi lo gini_lev if              big == 0 & dv == "gini", lcolor(gs4)
       xlabel(1 " ", noticks) xscale(titlegap(2))
       xtitle(" ") ytitle("Gini coefficient")  
	   ylabel(0(.03).12, format(%4.2fc) angle(0) gmax)
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_comp_gini.pdf", replace as(pdf);

#delimit;
replace labpos = mean + 0.005 if bunch == 1;
twoway bar     mean   gini_lev if bunch == 0 & big == 1 & dv == "gini", barwidth(.75) || 
	   bar     mean   gini_lev if bunch == 1 & big == 1 & dv == "gini", barwidth(.75) ||
	   scatter labpos gini_lev if              big == 1 & dv == "gini", msym(none) mlab(mean) mlabpos(12) mlabcolor(black)  ||
       rcap     hi lo gini_lev if              big == 1 & dv == "gini", lcolor(gs4)
       xlabel(1 " ", noticks) xscale(titlegap(2))
       xtitle(" ") ytitle("Gini coefficient")  
	   ylabel(0(.05).15, format(%4.2fc) angle(0))
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_comp_gini_bigf.pdf", replace as(pdf);


#delimit;
gen tmp = .;
twoway bar tmp gini_lev if bunch == 0 || bar tmp gini_lev if bunch == 1, 
       xtitle("") xscale(noline) xlabel(none) ytitle("") yscale(noline) ylabel(none)
	   legend(order (1 "Non-bunching firms" 2 "Bunching firms"))
	   plotregion(margin(zero) style(none)) graphregion(color(white)) bgcolor(white);	   
	   graph export "${res}/fig_wage_comp_leg.pdf", replace as(pdf);
	   

*** Panel C: =1 if wage remained constant ***;

#delimit;
use "${res}\buncher_coefficients.dta", replace;

keep if dv       == "wage_grw_zero";
keep if bunchvar == "buncher" | bunchvar == "buncher_bigf" ;

gen big = (bunchvar == "buncher_bigf"); drop bunchvar;
gen mean_buncher = mean_nonbuncher + beta;

reshape long mean, i(big beta se n) j(buncher) string;

gen bunching_firm = (buncher == "_buncher"); drop buncher;

gen hi = mean + 1.96*se if bunching_firm == 1;
gen lo = mean - 1.96*se if bunching_firm == 1;

sort big bunching_firm;

gen     level = .;
replace level = 1 if bunch == 0 & big == 0;
replace level = 2 if bunch == 1 & big == 0;
replace level = 1 if bunch == 0 & big == 1;
replace level = 2 if bunch == 1 & big == 1;

format mean %4.2f;
cap drop labpos;
gen      labpos = mean;
replace  labpos = mean + 0.005 if bunch == 1 & big == 0;

twoway bar     mean   level if bunch == 0 & big == 0, barwidth(.75)  
	|| bar     mean   level if bunch == 1 & big == 0, barwidth(.75)
	|| scatter labpos level if              big == 0, msym(none) mlab(mean) mlabpos(12) mlabcolor(black) 
    || rcap     hi lo level if              big == 0, lcolor(gs4)
       xlabel(1 " "  , noticks)
       xtitle("") ytitle("Fraction of new hires with a sticky wage")  
	   ylabel(0(0.10).40, format(%4.2fc) angle(0))
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_sticky.pdf", replace as(pdf);

#delimit;
cap drop labpos;
gen      labpos = mean;
replace  labpos = mean + 0.042  if bunch == 1 & big == 1;

twoway bar     mean   level if bunch == 0 & big == 1, barwidth(.75)  
	|| bar     mean   level if bunch == 1 & big == 1, barwidth(.75)
	|| scatter labpos level if              big == 1, msym(none) mlab(mean) mlabpos(12) mlabcolor(black) 
    || rcap     hi lo level if              big == 1, lcolor(gs4)
       xlabel(1 " "  , noticks)
       xtitle("") ytitle("Fraction of new hires with a sticky wage")   
	   ylabel(0(0.10).45, format(%4.2fc) angle(0))
	   legend(off) graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_wage_sticky_bigf.pdf", replace as(pdf);	 


/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
