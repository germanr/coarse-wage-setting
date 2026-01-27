/*--------------------------------------------------------------------------
This code generates the appendix figures

Uses:	 	hires_firms_panel.dta, firms_panel_merge.dta, theta.dta, theta_figs.dta, bins.dta
Produces: 		   
			Figure A1: Distribution of the last digits of new hires’ contracted salaries
			* Panel B. Last three digits
			* Panel A. Last two digits
			Figure A3: Histogram of the share of workers in each firm hired at a round salary
			* Panel A. All firms
			* Panel B. Firms that hired five or more workers in the sample
			* Panel B. Firms that hired five or more workers in the sample
			Figure E1: Comparison of parametric and non-parametric counterfactual distributions
			Figure E2: Robustness of the counterfactual distribution to alternative specifications
			* Panel A. Robustness to polynomial degree
			* Panel B. Robustness to kernel choice
			Figure E3: Estimation of the counterfactual distribution, excess mass, and missing mass
			* Panel A. Excess mass
			* Panel B. Missing mass
			Figure F1: Resignation rates below, at, and above salaries divisible by 100
			* Panel A. Average resignation rate in the vicinity of round salaries
			* Panel B. Regression Discontinuity estimates beta's and gamma's from equation
			Figure F2: Difference in the number of contracts around salaries divisible by 100
			* Panel A. Share of contracts just below and just above each side of the round salary
			* Panel B. Regression Discontinuity estimates beta’s from equation (F2)
			Figure F3: Fraction of workers hired at a coarse salary across industries and occupations
			* Panel A. Industry level
			* Panel B. Occupation level
			Figure F: Fraction of workers hired at firms with a CBA
			Figure F4: Firm size and fraction of workers hired through coarse wage-setting
			Figure F5: Distribution of contracted salaries and kinks in the income tax schedule during 2015
--------------------------------------------------------------------------*/

#delimit;
glo root "YOUR ROOT DIRECTORY";
glo cod "${root}\code"     ;
glo raw "${root}\raw_data";
glo dat "${root}\data"    ;


*** Figure A1: Distribution of the last digits of new hires’ contracted salaries;

#delimit;
use "${dat}/bins.dta" if group == "all", replace;
drop if hired_wage == 10101;

tostring hired_wage, gen(str_wage);
gen last_digit    = substr(str_wage,-1,.);
gen last_2_digits = substr(str_wage,-2,.);
gen last_3_digits = substr(str_wage,-3,.);

gen dummy_over_1000 = (hired_wage >= 1000);

collapse (sum) obs, by(last_* dummy_over_1000);

destring _all, replace;

* Panel B. Last three digits;
histogram last_3_digits if dummy_over_1000 == 1 [fw=obs], color(navy) width(1) fraction discrete
	xtitle("Last three digits of the contracted salary")  xlabel(0 "000" 100 200 300 400 500 600 700 800 900 999 "999")
	ytitle("Fraction of contracts") 	                   ylabel(0(0.05)0.15, angle(0) format(%4.2fc))                   yscale(titlegap(2))
	graphregion(color(white)) bgcolor(white);
	graph export "${res}/fig_last_3dig.pdf", replace as(pdf);


* Panel A. Last two digits;
collapse (sum) obs, by(last_digit last_2_digits);

histogram last_2_digits [fw=obs], color(navy) width(1) fraction discrete
	xtitle("Last two digits of the contracted salary") xlabel(0 "00" 10 20 30 40 50 60 70 80 90 99 "99")
	ytitle("Fraction of contracts") 	               ylabel(0(0.05)0.15, angle(0) format(%4.2fc))      yscale(titlegap(2))
	graphregion(color(white)) bgcolor(white);
	graph export "${res}/fig_last_2dig.pdf", replace as(pdf);



*** Figure A3: Histogram of the share of workers in each firm hired at a round salary;

#delimit;
use "${dat}\firms_panel_merge.dta", replace;

keep shr_hires_10 hires mean_size firm_size_no0 firmid;
keep if shr_hires_10 != .;
bys firmid: keep if _n == 1;

gen bin = floor(shr_hires_10*20)/20;
replace bin = bin + 0.025 if !inlist(shr_hires_10, 0, 1);
 
gen cnt           = 1               ;
gen cnt_big_hires = (hires >= 5)    ;
gen cnt_big_size  = (mean_size >= 5);

gcollapse (sum) cnt*, by(bin);

replace bin = 0 if bin < 0.0001;

egen tot           = total(cnt)          ; gen shr_hires_10           = cnt/tot;
egen tot_big_hires = total(cnt_big_hires); gen shr_hires_10_big_hires = cnt_big_hires/tot_big_hires;
egen tot_big_size  = total(cnt_big_size) ; gen shr_hires_10_big_size  = cnt_big_size/tot_big_size;

replace bin = bin - 0.025 if bin == 0 ;
replace bin = bin + 0.025 if bin == 1 ;

* Panel A. All firms;

twoway bar shr_hires_10 bin, color(navy) lcolor(white) barwidth(0.05)	
       ytitle("Fraction of firms")                              xscale(titlegap(2)) xlabel(0(.25)1, angle(0) format(%4.2fc)) 
       xtitle("Share of new employees hired at a round number") yscale(titlegap(2)) ylabel(0(.1).3, angle(0) format(%4.2fc) gmax gmin) 
       graphregion(color(white)) bgcolor(white);
       graph export "${res}/fig_shrhires_10.pdf", replace as(pdf);

* Panel B. Firms that hired five or more workers in the sample;

twoway bar shr_hires_10_big_size bin, color(navy) lcolor(white) barwidth(0.05)	
	   ytitle("Fraction of firms")                              xscale(titlegap(2)) xlabel(0(.25)1, angle(0) format(%4.2fc)) 
	   xtitle("Share of new employees hired at a round number") yscale(titlegap(2)) ylabel(0(.1).3, angle(0) format(%4.2fc) gmax gmin) 
	   graphregion(color(white)) bgcolor(white);
	   graph export "${res}/fig_shrhires_10_p5hires.pdf", replace as(pdf);


*** Figure E1: Comparison of parametric and non-parametric counterfactual distributions;

#delimit; 
include "${cod}/bunching.do";
use "${dat}/bins.dta" if group == "all", replace;
qui sum hired_wage; drop if hired_wage == r(max);

bunching hired_wage obs, para poly(7); gen obs_hat_para    = obs_hat;
bunching hired_wage obs, 	  poly(7); gen obs_hat_nonpara = obs_hat;

gen hat_lo = obs_hat - 1.96*se_obs; gen hat_hi = obs_hat + 1.96*se_obs;

#delimit;
twoway  line obs_hat_para obs_hat_nonpara hired_wage if hired_wage <= 3500, lcolor(maroon navy) lpattern(solid solid) ||
		line hat_lo hat_hi                hired_wage if hired_wage <= 3500, lcolor(gs8 gs8) lpattern(dash dash) lwidth(thin)
		xtitle("Contracted monthly earnings (in R$)") xlabel(500(500)3500) xscale(titlegap(2)) 
		ytitle("Number of workers") ylabel(0 "0k" 50000 "50k" 100000 "100k" 150000 "150k" 200000 "200k", angle(0) gmax gmin) 
		legend(order(1 "Global polynomial (7th-degree)" 2 "Local polynomial (7th-degree)" 3 "95 percent confidence interval") col(1) ring(0) position(1) bmargin(0))
		graphregion(color(white)) bgcolor(white);
		graph export "${res}/fig_para_nonpara.pdf", replace as(pdf);
		
*** Figure E2: Robustness of the counterfactual distribution to alternative specifications;

* Panel A. Robustness to polynomial degree;
#delimit;
include "${cod}/bunching.do";
use "${dat}/bins.dta" if group == "all", replace;
qui sum hired_wage; drop if hired_wage == r(max);

forvalues j = 5(1)10 {; bunching hired_wage obs, poly(`j'); gen poly`j' = obs_hat; drop se*; };

#delimit;
twoway line poly5 poly6 poly7 poly8 poly9 poly10 hired_wage if hired_wage <= 3500, lcolor(gs14 gs12 gs10 gs8 gs6 gs4 gs2)
		xtitle("Contracted monthly earnings (in R$)") xlabel(500(500)3500) xscale(titlegap(2)) 
		ytitle("Number of workers") ylabel(0 "0k" 50000 "50k" 100000 "100k" 150000 "150k" 200000 "200k", angle(0) gmin gmax) yscale(titlegap(2)) 
		legend(order(1 "5" 2 "6" 3 "7" 4 "8" 5 "9" 6 "10") col(1) ring(0) position(1) bmargin(0)) legend(subtitle("Degree:"))
		graphregion(color(white)) bgcolor(white);
		graph export "${res}/fig_robust_degree.pdf", replace as(pdf);

	
* Panel B. Robustness to kernel choice;
#delimit;
foreach k in "epa" "gaus" "trian" {; di in red "`k'"; bunching hired_wage obs, poly(7) kernel(`k'); gen `k' = obs_hat; drop se*; };

#delimit;
twoway line poly7 epa gaus trian hired_wage if hired_wage <= 3500, lcolor(orange maroon forest_green navy)
		xtitle("Contracted monthly earnings (in R$)") yscale(titlegap(2)) xlabel(500(500)3500) xscale(titlegap(2)) 
		ytitle("Number of workers") ylabel(0 "0k" 50000 "50k" 100000 "100k" 150000 "150k" 200000 "200k", angle(0) gmax gmin) yscale(titlegap(2)) 
		legend(order(1 "Uniform" 2 "Normal" 3 "Epanechnikov" 4 "Triangle") col(1) ring(0) position(1) bmargin(0)) legend(subtitle("Kernel:"))
		graphregion(color(white)) bgcolor(white);
		graph export "${res}/fig_robust_kernel.pdf", replace as(pdf);



*** Figure E3: Estimation of the counterfactual distribution, excess mass, and missing mass;

#delimit;
use "${dat}\theta.dta" if group == "all", replace;

keep hired_wage n_act n_hat_adj group;
keep if inrange(hired_wage,2834,3166); // These limits come from the missing mass = excess mass calculation;
gen excess = n_act-n_hat_adj; 
egen total = total(excess); di in red total; drop total;
gen mult_1000 = !mod(hired_wage,1000);


* Panel A. Excess mass;
gen x11 = 2930; gen y11 = 70000;
gen x12 = 3000; gen y12 = 54000;
gen x22 = 3090; gen y22 = 6000 ;
gen x21 = 3090; gen y21 = 70000;

twoway area n_act	 hired_wage, color(yellow) lcolor(white) lwidth(none) || 
	   area n_hat_adj hired_wage, lcolor(white) fcolor(white) lwidth(none) || 
	   connect  n_act hired_wage, mcolor(gs6) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs6) lwidth(vthin) || 
	   scatter n_act hired_wage if mult_1000 == 1 , mcolor(dkorange) msymbol(S) msize(medsmall) || 
	   line    n_hat_adj hired_wage if hired_wage != 3000, lcolor(maroon) lwidth(thin) ||  
	   pcarrow y11 x11 y12 x12, lcolor(brown) mcolor(brown) || 
	   pcarrow y21 x21 y22 x22, lcolor(maroon) mcolor(maroon) 
	   ylabel(0 "0k" 100000 "100k" 200000 "200k" 300000 "300k" , angle(0)) yscale(titlegap(2)) 
	   xlabel(2830 "R$2830" 2900 "R$2900" 3000 "R$3000" 3100 "R$3100" 3170 "R$3170", labsize(small)) 	
	   xtitle("Contractual monthly earnings") ytitle("Number of contracts") 
	   graphregion(color(white)) bgcolor(white) legend(off) 
	   text(80000 2930 "Excess mass") text(80000 3090 "Counterfactual density");
	   graph export "${res}/fig_bunch_3000_a.pdf", replace as(pdf);


* Panel B. Missing mass;
#delimit;
drop x* y*;
gen x1 = 3040 ; gen y1  = 77000;
gen x2 = 3040 ; gen y2  = 7500 ;
gen x11 = 2870; gen y11 = 77000;
gen x12 = 2834; gen y12 = 7500 ;
gen x22 = 3166; gen y21 = 77000;
gen x21 = 3140; gen y22 = 7500 ;

twoway area n_hat_adj hired_wage if !inrange(hired_wage,2999,3001) & n_hat_adj > n_act, color(red)    lcolor(white) lwidth(none) || 
	   area n_act     hired_wage if !inrange(hired_wage,2999,3001)                    , lcolor(white) fcolor(white) lwidth(none) || 
	   connect n_act hired_wage, mcolor(gs6) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs6) lwidth(vthin) || 
	   scatter n_act hired_wage  if mult_1000    == 1 , mcolor(dkorange) msymbol(S) msize(medsmall) || 
	   line n_hat_adj hired_wage if hired_wage != 3000, lcolor(maroon) lwidth(thin) ||  
	   pcarrow y1 x1 y2 x2, lcolor(red) mcolor(red) || 
	   pcarrow y11 x11 y12 x12, lcolor(forest_green) mcolor(forest_green) || 
	   pcarrow y21 x21 y22 x22, lcolor(navy) mcolor(navy) 
	   ylabel(0 "0k" 100000 "100k" 200000 "200k" 300000 "300k" , angle(0)) yscale(titlegap(2)) 
	   xlabel(2830 "R$2830" 2900 "R$2900" 3000 "R$3000" 3100 "R$3100" 3170 "R$3170", labsize(small)) 	
	   xtitle("Contractual monthly earnings") ytitle("Number of contracts") 
	   graphregion(color(white)) bgcolor(white) legend(off) 
	   text(90000 3040 "Missing mass") text(100000 2870 "Lower bound" "(R$2834)") text(100000 3140 "Upper bound" "(R$3166)");
	   graph export "${res}/fig_bunch_3000_b.pdf", replace as(pdf);



*** Figure F1: Resignation rates below, at, and above salaries divisible by 100;


#delimit;
tempname jobsep; tempfile jobsep_file; postfile `jobsep' str70(dv rn bw var coef se mean_below mean_at mean_above obs) using `jobsep_file', replace;

forvalues rn = 300(100)3500 {; di in red "Estimating R$`rn'"; loc lo = `rn'-50; loc hi = `rn'+50;

	use "${dat}\hires_firms_panel.dta" if inrange(hired_wage,`lo',`hi'), replace;
	keep hired_wage sep_cause pis adm_type id firmid yr;
	
	gen     sep = .;
	replace sep = 0 if sep_cause == 0 | sep_cause == .;
	replace sep = 1 if inlist(sep_cause,10, 11, 20, 21);

	gen     res = .;
	replace res = 0 if sep_cause == . | sep_cause == 0 | inlist(sep_cause,10, 11);
	replace res = 1 if inlist(sep_cause,20, 21);
	
	gen rv_`rn' = hired_wage - `rn';
	
	gen dummy_rd = 0; replace dummy_rd = 1 if rv_`rn' > 0 &  rv_`rn' != .;
	gen dummy_rn = 0; replace dummy_rn = 1 if rv_`rn' == 0;
	gen spline  = rv_`rn';
	gen inter   = rv_`rn'*dummy_rd;

	foreach bw in 10 {; keep if inrange(rv_`rn',-`bw',`bw');
		
		foreach dv in "sep" "res" { ;
	
			count; if r(N) < 30 continue; count if `dv' == 1; if r(N) < 10 continue;

			reg `dv' dummy_rd dummy_rn spline inter if inrange(rv_`rn',-`bw',`bw'), cluster(pis) robust;
			loc b_rd = _b[dummy_rd]; loc se_rd = _se[dummy_rd]; loc N = e(N);
			loc b_rn = _b[dummy_rn]; loc se_rn = _se[dummy_rn];
			qui sum `dv' if rv_`rn' <  0 ; loc mean_below = r(mean);
			qui sum `dv' if rv_`rn' == 0 ; loc mean_at    = r(mean);
			qui sum `dv' if rv_`rn' >  0 ; loc mean_above = r(mean);

			post `jobsep' ("`dv'") ("`rn'") ("`bw'") ("above") ("`b_rd'") ("`se_rd'") ("`mean_below'") ("`mean_at'") ("`mean_above'") ("`N'") ;
			
			post `jobsep' ("`dv'") ("`rn'") ("`bw'") ("rn") ("`b_rn'") ("`se_rn'") ("`mean_below'") ("`mean_at'") ("`mean_above'") ("`N'") ;

		};
	};

drop dummy_rd spline* inter*;

};

postclose `jobsep'; use `jobsep_file', clear; destring, replace; compress;

gen lo = coef - 1.96*se; gen hi = coef + 1.96*se;
save "${dat}\temp_rd_res.dta", replace;


* Panel A. Average resignation rate in the vicinity of round salaries;

#delimit;
use "${dat}\temp_rd_res.dta", replace;

keep if bw == 10;
keep if var == "above";

#delimit;
qui sum mean_below if dv == "res" [w=obs]; glo mean_below = r(mean); loc di_below : di %4.3f r(mean);
qui sum mean_at    if dv == "res" [w=obs]; glo mean_at   = r(mean); loc di_at    : di %4.3f r(mean);
qui sum mean_above if dv == "res" [w=obs]; glo mean_above = r(mean); loc di_above : di %4.3f r(mean);

twoway  scatter mean_below rn if dv == "res" & rn <= 3500, lcolor(maroon) mcolor(maroon) msym(D) ||
		scatter mean_at    rn if dv == "res" & rn <= 3500, lcolor(navy) mcolor(navy) msym(O) ||
		scatter mean_above rn if dv == "res" & rn <= 3500, lcolor(forest_green) mcolor(forest_green) msym(T) 
		ylabel(0(.03).15, format(%4.2fc) angle(0) gmax gmin)
		xtitle("Contracted monthly earnings (in R$)") xlabel(500(500)3500)
		ytitle("Average separation rate")
		graphregion(color(white)) bgcolor(white)
		yline($mean_below, lcolor(maroon) lpattern(dot)) yline($mean_at, lcolor(navy) lpattern(shortdash_dot)) yline($mean_above, lcolor(forest_green) lpattern(shortdash)) 
		text(0.025  950 "Average just below = `di_below'", color(maroon))
		text(0.015 1110 "Average at round numbers = `di_at'", color(navy))
		text(0.005  950 "Average just above = `di_above'", color(forest_green))
		legend(order(1 "Just below" 2 "At the round salary" 3 "Just above") row(3) ring(0) pos(5));
		graph export "${res}\fig_avgres_bw10.pdf", as(pdf) replace;		




* Panel B. Regression Discontinuity estimates beta's and gamma's from equation;
#delimit;

use "${dat}\temp_rd_res.dta", replace;
replace rn = rn + 25 if var == "above";
glo bw = 10;
cap drop x* z* y* w*;
gen x1 = 900; gen y1 =  0.073; gen x2 = 700; gen y2 = 0.025;
gen w1 = 900; gen z1 = -0.068; gen w2 = 700; gen z2 = -0.009;

qui sum coef if dv == "res" & var == "rn"    [w=obs]; glo mean_rn    = r(mean); loc di_rn   : di %4.3f r(mean);
qui sum coef if dv == "res" & var == "above" [w=obs]; glo mean_above = r(mean); loc di_above : di %4.3f r(mean);
twoway scatter coef  rn if dv == "res" & var == "rn"    & bw == $bw & rn <= 3500, lcolor(navy) mcolor(navy) msize(small) ||
		rcap   lo hi rn if dv == "res" & var == "rn"    & bw == $bw & rn <= 3500, lcolor(navy) lwidth(vthin) ||
		scatter coef rn if dv == "res" & var == "above" & bw == $bw & rn <= 3500, lcolor(forest_green) mcolor(forest_green) msize(small) msym(T) ||
		rcap   lo hi rn if dv == "res" & var == "above" & bw == $bw & rn <= 3500, lcolor(forest_green) lwidth(vthin) ||
		pcarrow y1 x1 y2 x2, lcolor(navy) mcolor(navy) ||
		pcarrow z1 w1 z2 w2, lcolor(forest_green) mcolor(forest_green)
		xtitle("Contractual monthly earnings (in R$)") xlabel(500(500)3500)
		ytitle("Estimated coefficient") ylabel(-.10(.05).10, format(%4.2fc) angle(0) gmax gmin)
		yline(0, lcolor(black) lwidth(vthin))
		yline(${mean_rn}   , lcolor(navy) 	   lpattern(dash))
		yline(${mean_above}, lcolor(forest_green) lpattern(dash))
		text( 0.090 1340  "Average coefficient on" "round number dummy", color(navy) size(medsmall))
		text(-0.088 1200 "Average coefficient on" `""above" dummy"', color(forest_green) size(medsmall))
		legend(order(1 "Round number dummy" 3 "Above round number dummy")  row(3) ring(0) pos(5) subtitle("Coefficient on:"))
		graphregion(color(white)) bgcolor(white);
		graph export "${res}\fig_rdres_bw10.pdf", as(pdf) replace;	

erase "${dat}\temp_rd_res.dta", replace;



*** Figure F2: Difference in the number of contracts around salaries divisible by 100;

* Panel A. Share of contracts just below and just above each side of the round salary;

#delimit ;
glo bw = 10;
include "${cod}/drop-mw.do";

use "${dat}/bins.dta" ,clear;

keep if group == "mth_yr"; 
gen yr = real(substr(string(level),-4,4));
drop_mw hired_wage level;
	
gcollapse (sum) obs, by(hired_wage);
	
glo bw = $bw;
gen closest_rn = round(hired_wage/100)*100;
gen lo = closest_rn - ${bw};
gen hi = closest_rn + ${bw};
gen inrange = (inrange(hired_wage,lo,hi)); keep if inrange; 
bys closest_rn (hired_wage): gen range = _N; qui sum range, d; keep if range == r(max); drop range;
by closest_rn: egen tot_inrange     = total(obs);
by closest_rn: egen aux_norn = total(obs) if hired_wage != closest_rn; by closest_rn: egen tot_inrange_norn = mean(aux_norn);
gen below_rn = (inrange(hired_wage,lo,closest_rn)) if hired_wage != closest_rn; gen obs_below = below_rn*obs; by closest_rn: egen tot_below = total(obs_below);
gen above_rn = (inrange(hired_wage,closest_rn,hi)) if hired_wage != closest_rn; gen obs_above = above_rn*obs; by closest_rn: egen tot_above = total(obs_above);

gen shr_below = tot_below/tot_inrange; gen shr_below_norn = tot_below/tot_inrange_norn;
gen shr_above = tot_above/tot_inrange; gen shr_above_norn = tot_above/tot_inrange_norn;

egen shr_obs_all = total(obs); replace shr_obs_all = tot_inrange/shr_obs;

keep if hired_wage == closest_rn; drop aux_norn obs_below obs_above closest_rn tot_inrange* inrange below_rn above_rn lo hi;

gen mult_1000 = (!mod(hired_wage,1000));
egen shr_obs = total(obs); replace shr_obs = obs/shr_obs;
gen excess = shr_above_norn - shr_below_norn;

#delimit;

qui sum shr_below_norn [w=obs]; glo mean_below = r(mean); loc di_below : di %4.3f r(mean);
qui sum shr_above_norn [w=obs]; glo mean_above = r(mean); loc di_above : di %4.3f r(mean);

twoway scatter shr_below_norn shr_above_norn hired_wage if hired_wage <= 3500, 
		lcolor(maroon forest_green) mcolor(maroon forest_green) msym(D T)
		ytitle("Share of contracts") ylabel(0(.1).65,  format(%4.2fc) angle(0) gmax gmin) yscale(titlegap(2)) 
		xtitle("Contracted monthly earnings (in R$)") xlabel(500(500)3500) yline(0.5, lcolor(gs12) lpattern(dash)) xscale(titlegap(2)) 
		legend(order(1 "Just below the round salary" 2 "Just above the round salary") ring(0) pos(5) col(1)) 
		text(0.06 950 "Average just below = `di_below'", color(maroon))
		text(0.01 950 "Average just above = `di_above'", color(forest_green))
		graphregion(color(white)) bgcolor(white);
		graph export "${res}\fig_mass_mult_100_bw${bw}.pdf", as(pdf) replace;


* Panel B. Regression Discontinuity estimates beta’s from equation (F2);

#delimit;

tempname jump; tempfile jump_file; postfile `jump' str70(rn bw rd_coef std_err bins N) using `jump_file', replace;

forvalues rn = 400(100)3500 {; di in red "Estimating R$`rn'"; loc lo = `rn'-50; loc hi = `rn'+50;

	use "${dat}\bins.dta" if inrange(hired_wage,`lo',`hi'), replace; 

	keep if group == "mth_yr"; 
	gen yr = real(substr(string(level),-4,4));
	drop_mw hired_wage level;
		
	gcollapse (sum) obs, by(hired_wage);

	gen rv = hired_wage - `rn'; drop if rv == 0;

	gen dummy_rd = 0; replace dummy_rd = 1 if rv >= 0 &  rv != .;
	gen spline  = rv;
	gen inter   = rv*dummy_rd;
	
	foreach bw in 10 {; keep if inrange(rv,-`bw',`bw');
			
			keep hired_wage obs rv dummy_rd spline inter;
			
			egen pct_obs = pc(obs);
			gen     kernel_wt = 0                if abs(rv) >  `bw';
			replace kernel_wt = 1 - abs(rv/`bw') if abs(rv) <= `bw';
			
			reg pct_obs dummy_rd spline inter [aw=kernel_wt], robust;
			loc b = _b[dummy_rd]; loc se = _se[dummy_rd]; loc bins = e(N);
			qui sum obs; loc N = r(sum);

			post `jump' ("`rn'") ("`bw'") ("`b'") ("`se'") ("`bins'") ("`N'");

		
	};

};

postclose `jump'; use `jump_file', clear; destring, replace; compress;
gen lower_bound95 = rd_coef - 1.96* std_err;
gen upper_bound95 = rd_coef + 1.96* std_err;

glo bw = 10;
gen x1 = 2150; gen y1 = 3.5; gen x2 = 2150; gen y2 = 0.5;

#delimit;
sum rd_coef if bw == $bw [w=N]; loc mean = r(mean);

twoway scatter rd_coef   rn if bw == $bw & rn <= 3500, lcolor(navy) mcolor(navy) ||
		rcap upper lower rn if bw == $bw & rn <= 3500, lcolor(navy) lwidth(vthin) ||
		pcarrow y1 x1 y2 x2, lcolor(maroon) mcolor(maroon) 
		xtitle("Contracted monthly earnings (in R$)") xlabel(500(500)3500) xscale(titlegap(2)) 
		ytitle("Estimated RD coefficient (percentage points)") ylabel(, format(%4.0fc) angle(0))
		yline(`mean', lcolor(maroon) lpattern(dash)) yline(0, lcolor(black) lwidth(vthin)) 
		text(4.5 2150 "Average" "RD coefficient", color(maroon)) 
		legend(order(1 "RD coefficient" 2 "95 percent confidence interval") ring(0) pos(5) col(1)) 
		graphregion(color(white)) bgcolor(white);
		graph export "${res}\fig_mccrary_bw${bw}.pdf", as(pdf) replace;	


*** Figure F3: Fraction of workers hired at a coarse salary across industries and occupations;

* Panel A. Industry level;
#delimit;
use "${dat}\theta_figs.dta" if group == "all", clear; qui sum theta; glo mean_theta = r(mean);
use "${dat}\theta_figs.dta" if group == "ind1d", replace; rename level sector; 

drop if sector == 11 | sector == 15 | sector == 16;

gsort -theta; gen order = _n; 

label define sector 
	1  "Primary sector" 
	2  "Mining and quarrying" 		
	3  "Manufacturing" 
	4  `""Electricity, gas, and" "water supply""' 	
	5  "Construction" 
	6  "Wholesale and retail trade" 
	7  "Hotels and restaurants" 
	8  `""Transportation, storage," "and communication""' 
	9  "Financial intermediation" 
	10 `""Real estate, renting," "and business activities""' 
	11 `""Public administration, defense" "and social security""' 
	12 "Education" 
	13 "Health and social work" 
	14 `""Other community, personal," "and social services""' 
	15 "Domestic services" 
	16 "Extraterritorial organizations";
label values sector sector;
decode sector, gen(sector_str);

bys order: gen aux_grp = (_n == 1); replace aux_grp = sum(aux_grp); local max = aux_grp[_N]; 

gen aux_example = _n;
forvalues i = 1/`max' {;
	sum aux_example if aux_grp == `i', meanonly;
	local label = sector_str[`r(min)'];
	local value = order[`r(min)'];
	label def order `value' `"`label'"', modify;
};
label val order order;
qui count; loc tot = r(N);

gen x1 = 0.27; gen y1 = 12.5; gen x2 = 0.23; gen y2 = 12.5;
twoway rspike theta_lo theta_hi order, horizontal || 
	scatter order theta, msym(d) mcolor(navy) ||
	pcarrow y1 x1 y2 x2, lcolor(maroon) mcolor(maroon)
	xtitle("Fraction of workers hired through coarse wage-setting ({&theta})")  xlabel(0(.1).4, format(%4.2fc)) xscale(titlegap(2))  	
	ytitle(" ") ylabel(1(1)`tot', valuelabel ang(h) labsize(vsmall) tstyle(minor_notick)) yscale(titlegap(-25))  
	xline($mean_theta, lcolor(maroon) lpattern(dash))
	text(12.5 0.31  "Average {&theta}", color(maroon))
	graphregion(color(white)) bgcolor(white) legend(off);
	graph export "${res}\fig_theta_ind.pdf", replace as(pdf);


* Panel B. Occupation level;

#delimit ;
use "${dat}\theta_figs.dta" if group == "occ1d", replace; 

rename level occupation;
drop if occupation == 0; 
gsort -theta; gen order = _n;

label define occupation 
	0  "Military" 
	1  "Management"		
	2  `""Professionals, artists" "and scientists""' 
	3  "Mid-level technicians" 	
	4  "Administrative workers" 
	5  "Service workers and vendors" 
	6  "Primary sector workers" 
	7  "Production and manufacturing, I" 
	8  "Production and manufacturing, II" 
	9  "Repair and maintenance workers";

label values occupation occupation; decode occupation, gen(occ_str);

bys order: gen aux_grp = (_n == 1); replace aux_grp = sum(aux_grp); local max = aux_grp[_N]; 

gen aux_example = _n;
forvalues i = 1/`max' {;
	sum aux_example if aux_grp == `i', meanonly;
	local label = occ_str[`r(min)'];
	local value = order[`r(min)'];
	label def order `value' `"`label'"', modify;
};
label val order order;
qui count; loc tot = r(N);

gen x1 = 0.27; gen y1 = 8.5; gen x2 = 0.23; gen y2 = 8.5;

twoway rspike theta_lo theta_hi order, horizontal || 
	scatter order theta, msym(d) mcolor(navy) ||
	pcarrow y1 x1 y2 x2, lcolor(maroon) mcolor(maroon)
	xtitle("Fraction of workers hired through coarse wage-setting ({&theta})")  xlabel(0(.1).4, format(%4.2fc)) xscale(titlegap(2))  	
	ytitle(" ") ylabel(1(1)`tot', valuelabel ang(h) labsize(vsmall) tstyle(minor_notick)) yscale(titlegap(-8))  
	xline($mean_theta, lcolor(maroon) lpattern(dash))
	text(8.5 0.31  "Average {&theta}", color(maroon))
	graphregion(color(white)) bgcolor(white) legend(off);
	graph export "${res}\fig_theta_occ.pdf", replace as(pdf);



*** Figure F: Fraction of workers hired at firms with a CBA;
#delimit ;
use "${dat}\theta_figs.dta" if regex(group, "cba"), replace; 

keep group level theta theta_fixed_sample theta_lo theta_hi std_error;

gen     order = .                             ;
replace order = 1 if group == "cba_any"       ;
replace order = 2 if group == "cba_wageclause";
replace order = 3 if group == "cba_firm_lvl"  ;

sort order level;

cap drop xaxis;
gen     xaxis = _n;
replace xaxis = xaxis + 1  if group == "cba_wageclause";
replace xaxis = xaxis + 2  if group == "cba_firm_lvl"  ;

sort xaxis;

#delimit;
twoway  bar  theta              xaxis if level == 0, barwidth(.8) bcolor(navy)
	 || rcap theta_hi theta_lo  xaxis if level == 0,              lcolor(black) lpattern(dash) lwidth(vthin)
     || bar  theta              xaxis if level == 1, barwidth(.8) bcolor(maroon) 
	 || rcap theta_hi theta_lo  xaxis if level == 1,              lcolor(black) lpattern(dash) lwidth(vthin)
	 ylabel(0(.05).25, format(%4.2fc)  angle(0) gmin) yscale(titlegap(2)) ytitle("Fraction hired through coarse wage-setting ({&theta})")
	 xlabel(1.5 "Any CBA" 4.5 "Wage-related CBA" 7.5 "Firm-level CBA") xtitle("") 
	 legend(order(1  "Has a CBA" 3 "Does not have a CBA") col(2) ) 
	 graphregion(color(white)) bgcolor(white);
  	 graph export "${res}\fig_theta_cba.pdf", replace; 




*** Figure F4: Firm size and fraction of workers hired through coarse wage-setting;
#delimit;
use "${dat}\theta_figs.dta" if group == "firm_size_group", replace; 
rename level firm_size_group;
drop if firm_size_group == 0 | firm_size_group == 101;

twoway  scatter theta firm_size_group, lcolor(navy) mcolor(navy) msymbol(o)
	 || rcap theta_hi theta_lo firm_size_group, lcolor(navy) mcolor(navy) 
		xtitle("Firm size (number of workers)") 
		xlabel(1 20 40 60 80 100, labsize(small)) xscale(titlegap(2))
		ylabel(, format(%4.2fc) angle(0) gmax gmin) ytitle("Fraction hired through coarse wage-setting ({&theta})") 
		legend(off)
		graphregion(color(white)) bgcolor(white);
		graph export "${res}\fig_theta_firmsize.pdf", replace as(pdf);



*** Figure F5: Distribution of contracted salaries and kinks in the income tax schedule during 2015;
#delimit;
use "${dat}/bins.dta" ,clear;

keep if group == "mth_yr"; 
gen yr = real(substr(string(level),-4,4));
keep if yr == 2015;

gcollapse (sum) obs*, by(hired_wage yr);

keep if inrange(hired_wage,0,5000);

include "${cod}/drop-mw.do"; 
drop_mw hired_wage yr;

foreach rn in "10" "50" "100" "500" "1000" {; gen mult_`rn' = !mod(hired_wage,`rn'); };

gen fig_10  = mult_10;   replace fig_10  = 0 if mult_50   == 1 | mult_100 == 1 | mult_1000 == 1;
gen fig_50  = mult_50;   replace fig_50  = 0 if mult_100  == 1;
gen fig_100 = mult_100;  replace fig_100 = 0 if mult_500  == 1;
gen fig_500 = mult_500;  replace fig_500 = 0 if mult_1000 == 1;
gen fig_1000 = mult_1000;

loc kink1 = 1903.98;
loc kink2 = 2826.65;
loc kink3 = 3751.05;
loc kink4 = 4664.68;

loc x = "xlabel(1000(500)5000)";
loc y = `"ylabel(0 "0k" 100000 "100k" 200000 "200k" 300000 "300k" 400000 "400k", angle(0) gmin gmax)"';

sort hired_wage obs;
twoway connect obs hired_wage, mcolor(gs12) mlwidth(vthin) msymbol(Oh) msize(vsmall) lcolor(gs14) lwidth(vthin) ||
	   scatter obs hired_wage if fig_10   == 1, mcolor(navy) mlwidth(vthin) msymbol(Oh) msize(vsmall) ||
	   scatter obs hired_wage if fig_50   == 1, mcolor(maroon) mlwidth(vthin) msymbol(D) msize(vsmall) ||
	   scatter obs hired_wage if fig_100  == 1, mcolor(forest_green) msymbol(T) msize(vsmall) ||
	   scatter obs hired_wage if fig_500  == 1, mcolor(dkorange) msymbol(S) msize(vsmall) ||
	   scatter obs hired_wage if fig_1000 == 1, mcolor(purple) msymbol(O) msize(small)
	   xtitle("Contracted monthly earnings (in R$)") ytitle("Number of contracts") yscale(titlegap(2)) `x' `y'
	   legend(order(0 "Salary divisible by: "2 "10" 3 "50" 4 "100" 5 "500" 6 "1000" 1 "Other") row(1) size(small))
	   graphregion(color(white)) bgcolor(white) xline(`kink1' `kink2' `kink3' `kink4', lcolor(maroon) lpattern(dash))
	   text(350000 1250 "Changes in the" "marginal tax rate", color(maroon));
	   graph export "${res}/fig_mtr_2015.pdf", replace as(pdf);



/* End of do-file */
* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
