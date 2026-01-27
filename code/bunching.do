cap program drop bunching
*! Bunching v1.0.0 German Reyes 08May2020

program bunching
	syntax varlist(min=2 max=2 numeric) [if] [aw fw iw pw],   	///
		[						  								///
		EXClude_width(numlist)	  								///
		RN(numlist)	  											///
		INC_var(varname) 	 	   								///
		COUNT_var(varname) 	 	  								///
		BIN_size(numlist)  									    ///
		DUMMIES_size(numlist)	 								///
		POLY_degree(numlist) 	 								///
		BW(numlist) 	 										///
		KERNEL(string) 	 										///
		PARAmetric												///
		]


*-------------------------Default options------------------------------*

    local inc_var "`1'"
    local count_var = subinstr("`2'", ",", "", .)
	local poly_terms ""
	local dummies ""
	local mw_dummies ""
	
	if "`bin_size'" 	 == "" 	loc bin_size = 1 
	if "`dummies_size'"  == ""  loc dummies_size = 10
	if "`poly_degree'"   == ""  loc poly_degree = 7
	if "`kernel'" 		 == ""  loc kernel = "rectangle"
	if "`bw'" 			 != ""  loc bw = "bw(`bw')"
	if "`weight'" 	     != ""  loc wt = "[aw = `weight']" 

	di in yellow "Estimating counterfactual distribution:" _newline ///
			     "Bin size = `bin_size'"				   _newline ///
			     "Dummies size = `dummies_size'"		   _newline ///
			     "Polynomial degree = `poly_degree'"	 

   
	* Make bins based on `bin_size' (Default: R$1  bins)	
	if `bin_size' != 1 {
		replace `inc_var' = floor(`inc_var'/`bin_size')*`bin_size'
		collapse (sum) `count_var' `if', by(`inc_var') fast
	}
	
	
	* Complete zeros
	tsset `inc_var'
	tsfill
	replace `count_var' = 0 if `count_var' == .

	* Degree polynomial based on `poly_degree' (Default: 7th degree)
	tempvar new_inc
	gen `new_inc'  = `inc_var'/100
	
	forvalues i = 1(1)`poly_degree'{
		tempvar wage_`i'
		gen double `wage_`i'' =  `new_inc'^`i'
		loc poly_terms "`poly_terms' `wage_`i''"
	}

	* Variables to exclude from lpoly
	tempvar to_exclude 
	gen `to_exclude' = 0
	
*-------------------------Create dummies ------------------------------*

	* Round number dummies of size `dummies_size' each (Default: R$10 each)
	qui sum `inc_var' `if'
	loc floor_min = floor(r(min)/`dummies_size')*`dummies_size'
	loc floor_max = floor(r(max)/`dummies_size')*`dummies_size'

	loc cnt = 1
	forvalues w = `floor_min'(`dummies_size')`floor_max' {
		tempvar d_`cnt'
		gen `d_`cnt'' = (`inc_var' == `w')
		qui tab `d_`cnt''
		if r(r) == 1 drop `d_`cnt''
		if r(r) == 2 {
			qui replace `to_exclude' = 1 if `inc_var' == `w'
			loc rn_dummies "`rn_dummies' `d_`cnt''"
		}
		loc ++cnt
	}
	
	* Dummies for each minimum wage
	forvalues yr = 2002(1)2017 {
		if `yr' == 2017 loc mw = 937
		if `yr' == 2016 loc mw = 880
		if `yr' == 2015 loc mw = 788
		if `yr' == 2014 loc mw = 724
		if `yr' == 2013 loc mw = 678
		if `yr' == 2012 loc mw = 622
		if `yr' == 2011 loc mw = 545
		if `yr' == 2010 loc mw = 510
		if `yr' == 2009 loc mw = 465
		if `yr' == 2008 loc mw = 415
		if `yr' == 2007 loc mw = 380
		if `yr' == 2006 loc mw = 350
		if `yr' == 2005 loc mw = 300
		if `yr' == 2004 loc mw = 260
		if `yr' == 2003 loc mw = 240
		if `yr' == 2002 loc mw = 200
		loc min_wages = "`mw' `min_wages'"
		qui replace `to_exclude' = 1 if `inc_var' == `mw'
	}
	
	foreach mw of local min_wages {
		if (!mod(`mw',`dummies_size') == 1) continue
		tempvar mw_`mw'
		gen `mw_`mw'' = (`inc_var' == `mw')
		qui replace `to_exclude' = 1 if `inc_var' == `mw'
		loc mw_dummies "`mw_dummies' `mw_`mw''"
	}

	* Dummies in the excluded range
	if "`exclude_width'" != "" & "`rn' "!= ""  {
		loc min_exc_range = `rn' - `exclude_width'
		loc max_exc_range  = `rn' + `exclude_width'
		loc cnt = 0
		forvalues bin = `min_exc_range'(`bin_size')`max_exc_range' {
			if (!mod(`bin', `dummies_size') == 1) continue
			tempvar omit_`cnt'
			gen `omit_`cnt'' = (`inc_var' == `bin')
			loc omit_dummies "`omit_dummies' `omit_`cnt''"
		}
	}
	
	* Parametric regression 
	cap drop obs_hat obs_hat_adj

	if "`parametric'" == "parametric" {
		di in yellow "Running Global Polynomial regression"
		qui reg `count_var' `poly_terms' `rn_dummies' `omit_dummies' `mw_dummies' `if'
		
		* Store all coefficients (including dummies)
		mat b = e(b)

		* Store only polynomials
		mat b_poly = b[1,1..`poly_degree']

		* Store the constant 
		loc const = _b[_cons]

		* Predicted values
		mat score obs_hat = b_poly `if'
		
		replace obs_hat = obs_hat + `const'
	}
	
	* Local Polynomial
	if "`parametric'" != "parametric" {
		di in yellow "Running Local Polynomial regression"
		tempvar count_var_aux
		gen `count_var_aux' = `count_var' if `to_exclude' == 0
		lpoly `count_var_aux' `inc_var' `if' , kernel(`kernel') degree(`poly_degree') gen(obs_hat) se(se_obs) at(`inc_var') `bw' nograph
	}
	
	* Adjustment factor to get the same total number of observations
	qui sum `count_var' `if', d
	loc tot_obs = r(sum)
	
	qui sum obs_hat `if', d
	loc tot_pred = r(sum) 
	loc adjt_factor = `tot_obs'/`tot_pred'

	* Counterfactual number of observations
	gen obs_hat_adj = obs_hat * `adjt_factor' `if'

end

