cap program drop drop_mw
program define drop_mw
	syntax varlist(min=0 max=2 numeric) [if]
	token `varlist'
	tempvar mw is_mw
	
	loc wage_var = "`1'"
	if  "`wage_var'" == "" loc wage_var "hired_wage"

	loc time_var = "`2'"
	if  "`time_var'" == "" loc time_var "yr"

	gen 	`mw' = .
	replace `mw' = 937 if `time_var' == 2017
	replace `mw' = 880 if `time_var' == 2016
	replace `mw' = 788 if `time_var' == 2015
	replace `mw' = 724 if `time_var' == 2014
	replace `mw' = 678 if `time_var' == 2013
	replace `mw' = 622 if `time_var' == 2012
	replace `mw' = 545 if `time_var' == 2011
	replace `mw' = 510 if `time_var' == 2010
	replace `mw' = 465 if `time_var' == 2009
	replace `mw' = 415 if `time_var' == 2008
	replace `mw' = 380 if `time_var' == 2007
	replace `mw' = 350 if `time_var' == 2006
	replace `mw' = 300 if `time_var' == 2005
	replace `mw' = 260 if `time_var' == 2004
	replace `mw' = 240 if `time_var' == 2003

	gen `is_mw' = (`wage_var' == `mw')
	drop if `is_mw' == 1

end 
