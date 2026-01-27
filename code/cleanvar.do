capture program drop cleanvar
*! Cleanvar v1.0.0 German Reyes 24Mar2020

* Clean variables program
program cleanvar, rclass 
syntax [anything(name=var_to_clean)]

	* From upper case to lower case accents
	replace `var_to_clean' = subinstr(`var_to_clean',"Ã","A",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Á","A",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Â","A",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ã","a",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"á","a",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"â","a",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ç","c",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ç","c",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"É","E",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ê","E",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"é","e",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ê","e",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Í","i",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"í","i",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Õ","O",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ó","O",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ô","O",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"õ","o",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ó","o",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ô","o",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ú","U",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ú","u",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"Ñ","n",.)
	replace `var_to_clean' = subinstr(`var_to_clean',"ñ","n",.)
	replace `var_to_clean' = upper(trim(itrim(`var_to_clean')))
end
