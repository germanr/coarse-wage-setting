
#delimit;

cap drop name 	  ;
cap drop firm_name      ;
cap drop random         ;
cap drop muni_worker    ;
cap drop zip           ;
cap drop leave_cause*   ;
cap drop day_ini_leave*  ;
cap drop day_end_leave*  ;
cap drop mth_ini_leave*  ;
cap drop mth_end_leave*  ;
cap drop sector         ;
cap drop migration_year;
cap drop n;
cap drop earn_jan earn_feb earn_mar earn_apr earn_may earn_jun earn_jul earn_aug earn_sep earn_oct earn_nov;
cap drop judicial ;
cap drop enrol_pat ;
cap drop enrol_simples;
cap drop nationality  ;
cap drop disab*      ;
cap drop sep_day      ;
cap drop sep_month    ;
cap drop leave_cause* ;
cap drop days_onleave;
cap drop cei;
cap drop has_cei;

compress;

