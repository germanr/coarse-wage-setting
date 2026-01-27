/*--------------------------------------------------------------------------
Last modification: 01/27/2026
Uses:              All do-files in this folder
Produces:          Reproduces all results for the paper

Paper:             "Coarse Wage-Setting and Behavioral Firms"
                   Reyes (Review of Economics and Statistics, 2024)

Notes:
- Run this file to reproduce all results
- Requires access to RAIS microdata (confidential)
- See README for data access instructions
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
set more off                                                                 ;
set type double                                                              ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/coarse-wage-setting";
glo cod "${root}/code"                                                       ;
glo raw "${root}/raw_data"                                                   ;
glo dat "${root}/data"                                                       ;
glo res "${root}/results"                                                    ;
set seed 20210221                                                            ;


*==========================================================================*;
*   CLEANING CODES                                                         *;
*==========================================================================*;

do "${cod}/clean/create-census-firms.do"                                     ;
do "${cod}/clean/create-firms-panel.do"                                      ;
do "${cod}/clean/create-new-hires-sample.do"                                 ;
do "${cod}/clean/estimate-bins-excess-mass.do"                               ;


*==========================================================================*;
*   FIGURES AND TABLES                                                     *;
*==========================================================================*;

do "${cod}/results/tables.do"                                                ;
do "${cod}/results/tables-appendix.do"                                       ;
do "${cod}/results/figures.do"                                               ;
do "${cod}/results/figures-appendix.do"                                      ;


exit                                                                         ;
