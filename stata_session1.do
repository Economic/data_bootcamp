set more off
clear all

*load 2020 CPS ORG
use epi_cpsorg_2020, clear
*Create indicator variable for Ohio
generate oh = 0
replace oh = 1 if statefip == 39
* age restriction
keep if age >= 16
* Ohio only
keep if oh == 1
* missing values
sum wage
count if wage == .

*race variable
tab wbho

* weights
sum wage [w=orgwgt]
sum wage [w=orgwgt] if wbho == 1
sum wage [w=orgwgt] if wbho == 2
sum wage [w=orgwgt] if wbho == 1 & female == 1

* by race and gender: 
bysort wbho female: sum wage [w=orgwgt]

*calculate avg wages by race
collapse (mean) wage [aw=orgwgt], by(wbho)

*export the data
export excel using ohio_wages.xlsx, replace firstrow(variables)