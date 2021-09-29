* File: earn_data_bootcamp.do
* Desc: compare wages by race and sex in Ohio using the ACS
* Auth: Zane Mokhiber

set more off
clear all

* Analysis with one year of data
* load 2020 CPS ORG
use epi_cpsorg_2020, clear
* Create indicator variable for Ohio
generate oh = 0
replace oh = 1 if statefip == 39
* age restriction
keep if age >= 16
* Ohio only
keep if oh == 1
* calculate avg wages by race
collapse (mean) wage [aw=orgwgt], by(wbho female)
reshape wide wage, i(female) j(wbho)
* rename reshaped variables
rename wage1 white
rename wage2 black
rename wage3 hispanic
rename wage4 other

* write data to an excel file
export excel using ohio_wages.xlsx, ///
  replace firstrow(variables)


*Analysis with multiple years of data

/* 
you can unzip the EPI data files with stata if needed
it will unzip to your working directory. ex:
*cd C:\data\cps\
unzipfile "C:\<PATH TO FILE>\epi_cpsorg_1979_2020.zip", replace
*cd C:\<YOUR WORKING DIRECTORY>
*/

*create macro for the data directory
local datadir C:\data\cps\
use `datadir'epi_cpsorg_2020.dta, clear

* check sample sizes by race and sex
tab wbho female if statefip == 39

* CPI-U-RS from https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm.
import excel using r-cpi-u-rs-alllessfe.xlsx, cellrange(A6:N49) case(lower) firstrow clear

keep year avg
rename avg cpiurs
keep if cpiurs ~= .
save cpiurs.dta, replace

* Load 2018-2020 CPS ORG
use `datadir'epi_cpsorg_2018.dta, clear
append using `datadir'epi_cpsorg_2019.dta
append using `datadir'epi_cpsorg_2020.dta

merge m:1 year using cpiurs.dta
display r(mean)

* inflation adjust wages
sum cpiurs if year == 2020
replace wage = wage * (r(mean) / cpiurs)

*calculate avg wages by race
collapse (mean) wage [aw=orgwgt], by(wbho female)
reshape wide wage, i(female) j(wbho)

* rename reshaped variables
rename wage1 white
rename wage2 black
rename wage3 hispanic
rename wage4 other

export excel using ohio_wages_pooled_years.xlsx, ///
  replace firstrow(variables)

  
* loop example
* load one year of data
use `datadir'epi_cpsorg_2011.dta,clear
* append years 2012-2020
forvalues year = 2011/2020{
    append using `datadir'epi_cpsorg_`year'.dta
}
* display years now available in memory
tab year
