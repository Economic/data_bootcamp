* Data Bootcamp 2022: EARNcon Cleveland
* Zane Mokhiber
* 9/28/2022

*This do file uses EPI written stata packages: install them with the following net install commands
*load_epiextracts: tool to load EPI CPS microdata
* net install load_epiextracts, from("https://microdata.epi.org/stata")
*binipolate: tool for creating smoothed percentiles
* net install binipolate, from("https://raw.githubusercontent.com/Economic/binipolate/master/")


*Best practice, set working directory to your project folder: ex, cd cd	"C:\Users\zmokhiber\Documents\data_bootcamp_2022\"

*Part 1: loading BLS data into stata
*import the excel data specifying the sheet, converting row 1 to variable names, and clearing data in memory
*example 1
import excel bls_laus_data.xlsx, sheet("untidy") firstrow clear

*check to make sure data is in tidy format (is data already in the format you want? if not reshape it)
rename * unemp*
rename unempSeriesID state
reshape long unemp, i(state) j(date) string


*example 2
import excel bls_laus_data.xlsx, sheet("tidy") firstrow clear

*example 3, download all state LAUS data 
*get series codes we want
import delim using https://download.bls.gov/pub/time.series/la/la.series, clear
*keep statewide measures, unemployment rate, and seasonally adjusted data
keep if area_type_code == "A"
keep if measure_code == 3
keep if seasonal == "S"

*get rid of redundant information
keep series_id series_title
tempfile series_ids
save `series_ids'

*get all LAUS statewide data
import delim using https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS, clear
*just keep time period we are interested in
keep if year >= 2019
*merge with series_ids we want and drop unmatched rows
merge m:1 series_id using `series_ids'
keep if _merge == 3

*create state name from series_title 
gen state_name = substr(series_title, 19,.)
replace state_name = subinstr(state_name, " (S)", "",.)

*create new month variable, redundant M in period variable (ie M01 to 1)
gen month = subinstr(period, "M", "",.)
destring month, replace

*extract state fips code info from series_id
gen statefips = substr(series_id, 6, 2)
destring statefips, replace
destring value, replace
*only keep wanted variables
keep year month value state_name statefips
*give value a descriptive name
rename value laus_unemp

*can now keep whatever states we want to use
keep if statefips == 39 
tempfile laus_data
save `laus_data'
*****************
*Part 2: using the CPS data to look at demographic information
*****************

*learn how to use EPI CPS data at https://microdata.epi.org/basicuse/
*save extracts in a dedicated folder on your laptop (ex, on mine it's "C:\data\cps\basic")
load_epiextracts, begin(2018m1) end(2022m8) sample(basic) keep(unemp emp wbhao statefips lfstat female age) sourcedir("C:\data\cps\basic") 

*keep states we want to look at
label list statefips 
*39 is ohio
keep if statefips == 39

*labor force and age restrictions
keep if age >= 16
label li lfstat
drop if lfstat == 3
tempfile cps_data
save `cps_data'

*creating table output 
*create table of unemployment rates by quarter and state
collapse (mean) unemp [pw=basicwgt], by(statefips year month)
*save overall unemp for later
tempfile ohio_unemp
save `ohio_unemp'


* by state and race, add epop
use `cps_data', clear
collapse (mean) unemp emp [pw=basicwgt], by(statefips wbhao year month)


*other collapse examples because i think it's useful 
*by state and race
use `cps_data', clear
collapse (mean) unemp [pw=basicwgt], by(statefips wbhao year month)
*draw a quick line graph to look at the volitility of the data

gen monthdate = ym(year, month)
format monthdate %tm

line unemp monthdate if wbhao == 1
line unemp monthdate, by(wbhao)


*sample sizes
use `cps_data', clear
gen sample = 1
collapse (sum) sample, by(statefips wbhao year month)
*get info about average sample sizes
bysort wbhao: sum sample

*if we just want a single number with pooled data, we can do that with however many months are needed
use `cps_data', clear
drop if year < 2021
drop if year == 2021 & month <= 8
*confirm i have the correct time period
tab month year
*create a sample variable to sum up 
gen sample = 1/basicwgt
collapse (mean) unemp (sum) sample [pw=basicwgt], by(wbhao)

*another route is to create moving averages of the data
use `cps_data', clear
keep if statefips == 39
*create a sample variable to sum up 
gen sample = 1/basicwgt
collapse (mean) unemp (sum) sample [pw=basicwgt], by( wbhao year month)

*create monthdate variable and set data up for time series analysis 
gen monthdate = ym(year, month)
*format to human readable months (ie. 2022m1)
format monthdate %tm

*set up the data as time_series data
tsset wbhao monthdate
tssmooth ma avgunemp = unemp, window(11,1)
drop if wbhao == 5
line avgunemp monthdate, by(wbhao)
tempfile ma_race
save `ma_race'

*EPI quarterly unemployment data route
use `ohio_unemp', clear
*tsset
gen monthdate = ym(year, month)
format monthdate %tm
tsset monthdate
tssmooth ma avg_overall_unemp = unemp, window(11,1)
*merge onto moving average unemployment data by race to create ratio of overall to each race category
merge 1:m monthdate using `ma_race'
drop _merge
gen cps_ratio = avgunemp / avg_overall_unemp
*drop before dec 2018 (because prior there is not enough data to have 12 full months averages)
drop if monthdate <=tm(2018m12)

*keep only relevant variables
keep year month cps_ratio wbhao
*merge data onto laus data
merge m:1 year month using `laus_data'
drop _merge
destring laus_unemp, replace
*apply ratio to laus data
gen constructed_unemp = cps_ratio * laus_unemp

* draw a line
gen monthdate = ym(year, month)
*format to human readable months (ie. 2022m1)
format monthdate %tm
line constructed_unemp monthdate, by(wbhao)

*reshape table for output to excel
keep monthdate laus_unemp constructed_unemp wbhao
reshape wide constructed_unemp, i(monthdate) j(wbhao)

rename (constructed_unemp1 constructed_unemp2 constructed_unemp3 constructed_unemp4) (white black hispanic asian)
export excel ohio.xlsx, firstrow(var) sheet("unemployment") replace

*****************
*Part 3: median wages and binipolate
*****************
*problem: wages in CPS data are "clumpy"
*full methodology here: https://www.epi.org/data/methodology/

*load the EPI extracts, but this time get wage data
load_epiextracts, begin(2018m1) end(2022m8) sample(basic) keep(unemp emp wbhao statefips lfstat female age wage weekpay minsamp) sourcedir("C:\data\cps\basic")
*ohio again
keep if statefips == 39
* just look at outgoing rotation group
keep if minsamp == 4 | minsamp == 8
tempfile org_data
save `org_data'

* look at clumping
use `org_data', clear
bysort year: sum wage, d

* binipolate solves this issues
binipolate wage [pw=orgwgt], binsize(.25) p(1 5 10 25 50 75 90 95 99) by(year)
li if percentile == 10

*compare pre-pandemic 12 month period to latest 12 months of data
use `org_data', clear

gen monthdate = ym(year, month)
*format to human readable months (ie. 2022m1)
format monthdate %tm
 
*generate variable for time periods
gen period = 1 if monthdate >= tm(2018m9) & monthdate <= tm(2019m8)
replace period = 2 if monthdate >= tm(2021m9) & monthdate <= tm(2022m8)

*use preserve and restore to bring the full dataset back in memory 
preserve
*get median wage for all wage earners in ohio
binipolate wage [pw=orgwgt], binsize(.25) p(50) by(period)
tempfile overall
save `overall'
restore

*median wages by race/ethnicity
binipolate wage [pw=orgwgt], binsize(.25) p(50) by(period wbhao)
append using `overall'
*remove other (sample size concerns)
drop if wbhao == 5

*reshape for output
replace wbhao = 0 if wbhao == . 
reshape wide wage_binned, i(wbhao) j(period)
rename (wage_binned1 wage_binned2) (p2019m8 p2022m8)

*calculate percent growth from period 1 to period 2
gen nom_wage_growth = (p2022m8 / p2019m8) - 1

*export to same workbook as before
export excel using ohio.xlsx, firstrow(var) sheet("med_wages") sheetmodify
