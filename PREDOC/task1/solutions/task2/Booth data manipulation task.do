*** Medicaire Advantage Task for Chicago Booth
*** November 7, 2019
clear all
cd Medicare_Advantage

** First I will import the csv file, ensure that the data for eligibles, enrolless
** and penetartion is stored in numeric variables, and then I will add the correct
** variable names. Once I've done that, I change missing values for eligibles, 
** enrollees, and penetration to zeros, clean up the countyssa data, and drop
** overseas territories and other unnecessary observations (e.g. observations 
** where the state variables is "Unusual SCounty Code") from the data set

import delimited scp-1205, clear numericcols(7/9)
rename v# (countyname state contract healthplanname typeofplan countyssa eligibles enrollees penetration ABrate)

foreach var of varlist eligibles enrollees penetration {
replace `var'=0 if `var'==.
}

destring countyssa, replace force
drop if countyssa==.
drop if state=="AS "|state=="PR "|state=="VI "


** Now that I have done basic cleaning, I will start generating the variables of 
** interest. To calculate the number of health plans by county with more than 10
** enrollees, I will create an indicator variable for each plan that is equal to 
** 1 if the plan has more than ten enrollees, and then I will sum up the number 
** of plans that  meet this threshold across counties. I follow the same procedure
** to calculate the number of health plans with penetration > 0.5. 

gen ten_enrollees=(enrollees>10)
bysort countyssa: egen numberofplans1=sum(ten)

gen min_penetration=(penetration>0.5)
bysort countyssa: egen numberofplans2=sum(min_penetration)


** Finally, I will calculate the total number of enrollees in the county with a
** MA health plan by summing up the number of enrollees in each plan in each 
** county, and I will calculate total penetration by diving this number by
** the number of people eligible for Medicaid. I will then drop duplicates 
** and unnecessary variables to generate the final dataset!
**
** Note: there is one county with a totalpenetartion value > 100. It seems there
** is some error with the provided data since the total number of enrollees in 
** the county is > the number of eligible individuals (and I doubt people are 
** signing up for two plans). Additionally, there are a couple of places where 
** the total penetration value is missing because the number of MA eligible 
** individuals is zero. This seems not entirely unplausible in small Alaskan counties, 
** but there does seem to be some error with the "Manassas Park City" county since
** there are individuals enrolled in Medicaid but apparently nobody is eligible. 
** Anyway, I've left the values as they are, but figured I should note the discrepancies.

bysort countyssa: egen totalenrollees = sum(enrollees)
bysort countyssa: gen totalpenetration=100*(totalenrollees/eligibles)


drop contract healthplanname typeofplan enrollees penetration ABrate ten_enrollees min_penetration 
duplicates drop

sort state countyname
save medicare_advantage, replace
