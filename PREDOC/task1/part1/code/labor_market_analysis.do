*** Labor Market Analysis for PREDOC Taks 1 Part 1
*** Febuary 5, 2022
clear all
use ./input/cps_wages_LFP.dta, clear

** Analyzing trends in LFPR. I start by keeping prime-age workers so that I have
** a consistent group of workers to compare across time. Additionally, I drop
** those in the armed forces because they aren't in the universe for the lfp
** variable (because I've restricted to prime age, I've also dropped those under
** 16 who also are niu). I then calculate the weighted overall lfpr rate before
** collapsing the data to calculate the lfpr rate for each skill/gender group.

preserve
fre empstat, all
drop if empstat == 1 // Drop those in the armed forces as they aren't in lfp variable's universe.
keep if age >= 25 & age <= 54 // Restrict population to prime-age.

** Compute overall LFP rate.
egen num = total(lfp * wtsupp), by(year)
egen den = total(wtsupp), by(year)
gen overall = 100 * (num / den)

** Collapse data to compute LFP rate for each skill/gender group in each year.
collapse (mean) lfp overall [pweight = wtsupp] , by(skilled sex year)
gen lfpr = lfp * 100

sort year
twoway line lfpr year if sex == 1 & skilled == 1, lcolor(dkgreen) lwidth(medthick) || ///
  line lfpr year if sex == 1 & skilled == 0, lcolor(dkgreen) lpattern(dash) lwidth(medthick) || ///
  line lfpr year if sex == 2 & skilled == 1, lcolor(cranberry) lwidth(medthick) || ///
  line lfpr year if sex == 2 & skilled == 0, lcolor(cranberry) lpattern(dash) lwidth(medthick) || ///
  line overall year, lwidth(thick) lcolor(black) ///
  title(U.S. LFPR by Gender and Skill-Level) ylabel(50(10)100) xlabel(1976 1986 1996 2006 2015) ///
  legend(order(1 "Skilled Male" 2 "Unskilled Male" 3 "Skilled Female" 4 "Unskilled Female" 5 "Overall"))

restore
browse
** Now I will do a similar kind of graphical analysis for wages. As before, I will
** look at prime-age workers not in the armed forces. Additionally, I drop observations
** where wage==. because the wage question is part of the Outgoing Rotation series so
** not every observation should have wage info - to calculate the correct overall average
** prior to collapsing the data, I need to drop these observations. After calculating
** these averages, I merge in BLS CPI-U data to calculate real wages in 2015 dollars.

preserve
drop if empstat == 1
keep if age >= 25 & age <= 54

drop if wage == .
egen num = total(wage * wtsupp), by(year)
egen den = total(wtsupp), by(year)
gen overall = (num / den)

collapse wage overall [pw = wtsupp], by(skilled sex year)

merge m:1 year using ./intermediate/inflation.dta, keep(match) nogen
gen cpiu_2015 = cpiu[_n] // Select last observation in CPIU variable.
gen real_wage = (wage * cpiu_2015) / cpiu
gen real_overall = (overall * cpiu_2015) / cpiu

sort year
twoway line real_wage year if sex == 1 & skilled == 1, lcolor(dkgreen) lwidth(medthick) || ///
  line real_wage year if sex == 1 & skilled == 0, lcolor(dkgreen) lpattern(dash) lwidth(medthick) || ///
  line real_wage year if sex == 2 & skilled == 1, lcolor(cranberry) lwidth(medthick) || ///
  line real_wage year if sex == 2 & skilled == 0, lcolor(cranberry) lpattern(dash) lwidth(medthick) || ///
  line overall year, lwidth(thick) lcolor(black) ///
  title(U.S. Real Wage by Gender and Skill-Level) ytitle(Real Wage) xtitle(Year) ///
  note(All observations in 2015 dollars) xlabel(1976 1986 1996 2006 2015) ///
  legend(order(1 "Skilled Male" 2 "Unskilled Male" 3 "Skilled Female" 4 "Unskilled Female" 5 "Overall"))

restore

** Part B
** Now I restrict the analysis to men over 25 and drop members of the armed forces.
keep if sex == 1
keep if age_group != 0
drop if empstat == 1

preserve

** I'll create graphs breaking down differences in LFPR based on the variables
** age_group, white, and skilled. I only include the graph on age_group in the write-up.
collapse lfp [pw = wtsupp], by(year age_group)
gen lfpr = lfp * 100

twoway line lfpr year if age_group == 1, lwidth(medthick) || ///
  line lfpr year if age_group == 2, lwidth(medthick) || ///
  line lfpr year if age_group == 3, lwidth(medthick) ///
  ylabel(0(20)100) xlabel(1976 1986 1996 2006 2015) ///
  title(U.S. LFPR by Age Group) ytitle(Participation Rate) xtitle(Year) ///
  legend(order(1 "25 to 45" 2 "45 to 65" 3 "65+"))

restore

foreach var of varlist white skilled {
  preserve
  drop if age_group == 3

  collapse lfp [pw = wtsupp], by(year `var')
  gen lfpr = lfp * 100

  twoway (line lfpr year if `var' == 0)(line lfpr year if `var' == 1), ///
    title("U.S. LFPR Over Time") ytitle("Labor Force Participation Rate") xtitle("Year") ///
    ylabel(0(20)100) xlabel(1976 1986 1996 2006 2015) ///
    legend(order(1 "Not-`var'" 2 "`var'"))

  restore
}
