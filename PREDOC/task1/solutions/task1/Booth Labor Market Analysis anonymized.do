*** Labor Market Analysis for Chicago Booth
*** November 12, 2019
clear all
use cps_wages_LFP.dta, clear


** Analyzing trends in LFPR. I start by keeping prime-age workers so that I have
** a consistent group of workers to compare across time. Additionally, I drop 
** those in the armed forces because they aren't in the universe for the lfp 
** variable (because I've restricted to prime age, I've also dropped those under
** 16 who also are niu). I then calculate the weighted overall lfpr rate before
** collapsing the data to calculate the lfpr rate for each skill/gender group.

preserve
drop if empstat==1
keep if age>=25 & age<=54

egen num=total(lfp*wtsupp), by(year)
egen den=total(wtsupp), by(year)
gen overall=100*(num/den)

collapse lfp overall, by(skilled sex year)
gen lfpr=lfp * 100

sort year
twoway line lfpr year if sex==1 & skilled==1, lcolor(dkgreen) lwidth(medthick) || ///
	line lfpr year if sex==1 & skilled==0, lcolor(dkgreen) lpattern(dash) lwidth(medthick) || ///
	line lfpr year if sex==2 & skilled==1, lcolor(cranberry) lwidth(medthick) || ///
	line lfpr year if sex==2 & skilled==0, lcolor(cranberry) lpattern(dash) lwidth(medthick) || ///
	line overall year, clwidth(thick) clcolor(black) ylabel(50(10)100) title(LFPR by Gender and Skill-Level) ///
	xlabel(1976 1986 1996 2006 2015) ytitle(Participation Rate) xtitle(Year) ///
	legend(order(1 "Skilled Male" 2 "Unskilled Male" 3 "Skilled Female" 4 "Unskilled Female" 5 "Overall"))

restore

** Now I will do a similar kind of graphical analysis for wages. As before, I will
** look at prime-age workers not in the armed forces. Additionally, I drop observations
** where wage==. because the wage question is part of the Outgoing Rotation series so 
** not every observation should have wage info - to calculate the correct overall average
** prior to collapsing the data, I need to drop these observations. After calculating
** these averages, I merge in BLS CPI-U data to calculate real wages in 2015 dollars. 


preserve
drop if empstat==1
keep if age>=25 & age<=54

drop if wage==.
egen num=total(wage*wtsupp), by(year)
egen den=total(wtsupp), by(year)
gen overall=(num/den)

collapse wage overall, by(skilled sex year)

merge m:1 year using inflation.dta, nogen
gen real_wage=(wage*237.017)/cpiu
gen real_overall=(overall*237.017)/cpiu

sort year
twoway line real_wage year if sex==1 & skilled==1, lcolor(dkgreen) lwidth(medthick) || ///
	line real_wage year if sex==1 & skilled==0, lcolor(dkgreen) lpattern(dash) lwidth(medthick) || ///
	line real_wage year if sex==2 & skilled==1, lcolor(cranberry) lwidth(medthick) || ///
	line real_wage year if sex==2 & skilled==0, lcolor(cranberry) lpattern(dash) lwidth(medthick) || ///
	line real_overall year, clwidth(thick) clcolor(black) title(Real Wage by Gender and Skill-Level) ///
	xlabel(1976 1986 1996 2006 2015) ytitle(Real Wage) xtitle(Year) note(All observations in 2015 dollars) ///
	legend(order(1 "Skilled Male" 2 "Unkilled Male" 3 "Skilled Female" 4 "Unskilled Female" 5 "Overall"))

restore
	
	
** Part B 
** Now I restrict the analysis to men over 25 and drop members of the armed forces. 
** I'll create graphs breaking down differences in LFPR based on the variables 
** age_group, white, and skilled. I only include the graph on age_group in the write-up.

keep if sex==1
keep if age_group!=0
drop if empstat==1 

preserve

collapse lfp [pw=wtsupp], by(year age_group)
gen lfpr = lfp *100

twoway line lfpr year if age_group==1, lwidth(medthick) || ///
	line lfpr year if age_group ==2, lwidth(medthick) || ///
	line lfpr year if age_group ==3, lwidth(medthick) ///
	ylabel(0(20)100) xlabel(1976 1986 1996 2006 2015) ///
	ytitle(Participation Rate) xtitle(Year) title(LFPR by Age Group) ///
	legend(order(1 "25 to 45" 2 "45 to 65" 3 "65+")) 

restore


foreach var of varlist white skilled {
preserve
drop if age_group==3

collapse lfp [pw=wtsupp], by(year `var')
gen lfpr = lfp*100

twoway (line lfpr year if `var'==0)(line lfpr year if `var'==1), ///
	ytitle("Labor Force Participation Rate") xtitle("Year") ///
	legend(order(1 "Not-`var'" 2 "`var'"))

restore
}



