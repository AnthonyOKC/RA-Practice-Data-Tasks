// First we will want to set up our directories, explain globals, etc.
global input_dir = "input"
global intermediate_dir = "intermediate"
global output_dir = "output"

// We want a measure of how geographically concentrated each county's social
// network is. One possible measure is the share of a county's friends that are in counties within
// 50 or 100 miles. We don't quite know the number of friends each county has in another
// county, but we can estimate it if we assume a county's FB population is proportional
// to its true population. Namely: FB Connections A-B = SCI A-B * Pop A * Pop B.
// (Note: the SCI is scaled by 1,000,000,000, so we will unscale it here) 

// To do that we need a dataset that looks like:
// location1, location2, sci, distance, pop1, pop2
// Lets prepare the three datasets we will need to combine.
import delimited ${input_dir}/county_county_sci.tsv // 1 SCI (user_loc, fr_loc)
save "${intermediate_dir}/county_county_sci.dta", replace 

import delimited ${input_dir}/county_demographics.csv, clear // 2 County populations (county_fips)
keep if measure == "total_population"
rename value total_population
keep county_fips total_population
destring total_population, replace
save "${intermediate_dir}/county_populations.dta", replace

import delimited "${input_dir}/sf12010countydistancemiles.csv", clear // 3 County dist (county1, county2)
save "${intermediate_dir}/county_distances.dta", replace

// First we read in the SCI data
use "${intermediate_dir}/county_county_sci.dta", clear

// Then we merge it to the distance data (remember the column names from above)
rename user_loc county1
rename fr_loc county2
merge 1:1 county1 county2 using "${intermediate_dir}/county_distances.dta"
// One important thing: self-loops are not included in the county-county data,
// but we want to include them here. Lets make sure to include those rows.
replace mi_to_county = 0 if county1 == county2
replace _merge = 3 if county1 == county2
keep if _merge==3 
drop _merge

// And finally we merge in the county populations for BOTH sides
rename county1 county_fips
merge m:1 county_fips using "${intermediate_dir}/county_populations.dta"
keep if _merge==3 
drop _merge

rename (county_fips total_population) (county1 total_population1)

rename county2 county_fips
merge m:1 county_fips using "${intermediate_dir}/county_populations.dta"
keep if _merge==3 
drop _merge

rename (county_fips total_population) (county2 total_population2)

// Now we can finally make our estiamte of total connections!
gen est_connections = (scaled_sci * total_population1 * total_population2) / 1000000000

// Now we need to get to a data set that looks like:
// county, share_friends_within_50mi, share_friends_within_100mi

// First we need the denominator: the total number of connections from each county
bysort county1: egen total_friend_links = total(est_connections)

// Lets make a dummy of whether connection is within 50mi or 100mi
gen within_50mi = mi_to_county <= 50
gen within_100mi = mi_to_county <= 100

// And now we can make each piece of the sum we need
gen share_connections_within_50mi = (within_50mi * est_connections) / total_friend_links
gen share_connections_within_100mi = (within_100mi * est_connections) / total_friend_links

// Finally we use the Stata collapse to get to our final dataset
// (The * character here will capture all columns that start with 'share_within')
collapse (sum) share_connections_within*, by(county1)

// Lets save this data
save "${intermediate_dir}/network_concentration.dta"

// Now we will add in some other demographic data and county description
import delimited ${input_dir}/county_description.csv, clear
save "${intermediate_dir}/county_description"

import delimited ${input_dir}/county_demographics.csv, clear

// We need to reshape this data so it is wide and can be merged with other data
destring value, gen(val_) force
drop value

reshape wide val_, i(county_fips) j(measure) string

// Now merge this with the county description
merge 1:1 county_fips using "${intermediate_dir}/county_description"
drop _merge

rename county_fips county1
merge 1:1 county1 using "${intermediate_dir}/network_concentration.dta"
drop _merge

// Now lets summarize these measures
summarize share_connections_within*

// Can save these using estout -- talk about SSC again
ssc install logout

logout, save(${output_dir}/summ_dispersion.csv) excel replace: summarize share_connections_within*

// Make a histogram of the share of friends within 100 miles
histogram share_connections_within_100mi, xtitle(Share of Friends Within 100 miles) freq
graph export "${output_dir}/share_connections_100mi_histogram.pdf", replace

/// Get Washtenaw's rank and Washtenaw's rank in Michigan
egen dispersion_rank = rank(share_connections_within_100mi)
egen dispersion_rank_within_state = rank(share_connections_within_100mi), by(state_fips)

gen N_counties = _N
bysort state_fips: gen N_counties_in_state=_N

// Describe preserve/restore
preserve

	keep if county_name == "Washtenaw" & state_name == "Michigan"
	keep county_name state_name share_connections_within_100mi dispersion_rank N_counties dispersion_rank_within_state N_counties_in_state
	export delimited "${output_dir}/washtenaw_dispersion_summary.csv", replace
	
restore


// Okay now we can look at the relationship between social network concentration and mean income
// Do wealthier places have wider social networks?
scatter val_mean_hh_income share_connections_within_100mi

// Looks like possibly a negative relationship. Lets look at in binscatter.
binscatter val_mean_hh_income share_connections_within_100mi

// Lets look at the help file
help binscatter

// Lets try a quadratic fit, adding more bins. Lets also report the regression.
binscatter val_mean_hh_income share_connections_within_100mi, nquantiles(40) linetype(qfit) reportreg

// Interesting! Finally, lets clean up the axes and save
binscatter val_mean_hh_income share_connections_within_100mi, nquantiles(30) linetype(qfit) reportreg ///
ytitle(Mean Household Income) xtitle(Share of Friends Within 100 miles)
graph export "${output_dir}/within_100mi_vs_mean_income.pdf", replace


// Lets try a couple more

// social mobility
binscatter val_e_rank_b share_connections_within_100mi, nquantiles(40) linetype(qfit) reportreg ///
ytitle(Social Mobility) xtitle(Share of Friends Within 100 miles)
graph export "${output_dir}/within_100mi_vs_socmob.pdf", replace


// Life expectancy for males in the first quartile of income distribution
binscatter val_le_agg_q1_m share_connections_within_100mi, nquantiles(40) linetype(qfit) reportreg ///
ytitle(Male Life Expectancy at Q1 Income) xtitle(Share of Friends Within 100 miles)
graph export "${output_dir}/within_100mi_vs_life_expectancy.pdf", replace


// Now lets make one regression table where we look at how strong these relationships are
// with FEs for State and Commuting Zone
eststo panel1: reg val_mean_hh_income share_connections_within_100mi
eststo panel2: reg val_mean_hh_income share_connections_within_100mi, absorb(state_fips)

eststo panel3: reg val_e_rank_b share_connections_within_100mi
eststo panel4: reg val_e_rank_b share_connections_within_100mi, absorb(state_fips)

eststo panel5: reg val_le_agg_q1_m share_connections_within_100mi
eststo panel6: reg val_le_agg_q1_m share_connections_within_100mi, absorb(state_fips)

esttab using ${output_dir}/reg_output.csv, replace


