// First we will want to set up our directories, explain globals, etc.
global input_dir = "input"
global intermediate_dir = "intermediate"
global output_dir = "output"

/// Read in raw data (tsv, csv, etc.)
import delimited ${input_dir}/county_county_sci.tsv

// Use 'br' to browse data

// Use summarize to look at a variable's values
summarize scaled_sci

// Okay now we want to add in names of counties. We will do this by merging in the county_info.
// But we need to save the dataset we just imported as a dta file.
save "${intermediate_dir}/county_county_sci.dta", replace // replace here will replace any old versions that exist

// Now we will read in the county info data
import delimited ${input_dir}/county_description.csv, clear // clear here will remove the first dataset from memory

// Use 'br' again to browse the data
// We see we have county_fips (the code we need).
// Lets keep county_fips, county_name, state_name
keep county_fips county_name state_name

// Okay lets save this now
save "${intermediate_dir}/county_names.dta", replace

// Now lets go back to our first dataset. Now we are reading a Stata dta file,
// we utilize the 'use' command
use "${intermediate_dir}/county_county_sci.dta", clear

// Now we want to merge by user_loc on to county_fips to get the county names for all of our user_locs
rename user_loc county_fips // need to rename
merge m:1 county_fips using "${intermediate_dir}/county_names.dta"

// We can see which observations we are dropping by doing br if _merge != 3
// Here we won't worry about these counties.
keep if _merge==3 
drop _merge

// I'll first try to find its fips by browsing the data
br if county_name == "Washtenaw"
// Make sure you are capitalizing the first letter and spelling correctly
// Sometimes the same county name will appear in more than one state. We can find this, by doing:
br if county_name == "Washtenaw" & state_name == "Michigan" 

// Looks like the county_fips is 26161
// Lets filter to that county. Here we use 'keep' to select rows!
keep if county_fips == 26161

// Lets also get rid of the own-county connections before summarizing
drop if fr_loc == 26161

// Okay now lets summarize SCI from Washtenaw country
summarize scaled_sci
summarize scaled_sci, detail // or for more detail

// Lets try our first basic plot to explore the data
// A histogram to explore the distribution of this measure of social connectedness
histogram scaled_sci, freq
// It looks like there is a pretty crazy positive-skew. Lets
// make this plot a little cleaner and save it
histogram scaled_sci, freq xtitle(SCI)

graph export "${output_dir}/washtenaw_sci_histogram.pdf", replace

// One thing we can do because of the skew is then
// log-tranform the data. Lets make a new variable
// that stores the log-transformed data.
gen log_sci = log(scaled_sci)
// Now lets try the histogram again
histogram log_sci, freq xtitle(log(SCI))
graph export "${output_dir}/washtenaw_log_sci_histogram.pdf", replace
// Okay nice! There is still a bit of a right tail, but the data become much
// more reasonable to work with.


// Which counties are most connected to Washtenaw county?
// Lets create a new variable that is the rank.
// The negative sign makes this descending
egen sci_rank = rank(-scaled_sci)
sort sci_rank // will sort the data
br if sci_rank <= 10

// Uh oh, we don't know which counties these are though.
// We need to merge the county info now onto the friend side
// Lets rename our county info first with the prefix "user" so we can tell
// which name corresponds with which fips code
rename county_fips user_county_fips
rename county_name user_county_name
rename state_name user_state_name

// Now we want to merge by fr_loc on to county_fips to get the county names for all of our fr_locs
rename fr_loc county_fips // need to rename
merge m:1 county_fips using "${intermediate_dir}/county_names.dta"
keep if _merge==3 
drop _merge

// We can also use rename to bulk rename
rename (county_fips county_name state_name) (fr_county_fips fr_county_name fr_state_name)

// Now lets look at top 10 again
sort sci_rank
br if sci_rank <= 10

// Okay, but now we want to actually save the top 10.
// We can do that using preserve and restore. This way
// we can temporarily drop data then restore it later.
preserve

	keep if sci_rank <= 10
	export delimited "${output_dir}/washtenaw_strongest_connections.csv", replace

restore

// In our exploratory tables, often we see that closer counties are likely to
// be the most connected areas. Lets explore this relationship more formally.
// To do this we are going to need data that has the distance between every county-county pair
// Before opening this data to look at it, lets save our new data set
save "${intermediate_dir}/washtenaw_county_sci_cleaned", replace

// Okay now lets read in the county-county distance dataset
import delimited "${input_dir}/sf12010countydistancemiles.csv", clear

// And lets merge it back to our county data we made earlier
rename county1 user_county_fips
rename county2 fr_county_fips
merge 1:1 user_county_fips fr_county_fips using "${intermediate_dir}/washtenaw_county_sci_cleaned.dta"
keep if _merge==3 
drop _merge

// Now we can look at the closest counties
egen closest_rank = rank(mi_to_county)
sort closest_rank // will sort the data
br if closest_rank <= 10

// Lets again look at distribution
histogram mi_to_county, freq
// Not as skewed as SCI, but seems like we might want to log scale again
gen log_distance = log(mi_to_county)
histogram log_distance, freq

// Okay now we can look at the log-log relationship between SCI and distance
scatter log_sci log_distance 
// Lets try adding a regression line
graph twoway (scatter log_sci log_distance ) (lfit log_sci log_distance )
// Or maybe a quadratic fit?
graph twoway (scatter log_sci log_distance ) (qfit log_sci log_distance )

// One increasingly popular tool used in economics is binscatter
ssc install binscatter
binscatter log_sci log_distance
// Lets look at some of the options we can do to clean this plot up:
help binscatter
// We can change the number of bins and make a quadratic fit
binscatter log_sci log_distance, nquantiles(40) linetype(qfit)
// We can also clean up the axis labels to make this look a little nicer
binscatter log_sci log_distance, nquantiles(20) linetype(qfit) ///
ytitle(Log of Social Connectedness Index) xtitle(Log of Distance (Miles))
graph export "${output_dir}/washtenaw_sci_dist_binscatter.pdf", replace


// We can also look at this in a regression framework
reg log_sci log_distance

// One thing we might have noticed is that some of the closest places are in Ohio,
// Lets try to control.
gen same_state_dummy = user_state_name == fr_state_name
reg log_sci log_distance same_state_dummy


// In the next lesson, we'll talk about how to create the types of regression tables
// that are commonly used in economics papers

/// Now lets make a map! This probably won't actually be necessary for a coding challenge,
// but shows how you can quickly do many more cool things by Googling around for packages.


// We are going to use maptile, a package from Michael Stepner.
// You can find more info here: https://michaelstepner.com/maptile/
ssc install maptile
ssc install spmap

// This will give us lots of good info
help maptile
// For example, it will tell us that we can find a list of geographies here:
// https://michaelstepner.com/maptile/geographies/

// We need to use the 2010 counties
maptile_install using "http://files.michaelstepner.com/geo_county2010.zip"

// If this doesn't work for some reason, the 2010 counties should be in your 
// current working directory as (input/geo_county2010), because we downloaded it already.
// Just add the option geofolder(${input_dir}/geo_county2010) at the end of the maptile command

// We just need to rename fr_county -> county and we'll be good to go!
rename fr_county_fips county
maptile log_sci, geo(county2010)

// Some asethetic tweaks we can learn about from helpfile examples:
// Add more bins, make colors proportional to difference, make it blues
maptile log_sci, geo(county2010) nq(9) rangecolor(white blue*1.2) propcolor ///
twopt(legend(title("Log(SCI)")))

graph export "${output_dir}/washtenaw_sci_map.pdf", replace



