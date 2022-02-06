import delim using ./input/CPIAUCSL.csv, clear
rename cpiaucsl cpiu
rename date year
replace year = substr(year, 1, 4)
destring year, replace
save ./intermediate/inflation, replace
