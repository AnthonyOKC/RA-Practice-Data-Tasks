# First we will want to load the tidyverse, a commonly used
# package for data analysis in R.

# install.packages("tidyverse") # This you only need to run once on your R setup

library(tidyverse) # This you need to run every time you use R

# First we will want to set up our directories, explain globals, etc.
# To do that we store these strings in variables
input_dir = "input"
intermediate_dir = "intermediate"
output_dir = "output"

# Read in raw data (tsv, csv, etc.)
# str_interp allows us to get the value from other string using ${}
county_county_sci = read_tsv(str_interp("${input_dir}/county_county_sci.tsv"))

# You can browse the data in Rstudio with View() View(county_county_sci)
# Another way to explore the data is with glimpse() glimpse(county_county_sci)

# Use '$' to grab a column by name
# You can call it in summary to look at a variable's values
summary(county_county_sci$scaled_sci)

# Okay now we want to add in names of counties. We will do this by merging in the county_info.
# First we will read in the county info data
county_description = read_csv(str_interp("${input_dir}/county_description.csv"))

# The tidyverse 'pipe' %>% allows us to pass the result of one function to another
# Say you want to:
# 1) take x,
# 2) use x as an input to a function f() then,
# 3) use the output of f(x) as an input to a function g() then,
# 4) use the output of g(f(x)) as an input to a function h()

# You could do this with h(g(f(x)))
# But your code will be much simpler if you instead write
# x %>% 
#   f() %>% 
#   g() %>% 
#   h()

# Here we select only the columns of data we want to keep
county_description = county_description %>% 
  select(county_fips,county_name, state_name)

# Now we want to merge by user_loc on to county_fips to get the county names for all of our user_locs
# inner_join here
county_county_sci = county_county_sci %>% 
  mutate(
    user_loc = as.numeric(user_loc),
    fr_loc = as.numeric(fr_loc)) %>%  # mutate to change the type
  inner_join(county_description, by=c("user_loc"="county_fips"))


# Okay, in this exercise we want to pick a single county to focus on.
# It can be any county that you want to look at the friendship network of!
# I'm interested in Washtenaw County, MI (but, again, feel free to chose your own!)
# I'll first try to find its fips by browsing the data
filter(county_county_sci, county_name == "Washtenaw")

# Make sure you are capitalizing the first letter and spelling correctly
# Sometimes the same county name will appear in more than one state. We can find this, by doing:
filter(county_county_sci, county_name == "Washtenaw" & state_name == "Michigan")

# Looks like the county_fips is 26161
# Lets filter to that county. Here we use 'keep' to select rows!
washtenaw_sci = filter(county_county_sci, user_loc == 26161)

# Lets also get rid of the own-county connections before summarizing
washtenaw_sci = filter(washtenaw_sci, fr_loc != 26161)

# Okay now lets summarize SCI from Washtenaw country
summary(washtenaw_sci$scaled_sci)


# Lets try our first basic plot to explore the data
# A histogram to explore the distribution of this measure of social connectedness
hist(washtenaw_sci$scaled_sci)

# It looks like there is a pretty crazy positive-skew. Lets
# make this plot a little cleaner and save it

# Open a pdf file
pdf(str_interp("${output_dir}/washtenaw_sci_histogram_R.pdf"))

# Create the plot
hist(washtenaw_sci$scaled_sci,
     xlab="SCI",
     main="SCI from Washtenaw County") 

# Close the pdf file
dev.off()

# The more popular way to make plots in R is now ggplot.
# It gives you way more flexability.
# We won't cover it in great detail today, but here is a histogram in ggplot
ggplot(washtenaw_sci, aes(x=scaled_sci)) +
  geom_histogram() +
  labs(x="SCI", title="SCI from Washtenaw County") +
  theme_bw()

ggsave(str_interp("${output_dir}/washtenaw_sci_histogram_R2.pdf"), height=4, width=4)


# One thing we can do because of the skew is then
# log-tranform the data. Lets make a new variable
# that stores the log-transformed data.
washtenaw_sci = washtenaw_sci %>% 
  mutate(log_sci = log(scaled_sci))

# Basic graph
pdf(str_interp("${output_dir}/washtenaw_log_sci_histogram_R.pdf"))
hist(washtenaw_sci$log_sci,
     xlab="SCI",
     main="SCI from Washtenaw County") 
dev.off()

# GGplot version
ggplot(washtenaw_sci, aes(x=log_sci)) +
  geom_histogram() +
  labs(x="SCI", title="SCI from Washtenaw County") +
  theme_bw()

ggsave(str_interp("${output_dir}/washtenaw_log_sci_histogram_R2.pdf"), height=4, width=4)



# Which counties are most connected to Washtenaw county?
# Lets create a new variable that is the rank.
washtenaw_sci = washtenaw_sci %>% 
  arrange(desc(scaled_sci)) %>% # sort the data
  mutate(sci_rank = 1:n()) # then here we use n(), number of rows, to make a rank

filter(washtenaw_sci, sci_rank <= 10)

# Uh oh, we don't know which counties these are though.
# We need to merge the county info now onto the friend side
# Lets rename our county info first with the prefix "user" so we can tell
# which name corresponds with which fips code
washtenaw_sci = washtenaw_sci %>% 
  rename(
    user_county_fips = user_loc,
    user_county_name = county_name,
    user_state_name = state_name)

# Now we want to merge by fr_loc on to county_fips to get the county names for all of our fr_locs
washtenaw_sci = washtenaw_sci %>% 
  inner_join(county_description, by=c("fr_loc"="county_fips"))

# Then we rename again
washtenaw_sci = washtenaw_sci %>% 
  rename(
    fr_county_fips = fr_loc,
    fr_county_name = county_name,
    fr_state_name = state_name)

# Now lets look at top 10 again
filter(washtenaw_sci, sci_rank <= 10)

# Okay, but now we want to actually save the top 10.
# In a little table (csv)
washtenaw_sci %>% 
  filter(sci_rank <= 10) %>% 
	write_csv(str_interp("${output_dir}/washtenaw_strongest_connections_R.csv"))


# In our exploratory tables, often we see that closer counties are likely to
# be the most connected areas. Lets explore this relationship more formally.
# To do this we are going to need data that has the distance between every county-county pair

# Okay now lets read in the county-county distance dataset
county_distance = read_csv(str_interp("${input_dir}/sf12010countydistancemiles.csv"))

county_distance = county_distance %>% 
  # We will want to make the counties numeric so that we can merge
  mutate(
    county1 = as.numeric(county1),
    county2 = as.numeric(county2)
  )

# And lets merge it back to our county data we made earlier
washtenaw_sci = washtenaw_sci %>% 
  inner_join(county_distance, by=c("user_county_fips"="county1",
                                   "fr_county_fips"="county2"))

# Now we can look at the closest counties
washtenaw_sci = washtenaw_sci %>% 
  arrange(mi_to_county) %>% # sort the data
  mutate(closest_rank = 1:n()) # then here we use n(), number of rows, to make a rank

filter(washtenaw_sci, closest_rank <= 10)

# Lets again look at distribution
hist(washtenaw_sci$mi_to_county)

# Not as skewed as SCI, but seems like we might want to log scale again
washtenaw_sci = washtenaw_sci %>% 
  mutate(log_distance = log(mi_to_county))

hist(washtenaw_sci$log_distance)

# Okay now we can look at the log-log relationship between SCI and distance
plot(washtenaw_sci$log_distance,
     washtenaw_sci$log_sci)

# Lets try adding a regression line
abline(lm(washtenaw_sci$log_sci ~ washtenaw_sci$log_distance), col="red") # regression line (y~x)
# Or maybe a LOWESS smooth regression line?
lines(lowess(washtenaw_sci$log_sci ~ washtenaw_sci$log_distance), col="blue")

# One increasingly popular tool used in economics is the binned scatter plot
# install.packages("binsreg")
library(binsreg)

binsreg(x=washtenaw_sci$log_distance,
        y=washtenaw_sci$log_sci,
        nbins=20)$bins_plot

# We can change the number of bins
binsreg(x=washtenaw_sci$log_distance,
        y=washtenaw_sci$log_sci,
        nbins=40, line=c(3,3), cb=c(3,3))$bins_plot


# We can also clean up the axis labels to make this look a little nicer
binsreg(x=washtenaw_sci$log_distance,
        y=washtenaw_sci$log_sci,
        nbins=40, line=c(3,3), cb=c(3,3))$bins_plot +
  labs(y = "Log of Social Connectedness Index", x = "Log of Distance (Miles)")

ggsave(str_interp("${output_dir}/washtenaw_sci_dist_binscatter_R.pdf"), height=4, width=4)

# We can also look at this in a regression framework
lm(log_sci ~ log_distance, data=washtenaw_sci)

# One thing we might have noticed is that some of the closest places are in Ohio,
# Lets try to control.
washtenaw_sci = washtenaw_sci %>% 
  mutate(same_state_dummy = user_state_name == fr_state_name)

lm(log_sci ~ log_distance + same_state_dummy, data=washtenaw_sci)


# In the next lesson, we'll talk about how to create the types of regression tables
# that are commonly used in economics papers

# Now lets make a map! This probably won't actually be necessary for a coding challenge,
# but shows how you can quickly do many more cool things by Googling around for packages.


# We are going to use a package created by the Urban Institute
# name urbnmapr. You can find more info here: https://github.com/UrbanInstitute/urbnmapr
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

counties_sf = get_urbn_map(map = "counties", sf = TRUE)

map_dat = counties_sf %>% 
  mutate(county_fips = as.numeric(county_fips)) %>% 
  inner_join(washtenaw_sci, by=c("county_fips"="fr_county_fips"))


ggplot(map_dat) +
  geom_sf(aes(fill=log_sci), size=0.01, color = "#ffffff") +
  theme_void()


# Spruce it up a bit with R color brewer
# install.packages("RColorBrewer")
library(RColorBrewer)

ggplot(map_dat) +
  geom_sf(aes(fill=log_sci), size=0, color = "#ffffff") +
  theme_void() +
  scale_fill_distiller(palette = "Blues", direction=2) +
  labs(fill = "Log(SCI)")

ggsave(str_interp("${output_dir}/washtenaw_sci_map_R.pdf"), height=6, width=8)



