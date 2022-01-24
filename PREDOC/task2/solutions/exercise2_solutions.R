# Again want to load the tidyverse, a commonly used
# package for data analysis in R.

# If you ran this last time, you don't need to run it again
# install.packages("tidyverse") # This you only need to run once on your R setup

library(tidyverse) # This you need to run every time you use R

# First we will want to set up our directories, explain variables, etc.
# To do that we store these strings in variables
input_dir = "input"
intermediate_dir = "intermediate"
output_dir = "output"


# We want a measure of how geographically concentrated each county's social
# network is. One possible measure is the share of a county's friends that are in counties within
# 50 or 100 miles. We don't quite know the number of friends each county has in another
# county, but we can estimate it if we assume a county's FB population is proportional
# to its true population. Namely: FB Connections A-B = SCI A-B * Pop A * Pop B.
# (Note: the SCI is scaled by 1,000,000,000, so we will un-scale it here) 

# To do that we need a dataset that looks like:
# location1, location2, sci, distance, pop1, pop2
# Lets prepare the three datasets we will need to combine.

# Again, str_interp allows us to get the value from other string using ${}

# Data set 1 = SCI
county_county_sci = read_tsv(str_interp("${input_dir}/county_county_sci.tsv"))

# Data set 2 = Population
county_demographics = read_csv(str_interp("${input_dir}/county_demographics.csv"))
county_populations = county_demographics %>% 
  filter(measure == "total_population") %>% # filter to only the rows where where measure == "total_population"
  select(county_fips, total_population=value) # select only a couple columns

# Data set 3 = Distance between counties
county_distances = read_csv(str_interp("${input_dir}/sf12010countydistancemiles.csv"))

# Now lets join the data sets together
# One important thing: self-loops are not included in the county-county data,
# but we want to include them here. Lets make sure to include those rows.
sci_and_distance = county_county_sci %>% 
  left_join(county_distances, by=c("user_loc"="county1","fr_loc"="county2")) %>%
  rename(county1=user_loc, county2=fr_loc) %>% 
  # Add in the 0 distance if the user_loc and fr_loc are the same
  mutate(mi_to_county = if_else(county1 == county2, 0, mi_to_county)) %>% 
  # For this exercise, we won't worry about other missing pairs between the two data sets
  filter(!is.na(mi_to_county))

# Now we add in the populations (on both the user and fr side)
full_dat = sci_and_distance %>% 
  # We need to make this numeric so we can merge to county_populations
  mutate(
    county1 = as.numeric(county1),
    county2 = as.numeric(county2)) %>% 
  inner_join(county_populations, by=c("county1"="county_fips")) %>% 
  rename(total_population1 = total_population) %>% 
  inner_join(county_populations, by=c("county2"="county_fips")) %>% 
  rename(total_population2 = total_population)

# Now we can finally make our estimate of total connections!
# Here we rescale by 1,000,000,000
full_dat = full_dat %>% 
  mutate(est_connections = (scaled_sci * total_population1 * total_population2) / 1000000000)

# Now we need to get to a data set that looks like:
# county, share_friends_within_50mi, share_friends_within_100mi

# First we need the denominator: the total number of connections FROM each county
# To do this we need to group_by, sum within the group, then ungroup
full_dat = full_dat %>% 
  group_by(county1) %>% 
  mutate(total_friend_links = sum(est_connections)) %>% 
  ungroup()

# Now lets make a dummy of whether connection is within 50mi or 100mi
full_dat = full_dat %>% 
  mutate(
    within_50mi = mi_to_county <= 50,
    within_100mi = mi_to_county <= 100)

# And now we can make each piece of the sum we need
full_dat = full_dat %>% 
  mutate(
    share_connections_within_50mi = (within_50mi * est_connections) / total_friend_links,
    share_connections_within_100mi = (within_100mi * est_connections) / total_friend_links)

# Finally we use group_by and summarise to collapse to our final data set
# (The * character here will capture all columns that start with 'share_within')
network_concentration = full_dat %>% 
  group_by(county1) %>% 
  summarise(
    share_connections_within_50mi = sum(share_connections_within_50mi),
    share_connections_within_100mi = sum(share_connections_within_100mi) ) %>% 
  ungroup()


# Now we will add in some other demographic data and county description
# We need to reshape this data so it is wide and can be merged with other data
county_demographics_wide = county_demographics %>% 
  pivot_wider(names_from=c("measure"), values_from="value")

# Now read in the county description and merge it all together
county_description = read_csv(str_interp("${input_dir}/county_description.csv"))

network_concentration = county_demographics_wide %>% 
  inner_join(county_description, by=c("county_fips")) %>% 
  inner_join(network_concentration, by=c("county_fips"="county1"))

# Now lets summarize these measures
network_concentration %>% 
  select(share_connections_within_50mi, share_connections_within_100mi) %>% 
  summary()

# We can make this a new table using group_by and summarise, then save to a csv
network_concentration_summary = network_concentration %>% 
  select(county_fips, share_connections_within_50mi, share_connections_within_100mi) %>% 
  # First pivot longer (this will allow us to make rows for each measure)
  pivot_longer(
    cols=c("share_connections_within_50mi", "share_connections_within_100mi")
  ) %>% 
  # Then group_by name (which is each measure)
  group_by(name) %>% 
  # Add summary stats in here
  summarise(
    N = n(),
    Mean = mean(value),
    `Std. Dev.` = sd(value),
    Min = min(value),
    Max = max(value),
    `25th Ptile` = quantile(value, .25),
    Median = quantile(value, .5),
    `75th Ptile` = quantile(value, .75)
  ) %>% 
  ungroup() %>% 
  # Arrange in reverse order (because R wants to put the 5 in 
  # 'share_connections_within_50mi' before the 1 in 'share_connections_within_100mi')
  arrange(desc(name))
  
# Write to csv
write_csv(network_concentration_summary,
          str_interp("${output_dir}/summ_dispersion_R.csv"))
  


# Make a histogram of the share of friends within 100 miles

# Open a pdf file
pdf(str_interp("${output_dir}/share_connections_100mi_histogram_R.pdf"))

# Create the plot
hist(network_concentration$share_connections_within_100mi,
     xlab="Share of Friends Within 100 Miles",
     main="Network Concentration Across U.S. Counties")

# Close the pdf file
dev.off()

# The more popular way to make plots in R is now ggplot.
# It gives you way more flexibility.
# We won't cover it in great detail today, but here is a histogram in ggplot
ggplot(network_concentration, aes(x=share_connections_within_100mi)) +
  geom_histogram() +
  labs(x="Share of Friends Within 100 Miles", title="Network Concentration Across U.S. Counties") +
  theme_bw()

ggsave(str_interp("${output_dir}/share_connections_100mi_histogram_R2.pdf"), height=4, width=4)


# Get Washtenaw's rank and Washtenaw's rank in Michigan
network_concentration = network_concentration %>% 
  mutate(
    dispersion_rank = rank(share_connections_within_100mi),
    N_counties = n()) %>% 
  group_by(state_fips) %>% 
  mutate(
    dispersion_rank_within_state = rank(share_connections_within_100mi),
    N_counties_in_state = n()) %>% 
  ungroup()

# The look at just Washtenaw County
washtenaw_network_concentration = network_concentration %>% 
  filter(county_name == "Washtenaw" & state_name == "Michigan") %>% 
  select(county_name, state_name, share_connections_within_100mi,
         dispersion_rank, N_counties,
         dispersion_rank_within_state, N_counties_in_state)

write_csv(washtenaw_network_concentration,
          str_interp("${output_dir}/washtenaw_dispersion_summary_R.csv"))


# Okay now we can look at the relationship between social network concentration and mean income
# Do wealthier places have wider social networks?
plot(network_concentration$share_connections_within_100mi,
     network_concentration$mean_hh_income)


# Looks like possibly a negative relationship. Lets look at in binscatter.

# Again, this is one increasingly popular tool used in economics is the binned scatter plot
# install.packages("binsreg")
library(binsreg)

binsreg(x=network_concentration$share_connections_within_100mi,
        y=network_concentration$mean_hh_income,
        nbins=20)$bins_plot

# Interesting!
# Lets add more bins, a line and confidence interval, and axis labels
binsreg(x=network_concentration$share_connections_within_100mi,
        y=network_concentration$mean_hh_income,
        nbins=40, line=c(3,3), cb=c(3,3))$bins_plot +
  labs(y = "Mean Household Income", x = "Share of Friends Within 100 miles")

ggsave(str_interp("${output_dir}/within_100mi_vs_mean_income_R.pdf"), height=4, width=4)




# Lets try a couple more:

# social mobility
binsreg(x=network_concentration$share_connections_within_100mi,
        y=network_concentration$e_rank_b,
        nbins=40, line=c(3,3), cb=c(3,3))$bins_plot +
  labs(y = "Social Mobility", x = "Share of Friends Within 100 miles")

ggsave(str_interp("${output_dir}/within_100mi_vs_socmob_R.pdf"), height=4, width=4)


# Life expectancy for males in the first quartile of income distribution
binsreg(x=network_concentration$share_connections_within_100mi,
        y=network_concentration$le_agg_q1_m,
        nbins=40, line=c(3,3), cb=c(3,3))$bins_plot +
  labs(y = "Male Life Expectancy at Q1 Income", x = "Share of Friends Within 100 miles")

ggsave(str_interp("${output_dir}/within_100mi_vs_life_expectancy_R.pdf"), height=4, width=4)




# Now lets make one regression table where we look at how strong these relationships are
# with FEs for State

# Here we use two packages:
# lfe for fast FE regressions
# stargazer for producing regression output tables

# install.packages("lfe")
# install.packages("stargazer")
# library(lfe)
# library(stargazer)

# The basic syntax for felm in lfe is: y ~ x1 + x2 | FE1 + FE2

# We are going to scale mean_hh_income by 10k so that the coefficients here are easier
# to interpret.
network_concentration = network_concentration %>% 
  mutate(mean_hh_income_10k = mean_hh_income/10000)

est1 <- felm(mean_hh_income_10k ~ share_connections_within_100mi, data=network_concentration)
est2 <- felm(mean_hh_income_10k ~ share_connections_within_100mi | state_fips, data=network_concentration)

est3 <- felm(e_rank_b ~ share_connections_within_100mi, data=network_concentration)
est4 <- felm(e_rank_b ~ share_connections_within_100mi | state_fips, data=network_concentration)

est5 <- felm(le_agg_q1_m ~ share_connections_within_100mi, data=network_concentration)
est6 <- felm(le_agg_q1_m ~ share_connections_within_100mi | state_fips, data=network_concentration)

# Now pass all of these estimations to stargazer
# (Here we use word doc output. Also possible to save to latex)
stargazer(est1, est2, est3, est4, est5, est6,
          # For now just keep the number of observations and R^2
          keep.stat = c("n","rsq"), 
          # Add a row to show where we added FEs
          add.lines=list(
            c("State FEs", "", "X", "", "X", "", "X")),
          type="html",
          out=str_interp("${output_dir}/reg_output_R.doc"))




