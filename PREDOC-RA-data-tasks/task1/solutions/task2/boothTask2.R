library(dplyr)

#importing data and renaming columns, then changing NAs to 0s in eligibles, enrollees, and penetration
#Lastly remove Guam and Puerto Rico

df <- read.csv("Medicare_Advantage/scp-1205.csv", header = F) %>% 
  rename(countyname ="V1", state = "V2", healthplanname = "V3", typeofplan = "V4",
         countyssa = "V5", eligibles = "V7", enrollees = "V8", penetration = "V9", 
         ABrate = "V10") %>% 
  mutate(eligibles = ifelse(is.na(eligibles), 0, eligibles)) %>% 
  mutate(enrollees = ifelse(is.na(enrollees), 0, enrollees)) %>% 
  mutate(penetration = ifelse(is.na(penetration), 0, penetration))%>% 
  filter(state != "GU") %>% 
  filter(state != "PR")

#Now the dataset is in better shape to be worked with
#Both these chunks could be done togother but I have them sperated for  for clarity
#This is the code chunk that creates the desired output data

df2 <- df %>% 
  mutate(moreThan10Enrollees = ifelse(enrollees >10, 1, 0)) %>% #Creating dummy variables for if they plans meet the critieron
  mutate(moreThan5Percent = ifelse(penetration > .5, 1, 0)) %>% 
  group_by(countyname) %>% #Group the data by county
  mutate(numberofplans1 = sum(moreThan10Enrollees)) %>% #summing gives number of plans with more than 10 enrollees than 10
  mutate(numberofplans2 = sum(moreThan5Percent)) %>%  #Same with more than 5% penetration
  mutate(totalenrollees = sum(enrollees)) %>%  #sum all enrollees in a county
  mutate(totalpenetration = totalenrollees/eligibles) %>%  #Create totalpenetratoin variable
  slice(1) %>% #Takes one row for each group (in this case each county is a group)
  select(countyname, state, numberofplans1, numberofplans2, countyssa, eligibles,
         totalenrollees, totalpenetration) %>% #Select varaibles we want
  arrange(state, countyname) %>% 
  filter(countyname != "UNDER-11 ") %>% #Filtering out two weird entries that probably don't below
  filter(state != "99") #Seemed like another entry we would not want

write.csv(df2, file  = "dataManipulationTask.csv")


