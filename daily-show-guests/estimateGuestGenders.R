# Estimate the gender of each guest using genderize.io (too lazy, er, efficient
# to just label each guest)

require(dplyr)
require(tidyr)
require(ggplot2)

source("lookupGenderForNames.R")
if (file.exists("apiKey.txt")) {
  apiKey <- readLines("apiKey.txt")
} else {
  apiKey <- NA
}


# Prepare the data --------------------------------------------------------

showData <- read.csv("daily_show_guests.csv", stringsAsFactors = FALSE)

# # Shows with multiple guests get multiple lines in the spreadsheet, but
# # Raw_Guest_List is the same for each line. Isolate the unique guest for each
# # show.
# showData <- showData %>% 
#   mutate(Row_Number = 1:nrow(.)) %>%
#   group_by(Show) %>% 
#   mutate(Guest_Number = row_number(Show))

# Isolate the "first" name of each guest
showData <- showData %>%
  mutate(Guest_Name = Raw_Guest_List,
         Guest_Name = sub("^Dr[^[:alpha:]]*", "", Guest_Name),
         Guest_Name = sub("^Sen[^[:alpha:]]*", "", Guest_Name),
         Guest_Name = sub("^President[^[:alpha:]]*", "", Guest_Name),
         Guest_Name = sub("^Admiral[^[:alpha:]]*", "", Guest_Name),
         First_Name = sub("([[:alpha:]]+).*", "\\1", Guest_Name))

firstNamesDF <- showData %>% select(First_Name) %>% distinct()
gendersDF <- lookupGenderForNames(firstNamesDF$First_Name, apiKey = apiKey)

# Join the new gender info to the show data
showData <- left_join(showData, 
                      select(gendersDF, name, gender),
                      by = c("First_Name" = "name"))


# Start plotting ----------------------------------------------------------

showData.hasGender <- showData %>%
  filter(!is.na(gender),
         !is.na(Group),
         Group != "#N/A") %>%
  mutate(Group = ifelse(Group == "media", "Media", Group))

# Let's look at the gender breakdown by guest group
showData.genderByGroup <- showData.hasGender %>% 
  group_by(Group, gender) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  spread(gender, count) %>% 
  mutate(percent_female = female / (female+male)) %>% 
  arrange(percent_female) %>% 
  gather("gender", "count", male, female)

showData.genderByGroup %>% 
  filter(gender == "female") %>%
  ggplot(aes(x=reorder(Group,percent_female), y=100*percent_female)) + 
  geom_bar(stat="identity", color="#000000", fill="#e6550d") +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Estimated Gender of Daily Show Guests",
       y = "Percent Female",
       x = "Guest Group")
ggsave("daily_show_guest_gender.png")

