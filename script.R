r_length <- 3
r_width <- 3
r_area <- r_length*r_width
r_area

#change values length and width
r_length <- 4
r_width <- 4
r_area

#column
respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
more_respondent_wall_type <- respondent_wall_type[c(1,2,3,2,1,3)]
more_respondent_wall_type[c(1,2,3,2,1,3)]

#conditional subsetting
hh_members <- c(3, 7, 10, 6)
hh_members[c(TRUE, FALSE, TRUE, TRUE)]
hh_members > 5 #will return logicals with true for the indices that meet the condition
## so we can use this to select only the values above 5
hh_members[hh_members >5]
hh_members[hh_members < 4| hh_members > 7]
hh_members[hh_members >= 4 & hh_members <= 7]

possessions <- c("car", "bicycle", "radio", "television", "mobile_phone")
possessions[possessions == "car" | possessions == "bicycle"]
possessions %in% c("car", "bicycle")
possessions %in% c("car", "bicycle", "motorcycle", "truck", "boat", "bus")
possessions[possessions %in% c("car", "bicycle", "motorcycle", "truck", "boat", "bus")]

#missing data
rooms <- c(2, 1, 1, NA, 7)
mean(rooms)
max(rooms)
mean(rooms, na.rm=TRUE)
max(rooms, na.rm=TRUE)
## Extract those elements which are not missing values.
## The ! character is also called the NOT operator
rooms[!is.na(rooms)]
## Count the number of missing values.
## The output of is.na() is a logical vector (TRUE/FALSE equivalent to 1/0) so the sum() function here is effectively counting
sum(is.na(rooms))
## Returns the object with incomplete cases removed. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
na.omit(rooms)
## Extract those elements which are complete cases. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
rooms[complete.cases(rooms)]
rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
rooms_no_na <- na.omit(rooms)
median(rooms)
rooms_above_2 <- rooms_no_na[rooms_no_na > 2]
length(rooms_above_2)
library(tidyverse)
library(here)

#importing data
interviews <- read_csv(here("data", "SAFI_clean.csv"),na = "NULL")
head(interviews)
class(interviews)

#inspecting data frames
dim(interviews)
nrow(interviews)
ncol(interviews)
head(interviews)
tail(interviews)
names(interviews)
str(interviews)
summary(interviews)
glimpse(interviews)

#indexing and subsetting data frames
## first element in the first column of the tibble
interviews[1,1]
## first element in the 6th column of the tibble 
interviews[1,6]
## first column of the tibble (as a vector)
interviews[[1]]
## first column of the tibble
interviews[1]
## first three elements in the 7th column of the tibble
interviews[1:3,7]
## the 3rd row of the tibble
interviews[3, ]
## equivalent to head_interviews <- head(interviews)
head_interviews <- interviews[1:6, ]
# The whole tibble, except the first column
interviews[,-1]
# Equivalent to head(interviews)
interviews[-c(7:131), ]
interviews["village"]
interviews[, "village"]
interviews[["village"]]
interviews$village
interviews_100 <- interviews[100, ]
nrow(interviews_100)
n_rows <- nrow(interviews)
tail(interviews)
nrow(interviews[131, ])
interviews_last <- interviews[n_rows, ]
interviews_middle <- interviews[median(1:n_rows), ]
interviews_head <- interviews[-(7:n_rows), ]

#factors
respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))
levels(respondent_floor_type)
nlevels(respondent_floor_type)
resident_floor_type
respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", "cement"))
respondent_floor_type
levels(respondent_floor_type)
levels(respondent_floor_type)[2] <- "brick"
levels(respondent_floor_type)
respondent_floor_type
respondent_floor_type_ordered <- factor(respondent_floor_type, ordered = TRUE)
respondent_floor_type_ordered

#Converting factors
as.character(respondent_floor_type)
year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct]

#renaming factors
## create a vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc
## convert it into a factor
memb_assoc <- as.factor(memb_assoc)
memb_assoc
## bar plot of the number of interview respondents who were
## members of irrigation association:
plot(memb_assoc)
## Let's recreate the vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc
## replace the missing data with "undetermined"
memb_assoc[is.na(memb_assoc)] <- "undetermined"
## convert it into a factor
memb_assoc <- as.factor(memb_assoc)
## let's see what it looks like
memb_assoc
## bar plot of the number of interview respondents who were
## members of irrigation association:
plot(memb_assoc)
## Rename levels. Note we need to keep the original level ordering when renaming.
levels(memb_assoc) <- c("No", "Undetermined", "Yes")
## Reorder levels. Note we need to use the new level names.
memb_assoc <- factor(memb_assoc, levels = c("No", "Yes", "Undetermined"))
plot(memb_assoc)

#Formatting dates
str(interviews)
library(lubridate)
dates <- interviews$interview_date
str(dates)
interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)
interviews

char_dates <- c("7/31/2012", "8/9/2014", "4/30/2016")
str(char_dates)
as_date(char_dates, format = "%m/%d/%Y")
as_date(char_dates, format = "%m/%d/%y")
as_date(char_dates, format = "%d/%m/%y")
mdy(char_dates)

library(tidyverse)

#Data wrangling
## load the tidyverse
library(tidyverse)
library(here)
interviews <- read_csv(here("data", "SAFI_clean.csv"), na = "NULL")

#inspect the data
interviews
# to select columns throughout the dataframe
select(interviews, village, no_membrs, months_lack_food)
# to select a series of connected columns
select(interviews, village:respondent_wall_type)
# filters observations where village name is "Chirodzo" 
filter(interviews, village == "Chirodzo")
# filters observations with "and" operator (comma)
# output dataframe satisfies ALL specified conditions
filter(interviews, village == "Chirodzo", rooms > 1, no_meals > 2)
# filters observations with "&" logical operator
# output dataframe satisfies ALL specified conditions
filter(interviews, village == "Chirodzo" & rooms > 1 & no_meals > 2)
# filters observations with "|" logical operator
# output dataframe satisfies AT LEAST ONE of the specified conditions
filter(interviews, village == "Chirodzo" | village == "Ruaca")

#Pipes
interviews2 <- filter(interviews, village == "Chirodzo")
interviews_ch <- select(filter(interviews, village == "Chirodzo"), village:respondent_wall_type)
interviews %>% filter(village == "Chirodzo") %>% select(village:respondent_wall_type)
interviews_ch <- interviews %>% filter(village == "Chirodzo") %>% select(village:respondent_wall_type)
interviews_ch
interviews %>% filter(memb_assoc == "yes") %>% select(affect_conflicts, liv_count, no_meals)

#Mutate
interviews %>% mutate(people_per_room = no_membrs / rooms)
interviews %>% filter(!is.na(memb_assoc)) %>% mutate(peole_per_room = no_membrs / rooms)
interviews_total_meals <- interviews %>% mutate(total_meals = no_membrs * no_meals) %>% filter(total_meals > 20) %>% select(village, total_meals)

#Split-apply-combine data analysis and the summarize() function
interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_members = mean(no_membrs))
interviews %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs))
interviews %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs)) %>% 
  ungroup()
interviews %>% 
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_members = mean(no_membrs))
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs))
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>% 
  arrange(min_membrs)
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(desc(min_membrs))

#Counting
interviews %>% count(village)
interviews %>% count(village, sort = TRUE)
interviews %>% count(no_meals)
interviews %>% group_by(village) %>% summarize(
  mean_no_membrs = mean(no_membrs), 
  min_no_membrs = min(no_membrs), 
  max_no_membrs = max(no_membrs),
  n = n())
library(lubridate)       
interviews %>% mutate(month = month(interview_date),
                      day = day(interview_date),
                      year = year(interview_date)) %>% 
  group_by(year, month) %>% 
  summarize(max_no_membrs = max(no_membrs))

#Long and wide data formats
interviews %>% 
  select(key_ID, village, interview_date, instanceID)
interviews %>% 
  filter(village == "Choridzo") %>% 
  select(key_ID, village, interview_date, instanceID) %>% 
  sample_n(size = 10)

#Pivoting wider
interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))
dim(interviews)
dim(interviews_wide)

#Pivoting longer
interviews_long <- interviews_wide %>% 
  pivot_longer(cols = muddaub:cement,
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical")
interviews_long <- interviews_wide %>% 
  pivot_longer(cols = c(burntbricks, cement, muddaub, sunbricks),
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical") %>%
  filter(wall_type_logical) %>% 
  select(-wall_type_logical)

#Applying pivot_wider() to clean our data
interviews_items_owned <- interviews %>% 
  separate_rows(items_owned, sep = ";") %>% 
  replace_na(list(items_owned = "no_listed_items")) %>% 
  mutate(items_owned_logical = TRUE) %>% 
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE))

nrow(interviews_items_owned)
nrow(interviews_items_owned)
interviews_items_owned %>% 
  filter(bicycle) %>% 
  group_by(village) %>% 
  count(bicycle)
interviews_items_owned %>%
  mutate(number_items = rowSums(select(., bicycle:car))) %>% 
  group_by(village) %>% 
  summarize(mean_items = mean(number_items))
interviews_months_lack_food <- interviews %>%
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>% 
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE))
interviews_months_lack_food %>% 
  mutate(number_months = rowSums(select(., Jan:May))) %>% 
  group_by(memb_assoc) %>% 
  summarize(mean_months = mean(number_months))

#Exporting data
interviews_plotting <- interviews %>%
  separate_rows(items_owned, sep = ";") %>%
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

write_csv (interviews_plotting, file = "data_output/interviews_plotting.csv")

#Data visualisation with Ggplot2 
library(tidyverse)
interviews_plotting <- read_csv("data_output/interviews_plotting.csv")
interviews_plotting %>% ggplot(aes(x = no_membrs, y = number_items)) + geom_point()

# Assign plot to a variable
interviews_plot <- interviews_plotting %>% ggplot(aes(x = no_membrs, y = number_items))
# Draw the plot as a dot plot
interviews_plot + geom_point()
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_point(alpha = 0.5)
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter()
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter(alpha = 0.5,
              color = "blue",
              width = 0.2,
              height = 0.2)
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter(aes(color = village), alpha = 0.5, width = 0.2, height = 0.2)

#Notes
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items, color = village)) + geom_count()
interviews_plotting %>% 
  ggplot(aes(x = village, y = rooms)) +
  geom_jitter(aes(color = respondent_wall_type),
              alpha = 0.5,
              width = 0.2,
              height = 0.2)
#Boxplot
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot()
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + 
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5,
              color = "tomato",
              width = 0.2,
              height = 0.2)
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) +
  geom_violin(alpha = 0) +
  geom_jitter(alpha = 0.5,
              color = "tomato",
              width = 0.2,
              height = 0.2)
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = liv_count)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5,
              width = 0.2,
              height = 0.2)
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = liv_count)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(aes(color = memb_assoc),
              alpha = 0.5,
              width = 0.2,
              height = 0.2)

#Barplots
interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar()
interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village))
interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village), position = "dodge")

percent_wall_type <- interviews_plotting %>% 
  filter(respondent_wall_type != "cement") %>%
  count(village, respondent_wall_type) %>% 
  group_by(village) %>%
  mutate(percent = (n/sum(n))*100) %>%
  ungroup()
percent_wall_type %>%
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge")

percent_memb_assoc <- interviews_plotting %>%
  filter(!is.na(memb_assoc)) %>% 
  count(village, memb_assoc) %>% 
  group_by(village) %>%
  mutate(percent = (n/sum(n))*100) %>% 
  ungroup()
percent_memb_assoc %>% 
  ggplot(aes(x = village, y = percent, fill = memb_assoc)) +
  geom_bar(stat = "identity", position = "dodge")

#Adding Labels and Titles
percent_wall_type %>%
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of wall type by village",
       fill = "Type of Wall in Home", 
       x = "Village",
       y = "Percent")

#Faceting
percent_wall_type %>%
  ggplot(aes(x = respondent_wall_type, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Proportion of wall type by village",
       x="Wall Type",
       y="Percent") +
  facet_wrap(~ village)
percent_wall_type %>%
  ggplot(aes(x = respondent_wall_type, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Proportion of wall type by village",
       x="Wall Type",
       y="Percent") +
  facet_wrap(~ village) +
  theme_bw() +
  theme(panel.grid = element_blank())

percent_items <- interviews_plotting %>%
  group_by(village) %>% 
  summarize(across(bicycle:no_listed_items, ~ sum(.x) / n() * 100)) %>% 
  pivot_longer(bicycle:no_listed_items, names_to = "items", values_to = "percent")

percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  theme_bw() +
  theme(panel.grid = element_blank())

#ggplot2 themes
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  theme_minimal() +
  theme(panel.grid = element_blank())
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  theme_light() +
  theme(panel.grid = element_blank())
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  theme_dark() +
  theme(panel.grid = element_blank())

#Customization
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw()
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(text = element_text(size = 16))
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))
grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12,
                                               angle = 45, hjust = 0.5,
                                               vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16),
                    plot.title = element_text(hjust = 0.5))
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  grey_theme
my_plot <- percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(color = "grey20", size = 12),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))
ggsave("fig_output/name_of_file.png", my_plot, width = 15, height = 10)
