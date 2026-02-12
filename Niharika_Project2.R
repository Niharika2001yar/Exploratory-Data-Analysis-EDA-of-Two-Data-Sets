#Assignment Part 1

#1.Read the CSV and load the data set
data_2015<- read.csv("C:/users/nihar/OneDrive/Desktop/Niharika_Project2/2015.csv")
head(data_2015)

#2.Check the column names of the data set
names(data_2015)

#3. View the data set in a new tab (RStudio specific)
library(tidyverse)
#view(data_2015)

#4. Get a brief overview of the data set 
library(dplyr)
glimpse(data_2015)

#5. Clean column names
library(janitor)
data_2015 <- clean_names(data_2015)
data_2015

#6. Select specific columns
happy_df <- data_2015 %>%
  select(country,region, happiness_score,freedom)

head(happy_df)

#7. Slice the first 10 rows
top_ten_df <- happy_df %>% 
  slice(1:10)
print(top_ten_df)

#8. Filter rows where freedom is less than 0.20
no_freedom_df <- happy_df%>%
  filter(freedom < 0.20)
head(no_freedom_df)

#9. Arrange by freedom in descending order
best_freedom_df <- happy_df%>%
  arrange(desc(freedom))
head(best_freedom_df)

#10. Create a new column gff_stat
data_2015 <- data_2015 %>%
  mutate(gff_stat = family + freedom + generosity)
head(data_2015)

#11. Group by region and summarize
# Group by region and summarize the data
regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarise(
    country_count = n(),                     # Count the number of countries
    mean_happiness = mean(happiness_score),  # Mean happiness
    mean_freedom = mean(freedom)             # Mean freedom
  )
# View the resulting table
print(regional_stats_df)

#Assignment Part 2

#12. Load the baseball data
baseball <- read.csv("C:/Users/nihar/OneDrive/Desktop/Niharika_Project2/baseball.csv")
head(baseball)

#13. Explore the dataset
glimpse(baseball)
summary(baseball)

#14. Filter players with at least 1 at-bat
baseball <- baseball %>%
  filter(AB > 0)
head(baseball)

#15. Add Batting Average column
baseball <- baseball %>%
  mutate(BA = H/AB)
head(baseball)

#16. Add On-Base Percentage (OBP) column
baseball <- baseball %>%
  mutate(OBP =  (H + BB) / (AB + BB))
head(baseball)

#17. Find the top 10 strikeout players
strikeout_artist <- baseball%>%
  
  arrange(desc(SO))%>% # Sort by strikeouts in descending order
  slice(1:10)          # Select the top 10 players


# View the resulting data set
print(strikeout_artist)

#18. Filter eligible players (at least 300 AB or 100 games)
# Filter players based on eligibility criteria
eligible_df <- baseball %>%
  filter(AB >= 300 | G >= 100)

# View the resulting data set
head(eligible_df)

#19. Create histogram for Batting Average
# Create a histogram of batting average (BA) for eligible players
library(ggplot2)
ggplot(eligible_df, aes(x = BA)) +
  geom_histogram( fill = "red", color = "white") +
  labs(title = "Histogram of Batting Average for Eligible Players", 
       x = "Batting Average (BA)", 
       y = "Count") 
theme_minimal()


#20. Analyze the eligible players based on OBP, HR, RBI, etc., and make a decision.
mvp_players <- baseball %>%
  select(First,Last, OBP, HR, RBI) %>%
  arrange(desc(OBP), desc(HR), desc(RBI))  

head(mvp_players) 
library(pacman)