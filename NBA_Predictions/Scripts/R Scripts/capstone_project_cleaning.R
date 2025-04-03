rm(list = ls())
## import packages
library(openxlsx)
library(dplyr)
library(ggplot2)
library(caTools)
library(neuralnet)
library(tidyverse)
library(rpart)
library(ggplot2)
library(caret)
library(readr)
library(rpart)
library(MASS)
library(car)
library(bestNormalize)

## read in data
Player_Totals <- read_csv("Player Totals.csv")
Team_Totals <- read_csv("Team Summaries.csv")
Team_Stats_Per_Game <- read_csv("Team Stats Per Game.csv")
All_Stars <- read_csv("All-Star Selections.csv")
View(Player_Totals)
summary(Player_Totals)

## check years and teams for the datasets
years <- unique(Player_Totals$season)
teams <- unique(Player_Totals$tm)
team_teams <- unique(Team_Totals$team)
team_year <- unique(Team_Totals$season)

## joining all star data to player dataset
all_star_players <- merge(Player_Totals, All_Stars, by = c("player", "season"), all.x = TRUE)
all_star_players$all_star[is.na(all_star_players$replaced)] <- 0
all_star_players$all_star[is.na(all_star_players$all_star)] <- 1
Player_Totals <- all_star_players

## removing unwanted features from player data
Player_Totals <- Player_Totals %>%
  dplyr::select(-team)%>%
  dplyr::select(-lg.y)%>%
  dplyr::select(-replaced)

## adjust column names to join datasets on team and season
colnames(Player_Totals)[colnames(Player_Totals) == 'tm'] <- 'team'
colnames(Team_Totals)[colnames(Team_Totals) == 'team'] <- 'team_name'
colnames(Team_Totals)[colnames(Team_Totals) == 'abbreviation'] <- 'team'



## eliminate all seasons prior to 82 game standardization, all non existant leagues, and league average rows

standardized_teams <- Team_Totals %>%
  filter(season > 1968) %>%
  filter(season != 1999) %>%
  filter(season != 2012) %>%
  filter(season != 2013) %>%
  filter(season != 2020) %>%
  filter(season != 2021)

standardized_teams <- standardized_teams %>%
  filter(lg != 'ABA') %>%
  filter(team_name != 'League Average')

standardized_players <- Player_Totals %>%
  filter(season > 1973) %>%
  filter(season != 1999) %>%
  filter(season != 2012) %>%
  filter(season != 2013) %>%
  filter(season != 2020) %>%
  filter(season != 2021)

standardized_players <- standardized_players %>%
  filter(lg.x != 'ABA')

standardized_teams_avgs <- Team_Stats_Per_Game %>%
  filter(lg != 'ABA') %>%
  filter(team != 'League Average')
  
standardized_teams_avgs <- standardized_teams_avgs %>%
  filter(season > 1973) %>%
  filter(season != 1999) %>%
  filter(season != 2012) %>%
  filter(season != 2013) %>%
  filter(season != 2020) %>%
  filter(season != 2021)

## renaming team field and joining datasets
standardized_teams_avgs <- standardized_teams_avgs %>%
  rename(team_name = team)

all_team_data <- merge(standardized_teams, standardized_teams_avgs, by = c("team_name", "season"))

all_data <- inner_join(all_team_data, standardized_players, by = c("team", "season"))

## wins and playoffs only
pw_subset <- standardized_teams[, c("season", "team", "playoffs", "w")]
team_pw <- merge(pw_subset, standardized_players, by = c("team", "season"))

## removing features from datasets and filtering
all_data <- all_data %>%
  dplyr :: select(-lg.x.x)

all_data_3pt <- all_data %>%
  filter(season > 1979) 

## imputing mimssing 3pt values
quartiles <- quantile(all_team_data$x3p_percent, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

print(quartiles)


data_3pt_imputed <- all_team_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), quantile(., 0, na.rm = TRUE), .)))



## function to count outliers in a whole dataframe
outlier_count <- function(data) {
  num_outliers <- c()
  
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      mean_val <- mean(data[[col]], na.rm = TRUE)
      std_dev <- sd(data[[col]], na.rm = TRUE)
      
      up_bnd <- mean_val + (4 * std_dev)
      low_bnd <- mean_val - (4 * std_dev)
      
      above_bnd <- sum(data[[col]] > up_bnd, na.rm = TRUE)
      below_bnd <- sum(data[[col]] < low_bnd, na.rm = TRUE)
      
      outliers <- above_bnd + below_bnd
      
      
      num_outliers[col] <- outliers
    } else {
      
      cat("Skipping non-numeric column:", col, "\n")
    }
  }
  
  print(num_outliers)
}

outlier_count(data_3pt_imputed)

## checking column names and adjusting repeated names to new names
print(colnames(all_data))
print(unique(all_data$g.x))
unique_gx <- unique(all_data$g.x)
unique_gx <- setdiff(unique_gx, 82)
unique_teams_years <- all_data[all_data$g.x %in% unique_gx, unique("season")]
print(unique(unique_teams_years))
names(all_data)[names(all_data) == 'age.y'] <- 'player_age'
names(all_data)[names(all_data) == 'age.x'] <- 'team_avg_age'
names(all_data)[names(all_data) == 'lg.y'] <- 'lg'


## creating the player buckets and player averages
breaks <- seq(0, max(Player_Totals$pts / Player_Totals$g) + 5, by = 5)

bucket_labels <- 1:(length(breaks) - 1)

Points_Buckets <- team_pw %>%
  mutate(points_bucket = cut(pts/g, breaks = breaks, labels = bucket_labels, right = FALSE))


main_stats_buckets <- Points_Buckets %>%
  mutate(
    pts_per_game = pts / g,
    stl_per_game = stl / g,
    trb_per_game = trb / g,
    ast_per_game = ast / g,
    blk_per_game = blk / g,
    orb_per_game = orb / g,
    drb_per_game = drb / g
  )


stl_breaks   <- seq(0, max(main_stats_buckets$stl_per_game) + 2, by = 2)
reb_breaks   <- seq(0, max(main_stats_buckets$trb_per_game) + 2, by = 2)
ast_breaks   <- seq(0, max(main_stats_buckets$ast_per_game) + 2, by = 2)
blk_breaks   <- seq(0, max(main_stats_buckets$blk_per_game) + 2, by = 2)
oreb_breaks  <- seq(0, max(main_stats_buckets$orb_per_game) + 2, by = 2)
dreb_breaks  <- seq(0, max(main_stats_buckets$drb_per_game) + 2, by = 2)


reb_labels <- 1:(length(reb_breaks) - 1)
ast_labels <- 1:(length(ast_breaks) - 1)
blk_labels <- 1:(length(blk_breaks) - 1)
stl_labels <- 1:(length(stl_breaks) - 1)
oreb_labels <- 1:(length(oreb_breaks) - 1)
dreb_labels <- 1:(length(dreb_breaks) - 1)


main_stats_buckets <- main_stats_buckets %>%
  mutate(
    rebound_bucket = cut(trb_per_game, breaks = reb_breaks, labels = reb_labels, right = FALSE),
    assist_bucket  = cut(ast_per_game, breaks = ast_breaks, labels = ast_labels, right = FALSE),
    block_bucket   = cut(blk_per_game, breaks = blk_breaks, labels = blk_labels, right = FALSE),
    steal_bucket   = cut(stl_per_game, breaks = stl_breaks, labels = stl_labels, right = FALSE),
    oreb_bucket    = cut(orb_per_game, breaks = oreb_breaks, labels = oreb_labels, right = FALSE),
    dreb_bucket    = cut(drb_per_game, breaks = dreb_breaks, labels = dreb_labels, right = FALSE)
  )


## creating the final datasets
final_player_dataset <- main_stats_buckets %>%
  filter(g > 50)
colSums(is.na(final_player_dataset))
final_player_dataset[is.na(final_player_dataset)] <- 0
outlier_count(final_player_dataset)
summary(main_stats_buckets$g)

final_player_dataset <- final_player_dataset%>%
  dplyr :: select(-birth_year)

all_star_counts <- final_player_dataset %>%
  group_by(team, season) %>%
  summarise(all_star = sum(all_star, na.rm = TRUE))

team_all_stars <- data_3pt_imputed %>%
 dplyr :: left_join(all_star_counts, by = c("team", "season"))
colnames(team_all_stars)

team_pw <- team_pw %>%
  filter(g > 50)
team_pw <-  team_pw %>%
  mutate(across(everything(), ~ ifelse(is.na(.), quantile(., 0.25, na.rm = TRUE), .)))
team_pw <- team_pw%>%
  dplyr :: select(-birth_year)


data_3pt_imputed <- data_3pt_imputed%>%
  dplyr :: select(-lg.x)%>%
  dplyr :: select(-pw)

data_3pt_imputed <- data_3pt_imputed%>%
  dplyr :: select(-pl)%>%
  dplyr :: select(-attend)%>%
  dplyr :: select(-attend_g)%>%
  dplyr :: select(-lg.y)

data_3pt_imputed <- data_3pt_imputed%>%
  dplyr :: select(-playoffs.y)
colnames(data_3pt_imputed)[colnames(data_3pt_imputed) == 'playoffs.x'] <- 'playoffs'

all_data <- all_data%>%
  dplyr :: select(-pl)%>%
  dplyr :: select(-attend)%>%
  dplyr :: select(-attend_g)%>%
  dplyr :: select(-playoffs.y)%>%
  filter(season > 1973) %>%
  filter(season != 1999) %>%
  filter(season != 2012) %>%
  filter(season != 2013) %>%
  filter(season != 2020) %>%
  filter(season != 2021)

all_data <- all_data %>%
  filter(g.y > 50)

colnames(final_player_dataset)[colnames(final_player_dataset) == 'lg.x'] <- 'lg'
response_vars <- c('playoffs', 'w', 'season')  
predictors <- data_3pt_imputed[, !(names(data_3pt_imputed) %in% response_vars)]


numeric_columns <- sapply(predictors, is.numeric)



colnames(final_player_dataset)
colnames(data_3pt_imputed)

names(data_3pt_imputed)[names(data_3pt_imputed) == "playoffs.x"] <- "playoffs"

## writing final datasets to xlsx files
write.xlsx(final_player_dataset, 'all_star_data.xlsx')
write.xlsx(team_all_stars, 'team_all_stars.xlsx')
getwd()


## plots begin below- these are all data exploration type visualizations

ggplot(all_team_data, aes(x = o_rtg)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of Offensive Rating", x = "Offensive Rating")

ggplot(all_team_data, aes(x = d_rtg)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Distribution of Defensive Rating", x = "Defensive Rating")


average_fg_per_year <- standardized_teams_avgs %>%
  group_by(season) %>%
  summarize(avg_fg_per_game = mean(fg_per_game, na.rm = TRUE))

ggplot(average_fg_per_year, aes(x = season, y = avg_fg_per_game)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +  
  labs(title = "Average Field Goals per Game Over the Years",
       x = "Year",
       y = "Average Field Goals per Game") +
  theme_minimal()

average_fga_per_year <- standardized_teams_avgs %>%
  group_by(season) %>%
  summarize(avg_fga_per_game = mean(fga_per_game, na.rm = TRUE))

ggplot(average_fga_per_year, aes(x = season, y = avg_fga_per_game)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +  
  labs(title = "Average Field Goals Attempts per Game Over the Years",
       x = "Year",
       y = "Average Field Goals Attempts per Game") +
  theme_minimal()

fgs_per_season <- merge(average_fg_per_year, average_fga_per_year, by = "season")

ggplot(fgs_per_season, aes(x = season)) +
  geom_line(aes(y = avg_fg_per_game, color = "Average FG per Game")) +
  geom_line(aes(y = avg_fga_per_game, color = "Average FGA per Game")) +
  geom_point(aes(y = avg_fg_per_game, color = "Average FG per Game")) +
  geom_point(aes(y = avg_fga_per_game, color = "Average FGA per Game")) +
  labs(title = "Average Field Goals and Field Goal Attempts per Game Over the Years",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Metric", values = c("Average FG per Game" = "blue", "Average FGA per Game" = "red")) +
  theme_minimal()

average_offensive_stats <- standardized_teams_avgs %>%
  group_by(season) %>%
  summarize(avg_points = mean(pts_per_game, na.rm = TRUE),
            avg_assists = mean(ast_per_game, na.rm = TRUE),
            avg_off_rebounds = mean(orb_per_game, na.rm = TRUE))


average_team_stats <- standardized_teams_avgs %>%
  group_by(season) %>%
  summarize(avg_steals = mean(stl_per_game, na.rm = TRUE),
            avg_blocks = mean(blk_per_game, na.rm = TRUE),
            avg_def_rebounds = mean(drb_per_game, na.rm = TRUE),
            avg_points = mean(pts_per_game, na.rm = TRUE),
            avg_assists = mean(ast_per_game, na.rm = TRUE),
            avg_off_rebounds = mean(orb_per_game, na.rm = TRUE))

ggplot(average_offensive_stats, aes(x = season)) +
  geom_line(aes(y = avg_points, color = "Average Points")) +
  geom_line(aes(y = avg_assists, color = "Average Assists")) +
  geom_line(aes(y = avg_off_rebounds, color = "Average Offensive Rebounds")) +
  geom_point(aes(y = avg_points, color = "Average Points")) +
  geom_point(aes(y = avg_assists, color = "Average Assists")) +
  geom_point(aes(y = avg_off_rebounds, color = "Average Offensive Rebounds")) +
  labs(title = "Average Points, Assists, and Offensive Rebounds per Game Over the Years",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Points" = "blue", 
                                "Average Assists" = "green", 
                                "Average Offensive Rebounds" = "red")) +
  theme_minimal()

ggplot(average_team_stats, aes(x = season)) +
  geom_line(aes(y = avg_steals, color = "Average Steals")) +
  geom_line(aes(y = avg_blocks, color = "Average Blocks")) +
  geom_point(aes(y = avg_steals, color = "Average Steals")) +
  geom_point(aes(y = avg_blocks, color = "Average Blocks")) +
  labs(title = "Average Steals and Blocks per Game 1969-2024",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Steals" = "green", 
                                "Average Blocks" = "red")) +
  theme_minimal()

ggplot(all_team_data, aes(x = season)) +
  geom_line(aes(y = o_rtg, color = "Average Offensive Rating")) +
  geom_line(aes(y = d_rtg, color = "Average Defensive Rating")) +
  geom_point(aes(y = o_rtg, color = "Average Offensive Rating")) +
  geom_point(aes(y = d_rtg, color = "Average Defensive Rating")) +
  labs(title = "League Average Offensive and Defensive Ratings 1969-2024",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Offensive Rating" = "green", 
                                "Average Defensive Rating" = "red")) +
  theme_minimal()

ggplot(all_team_data, aes(x = season)) +
  geom_line(aes(y = d_rtg, color = "Average Defensive Rating")) +
  geom_point(aes(y = d_rtg, color = "Average Defensive Rating")) +
  labs(title = "League Average Defensive Ratings 1969-2024",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Defensive Rating" = "red")) +
  theme_minimal()

ggplot(all_team_data, aes(x = season)) +
  geom_line(aes(y = o_rtg, color = "Average Offensive Rating")) +
  geom_point(aes(y = o_rtg, color = "Average Offensive Rating")) +
  labs(title = "League Average Offensive Ratings 1969-2024",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Offensive Rating" = "green")) +
  theme_minimal()


ggplot(average_team_stats, aes(x = season)) +
  geom_line(aes(y = avg_steals, color = "Average Steals")) +
  geom_line(aes(y = avg_blocks, color = "Average Blocks")) +
  geom_point(aes(y = avg_steals, color = "Average Steals")) +
  geom_point(aes(y = avg_blocks, color = "Average Blocks")) +
  labs(title = "Average Steals, Blocks, and Defensive Rebounds per Game Over the Years",
       x = "Season",
       y = "Average per Game") +
  scale_color_manual(name = "Stat", 
                     values = c("Average Steals" = "blue", 
                                "Average Blocks" = "green")) +
  theme_minimal()

ggplot(standardized_teams_avgs, aes(x = as.factor(season), y = pts_per_game)) +
  geom_boxplot() +
  labs(title = "Distribution of Points per Game by Season", x = "Season", y = "Points per Game") +
  theme_minimal()

position_data <- main_stats_buckets %>%
  filter(pos %in% c("PG", "SG", "SF", "PF", "C"))


ggplot(position_data, aes(x = pos, y = pts_per_game)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Points per Game by Position",
       x = "Position",
       y = "Points per Game") +
  theme_minimal()

ggplot(position_data, aes(x = pos, y = ast_per_game)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Assists per Game by Position",
       x = "Position",
       y = "Assists per Game") +
  theme_minimal()

ggplot(position_data, aes(x = pos, y = trb_per_game)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rebounds per Game by Position",
       x = "Position",
       y = "Rebounds per Game") +
  theme_minimal()

ggplot(position_data, aes(x = pos, y = stl_per_game)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Steals per Game by Position",
       x = "Position",
       y = "Steals per Game") +
  theme_minimal()

ggplot(position_data, aes(x = pos, y = blk_per_game)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Blocks per Game by Position",
       x = "Position",
       y = "Blocks per Game") +
  theme_minimal()


## create subset of teams that made playoffs

playoff_data <- team_facts %>%
  filter(playoffs == TRUE) %>%
  group_by(season_id) %>%
  summarise(lowest_win_total = min(w, na.rm = TRUE)) %>%
  ungroup()

#average wins of lowest win total teams to make playoffs each respective year

average_wins <- mean(playoff_data$lowest_win_total, na.rm = TRUE)


ggplot(playoff_data, aes(x = season_id, y = lowest_win_total)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") +  
  geom_hline(yintercept = average_wins, color = "black", linetype = "dashed") + 
  annotate("text", x = Inf, y = average_wins, label = paste("Average Wins:", round(average_wins, 2)), 
           hjust = 1.2, vjust = -0.5, color = "black") + 
  labs(title = "Lowest Win Total to Make the Playoffs with Average Wins Line",
       x = "Year",
       y = "Wins") +
  theme_minimal()


## bullets qualified in 1953 with only 16 wins, lowest ever. This past season, 2024 had the highest ever qualification

## similar to above, except this time using average wins of all teams that made playoffs

average_wins_playoffs <- team_facts %>%
  filter(playoffs == TRUE) %>%
  group_by(season_id) %>%
  summarise(average_wins = mean(w, na.rm = TRUE)) %>%
  ungroup()

overall_average_wins <- mean(average_wins_playoffs$average_wins, na.rm = TRUE)


ggplot(average_wins_playoffs, aes(x = season_id, y = average_wins)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") +  
  geom_hline(yintercept = overall_average_wins, color = "black", linetype = "dashed") +  
  annotate("text", x = Inf, y = overall_average_wins, 
           label = paste("Overall Average Wins:", round(overall_average_wins, 2)), 
           hjust = 1.1, vjust = -0.5, color = "black") +  
  labs(title = "Average Wins of Teams That Made the Playoffs by Year",
       x = "Year",
       y = "Average Wins") +
  theme_minimal()

## adding the + 5 ensures the max will fall in the final bucket

table(team_facts$playoffs)

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


decision_tree <- rpart(playoffs ~ w, data = team_facts, method = "class")

summary(decision_tree)
rpart.plot(decision_tree, type = 1, extra = 104, fallen.leaves = TRUE)


ggplot(main_stats_buckets, aes(x = pts_per_game)) + 
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Points Per Game", x = "Points", y = "Frequency")


ggplot(main_stats_buckets, aes(x = ast_per_game)) + 
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Assists Per Game", x = "Assists", y = "Frequency")

ggplot(main_stats_buckets, aes(x = trb_per_game)) + 
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Rebounds Per Game", x = "Rebounds", y = "Frequency")

ggplot(main_stats_buckets, aes(x = stl_per_game)) + 
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Steals Per Game", x = "Steals", y = "Frequency")


ggplot(main_stats_buckets, aes(x = blk_per_game)) + 
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Blocks Per Game", x = "Blocks", y = "Frequency")

ggplot(main_stats_buckets, aes(x = ast_per_game, y = (pts/g))) + 
  geom_point() +
  labs(title = "Points vs. Assists", x = "Assists Per Game", y = "Points Per Game")

top_players <- main_stats_buckets %>% 
  arrange(desc(pts_per_game)) %>% 
  head(10)


summary(top_players$pts_per_game)



ggplot(top_players, aes(x = reorder(paste(player, season, sep = " ("), -pts_per_game), y = pts_per_game)) +
  geom_bar(stat = "identity", fill = 'green') +
  ylim(0, 40) +  
  geom_text(aes(label = round(pts_per_game, 2)), vjust = -0.5, size = 3) +  
  labs(x = "Player (Season)", y = "Points per Game", title = "Top Players by Points per Game") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


