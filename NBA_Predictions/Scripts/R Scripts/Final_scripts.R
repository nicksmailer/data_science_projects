## begin by importing all packages to be used during the project. If the command fails, it is likely
## necessary to run install.packages("x") subbing x for the package name
library(openxlsx)
library(jsonlite)
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
library(bestNormalize)
library(RPostgres)
library(Boruta)
library(corrplot)
library(randomForest)
library(glmnet)
library(cluster)

## read JSON files in, adjust file path to reflect where they are located on your device after download
team_facts <-  fromJSON('C:\\Users\\Nick\\JSON Team\\fact_table_202411301438.json')
team_facts <- team_facts[[1]]
team_seasons <- fromJSON('C:\\Users\\Nick\\JSON Team\\season_202411301438.json')
team_seasons <- team_seasons[[1]]
team_teams <- fromJSON('C:\\Users\\Nick\\JSON Team\\team_202411301439.json')
team_teams <- team_teams[[1]]

player_facts <- fromJSON('C:\\Users\\Nick\\JSON Players\\fact_table_202411301425.json')
player_facts <- player_facts[[1]]
player_teams <- fromJSON('C:\\Users\\Nick\\JSON Players\\team_202411301428.json')
player_teams <- player_teams[[1]]
player_seasons <- fromJSON('C:\\Users\\Nick\\JSON Players\\season_202411301428.json')
player_seasons <- player_seasons[[1]]
player_players <- fromJSON('C:\\Users\\Nick\\JSON Players\\player_202411301427.json')
player_players <- player_players[[1]]

## define numeric columns for transformation
numeric_columns <- sapply(team_facts, is.numeric)

## ensure buckets are numeric
player_facts <- player_facts %>% 
  mutate_at(c('points_bucket', 'assist_bucket', 'rebound_bucket', 'block_bucket', 'steal_bucket', 'oreb_bucket', 'dreb_bucket'), as.numeric)

## save team facts into new variable for transformation in order to preserve original facts
## exclude non numeric columns ids, and responses
data_transforming <- team_facts 
non_numeric_cols <- names(data_transforming)[sapply(data_transforming, Negate(is.numeric))]
exclude_cols <- c("season_id", "team_id", "w", "l", "g", non_numeric_cols)

data_transforming <- data_transforming[, !(names(data_transforming) %in% exclude_cols)]
transformed_data <- data_transforming
print(sum(is.na(transformed_data)))
print(is.numeric(transformed_data))

## apply Yeo-Johnson transformation
for (col in names(data_transforming)) {
  norm_result <- bestNormalize(data_transforming[[col]], method = "yeo.johnson")
  transformed_data[[col]] <- norm_result$x.t  
}

## repeat the process using the player data
player_transforming <- player_facts
non_numeric_cols_p <- names(player_transforming)[sapply(player_transforming, Negate(is.numeric))]
exclude_players <- c("player_id", "season_id", "team_id", "w", "playoffs", non_numeric_cols_p)
player_transforming <- player_transforming[, !names(player_transforming) %in% exclude_players]

player_transformed <- player_transforming
for (col in names(player_transforming)) {
  norm_result <- bestNormalize(player_transforming[[col]], method = "yeo.johnson")
  player_transformed[[col]] <- norm_result$x.t  
}

## checking results to make sure no missing values

sapply(data_transforming, function(x) sum(is.na(x)))
sapply(data_transforming, function(x) var(x, na.rm = TRUE))

## saving response variable and adding it into the transformed team data
response_var <- data_transforming$w  
transformed_data$w <- team_facts$w

## Boruta on team data and checking confirmed attributes and importance scores
boruta_result <- Boruta(w ~ ., data = transformed_data, doTrace = 2, maxRuns = 300)
print(boruta_result)
confirmed_attributes <- getSelectedAttributes(boruta_result, withTentative = TRUE)
print(confirmed_attributes)

importance_scores <- attStats(boruta_result)
print(importance_scores)
transformed_data <- transformed_data%>%
  dplyr::select(-w)

## Boruta again, this time using playoffs as the response
transformed_data$playoffs <- team_facts$playoffs
boruta_result2 <- Boruta(playoffs ~ ., data = transformed_data, doTrace = 2, maxRuns = 300)
confirmed_attributes2 <- getSelectedAttributes(boruta_result2, withTentative = TRUE)
print(confirmed_attributes2)

transformed_data <- transformed_data%>%
  dplyr::select(-playoffs)
transformed_data$l <- team_facts$l

## Boruta again, this time using losses as the response
boruta_result3 <- Boruta(l ~ ., data = transformed_data, doTrace = 2, maxRuns = 300)
confirmed_attributes3 <- getSelectedAttributes(boruta_result3, withTentative = TRUE)
print(confirmed_attributes3)

transformed_data <- transformed_data%>%
  dplyr::select(-l)

## Same thing for the player dataset
player_transformed$w <- player_facts$w
boruta_result4 <- Boruta(w ~ ., data = player_transformed, doTrace = 2)
print(boruta_result4)
confirmed_attributes4 <- getSelectedAttributes(boruta_result4, withTentative = TRUE)
print(confirmed_attributes4)

player_transformed <- player_transformed%>%
  dplyr::select(-w)

player_transformed$playoffs <- player_facts$playoffs
boruta_result5 <- Boruta(playoffs ~ ., data = player_transformed, doTrace = 2, maxRuns = 300)
confirmed_attributes5 <- getSelectedAttributes(boruta_result5, withTentative = TRUE)
print(confirmed_attributes5)

## correlation matrix for both datasets, check for multicolinearity. Had to increae print limit
cor_matrix <- cor(transformed_data, use = "complete.obs")
options(max.print=999999)
print(cor_matrix)

cor_matrix2 <- cor(player_transformed, use = "complete.obs")
print(cor_matrix2)

##begin model creation, create modeling data by eliminating fields identified through previous steps
##Initially I removed playoffs from team modeling, as it was highly correlated with wins, and I wanted
##to use wins to predict playoffs later on, not use playoffs to predict wins.
##Additionally, I modeled previously using the transformed data with poor results and since random
##forest does not require normalization I did not model using it.
team_modeling <- team_facts %>%
  dplyr::select(-mov)%>%
  dplyr::select(-pace)%>%
  dplyr::select(-srs)%>%
  dplyr::select(-mp_per_game)%>%
  dplyr::select(-pf_per_game)%>%
  dplyr::select(-arena)

player_modeling <- player_transformed %>%
  dplyr::select(-pts)%>%
  dplyr::select(-trb)%>%
  dplyr::select(-orb)%>%
  dplyr::select(-drb)%>%
  dplyr::select(-ast)%>%
  dplyr::select(-blk)%>%
  dplyr::select(-stl)%>%
  dplyr::select(-pf)%>%
  dplyr::select(-x3p)

team_modeling1 <- team_modeling %>%
  dplyr::select(-sos)%>%
  dplyr::select(-f_tr)%>%
  dplyr::select(-ft_fga)%>%
  dplyr::select(-opp_e_fg_percent)%>%
  dplyr::select(-opp_tov_percent)%>%
  dplyr::select(-opp_drb_percent)%>%
  dplyr::select(-opp_ft_fga)%>%
  dplyr::select(-x3p_ar)%>%
  dplyr::select(-tov_percent)%>%
  dplyr::select(-orb_percent)%>%
  dplyr::select(-x3p_per_game)%>%
  dplyr::select(-x2p_per_game)%>%
  dplyr::select(-fta_per_game)%>%
  dplyr::select(-o_rtg)%>%
  dplyr::select(-d_rtg)%>%
  dplyr::select(-l)%>%
  dplyr::select(-w)%>%
  dplyr::select(-g)

player_modeling1 <- player_facts %>%
  dplyr::select(-pts)%>%
  dplyr::select(-trb)%>%
  dplyr::select(-orb)%>%
  dplyr::select(-drb)%>%
  dplyr::select(-ast)%>%
  dplyr::select(-blk)%>%
  dplyr::select(-stl)%>%
  dplyr::select(-pf)%>%
  dplyr::select(-x3p)

top_players <- player_modeling1 %>%
  group_by(team_id, season_id) %>%
  arrange(desc(mp)) %>%       
  slice_head(n = 5) %>%            
  ungroup()                        

## create more subsets for buckets vs per game stats
player_averages <- player_modeling1%>%
  dplyr::select(-points_bucket)%>%
  dplyr::select(-rebound_bucket)%>%
  dplyr::select(-oreb_bucket)%>%
  dplyr::select(-dreb_bucket)%>%
  dplyr::select(-steal_bucket)%>%
  dplyr::select(-block_bucket)%>%
  dplyr::select(-assist_bucket)

player_buckets <- player_modeling1%>%
  dplyr::select(-pts_per_game)%>%
  dplyr::select(-ast_per_game)%>%
  dplyr::select(-orb_per_game)%>%
  dplyr::select(-drb_per_game)%>%
  dplyr::select(-trb_per_game)%>%
  dplyr::select(-stl_per_game)%>%
  dplyr::select(-blk_per_game)

player_modeling$w <- player_facts$w

## error functions- mean average error, mean squared error and root mean squared error
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

## first player modeling, linear regression, decision tree, and random forest. Later on decision tree 
## was eliminated as random forest does the same thing better. after each models training, I test the 
## results using the error functions defined above.
set.seed(99)
split_ratio <- sample.split(player_modeling$w, SplitRatio = 0.75)
player_training <- subset(player_modeling, split_ratio == TRUE)
player_test <- subset(player_modeling, split_ratio == FALSE)
actual_w <- player_test$w

player_lm_model <- lm(w ~ ., data = player_training)
player_lm_predictions <- predict(player_lm_model, newdata = player_test)
print(mse(player_lm_predictions, actual_w))
print(mae(player_lm_predictions, actual_w))
summary(player_lm_model)$r.squared
res <- player_lm_model$residuals
plot(fitted(player_lm_model), res)
summary(player_lm_model)

player_rf_model <- randomForest(w ~ ., data = player_training, ntree = 100)
player_rf_predictions <- predict(player_rf_model, newdata = player_test)
print(mse(player_rf_predictions, actual_w))
print(mae(player_rf_predictions, actual_w))

player_dt_model <- rpart(w ~ ., data = player_training)
player_dt_predictions <- predict(player_dt_model, newdata = player_test)
print(mse(player_dt_predictions, actual_w))
print(mae(player_dt_predictions, actual_w))

## then I repeat using the team dataset
team_modeling$w <- team_facts$w
set.seed(99)
split_ratio <- sample.split(team_modeling$w, SplitRatio = 0.75)
team_training <- subset(team_modeling, split_ratio == TRUE)
team_test <- subset(team_modeling, split_ratio == FALSE)
actual_w <- team_test$w

team_lm_model <- lm(w ~ ., data = team_training)
team_lm_predictions <- predict(team_lm_model, newdata = team_test)
print(mse(team_lm_predictions, actual_w))
print(mae(team_lm_predictions, actual_w))
summary(team_lm_model)
res1 <- team_lm_model$residuals
plot(fitted(team_lm_model), res1)
abline(0,0)
summary(team_lm_model)

team_rf_model <- randomForest(w ~ ., data = team_training, ntree = 100)
team_rf_predictions <- predict(team_rf_model, newdata = team_test)
print(mse(team_rf_predictions, actual_w))
print(mae(team_rf_predictions, actual_w))

team_dt_model <- rpart(w ~ ., data = team_training)
team_dt_predictions <- predict(team_dt_model, newdata = team_test)
print(mse(team_dt_predictions, actual_w))
print(mae(team_dt_predictions, actual_w))

## and again with the average subset
player_averages$w <- player_facts$w
set.seed(99)
split_ratio <- sample.split(player_averages$w, SplitRatio = 0.75)
player_avg_training <- subset(player_averages, split_ratio == TRUE)
player_avg_test <- subset(player_averages, split_ratio == FALSE)
actual_w <- player_avg_test$w

player_avg_lm_model <- lm(w ~ ., data = player_avg_training)
player_avg_lm_predictions <- predict(player_avg_lm_model, newdata = player_avg_test)
print(mse(player_avg_lm_predictions, actual_w))
print(mae(player_avg_lm_predictions, actual_w))
summary(player_avg_lm_model)

player_avg_rf_model <- randomForest(w ~ ., data = player_avg_training, ntree = 100)
player_avg_rf_predictions <- predict(player_avg_rf_model, newdata = player_avg_test)
print(mse(player_avg_rf_predictions, actual_w))
print(mae(player_avg_rf_predictions, actual_w))

player_avg_dt_model <- rpart(w ~ ., data = player_avg_training)
player_avg_dt_predictions <- predict(player_avg_dt_model, newdata = player_avg_test)
print(mse(player_avg_dt_predictions, actual_w))
print(mae(player_avg_dt_predictions, actual_w))

## then with the buckets subset

player_buckets$w <- player_facts$w
set.seed(99)
split_ratio <- sample.split(player_buckets$w, SplitRatio = 0.75)
player_bucket_training <- subset(player_buckets, split_ratio == TRUE)
player_bucket_test <- subset(player_buckets, split_ratio == FALSE)
actual_w <- player_test$w

player_bucket_lm_model <- lm(w ~ ., data = player_bucket_training)
player_bucket_lm_predictions <- predict(player_bucket_lm_model, newdata = player_bucket_test)
print(mse(player_bucket_lm_predictions, actual_w))
print(mae(player_bucket_lm_predictions, actual_w))
summary(player_bucket_lm_model)

player_bucket_rf_model <- randomForest(w ~ ., data = player_bucket_training, ntree = 100)
player_bucket_rf_predictions <- predict(player_bucket_rf_model, newdata = player_bucket_test)
print(mse(player_bucket_rf_predictions, actual_w))
print(mae(player_bucket_rf_predictions, actual_w))


player_bucket_dt_model <- rpart(w ~ ., data = player_bucket_training)
player_bucket_dt_predictions <- predict(player_bucket_dt_model, newdata = player_bucket_test)
print(mse(player_bucket_dt_predictions, actual_w))
print(mae(player_bucket_dt_predictions, actual_w))


## create subsets by team for nuance
team_season_subsets <- player_facts %>%
  group_by(season_id, team_id) %>%
  group_split() 

clean_team_sets <- team_season_subsets[sapply(team_season_subsets, nrow) > 0]

clean_team_sets <- lapply(clean_team_sets, as.data.frame)

## features that need to be removed, then removing them
remove <- c("player_id", "g", "orb", "drb", "trb", "ast", "stl", "blk", "pts")  

clean_team_sets <- lapply(clean_team_sets, function(i) {
  i %>% dplyr::select(-all_of(remove))
})


## defining count and average fields for the aggregations below

count_fields <- c("fg", "fga", "x3p", "x3pa", "x2p", "x2pa", "ft", "fta", "orb_per_game",
                  "drb_per_game", "trb_per_game", "ast_per_game", "stl_per_game",
                  "blk_per_game", "pts_per_game", "tov", "pf", "oreb_bucket", "dreb_bucket", 
                  "rebound_bucket", "assist_bucket", "points_bucket", "steal_bucket",
                  "block_bucket", "experience", "age")

avg_fields <- c("fg_percent", "x3p_percent", "x2p_percent", "e_fg_percent", "ft_percent",
                "w", "gs", "mp")


## top three players function- used for top 5 players mostly
top3_players <- function(input) {
  
  top_players <- input %>%
    arrange(desc(mp)) %>%
    slice_head(n = 5)
  
  
  aggregated_stats <- top_players %>%
    summarise(
      across(all_of(count_fields), sum, na.rm = TRUE),
      across(all_of(avg_fields), mean, na.rm = TRUE)
    )
  
  return(aggregated_stats)
}

## applying function to top players and checking the result
aggregated_data <- lapply(clean_team_sets, top3_players) %>% bind_rows()
head(aggregated_data)



## repeating transformation process using aggregated data
data_transforming <- aggregated_data
non_numeric_cols <- names(data_transforming)[sapply(data_transforming, Negate(is.numeric))]
exclude_cols <- c("season_id", "team_id", "w", "g", non_numeric_cols)

data_transforming <- data_transforming[, !(names(data_transforming) %in% exclude_cols)]
transformed_data <- data_transforming
print(sum(is.na(transformed_data)))
print(is.numeric(transformed_data))

for (col in names(data_transforming)) {
  norm_result <- bestNormalize(data_transforming[[col]], method = "yeo.johnson")
  transformed_data[[col]] <- norm_result$x.t  
}

## adding wins back in and renaming
transformed_data$w <- aggregated_data$w
aggregated_data <- transformed_data

## modeling using aggregated data
set.seed(99)
split_ratio <- sample.split(aggregated_data$w, SplitRatio = 0.75)
agg_training <- subset(aggregated_data, split_ratio == TRUE)
agg_test <- subset(aggregated_data, split_ratio == FALSE)
agg_w <- agg_test$w

agg_lm_model <- lm(w ~ ., data = agg_training)
agg_lm_predictions <- predict(agg_lm_model, newdata = agg_test)
print(mae(agg_lm_predictions, agg_w))
summary(agg_lm_model)

agg_rf_model <- randomForest(w ~ ., data = agg_training, ntree = 50)
agg_rf_predictions <- predict(agg_rf_model, newdata = agg_test)
print(mae(agg_rf_predictions, agg_w))

## seperating aggregated averages and buckets
agg_averages <- aggregated_data%>%
  dplyr::select(-points_bucket)%>%
  dplyr::select(-rebound_bucket)%>%
  dplyr::select(-oreb_bucket)%>%
  dplyr::select(-dreb_bucket)%>%
  dplyr::select(-steal_bucket)%>%
  dplyr::select(-block_bucket)

agg_buckets <- aggregated_data%>%
  dplyr::select(-pts_per_game)%>%
  dplyr::select(-ast_per_game)%>%
  dplyr::select(-orb_per_game)%>%
  dplyr::select(-drb_per_game)%>%
  dplyr::select(-trb_per_game)%>%
  dplyr::select(-stl_per_game)%>%
  dplyr::select(-blk_per_game)


## modeling using aggregated averages
set.seed(99)
split_ratio <- sample.split(agg_averages$w, SplitRatio = 0.75)
agg_avg_training <- subset(agg_averages, split_ratio == TRUE)
agg_avg_test <- subset(agg_averages, split_ratio == FALSE)
agg_w <- agg_avg_test$w

agg_avg_lm_model <- lm(w ~ ., data = agg_avg_training)
agg_avg_lm_predictions <- predict(agg_avg_lm_model, newdata = agg_avg_test)
print(mae(agg_avg_lm_predictions, agg_w))
summary(agg_avg_lm_model)

agg_avg_rf_model <- randomForest(w ~ ., data = agg_avg_training, ntree = 100)
agg_avg_rf_predictions <- predict(agg_avg_rf_model, newdata = agg_avg_test)
print(mae(agg_avg_rf_predictions, agg_w))

## modeling using aggregated buckets

set.seed(99)
split_ratio <- sample.split(agg_buckets$w, SplitRatio = 0.75)
agg_bucket_training <- subset(agg_buckets, split_ratio == TRUE)
agg_bucket_test <- subset(agg_buckets, split_ratio == FALSE)
actual_w <- agg_bucket_test$w

agg_bucket_lm_model <- lm(w ~ ., data = agg_bucket_training)
agg_bucket_lm_predictions <- predict(agg_bucket_lm_model, newdata = agg_bucket_test)
print(mae(agg_bucket_lm_predictions, actual_w))
summary(agg_bucket_lm_model)

agg_bucket_rf_model <- randomForest(w ~ ., data = agg_bucket_training, ntree = 100)
agg_bucket_rf_predictions <- predict(agg_bucket_rf_model, newdata = agg_bucket_test)
print(mae(agg_bucket_rf_predictions, actual_w))


## model tuning for players
grid <- expand.grid(
  .mtry = c(1, 2, 3, 4, 5)    
)

top_players1 <- top_players%>%
  dplyr::select(-w)
top_players1$playoffs <- as.integer(top_players1$playoffs)
top_players1$playoffs <- as.factor(top_players1$playoffs)
train_control <- trainControl(method = "cv", number = 3) 

player_rf_model <- train(
  playoffs ~ .,                  
  data = top_players1,                  
  method = "rf",                 
  trControl = train_control,     
  tuneGrid = grid,               
  metric = "Accuracy"            
)

## determine best mtry
print(player_rf_model)

## determine the best number of trees
ntree_values <- c(50, 100, 200, 500)

ntree_results <- data.frame(ntree = integer(), accuracy = numeric())

for (nt in ntree_values) {
  
  player_rf_model <- train(
    playoffs ~ .,
    data = top_players1,
    method = "rf",
    trControl = trainControl(method = "cv", number = 3),  
    tuneGrid = data.frame(mtry = 4),  
    ntree = nt  
  )
  
  
  mean_accuracy <- player_rf_model$results$Accuracy[1]  
  
  
  ntree_results <- rbind(ntree_results, data.frame(ntree = nt, accuracy = mean_accuracy))
}

## determine ideal number of trees
print(ntree_results)

## training to test ideal node size
split_ratio <- sample.split(top_players1$playoffs, SplitRatio = 0.75)
top_players_training <- subset(top_players1, split_ratio == TRUE)
top_players_test <- subset(top_players1, split_ratio == FALSE)

nodesize_values <- c(1, 5, 10, 15, 20)

node_results <- data.frame(
  Nodesize = numeric(),
  Accuracy = numeric()
)

for (nsize in nodesize_values) {
  player_rf_model <- randomForest(
    playoffs ~ .,
    data = top_players_training,
    ntree = 100,
    mtry = 4,
    nodesize = nsize
  )
  acc <- mean(predict(player_rf_model, top_players_test) == top_players_test$playoffs)
  print(paste("Nodesize:", nsize, "Accuracy:", acc))
  node_results <- rbind(node_results, data.frame(Nodesize = nsize, Accuracy = acc))
}


## repeat using the team data
grid1 <- expand.grid(
  .mtry = c(1, 2, 3, 4, 5, 6, 7, 8, 9)    
  
)
team_modeling1$playoffs <- as.integer(team_modeling1$playoffs)
team_modeling1$playoffs <- as.factor(team_modeling1$playoffs)

team_rf_model <- train(
  playoffs ~ .,                  
  data = team_modeling1,                  
  method = "rf",                 
  trControl = train_control,     
  tuneGrid = grid1,               
  metric = "Accuracy"            
)

# determine the best mtry
print(team_rf_model)


ntree_results1 <- data.frame(ntree = integer(), accuracy = numeric())


for (nt in ntree_values) {
  
  team_rf_model <- train(
    playoffs ~ .,
    data = team_modeling1,
    method = "rf",
    trControl = trainControl(method = "cv", number = 3),  
    tuneGrid = data.frame(mtry = 8),  
    ntree = nt  
  )
  
  
  mean_accuracy <- team_rf_model$results$Accuracy[1]  
  
  
  ntree_results1 <- rbind(ntree_results1, data.frame(ntree = nt, accuracy = mean_accuracy))
}

print(ntree_results1)

set.seed(99)
split_ratio <- sample.split(team_modeling1$playoffs, SplitRatio = 0.75)
team_modeling1_training <- subset(team_modeling1, split_ratio == TRUE)
team_modeling1_test <- subset(team_modeling1, split_ratio == FALSE)


node_results1 <- data.frame(
  Nodesize = numeric(),
  Accuracy = numeric()
)

for (nsize in nodesize_values) {
  team_rf_model <- randomForest(
    playoffs ~ .,
    data = team_modeling1_training,
    ntree = 100,
    mtry = 8,
    nodesize = nsize
  )
  acc <- mean(predict(team_rf_model, team_modeling1_test) == team_modeling1_test$playoffs)
  print(paste("Nodesize:", nsize, "Accuracy:", acc))
  node_results1 <- rbind(node_results1, data.frame(Nodesize = nsize, Accuracy = acc))
}


## checking historic playoff appearance by specific win totals
teams_with_50_wins <- team_facts %>%
  filter(w >= 50)

playoff_counts_50 <- teams_with_50_wins %>%
  group_by(playoffs) %>%
  summarize(count = n(), .groups = 'drop')

print(playoff_counts_50)


teams_with_42_wins <- team_facts %>%
  filter(w >= 42)

playoff_counts_42 <- teams_with_42_wins %>%
  group_by(playoffs) %>%
  summarize(count = n(), .groups = 'drop')

print(playoff_counts_42)

## cluster analysis of all star statistics, k-means
## defining stats to cluster
cluster_stats <- player_modeling1 %>% 
  dplyr::select(pts_per_game, ast_per_game, trb_per_game, stl_per_game, blk_per_game, all_star) 

## class standardization
allstar_classes <- table(cluster_stats$all_star)
min_class <- min(allstar_classes)
cluster_stats <- cluster_stats %>%
  group_by(all_star) %>%
  sample_n(min_class) %>%
  ungroup()

## seperation into all star and non-all-star groups
all_star_clustering <- cluster_stats %>%
  dplyr::filter(all_star == 1)

non_all_star_clustering <- cluster_stats %>%
  dplyr::filter(all_star == 0)

## clustering- first on all-star, then on non-all-star
set.seed(123)
kmeans_all_star <- kmeans(all_star_clustering, centers = 1) 

table(kmeans_all_star$cluster)
kmeans_all_star$centers

set.seed(123)
kmeans_non_all_star <- kmeans(non_all_star_clustering, centers = 1) 

table(kmeans_non_all_star$cluster)
kmeans_non_all_star$centers

all_star_clusters <- all_star_clustering %>%
  mutate(group = "All-Star")

non_all_star_clusters <- non_all_star_clustering %>%
  mutate(group = "Non-All-Star")

all_star_clusters$all_star_cluster <- kmeans_all_star$cluster
non_all_star_clusters$all_star_cluster <- kmeans_non_all_star$cluster

## combining the results for easy visualizations
combined_clusters <- bind_rows(all_star_clusters, non_all_star_clusters)
colnames(combined_clusters)

combined_clusters$group_type <- ifelse(combined_clusters$all_star == 1, "All-Star", "Non-All-Star")

## visualizing the results by group
ggplot(combined_clusters, aes(x = group_type, y = pts_per_game, fill = group_type)) +
  geom_boxplot() +
  labs(
    title = "Points Per Game: All-Star vs Non-All-Star Players by Cluster",
    x = "Group",
    y = "Points Per Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(combined_clusters, aes(x = group_type, y = ast_per_game, fill = group_type)) +
  geom_boxplot() +
  labs(
    title = "Assists Per Game: All-Star vs Non-All-Star Players by Cluster",
    x = "Group",
    y = "Assists Per Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(combined_clusters, aes(x = group_type, y = trb_per_game, fill = group_type)) +
  geom_boxplot() +
  labs(
    title = "Rebounds Per Game: All-Star vs Non-All-Star Players by Cluster",
    x = "Group",
    y = "Rebounds Per Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(combined_clusters, aes(x = group_type, y = stl_per_game, fill = group_type)) +
  geom_boxplot() +
  labs(
    title = "Steals Per Game: All-Star vs Non-All-Star Players by Cluster",
    x = "Group",
    y = "Steals Per Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(combined_clusters, aes(x = group_type, y = blk_per_game, fill = group_type)) +
  geom_boxplot() +
  labs(
    title = "Blocks Per Game: All-Star vs Non-All-Star Players by Cluster",
    x = "Group",
    y = "Blocks Per Game"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

## clustering by position for specific stats
main_stats <- c("pts_per_game", "ast_per_game", "trb_per_game", "stl_per_game", "blk_per_game", "all_star")

## joining tables to get position field
joined_data <- merge(player_facts, player_players, by = "player_id")

## subseting data by position for specific stats
PG_data <- joined_data %>% filter(pos == "PG") %>% dplyr::select(all_of(main_stats))
SG_data <- joined_data %>% filter(pos == "SG") %>% dplyr::select(all_of(main_stats))
SF_data <- joined_data %>% filter(pos == "SF") %>% dplyr::select(all_of(main_stats))
PF_data <- joined_data %>% filter(pos == "PF") %>% dplyr::select(all_of(main_stats))
C_data  <- joined_data %>% filter(pos == "C")  %>% dplyr::select(all_of(main_stats))

## creating positional all star subsets
PG_all_star <- PG_data %>%
  dplyr::filter(all_star==1)
PG_non_all_star <- PG_data %>%
  dplyr::filter(all_star==0)
SG_all_star <- SG_data %>%
  dplyr::filter(all_star==1)
SG_non_all_star <- SG_data %>%
  dplyr::filter(all_star==0)
PF_all_star <- PF_data %>%
  dplyr::filter(all_star==1)
PF_non_all_star <- PF_data %>%
  dplyr::filter(all_star==0)
SF_all_star <- SF_data %>%
  dplyr::filter(all_star==1)
SF_non_all_star <- SF_data %>%
  dplyr::filter(all_star==0)
C_all_star <- C_data %>%
  dplyr::filter(all_star==1)
C_non_all_star <- C_data %>%
  dplyr::filter(all_star==0)

## positional all-star clustering
set.seed(123)
kmeans_PG_all_star <- kmeans(PG_all_star, centers = 1) 
kmeans_PG_all_star$centers


set.seed(123)
kmeans_PG_non_all_star <- kmeans(PG_non_all_star, centers = 1) 
kmeans_PG_non_all_star$centers


set.seed(123)
kmeans_SG_all_star <- kmeans(SG_all_star, centers = 1) 
kmeans_SG_all_star$centers

set.seed(123)
kmeans_SG_non_all_star <- kmeans(SG_non_all_star, centers = 1) 
kmeans_SG_non_all_star$centers


set.seed(123)
kmeans_PF_all_star <- kmeans(PF_all_star, centers = 1) 
kmeans_PF_all_star$centers

set.seed(123)
kmeans_PF_non_all_star <- kmeans(PF_non_all_star, centers = 1) 
kmeans_PF_non_all_star$centers


set.seed(123)
kmeans_SF_all_star <- kmeans(SF_all_star, centers = 1) 
kmeans_SF_all_star$centers


set.seed(123)
kmeans_SF_non_all_star <- kmeans(SF_non_all_star, centers = 1) 
kmeans_SF_non_all_star$centers


set.seed(123)
kmeans_C_all_star <- kmeans(C_all_star, centers = 1) 
kmeans_C_all_star$centers


set.seed(123)
kmeans_C_non_all_star <- kmeans(C_non_all_star, centers = 1) 
kmeans_C_non_all_star$centers

## getting all Center data then plotting their historic 3pt attempts
center_data <- joined_data %>%
  dplyr::filter(pos == "C")


ggplot(center_data, aes(x = season_id, y =x3pa)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(
    title = "Three-Point Attempts by Centers Over the Years",
    x = "Season ID",
    y = "3pt Attempts"
  ) +
  theme_minimal()

## creating subset for teams that reached the playoffs
playoff_teams <- team_facts %>%
  dplyr::filter(playoffs==TRUE)

print(summary(playoff_teams$w))

summary(player_facts$mp)
playoff_teams <- playoff_teams %>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id) %>%
  dplyr::select(-sos) %>%
  dplyr::select(-f_tr) %>%
  dplyr::select(-mp_per_game) %>%
  dplyr::select(-tov_percent) %>%
  dplyr::select(-ft_fga) %>%
  dplyr::select(-arena) %>%
  dplyr::select(-srs) %>%
  dplyr::select(-pf_per_game) %>%
  dplyr::select(-opp_ft_fga) %>%
  dplyr::select(-opp_drb_percent) %>%
  dplyr::select(-opp_tov_percent) %>%
  dplyr::select(-g) %>%
  dplyr::select(-mov)


colnames(playoff_teams)

## clustering for playoff teams
set.seed(123)
kmeans_playoff_teams <- kmeans(playoff_teams, centers = 1) 
kmeans_playoff_teams$centers

## repeating for non playoff teams
non_playoff_teams  <- team_facts %>%
  dplyr::filter(playoffs == FALSE)
non_playoff_teams <- non_playoff_teams %>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id) %>%
  dplyr::select(-sos) %>%
  dplyr::select(-f_tr) %>%
  dplyr::select(-mp_per_game) %>%
  dplyr::select(-tov_percent) %>%
  dplyr::select(-ft_fga) %>%
  dplyr::select(-arena) %>%
  dplyr::select(-srs) %>%
  dplyr::select(-pf_per_game) %>%
  dplyr::select(-opp_ft_fga) %>%
  dplyr::select(-opp_drb_percent) %>%
  dplyr::select(-opp_tov_percent) %>%
  dplyr::select(-g) %>%
  dplyr::select(-mov)

non_playoff_clustering <- kmeans(non_playoff_teams, centers = 1)
non_playoff_clustering$centers


## shot attempts for both groups- interesting find here. Teams shoot roughly the same number of 
## attempts, yet the top players shoot so many more times as we will see shortly. 
summary(playoff_teams$fg_per_game)
summary(non_playoff_teams$fg_per_game)

## using silhouette score to determine ideal number of clusters for players
silhouette_scores <- numeric() 

for (k in 2:10) {  
  kmeans_model <- kmeans(top_players, centers = k, nstart = 25)
  silhouette <- silhouette(kmeans_model$cluster, dist(top_players))
  silhouette_scores[k] <- mean(silhouette[, 3])  
}

## cluster values to test
k_values <- 2:10  

## plotting the results for easy visual comparison
ggplot(data.frame(k = k_values, score = silhouette_scores[2:10]), aes(x = k, y = score)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette Scores for Different Numbers of Clusters",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Score"
  ) +
  theme_minimal()

## using WCSS score to determine ideal number of clusters
set.seed(95)
wss_score<- NULL
for (i in 1:10){
  fit = kmeans(top_players ,centers = i)
  wss_score = c(wss_score, fit$tot.withinss)
}
plot(1:10, wss_score, type = "o")

## clustering using value identified (4)
set.seed(123)
all_players_kmeans <- kmeans(top_players[main_stats], centers = 4)
all_players_kmeans$centers
all_players_clustering <- kmeans(cluster_stats, centers =4)
all_players_clustering$centers

## checking clustering results compared to all star selections
ground_truth <- cluster_stats$all_star
contingency_table <- table(Cluster = all_players_clustering$cluster, all_star = ground_truth)
print(contingency_table)


## splitting the data into best and worst datasets using the quantile values for wins

wins_quantiles <- quantile(top_players$w, probs = c(0.25, 0.75), na.rm = TRUE)
summary(top_players$w)
Q1 <- wins_quantiles[1]
Q3 <- wins_quantiles[2]

## worst players are below first quartile while best are above third quartile
worst_players <- subset(top_players, w < Q1)
best_players <- subset(top_players, w > Q3)

## checking the 5 number summary
summary(best_players)
summary(worst_players)

## defining key metrics to examine
metrics <- c("pts_per_game", "ast_per_game", "trb_per_game", "stl_per_game", "blk_per_game")

sapply(worst_players[metrics], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))

sapply(best_players[metrics], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))

## best players ahead in every metric, only slightly but still ahead across the board

## using silhouette score to determine ideal number of clusters for worst players
silhouette_scores <- numeric() 

for (k in 2:10) {  
  kmeans_model <- kmeans(worst_players, centers = k, nstart = 25)
  silhouette <- silhouette(kmeans_model$cluster, dist(worst_players))
  silhouette_scores[k] <- mean(silhouette[, 3])  
}

## cluster values to test
k_values <- 2:10  

## plotting the results for easy visual comparison
ggplot(data.frame(k = k_values, score = silhouette_scores[2:10]), aes(x = k, y = score)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette Scores for Different Numbers of Clusters",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Score"
  ) +
  theme_minimal()

## same thing for the best players

silhouette_scores <- numeric()

for (k in 2:10) {  
  kmeans_model <- kmeans(best_players, centers = k, nstart = 25)
  silhouette <- silhouette(kmeans_model$cluster, dist(best_players))
  silhouette_scores[k] <- mean(silhouette[, 3])  
}


ggplot(data.frame(k = k_values, score = silhouette_scores[2:10]), aes(x = k, y = score)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette Scores for Different Numbers of Clusters",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Score"
  ) +
  theme_minimal()

## clustering for each dataset using determined number of clusters
set.seed(123)
worst_kmeans <- kmeans(worst_players[metrics], centers = 2)
best_kmeans <- kmeans(best_players[metrics], centers = 2)

worst_kmeans$centers
best_kmeans$centers

worst_centers <- as.data.frame(worst_kmeans$centers)
worst_centers$Cluster <- paste0("Worst_Cluster", 1:nrow(worst_centers))

best_centers <- as.data.frame(best_kmeans$centers)
best_centers$Cluster <- paste0("Best_Cluster", 1:nrow(best_centers))

## combining the results for visual comparison
comparison <- bind_rows(
  worst_centers %>% pivot_longer(-Cluster, names_to = "Stat", values_to = "Value"),
  best_centers %>% pivot_longer(-Cluster, names_to = "Stat", values_to = "Value")
)

print(comparison)

## plotting clustering results
ggplot(comparison, aes(x = Stat, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Cluster Centers for Player Stats",
    x = "Statistic",
    y = "Value",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## showing the disparity in the values
summary(best_players$fga)
summary(worst_players$fga)
summary(best_players$fta)
summary(worst_players$fta)

## selecting the data for the best regular season team of all time
best_team <- team_facts %>%
  dplyr::filter(w == 73)
print(best_team)

## beginning second stage of modeling using new hyper parameters starting with the 42-win 
## threshold response adjustments
w_threshold <- 42

player_threshold_avg42 <- player_averages
player_threshold_avg42 <- player_threshold_avg42 %>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-mp)

player_threshold_avg42$w <- ifelse(player_averages$w > w_threshold, 1, 0)


player_threshold_bucket42 <- player_buckets
player_threshold_bucket42$w <- ifelse(player_buckets$w > w_threshold, 1, 0)
player_threshold_bucket42 <- player_threshold_bucket42 %>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-mp)

## treating class imbalance

class_counts_a42 <- table(player_threshold_avg42$w)

min_class_size <- min(class_counts_a42)


player_threshold_avg42 <- player_threshold_avg42 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

class_counts_b42 <- table(player_threshold_bucket42$w)


min_class_size1 <- min(class_counts_b42)

player_threshold_bucket42 <- player_threshold_bucket42 %>%
  group_by(w) %>%
  sample_n(min_class_size1) %>%
  ungroup()



## modeling using the player dataset. logistic regression now that response is binary, and
## confusion matrix relied on for evaluating random forest. also changed data and response to 
## features before modeling with random forest. final hyper parameters introduced, as well.
## modeling begins with player averages, 42-win threshold
set.seed(99)
split_ratio <- sample.split(player_threshold_avg42$w, SplitRatio = 0.75)
avg_42threshold_training <- subset(player_threshold_avg42, split_ratio == TRUE)
avg_42threshold_test <- subset(player_threshold_avg42, split_ratio == FALSE)
actual_w <- avg_42threshold_test$w


avg_42threshold_lm_model <- glm(w ~ ., data = avg_42threshold_training)
avg_42threshold_lm_predictions <- predict(avg_42threshold_lm_model, newdata = avg_42threshold_test)
print(mae(avg_42threshold_lm_predictions, actual_w))
summary(avg_42threshold_lm_model)


actual_w <- as.factor(actual_w)
avg_42threshold_training$w <- as.factor(avg_42threshold_training$w)
avg_42threshold_test$w <- as.factor(avg_42threshold_test$w)
avg_42threshold_training1 <- avg_42threshold_training %>%
  dplyr::select(-w)
player_rf_avg42_tuned <- 
  randomForest(
    x = avg_42threshold_training1,
    y = avg_42threshold_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

avg_42threshold_rf_predictions <- predict(player_rf_avg42_tuned, newdata = avg_42threshold_test)
avg_42rf_conf_matrix <- confusionMatrix(avg_42threshold_rf_predictions, actual_w)
print(avg_42rf_conf_matrix)

## same thing using the player buckets

set.seed(99)
split_ratio <- sample.split(player_threshold_bucket42$w, SplitRatio = 0.75)
bucket_42threshold_training <- subset(player_threshold_bucket42, split_ratio == TRUE)
bucket_42threshold_test <- subset(player_threshold_bucket42, split_ratio == FALSE)
actual_w <- bucket_42threshold_test$w
actual_w <- as.factor(actual_w)
bucket_42threshold_training$w <- as.factor(bucket_42threshold_training$w)
bucket_42threshold_test$w <- as.factor(bucket_42threshold_test$w)
bucket_42threshold_training1 <- bucket_42threshold_training %>%
  dplyr::select(-w)
player_rf_bucket42_tuned <- 
  randomForest(
    x = bucket_42threshold_training1,
    y = bucket_42threshold_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
bucket_42threshold_rf_predictions <- predict(player_rf_bucket42_tuned, newdata = bucket_42threshold_test)
bucket_42rf_conf_matrix <- confusionMatrix(bucket_42threshold_rf_predictions, actual_w)
print(bucket_42rf_conf_matrix)


## repeating with 50 win threshold
w_threshold <- 50

player_threshold_avg50 <- player_averages
player_threshold_avg50 <- player_threshold_avg50 %>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-mp)
player_threshold_avg50$w <- ifelse(player_averages$w > w_threshold, 1, 0)


player_threshold_bucket50 <- player_buckets
player_threshold_bucket50$w <- ifelse(player_buckets$w > w_threshold, 1, 0)
player_threshold_bucket50 <- player_threshold_bucket50 %>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-mp)

## treating class imbalance

class_counts_a50 <- table(player_threshold_avg50$w)

min_class_size <- min(class_counts_a50)


player_threshold_avg50 <- player_threshold_avg50 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

class_counts_b50 <- table(player_threshold_bucket50$w)


min_class_size1 <- min(class_counts_b50)

player_threshold_bucket50 <- player_threshold_bucket50 %>%
  group_by(w) %>%
  sample_n(min_class_size1) %>%
  ungroup()


## modeling using the player datasets and 50 win threshold, starting with player averages
set.seed(99)
split_ratio <- sample.split(player_threshold_avg50$w, SplitRatio = 0.75)
avg_50threshold_training <- subset(player_threshold_avg50, split_ratio == TRUE)
avg_50threshold_test <- subset(player_threshold_avg50, split_ratio == FALSE)
actual_w <- avg_50threshold_test$w


avg_50threshold_lm_model <- glm(w ~ ., data = avg_50threshold_training)
avg_50threshold_lm_predictions <- predict(avg_50threshold_lm_model, newdata = avg_50threshold_test)
print(mae(avg_50threshold_lm_predictions, actual_w))
summary(avg_50threshold_lm_model)


actual_w <- as.factor(actual_w)
avg_50threshold_training$w <- as.factor(avg_50threshold_training$w)
avg_50threshold_test$w <- as.factor(avg_50threshold_test$w)
avg_50threshold_training1 <- avg_50threshold_training %>%
  dplyr::select(-w)
player_rf_avg50_tuned <- 
  randomForest(
    x = avg_50threshold_training1,
    y = avg_50threshold_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )


avg_50threshold_rf_predictions <- predict(player_rf_avg50_tuned, newdata = avg_50threshold_test)
avg_50rf_conf_matrix <- confusionMatrix(avg_50threshold_rf_predictions, actual_w)
print(avg_50rf_conf_matrix)
## saving model, as this achieved best results during my final test
saveRDS(avg_50threshold_training1, file = "best_player_rf.rds")
## same thing using the player buckets

set.seed(99)
split_ratio <- sample.split(player_threshold_bucket50$w, SplitRatio = 0.75)
bucket_50threshold_training <- subset(player_threshold_bucket50, split_ratio == TRUE)
bucket_50threshold_test <- subset(player_threshold_bucket50, split_ratio == FALSE)
actual_w <- bucket_50threshold_test$w
actual_w <- as.factor(actual_w)
bucket_50threshold_training$w <- as.factor(bucket_50threshold_training$w)
bucket_50threshold_test$w <- as.factor(bucket_50threshold_test$w)
bucket_50threshold_training1 <- bucket_50threshold_training %>%
  dplyr::select(-w)
player_rf_bucket50_tuned <- 
  randomForest(
    x = bucket_50threshold_training1,
    y = bucket_50threshold_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
bucket_50threshold_rf_predictions <- predict(player_rf_bucket50_tuned, newdata = bucket_50threshold_test)
bucket_50rf_conf_matrix <- confusionMatrix(bucket_50threshold_rf_predictions, actual_w)
print(bucket_50rf_conf_matrix)


## create the categories based on quartile values below
summary(player_facts$w)
player_avg_categories <- player_averages
quartiles <- quantile(player_averages$w, probs = c(0.25, 0.75))

player_avg_categories$w <- cut(player_averages$w, 
                               breaks = c(-Inf, quartiles, Inf), 
                               labels = c("Low", "Medium", "High"), 
                               right = FALSE)

## check results and adjust imbalance
head(player_avg_categories$w)
cat_counts_avg <- table(player_avg_categories$w)


min_class_size <- min(cat_counts_avg)

balanced_cat_avg <- player_avg_categories %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

# check the results
table(balanced_cat_avg$w)

## categorical player average modeling
set.seed(99)
split_ratio <- sample.split(balanced_cat_avg$w, SplitRatio = 0.75)
category_avg_training <- subset(balanced_cat_avg, split_ratio == TRUE)
category_avg_test <- subset(balanced_cat_avg, split_ratio == FALSE)
actual_w <- category_avg_test$w

category_avg_training1 <- category_avg_training %>%
  dplyr::select(-w)
avg_category_rf_model <- 
  randomForest(
    x = category_avg_training1,
    y = category_avg_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

avg_category_rf_predictions <- predict(avg_category_rf_model, newdata = category_avg_test)
avg_category_conf_matrix <- confusionMatrix(avg_category_rf_predictions, actual_w)
print(avg_category_conf_matrix)


## modeling using the binary threshold with the aggregated data

aggregated_data_thresh <- aggregated_data
aggregated_data_thresh$w <- ifelse(aggregated_data$w > w_threshold, 1, 0)

set.seed(99)
split_ratio <- sample.split(aggregated_data_thresh$w, SplitRatio = 0.75)
agg_thresh_training <- subset(aggregated_data_thresh, split_ratio == TRUE)
agg_thresh_test <- subset(aggregated_data_thresh, split_ratio == FALSE)
agg_w <- agg_thresh_test$w

agg_thresh_training$w <- as.factor(agg_thresh_training$w)
agg_thresh_test$w <- as.factor(agg_thresh_test$w)
agg_w <- as.factor(agg_w)
agg_thresh_training1 <- agg_thresh_training %>%
  dplyr::select(-w)

agg42_rf_model <- 
  randomForest(
    x = agg_thresh_training1,
    y = agg_thresh_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

agg_thresh_rf_predictions <- predict(agg42_rf_model, newdata = agg_thresh_test)
agg_rf_conf_matrix <- confusionMatrix(agg_thresh_rf_predictions, agg_w)
print(agg_rf_conf_matrix)

## team modeling using 42 game threshold


w_threshold <- 42
team_modeling1$w <- team_modeling$w
team_threshold42 <- team_modeling1
team_threshold42$w <- ifelse(team_threshold42$w > w_threshold, 1, 0)


class_counts3 <- table(team_threshold42$w)
print(class_counts3)

min_class_size <- min(class_counts3)

team_threshold42 <- team_threshold42 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

class_counts4 <- table(team_threshold42$w)
print(class_counts4)

## modeling using the team threshold data
set.seed(99)
split_ratio <- sample.split(team_threshold42$w, SplitRatio = 0.75)
team_42threshold_training <- subset(team_threshold42, split_ratio == TRUE)
team_42threshold_test <- subset(team_threshold42, split_ratio == FALSE)
actual_w <- team_42threshold_test$w


team_42threshold_lm_model <- glm(w ~ ., data = team_42threshold_training)
team_42threshold_lm_predictions <- predict(team_42threshold_lm_model, newdata = team_42threshold_test)
print(mae(team_42threshold_lm_predictions, actual_w))
summary(team_42threshold_lm_model)


actual_w <- as.factor(actual_w)
team_42threshold_training$w <- as.factor(team_42threshold_training$w)
team_42threshold_test$w <- as.factor(team_42threshold_test$w)
team_42threshold_training1 <- team_42threshold_training %>%
  dplyr::select(-w)
team_42threshold_rf_model <- 
  randomForest(
    x = team_42threshold_training1,
    y = team_42threshold_training$w,
    mtry = 7,          
    ntree = 100,      
    nodesize = 10       
  )

team_42threshold_rf_predictions <- predict(team_42threshold_rf_model, newdata = team_42threshold_test)
team_42rf_conf_matrix <- confusionMatrix(team_42threshold_rf_predictions, actual_w)
print(team_42rf_conf_matrix)


## repeating the process using the 50 win threshold
w_threshold <- 50

team_50threshold <- team_modeling1
team_50threshold$w <- ifelse(team_50threshold$w > w_threshold, 1, 0)


class_counts_t50 <- table(team_50threshold$w)
print(class_counts_t50)

min_class_size <- min(class_counts_t50)

team_50threshold <- team_50threshold %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

class_counts200 <- table(team_50threshold$w)
print(class_counts200)

set.seed(99)
split_ratio <- sample.split(team_50threshold$w, SplitRatio = 0.75)
team_50threshold_training <- subset(team_50threshold, split_ratio == TRUE)
team_50threshold_test <- subset(team_50threshold, split_ratio == FALSE)
actual_w <- team_50threshold_test$w


team_50threshold_lm_model <- glm(w ~ ., data = team_50threshold_training)
team_50threshold_lm_predictions <- predict(team_50threshold_lm_model, newdata = team_50threshold_test)
print(mae(team_50threshold_lm_predictions, actual_w))
summary(team_50threshold_lm_model)


actual_w <- as.factor(actual_w)
team_50threshold_training$w <- as.factor(team_50threshold_training$w)
team_50threshold_test$w <- as.factor(team_50threshold_test$w)
team_50threshold_training1 <- team_50threshold_training %>%
  dplyr::select(-w)
team_50threshold_rf_model <- 
  randomForest(
    x = team_42threshold_training1,
    y = team_42threshold_training$w,
    mtry = 7,          
    ntree = 100,      
    nodesize = 10       
  )

team_50threshold_rf_predictions <- predict(team_50threshold_rf_model, newdata = team_50threshold_test)
team_50rf_conf_matrix <- confusionMatrix(team_50threshold_rf_predictions, actual_w)
print(team_50rf_conf_matrix)


## refining the team dataset to model again
team_42threshold2 <- team_threshold42 %>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-fga_per_game)%>%
  dplyr::select(-x3pa_per_game)%>%
  dplyr::select(-x2pa_per_game)

## modeling using refined team dataset
set.seed(99)
split_ratio <- sample.split(team_42threshold2$w, SplitRatio = 0.75)
team_42threshold_training2 <- subset(team_42threshold2, split_ratio == TRUE)
team_42threshold_test2 <- subset(team_42threshold2, split_ratio == FALSE)
actual_w <- team_42threshold_test2$w


team_42threshold_lm_model2 <- glm(w ~ ., data = team_42threshold_training2)
team_42threshold_lm_predictions2 <- predict(team_42threshold_lm_model2, newdata = team_42threshold_test2)
print(mae(team_42threshold_lm_predictions2, actual_w))
summary(team_42threshold_lm_model2)


actual_w <- as.factor(actual_w)
team_42threshold_training2$w <- as.factor(team_42threshold_training2$w)
team_42threshold_test2$w <- as.factor(team_42threshold_test2$w)
team_42threshold_training3 <- team_42threshold_training2 %>%
  dplyr::select(-w)

team_42threshold_rf_model2 <- 
  randomForest(
    x = team_42threshold_training3,
    y = team_42threshold_training2$w,
    mtry = 7,          
    ntree = 100,      
    nodesize = 10       
  )


team_42threshold_rf_predictions2 <- predict(team_42threshold_rf_model2, newdata = team_42threshold_test2)
team_42rf_conf_matrix2 <- confusionMatrix(team_42threshold_rf_predictions2, actual_w)
print(team_42rf_conf_matrix2)


## repeating the process with the 50 win threshold
team_50threshold2 <- team_modeling1
team_50threshold2$w <- ifelse(team_50threshold2$w > w_threshold, 1, 0)
team_50modeling2 <- team_50threshold %>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-fga_per_game)%>%
  dplyr::select(-x3pa_per_game)%>%
  dplyr::select(-x2pa_per_game)


class_counts502 <- table(team_50threshold2$w)
print(class_counts502)

min_class_size <- min(class_counts502)

team_50threshold2 <- team_50threshold2 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

class_counts503 <- table(team_50threshold2$w)
print(class_counts503)

set.seed(99)
split_ratio <- sample.split(team_50threshold2$w, SplitRatio = 0.75)
team_50threshold_training2 <- subset(team_50threshold2, split_ratio == TRUE)
team_50threshold_test2 <- subset(team_50threshold2, split_ratio == FALSE)
actual_w <- team_50threshold_test2$w


team_50threshold_lm_model2 <- glm(w ~ ., data = team_50threshold_training2)
team_50threshold_lm_predictions2 <- predict(team_50threshold_lm_model2, newdata = team_50threshold_test2)
print(mae(team_50threshold_lm_predictions2, actual_w))
summary(team_50threshold_lm_model2)


actual_w <- as.factor(actual_w)
team_50threshold_training2$w <- as.factor(team_50threshold_training2$w)
team_50threshold_test2$w <- as.factor(team_50threshold_test2$w)

team_50threshold_training3 <- team_50threshold_training2 %>%
  dplyr::select(-w)
team_50threshold_rf_model2 <- 
  randomForest(
    x = team_50threshold_training3,
    y = team_50threshold_training2$w,
    mtry = 7,          
    ntree = 100,      
    nodesize = 10       
  )

team_50threshold_rf_predictions2 <- predict(team_50threshold_rf_model2, newdata = team_50threshold_test2)
team_50rf_conf_matrix2 <- confusionMatrix(team_50threshold_rf_predictions2, actual_w)
print(team_50rf_conf_matrix2)
## saving model, as this one achieved the best performance out of the team models
saveRDS(team_50threshold_rf_model2,file = "best_team_rf.rds")
summary(team_modeling$w)

## modeling using categories with the team dataset. defining the quartiles and adjusting the data
team_categories <- team_modeling
quartiles1 <- quantile(team_modeling$w, probs = c(0.25, 0.75))

team_categories$w <- cut(team_modeling$w, 
                         breaks = c(-Inf, quartiles1, Inf), 
                         labels = c("Low", "Medium", "High"), 
                         right = FALSE)

## checking the results and adjusting for class imbalance
head(team_categories$w)
class_counts7 <- table(team_categories$w)

min_class_size <- min(class_counts7)

team_categories <- team_categories %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

table(team_categories$w)

## categorical modeling using the team dataset
set.seed(99)
split_ratio <- sample.split(team_categories$w, SplitRatio = 0.75)
team_category_training <- subset(team_categories, split_ratio == TRUE)
team_category_test <- subset(team_categories, split_ratio == FALSE)
actual_w <- team_category_test$w

team_category_training1 <- team_category_training %>%
  dplyr::select(-w)
team_category_rf_model <- 
  randomForest(
    x = team_category_training1,
    y = team_category_training$w,
    mtry = 7,          
    ntree = 100,      
    nodesize = 10       
  )
team_category_rf_predictions <- predict(team_category_rf_model, newdata = team_category_test)
team_category_conf_matrix <- confusionMatrix(team_category_rf_predictions, actual_w)
print(team_category_conf_matrix)


## refining the player averages once again before modeling
player_42averages4 <- player_threshold_avg42 %>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-x2p_percent)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)


## modeling using the again refined player averages and 42 win threshold
set.seed(99)
split_ratio <- sample.split(player_42averages4$w, SplitRatio = 0.75)
averages5_training <- subset(player_42averages4, split_ratio == TRUE)
averages5_test <- subset(player_42averages4, split_ratio == FALSE)
actual_w <- averages5_test$w

avg5_threshold_lm_model <- glm(w ~ ., data = averages5_training)
avg5_threshold_lm_predictions <- predict(avg5_threshold_lm_model, newdata = averages5_test)
print(mae(avg5_threshold_lm_predictions, actual_w))
summary(avg5_threshold_lm_model)

actual_w <- as.factor(actual_w)
averages5_training$w <- as.factor(averages5_training$w)
averages5_test$w <- as.factor(averages5_test$w)
averages5_training1 <- averages5_training %>%
  dplyr::select(-w)
avg5_threshold_rf_model <- 
  randomForest(
    x = averages5_training1,
    y = averages5_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
avg5_threshold_rf_predictions <- predict(avg5_threshold_rf_model, newdata = averages5_test)
avg5_rf_conf_matrix <- confusionMatrix(avg5_threshold_rf_predictions, actual_w)
print(avg5_rf_conf_matrix)


### then repeating refining player buckets this time
player_42buckets2 <- player_threshold_bucket42 %>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-x2p_percent)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)



## modeling using second refined bucket data
set.seed(99)
split_ratio <- sample.split(player_42buckets2$w, SplitRatio = 0.75)
buckets5_training <- subset(player_42buckets2, split_ratio == TRUE)
buckets5_test <- subset(player_42buckets2, split_ratio == FALSE)
actual_w <- buckets5_test$w


bucket5_threshold_lm_model <- glm(w ~ ., data = buckets5_training)
bucket5_threshold_lm_predictions <- predict(bucket5_threshold_lm_model, newdata = buckets5_test)
print(mae(bucket5_threshold_lm_predictions, actual_w))
summary(bucket5_threshold_lm_model)


actual_w <- as.factor(actual_w)
buckets5_training$w <- as.factor(buckets5_training$w)
buckets5_test$w <- as.factor(buckets5_test$w)
buckets5_training1 <- buckets5_training %>%
  dplyr::select(-w)
buckets5_threshold_rf_model <- 
  randomForest(
    x = buckets5_training1,
    y = buckets5_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
buckets5_threshold_rf_predictions <- predict(buckets5_threshold_rf_model, newdata = buckets5_test)
buckets5_rf_conf_matrix <- confusionMatrix(buckets5_threshold_rf_predictions, actual_w)
print(buckets5_rf_conf_matrix)


## repeating the process using the 50 win threshold, starting with player averages
w_threshold <- 50

player_averages6 <- player_threshold_avg50 %>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)%>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-tov)

class_counts15 <- table(player_averages6$w)

min_class_size <- min(class_counts15)

player_averages6 <- player_averages6 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

table(player_averages6$w)

## modeling using 50 win threshold and refined average data
set.seed(99)
split_ratio <- sample.split(player_averages6$w, SplitRatio = 0.75)
averages6_training <- subset(player_averages6, split_ratio == TRUE)
averages6_test <- subset(player_averages6, split_ratio == FALSE)
actual_w <- averages6_test$w

avg6_threshold_lm_model <- glm(w ~ ., data = averages6_training)
avg6_threshold_lm_predictions <- predict(avg6_threshold_lm_model, newdata = averages6_test)
print(mae(avg6_threshold_lm_predictions, actual_w))
summary(avg6_threshold_lm_model)


actual_w <- as.factor(actual_w)
averages6_training$w <- as.factor(averages6_training$w)
averages6_test$w <- as.factor(averages6_test$w)
averages6_training1 <- averages6_training %>%
  dplyr::select(-w)
avg6_threshold_rf_model <- 
  randomForest(
    x = averages6_training1,
    y = averages6_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
avg6_threshold_rf_predictions <- predict(avg6_threshold_rf_model, newdata = averages6_test)
avg6_rf_conf_matrix <- confusionMatrix(avg6_threshold_rf_predictions, actual_w)
print(avg6_rf_conf_matrix)

## repeating the process using buckets, refining the data then modeling with the 50 win threshold

player_buckets6 <- player_buckets%>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-mp)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-tov)


player_buckets6$w <- ifelse(player_buckets6$w > w_threshold, 1, 0)
class_counts90 <- table(player_buckets6$w)
print(class_counts90)
min_class_size <- min(class_counts90)

player_buckets6 <- player_buckets6 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

set.seed(99)
split_ratio <- sample.split(player_buckets6$w, SplitRatio = 0.75)
buckets6_training <- subset(player_buckets6, split_ratio == TRUE)
buckets6_test <- subset(player_buckets6, split_ratio == FALSE)
actual_w <- buckets6_test$w

bucket6_threshold_lm_model <- glm(w ~ ., data = buckets6_training)
bucket6_threshold_lm_predictions <- predict(bucket6_threshold_lm_model, newdata = buckets6_test)
print(mae(bucket6_threshold_lm_predictions, actual_w))
summary(bucket6_threshold_lm_model)


actual_w <- as.factor(actual_w)
buckets6_training$w <- as.factor(buckets6_training$w)
buckets6_test$w <- as.factor(buckets6_test$w)
buckets6_training1 <- buckets6_training %>%
  dplyr::select(-w)
buckets6_threshold_rf_model <- 
  randomForest(
    x = buckets6_training1,
    y = buckets6_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
buckets6_threshold_rf_predictions <- predict(buckets6_threshold_rf_model, newdata = buckets6_test)
buckets6_rf_conf_matrix <- confusionMatrix(buckets6_threshold_rf_predictions, actual_w)
print(buckets6_rf_conf_matrix)


## repeating refined bucket modeling using the 42 win threshold
w_threshold <- 42

player_averages6 <- player_averages %>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-mp)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)%>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-tov)

player_averages6$w <- ifelse(player_averages6$w > w_threshold, 1, 0)
class_counts80 <- table(player_averages6$w)
print(class_counts80)
min_class_size <- min(class_counts80)

player_averages6 <- player_averages6 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

set.seed(99)
split_ratio <- sample.split(player_averages6$w, SplitRatio = 0.75)
averages7_training <- subset(player_averages6, split_ratio == TRUE)
averages7_test <- subset(player_averages6, split_ratio == FALSE)
actual_w <- averages7_test$w

avg7_threshold_lm_model <- glm(w ~ ., data = averages7_training)
avg7_threshold_lm_predictions <- predict(avg7_threshold_lm_model, newdata = averages7_test)
print(mae(avg7_threshold_lm_predictions, actual_w))
summary(avg7_threshold_lm_model)


actual_w <- as.factor(actual_w)
averages7_training$w <- as.factor(averages7_training$w)
averages7_test$w <- as.factor(averages7_test$w)
averages7_training1 <- averages7_training %>%
  dplyr::select(-w)
avg7_threshold_rf_model <- 
  randomForest(
    x = averages7_training1,
    y = averages7_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
avg7_threshold_rf_predictions <- predict(avg7_threshold_rf_model, newdata = averages7_test)
avg7_rf_conf_matrix <- confusionMatrix(avg7_threshold_rf_predictions, actual_w)
print(avg7_rf_conf_matrix)


## bucket modeling using refined data
player_buckets6 <- player_buckets%>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-mp)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-tov)

## adjusting response for 42 win threshold and adjusting class
player_buckets6$w <- ifelse(player_buckets6$w > w_threshold, 1, 0)
class_counts90 <- table(player_buckets6$w)
print(class_counts90)
min_class_size <- min(class_counts90)

player_buckets6 <- player_buckets6 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()


## modeling using 42 win threshold
set.seed(99)
split_ratio <- sample.split(player_buckets6$w, SplitRatio = 0.75)
buckets7_training <- subset(player_buckets6, split_ratio == TRUE)
buckets7_test <- subset(player_buckets6, split_ratio == FALSE)
actual_w <- buckets7_test$w

bucket7_threshold_lm_model <- glm(w ~ ., data = buckets7_training)
bucket7_threshold_lm_predictions <- predict(bucket7_threshold_lm_model, newdata = buckets7_test)
print(mae(bucket7_threshold_lm_predictions, actual_w))
summary(bucket7_threshold_lm_model)


actual_w <- as.factor(actual_w)
buckets7_training$w <- as.factor(buckets7_training$w)
buckets7_test$w <- as.factor(buckets7_test$w)
buckets7_training1 <- buckets7_training %>%
  dplyr::select(-w)
buckets7_threshold_rf_model <- 
  randomForest(
    x = buckets7_training1,
    y = buckets7_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

buckets7_threshold_rf_predictions <- predict(buckets7_threshold_rf_model, newdata = buckets7_test)
buckets7_rf_conf_matrix <- confusionMatrix(buckets7_threshold_rf_predictions, actual_w)
print(buckets7_rf_conf_matrix)

## defining positions
positions <- c("PG", "SG", "SF", "PF", "C")

## creating top players positionally
top_players_pos <- joined_data %>%
  filter(pos %in% positions) %>%          
  group_by(team_id, season_id, pos) %>%      
  slice_max(mp, n = 1, with_ties = FALSE) %>%  
  ungroup()

## checking results and eliminating features
head(top_players_pos)

top_players_pos <- top_players_pos %>%
  dplyr::select(-pts)%>%
  dplyr::select(-trb)%>%
  dplyr::select(-orb)%>%
  dplyr::select(-drb)%>%
  dplyr::select(-ast)%>%
  dplyr::select(-blk)%>%
  dplyr::select(-stl)%>%
  dplyr::select(-pf)%>%
  dplyr::select(-x3p)%>%
  dplyr::select(-player)%>%
  dplyr::select(-player_id)%>%
  dplyr::select(-pos)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-points_bucket)%>%
  dplyr::select(-assist_bucket)%>%
  dplyr::select(-steal_bucket)%>% 
  dplyr::select(-rebound_bucket)%>%
  dplyr::select(-oreb_bucket)%>%
  dplyr::select(-dreb_bucket)%>%
  dplyr::select(-block_bucket)%>%
  dplyr::select(-playoffs)



## modeling using the top players and 42 win threshold
w_threshold <- 42
top_players1 <- top_players
top_players1$w <- ifelse(top_players$w > w_threshold, 1, 0)

class_counts60 <- table(top_players1$w)
print(class_counts60)

min_class_size <- min(class_counts60)

top_players1 <- top_players1 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()

## creating top players averages
top_players1 <- top_players1 %>%
  dplyr::select(-points_bucket)%>%
  dplyr::select(-assist_bucket)%>%
  dplyr::select(-rebound_bucket)%>%
  dplyr::select(-oreb_bucket)%>%
  dplyr::select(-dreb_bucket)%>%
  dplyr::select(-steal_bucket)%>%
  dplyr::select(-block_bucket)%>%
  dplyr::select(-playoffs)


## modeling with top players averages
set.seed(99)
split_ratio <- sample.split(top_players1$w, SplitRatio = 0.75)
top_players_training <- subset(top_players1, split_ratio == TRUE)
top_players_test <- subset(top_players1, split_ratio == FALSE)
actual_w <- top_players_test$w


top_players_lm_model <- glm(w ~ ., data = top_players_training)
top_players_lm_predictions <- predict(top_players_lm_model, newdata = top_players_test)
print(mae(top_players_lm_predictions, actual_w))
summary(top_players_lm_model)


actual_w <- as.factor(actual_w)
top_players_training$w <- as.factor(top_players_training$w)
top_players_test$w <- as.factor(top_players_test$w)
top_players_training1 <- top_players_training %>%
  dplyr::select(-w)
top_players_rf_model <- 
  randomForest(
    x = top_players_training1,
    y = top_players_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

top_players_rf_predictions <- predict(top_players_rf_model, newdata = top_players_test)
top_players_rf_conf_matrix <- confusionMatrix(top_players_rf_predictions, actual_w)
print(top_players_rf_conf_matrix)

## preparing to model with 50 game threshold
top_players2 <- top_players
top_players2 <- top_players %>%
  dplyr::select(-points_bucket)%>%
  dplyr::select(-assist_bucket)%>%
  dplyr::select(-rebound_bucket)%>%
  dplyr::select(-oreb_bucket)%>%
  dplyr::select(-dreb_bucket)%>%
  dplyr::select(-steal_bucket)%>%
  dplyr::select(-block_bucket)%>%
  dplyr::select(-playoffs)

## modeling using the top players and 50 win threshold
w_threshold <- 50
top_players2$w <- ifelse(top_players$w > w_threshold, 1, 0)

class_counts60 <- table(top_players2$w)
print(class_counts60)

min_class_size <- min(class_counts60)

top_players2 <- top_players2 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()


## top players 50 game threshold using averages
set.seed(99)
split_ratio <- sample.split(top_players2$w, SplitRatio = 0.75)
top_players_training2 <- subset(top_players2, split_ratio == TRUE)
top_players_test2 <- subset(top_players2, split_ratio == FALSE)
actual_w <- top_players_test2$w


top_players2_lm_model <- glm(w ~ ., data = top_players_training2)
top_players2_lm_predictions <- predict(top_players2_lm_model, newdata = top_players_test2)
print(mae(top_players2_lm_predictions, actual_w))
summary(top_players2_lm_model)


actual_w <- as.factor(actual_w)
top_players_training2$w <- as.factor(top_players_training2$w)
top_players_test2$w <- as.factor(top_players_test2$w)
top_players_training3 <- top_players_training2 %>%
  dplyr::select(-w)
top_players_rf_model2 <- 
  randomForest(
    x = top_players_training3,
    y = top_players_training2$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

top_players2_rf_predictions <- predict(top_players_rf_model2, newdata = top_players_test2)
top_players2_rf_conf_matrix <- confusionMatrix(top_players2_rf_predictions, actual_w)
print(top_players2_rf_conf_matrix)

## repeating the process using the positional top players
w_threshold <- 42
top_positionally <- top_players_pos
top_positionally$w <- ifelse(top_positionally$w > w_threshold, 1, 0)

class_counts60 <- table(top_positionally$w)
print(class_counts60)

min_class_size <- min(class_counts60)

top_positionally <- top_positionally %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()


set.seed(99)
split_ratio <- sample.split(top_positionally$w, SplitRatio = 0.75)
top_pos_training <- subset(top_positionally, split_ratio == TRUE)
top_pos_test <- subset(top_positionally, split_ratio == FALSE)
actual_w <- top_pos_test$w


top_pos_lm_model <- glm(w ~ ., data = top_pos_training)
top_pos_lm_predictions <- predict(top_pos_lm_model, newdata = top_pos_test)
print(mae(top_pos_lm_predictions, actual_w))
summary(top_pos_lm_model)


actual_w <- as.factor(actual_w)
top_pos_training$w <- as.factor(top_pos_training$w)
top_pos_test$w <- as.factor(top_pos_test$w)
top_pos_training1 <- top_pos_training %>%
  dplyr::select(-w)
top_pos_rf_model <- 
  randomForest(
    x = top_pos_training1,
    y = top_pos_training$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
top_pos_rf_predictions <- predict(top_pos_rf_model, newdata = top_pos_test)
top_pos_rf_conf_matrix <- confusionMatrix(top_pos_rf_predictions, actual_w)
print(top_pos_rf_conf_matrix)

## 50-win threshold for top players by position
w_threshold <- 50
top_pos2 <- top_players_pos
top_pos2$w <- ifelse(top_pos2$w > w_threshold, 1, 0)

class_counts60 <- table(top_pos2$w)
print(class_counts60)

min_class_size <- min(class_counts60)

top_pos2 <- top_pos2 %>%
  group_by(w) %>%
  sample_n(min_class_size) %>%
  ungroup()


set.seed(99)
split_ratio <- sample.split(top_pos2$w, SplitRatio = 0.75)
top_pos_training2 <- subset(top_pos2, split_ratio == TRUE)
top_pos_test2 <- subset(top_pos2, split_ratio == FALSE)
actual_w <- top_pos_test2$w


top_pos2_lm_model <- glm(w ~ ., data = top_pos_training2)
top_pos2_lm_predictions <- predict(top_pos2_lm_model, newdata = top_pos_test2)
print(mae(top_pos2_lm_predictions, actual_w))
summary(top_pos2_lm_model)


actual_w <- as.factor(actual_w)
top_pos_training2$w <- as.factor(top_pos_training2$w)
top_pos_test2$w <- as.factor(top_pos_test2$w)
top_pos_training3 <- top_pos_training2 %>%
  dplyr::select(-w)
top_pos2_rf_model <- 
  randomForest(
    x = top_pos_training3,
    y = top_pos_training2$w,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

top_pos2_rf_predictions <- predict(top_pos2_rf_model, newdata = top_pos_test2)
top_pos2_rf_conf_matrix <- confusionMatrix(top_pos2_rf_predictions, actual_w)
print(top_pos2_rf_conf_matrix)


## finally, all-star modeling
non_numeric_cols <- names(team_facts)[sapply(team_facts, Negate(is.numeric))]
exclude_cols <- c("season_id", "team_id", "l", "g", non_numeric_cols)


team_facts1 <- team_facts[, !(names(team_facts) %in% exclude_cols)]

## boruta on the all star data
boruta_result <- Boruta(all_star ~ ., data = team_facts1, doTrace = 2, maxRuns = 100)
print(boruta_result)
confirmed_attributes <- getSelectedAttributes(boruta_result, withTentative = TRUE)


## all-star class standardization

class_counts <- table(player_averages$all_star)
min_class_size <- min(class_counts)

all_star_avg <- player_averages %>%
  group_by(all_star) %>%
  sample_n(min_class_size) %>%
  ungroup()


## all star predictions based on avg statistics

set.seed(99)
split_ratio <- sample.split(all_star_avg$all_star, SplitRatio = 0.75)
player_avg_all_star_train <- subset(all_star_avg, split_ratio == TRUE)
player_avg_all_star_test <- subset(all_star_avg, split_ratio == FALSE)
actual_all_star <- player_avg_all_star_test$all_star


avg_all_star_lm_model <- glm(all_star ~ ., data = player_avg_all_star_train)
avg_all_star_lm_predictions <- predict(avg_all_star_lm_model, newdata = player_avg_all_star_test)
print(mae(avg_all_star_lm_predictions, actual_all_star))
summary(avg_all_star_lm_model)

actual_all_star <- as.factor(actual_all_star)
player_avg_all_star_train$all_star <- as.factor(player_avg_all_star_train$all_star)
player_avg_all_star_test$all_star <- as.factor(player_avg_all_star_test$all_star)

player_avg_all_star_train1 <- player_avg_all_star_train %>%
  dplyr::select(-all_star)
player_avg_all_star <- 
  randomForest(
    x = player_avg_all_star_train1,
    y = player_avg_all_star_train$all_star,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
player_avg_all_star_rf_predictions <- predict(player_avg_all_star, newdata = player_avg_all_star_test)
player_avg_all_star_conf_matrix <- confusionMatrix(player_avg_all_star_rf_predictions, actual_all_star)
print(player_avg_all_star_conf_matrix)



## eliminating features and modeling again, all stars
all_star_avg1 <- all_star_avg %>%
  dplyr::select(-player_id)%>%
  dplyr::select(-team_id)%>%
  dplyr::select(-season_id)%>%
  dplyr::select(-g)%>%
  dplyr::select(-gs)%>%
  dplyr::select(-mp)%>%
  dplyr::select(-fg)%>%
  dplyr::select(-fga)%>%
  dplyr::select(-x3pa)%>%
  dplyr::select(-x2pa)%>%
  dplyr::select(-x2p)%>%
  dplyr::select(-fta)%>%
  dplyr::select(-ft)%>%
  dplyr::select(-playoffs)%>%
  dplyr::select(-w)


set.seed(99)
split_ratio <- sample.split(all_star_avg1$all_star, SplitRatio = 0.75)
player_avg_all_star_train2 <- subset(all_star_avg1, split_ratio == TRUE)
player_avg_all_star_test1 <- subset(all_star_avg1, split_ratio == FALSE)
actual_all_star <- player_avg_all_star_test1$all_star

actual_all_star <- as.factor(actual_all_star)
player_avg_all_star_train2$all_star <- as.factor(player_avg_all_star_train2$all_star)
player_avg_all_star_train$all_star <- as.factor(player_avg_all_star_train2$all_star)
player_avg_all_star_test1$all_star <- as.factor(player_avg_all_star_test1$all_star)

player_avg_all_star_train3 <- player_avg_all_star_train2 %>%
  dplyr::select(-all_star)
player_avg_all_star1 <- 
  randomForest(
    x = player_avg_all_star_train3,
    y = player_avg_all_star_train2$all_star,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )
player_avg_all_star_rf_predictions1 <- predict(player_avg_all_star1, newdata = player_avg_all_star_test1)
player_avg_all_star_conf_matrix1 <- confusionMatrix(player_avg_all_star_rf_predictions1, actual_all_star)
print(player_avg_all_star_conf_matrix1)

##very high accuracy achieved once again using small subset of data, only 17 fields
all_star_avg2 <- all_star_avg1 %>%
  dplyr::select(-x2p_percent)%>%
  dplyr::select(-tov)%>%
  dplyr::select(-drb_per_game)%>%
  dplyr::select(-fg_percent)%>%
  dplyr::select(-x3p_percent)%>%
  dplyr::select(-e_fg_percent)%>%
  dplyr::select(-ft_percent)


set.seed(99)
split_ratio <- sample.split(all_star_avg2$all_star, SplitRatio = 0.75)
player_avg_all_star_training4 <- subset(all_star_avg2, split_ratio == TRUE)
player_avg_all_star_test3 <- subset(all_star_avg2, split_ratio == FALSE)
actual_all_star <- player_avg_all_star_test3$all_star

actual_all_star <- as.factor(actual_all_star)
player_avg_all_star_training4$all_star <- as.factor(player_avg_all_star_training4$all_star)
player_avg_all_star_test3$all_star <- as.factor(player_avg_all_star_test3$all_star)

player_avg_all_star_train5 <- player_avg_all_star_training4 %>%
  dplyr::select(-all_star)
player_avg_all_star2 <- 
  randomForest(
    x = player_avg_all_star_train5,
    y = player_avg_all_star_training4$all_star,
    mtry = 4,          
    ntree = 200,      
    nodesize = 5       
  )

player_avg_all_star_rf_predictions2 <- predict(player_avg_all_star2, newdata = player_avg_all_star_test3)
player_avg_all_star_conf_matrix2 <- confusionMatrix(player_avg_all_star_rf_predictions2, actual_all_star)
print(player_avg_all_star_conf_matrix2)


## playoffs given all star count
team_as_po <- team_modeling %>%
  dplyr:: select(playoffs, all_star)


## count of playoffs by all star count
team_modeling$playoffs <- team_facts$playoffs
playoffs_allstars_summary <- team_modeling %>%
  group_by(all_star, playoffs) %>%
  dplyr:: summarize(count = n(), .groups = "drop") %>%
  arrange(all_star, playoffs)

team_modeling <- team_modeling %>%
  dplyr::select(-playoffs)

print(playoffs_allstars_summary)

## >90% playoffs with 1+ all star, >95% chance with 2+, 100% chance with 3+

position_list <- c("PG", "SG", "SF", "PF", "C", "F")

all_star_counts_by_position <- joined_data %>%
  filter(pos %in% position_list) %>%  
  group_by(pos) %>%  
  summarize(all_star_count = sum(all_star, na.rm = TRUE))  


print(all_star_counts_by_position)
print(all_star_counts_by_position)

all_star_counts_by_year_position <- joined_data %>%
  filter(pos %in% position_list) %>%  
  group_by(season_id, pos) %>%  
  summarize(all_star_count = sum(all_star, na.rm = TRUE), .groups = "drop")  


ggplot(all_star_counts_by_year_position, aes(x = season_id, y = all_star_count, color = pos, group = pos)) +
  geom_line() +  
  geom_point() + 
  labs(
    title = "All-Star Selections by Position Over the Years",
    x = "Year",
    y = "All-Star Count",
    color = "Position"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("PG" = "blue", "SG" = "red", "SF" = "green", "PF" = "purple", "C" = "orange"))


