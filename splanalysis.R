library(readxl)
library(vip)
SPL <- read_excel("ScoFootball.xlsx", 
                          col_types = c("text", "text", "date", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "text", "numeric", "numeric", 
                                        "text", "text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text"))
head(SPL)

SPL <- SPL %>%
  arrange(Source.Name, Date)

head(SPL)
summary(SPL)

SPL <- drop_na(SPL, FTHG)


SPL.indep <- data.frame(SRC=SPL$'Source.Name',Date=SPL$Date,
                        Team=as.factor(c(as.character(SPL$HomeTeam),
                                         as.character(SPL$AwayTeam))),
                        Opponent=as.factor(c(as.character(SPL$AwayTeam),
                                             as.character(SPL$HomeTeam))),
                        Goals=c(SPL$FTHG, SPL$FTAG),
                        GD=c(abs(SPL$FTHG - SPL$FTAG),abs(SPL$FTAG - SPL$FTHG)),
                        Home=c(rep(1, dim(SPL)[1]), rep(0, dim(SPL)[1])),
                        FTR=SPL$FTR,
                        Shot=c(SPL$HS,SPL$AS),
                        ShotOnTarg=c(SPL$HST,SPL$AST),
                        Foul=c(SPL$HF,SPL$AF),
                        Corner=c(SPL$HC,SPL$AC),
                        YellowC=c(SPL$HY,SPL$AY),
                        RedC=c(SPL$HR,SPL$AR))

SPL.indep <- SPL.indep %>%
  arrange(SRC, Date)

head(SPL.indep)
## calculating points based upon results
SPLnew <- SPL.indep %>%
  mutate(Points = case_when(
    FTR == 'H' & Home == 1 ~ 3,
    FTR == 'A' & Home == 1 ~ 0,
    FTR == 'H' & Home == 0 ~ 0,
    FTR == 'A' & Home == 0 ~ 3,
    TRUE ~ 1
  ))
## convert home indicator into factor
SPLnew$Home <- as.factor(SPLnew$Home)
# renaming home indicator
SPLnew <- SPLnew %>%
  rename(HomeTeam = Home)
## calculating average home and away goal scored and concede 
avg_goals_per_home_game <- mean(SPLnew$Goals[SPLnew$HomeTeam == 1], na.rm = TRUE)
avg_goals_per_away_game <- mean(SPLnew$Goals[SPLnew$HomeTeam == 0], na.rm = TRUE)
average_concede_per_away <- avg_goals_per_home_game
average_concede_per_home <- avg_goals_per_away_game
# Initialize strengths to zero
# strengths <- data.frame(
#   HomeAttackStrength = 0,
#   HomeDefenceWeekness = 0,
#   AwayAttackStrength = 0,
#   AwayDefenceWeekness = 0
# )

# Iterate over each match and calculate strengths
for (i in 1:nrow(SPLnew)) {
  match <- SPLnew[i, ]
  team_i <- match$Team
  home <- match$HomeTeam
  FTR <- match$FTR
 
  
  
  # Filter data for the specific team and home/away
  team_data_home <- SPLnew[SPLnew$Team == team_i & SPLnew$HomeTeam == 1, ]
  team_data_away <- SPLnew[SPLnew$Team == team_i & SPLnew$HomeTeam == 0, ]
  
  
  # Calculate average strength based upon home and away goal scored and concede
  avg_strength_X_home_scored <- mean(team_data_home$Goals, na.rm = TRUE)
  avg_strength_X_home_conceded <- mean(ifelse(team_data_home$FTR=='A',abs(team_data_home$GD + team_data_home$Goals),abs(team_data_home$GD - team_data_home$Goals)), na.rm = TRUE)
  avg_strength_X_away_scored <- mean(team_data_away$Goals, na.rm = TRUE)
  avg_strength_X_away_conceded <- mean(ifelse(team_data_away$FTR=='H',abs(team_data_away$GD + team_data_away$Goals),abs(team_data_away$GD - team_data_away$Goals)), na.rm = TRUE)
  
  # Update strengths based on home or away match
  #strengths$HomeAttackStrength <- strengths$HomeAttackStrength + avg_strength_X_home_scored / avg_goals_per_game
  #strengths$HomeDefenceWeekness <- strengths$HomeDefenceStrength + avg_strength_X_home_conceded / avg_goals_per_game
  #strengths$AwayAttackStrength <- strengths$AwayAttackStrength + avg_strength_X_away_scored / avg_goals_per_game
  #strengths$AwayDefenceWeekness <- strengths$AwayDefenceStrength + avg_strength_X_away_conceded / avg_goals_per_game
  
  # Assign strengths to the original dataframe where avergage strength of goal score become attacking strength and goal concede become defensive weakness
  SPLnew$AttackStrength[i] <- ifelse(home == 1, avg_strength_X_home_scored / avg_goals_per_home_game, avg_strength_X_away_scored / avg_goals_per_away_game) 
  SPLnew$DefenceWeekness[i] <- ifelse(home == 1, avg_strength_X_home_conceded / average_concede_per_home, avg_strength_X_away_conceded / average_concede_per_away)
}

head(SPLnew)

SPLnew_stren <- SPLnew %>%
  group_by(Team,HomeTeam) %>%
  summarise(
    AttackStrength = mean(AttackStrength, na.rm = TRUE),
    DefenceWeekness = mean(DefenceWeekness, na.rm = TRUE)
  )



# Iterate through each row and calculate values
for (i in 1:nrow(SPLnew)) {
  home<-SPLnew$HomeTeam[i]
  team <- SPLnew$Team[i]
  opponent <- SPLnew$Opponent[i]
  
  if(home==1) {
    # Calculate Team's AttackStrength and DefenceWeekness
    SPLnew$Team_AttackStrength[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == team & SPLnew_stren$HomeTeam == 1, "AttackStrength"])
    SPLnew$Team_DefenceWeekness[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == team & SPLnew_stren$HomeTeam == 1, "DefenceWeekness"])
    
    # Calculate Opponent's AttackStrength and DefenceWeekness
    SPLnew$Opponent_AttackStrength[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == opponent & SPLnew_stren$HomeTeam == 0, "AttackStrength"])
    SPLnew$Opponent_DefenceWeekness[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == opponent & SPLnew_stren$HomeTeam == 0, "DefenceWeekness"])
  }
  else
  {
    SPLnew$Team_AttackStrength[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == team & SPLnew_stren$HomeTeam == 0, "AttackStrength"])
    SPLnew$Team_DefenceWeekness[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == team & SPLnew_stren$HomeTeam == 0, "DefenceWeekness"])
    
    # Calculate Opponent's AttackStrength and DefenceWeekness
    SPLnew$Opponent_AttackStrength[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == opponent & SPLnew_stren$HomeTeam == 1, "AttackStrength"])
    SPLnew$Opponent_DefenceWeekness[i] <- as.numeric(SPLnew_stren[SPLnew_stren$Team == opponent & SPLnew_stren$HomeTeam == 1, "DefenceWeekness"])
    
  }
}
## Graph shows the Home and Away attacking strength and weakness in defence for a team
SPLnew_stren  %>%
  ggplot(aes(x = AttackStrength, y = DefenceWeekness)) +
  geom_point(alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = Team)) +
  theme_minimal() +
  labs(title = "Attack vs Defense",
       x = "Attack Strength",
       y = "Defence Weakness") +
  facet_wrap(~ factor(HomeTeam, levels = c("0", "1"), labels = c("Away", "Home")))

SPLnew_Winner <- SPLnew %>%
  group_by(SRC,Team,year(Date)) %>%
  summarise(
    TotalPoints = sum(Points, na.rm = TRUE)
  )
head(SPLnew_Winner)
# Winner of Scottish premiership and their total points season-wise
SPLnew_Winner2 <- SPLnew_Winner %>%
  mutate(
    Season = paste(min(`year(Date)`), max(`year(Date)`), sep = "-")
  ) %>%
  group_by(SRC, Team, Season) %>%
  summarise(
    TotalPoints = sum(TotalPoints, na.rm = TRUE)
  ) %>%
ungroup() %>%
  group_by(SRC,Season) %>%
  slice(which.max(TotalPoints))
## Aranging season
SPLnew_Winner2 <- SPLnew_Winner2 %>%
  arrange(Season)

## Bar plot to shows winner
ggplot(data = SPLnew_Winner2, aes(x = Season, y = TotalPoints, fill = Team)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Poisson modelling by GLM method
pois_model <- glm(Goals ~ HomeTeam + Team + Opponent, family=poisson(link=log), data=SPLnew)
summary(pois_model)

# teams <- unique(SPLnew$Team)
# teams <- teams[teams != "St Mirren"]
# print(teams)
# # Create an empty dataframe to store the results
# prediction_df <- data.frame(home = character(),
#                             away = character(),
#                             homegoals = integer(),
#                             awaygoals = integer(),
#                             stringsAsFactors = FALSE)
# 
# # Loop through each team and predict the scores
# for (opponent_team in teams) {
#   # Predict for home match (HomeTeam = 1)
#   home_prediction <- round(predict(pois_model, 
#                                    data.frame(HomeTeam = '1', Team = "Gretna", Opponent = opponent_team), 
#                                    type = "response"), 0)
#   
#   # Predict for away match (HomeTeam = 0)
#   away_prediction <- round(predict(pois_model, 
#                                    data.frame(HomeTeam = '0', Team = opponent_team, Opponent = "Gretna"), 
#                                    type = "response"), 0)
#   
#   # Create data frame with the predictions
#   prediction_row <- data.frame(
#     home = "Gretna",
#     away = opponent_team,
#     homegoals = home_prediction,
#     awaygoals = away_prediction
#   )
#   
#   # Bind the row to the main dataframe
#   prediction_df <- rbind(prediction_df, prediction_row)
# }
# 


# wrap match predictions in a function
simulate_match <- function(foot_model, team, opponent, max_goals=10){
  home_goals_avg <- predict(foot_model,
                            data.frame(HomeTeam='1', Team=team, 
                                       Opponent=opponent), type="response")
  away_goals_avg <- predict(foot_model, 
                            data.frame(HomeTeam='0', Team=opponent, 
                                       Opponent=team), type="response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}

#simulate_match(pois_model, "Rangers", "Celtic", max_goals=4)
rangVSgret <- simulate_match(pois_model, "Rangers", "Gretna", max_goals=4)

#Drafted result in table for home(Ranger) and away(Gretna) match statistics
result_df <- as.data.frame(as.table(rangVSgret))
names(result_df) <- c("hgoal", "agoal", "probability")

# Add team names for home and away
result_df$home <- "Rangers"
result_df$away <- "Gretna"

result_df$hgoal <- as.numeric(factor(result_df$hgoal, levels = unique(result_df$hgoal)))-1
result_df$agoal <- as.numeric(factor(result_df$agoal, levels = unique(result_df$agoal)))-1

result_df <- result_df %>%
  mutate(
    probability = round(probability, 4))

# Reorder columns
result_df <- result_df[, c("home", "away", "hgoal", "agoal", "probability")]

# Display the result
print(result_df)

#Heatmap of score line
ggplot(result_df, aes(x = hgoal, y = agoal, fill = probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Probability Heatmap of scorlines Rangers vs Gretna", x = "Home Goals(Rangers scores)", y = "Away Goals(Gretna scores)") +
  theme_minimal()


# looking at the odds for St Mirren v Gretna
Gret_Mir <- simulate_match(pois_model, "St Mirren", "Gretna", max_goals=4)
max_position <- which(Ran_Mir == max(Gret_Mir), arr.ind = TRUE)

# Print the results
cat("Maximum probability position: Row =", max_position[1]-1, ", Column =", max_position[2]-1, "\n")

# Rangers win
Rangerwinning<-sum(Ran_Mir[lower.tri(Ran_Mir)])
RangersWin_Odds <- 1/Rangerwinning
RangersWin_Odds

# draw
match_draw <- sum(diag(Ran_Mir))
match_draw_odds <- 1/match_draw
match_draw_odds

# St Mirren win
Mirren_Winning <- sum(Ran_Mir[upper.tri(Ran_Mir)])
MirrenWins_Odds <- 1/Mirren_Winning
MirrenWins_Odds


#Model2:
head(SPLnew)
summary(SPLnew)
pois_model2 <- glm(Goals ~ HomeTeam + Team + Opponent + Shot + ShotOnTarg + Foul
                  + Corner + AttackStrength,  family=poisson(link=log), data=SPLnew)
summary(pois_model2)

#model.0 = glm(Goals ~ HomeTeam + Team + Opponent, family=poisson(link=log), data=SPLnew)
#model.1 = glm(Goals ~ HomeTeam + Team + Opponent + Shot + ShotOnTarg + Foul
#              + Corner + AttackStrength,  family=poisson(link=log), data=SPLnew)
#wei_model <- renewalCount(formula = Goals ~ HomeTeam + Team + Opponent, data = SPLnew,
#                         dist = "weibull", weiMethod = "conv_dePril",
 #                        computeHessian = FALSE,
  #                       control = renewal.control(trace = 0,
   #                                                method = "nlminb"))

#compareToGLM(model.0, model.1)
#summary(wei_model)

#breaks_ <- 0:5
#chiSq_gof(model.0,breaks=breaks_)
#chiSq_gof(wei_model,breaks=breaks_)
#compareToGLM(poisson_model = model.0, breaks = breaks_, weibull = wei_model)

head(SPLnew_Winner2)

str(SPLnew_Winner2)

SPLnew2 <- dplyr::select(SPLnew_Winner2,  SRC,  Season)

SPLnew3 <-SPLnew %>% left_join(SPLnew2,by="SRC")
set.seed(123)
SPL_train <- subset(SPLnew3,Season != '2022-2023')
SPL_test <- subset(SPLnew3,Season == '2022-2023')

train_fit <- step(glm(Goals ~ HomeTeam + Team + Opponent + Shot + ShotOnTarg + Foul
                 + Corner + AttackStrength + DefenceWeekness
                 +I(AttackStrength^2),  family=poisson(link=log), data=SPL_train))
summary(train_fit)

pred1 <- predict(train_fit,SPL_test,type='response',se.fit = TRUE)

SPL_df <- data.frame(
  Goals = SPL_test$Goals,  # Replace this with your actual observed data
  prediction = pred1
)

ggplot(SPL_df, aes(x = factor(Goals), fill = factor(round(prediction.fit)))) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Goal Counts",
       x = "Actual Goals",
       y = "Count",
       fill = "Predicted Goals") +
  theme_minimal()

r2 <- cor(SPL_test$Goals,pred1)^2
r2

plot(SPL_test$Goals,pred1,pch=20)


#Function to generate probability for Home win draw

simulate_match2 <- function(foot_model, team, opponent,data, max_goals = 10) {
  attacking_strength_home <- unique(data[data$Team == team & data$HomeTeam == 1, "Team_AttackStrength"])
  defensive_weakness_home <- unique(data[data$Team == team & data$HomeTeam == 1, "Team_DefenceWeekness"])
  
  attacking_strength_away <- unique(data[data$Opponent == opponent & data$HomeTeam == 1, "Opponent_AttackStrength"])
  defensive_weakness_away <- unique(data[data$Opponent == opponent & data$HomeTeam == 1, "Opponent_DefenceWeekness"])
  
  home_goals_avg <- predict(foot_model,
                            data.frame(HomeTeam = '1', Team = team,
                                       Opponent = opponent
                                       #Team_AttackStrength=attacking_strength_home,
                                       #Team_DefenceWeekness=defensive_weakness_home,
                                       #Opponent_AttackStrength=attacking_strength_away,
                                       #Opponent_DefenceWeekness=defensive_weakness_away
                                       ),
                            type = "response")
  
  away_goals_avg <- predict(foot_model,
                            data.frame(HomeTeam = '0', Team = opponent,
                                       Opponent = team
                                       #Team_AttackStrength=attacking_strength_away,
                                       #Team_DefenceWeekness=defensive_weakness_away,
                                       #Opponent_AttackStrength=attacking_strength_home,
                                       #Opponent_DefenceWeekness=defensive_weakness_home
                                       ),
                            type = "response")
  
  match_simulations <- dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg)
  
  # Return the match simulations
  return(match_simulations)
}


#fitting model

head(SPLnew)
str(SPLnew)
pois_model2 <- step(glm(Goals ~ HomeTeam + Team + Opponent + ShotOnTarg,  
                   family=poisson(link=log), data=SPLnew))

summary(pois_model2)


# Get unique teams and opponents
summary(SPLnew)
teams <- setdiff(unique(SPLnew$Team), "Raith Rvs")
opponents <- setdiff(unique(SPLnew$Opponent), "Raith Rvs")


# Initialize an empty list to store results
results <- list()

# Loop over unique team-opponent combinations
for (team in teams) {
  for (opponent in opponents) {
    
    # Skip if team equals opponent
    if (team == opponent) {
      next
    }
  
    
    # Subset data for the current team-opponent combination
    subset_data <- SPLnew[SPLnew$Team == team & SPLnew$Opponent == opponent, ]
   
 
  
    # Check if there is a match in the subset
    if (nrow(subset_data) == 0) {
      cat("No match found for", team, "vs", opponent, "\n")
      next  # Skip the iteration
    }
    
   
    
    # Get attacking strengths and defensive weaknesses
    attacking_strength_home <- unique(subset_data[subset_data$Team == team & subset_data$HomeTeam == 1, "Team_AttackStrength"])
    defensive_weakness_home <- unique(subset_data[subset_data$Team == team & subset_data$HomeTeam == 1, "Team_DefenceWeekness"])
    shot_home <- mean(subset_data[subset_data$Team == team & subset_data$HomeTeam == 1, "ShotOnTarg"],na.rm=TRUE)
 
    attacking_strength_away <- unique(subset_data[subset_data$Opponent == opponent & subset_data$HomeTeam == 1, "Opponent_AttackStrength"])
    defensive_weakness_away <- unique(subset_data[subset_data$Opponent == opponent & subset_data$HomeTeam == 1, "Opponent_DefenceWeekness"])
    shot_Away <- mean(subset_data[subset_data$Team == team & subset_data$HomeTeam == 0, "ShotOnTarg"],na.rm=TRUE)
    
  
   
   
    # Predict goals
    home_goals <- round(predict(pois_model2, 
                                data.frame(HomeTeam = '1', Team = team, Opponent = opponent,
                                           ShotOnTarg=shot_home
                                           
                                           #Team_AttackStrength = attacking_strength_home,
                                           #Team_DefenceWeekness = defensive_weakness_home,
                                           #Opponent_AttackStrength = attacking_strength_away,
                                           #Opponent_DefenceWeekness = defensive_weakness_away
                                           ), type = "response"), 0)
    
    away_goals <- round(predict(pois_model2, 
                                data.frame(HomeTeam = '0', Team = opponent, Opponent = team,
                                           ShotOnTarg=shot_Away
                                           
                                           #Team_AttackStrength = attacking_strength_away,
                                           #Team_DefenceWeekness = defensive_weakness_away,
                                           #Opponent_AttackStrength = attacking_strength_home,
                                           #Opponent_DefenceWeekness = defensive_weakness_home
                                           ), type = "response"), 0)
    
    # Store the result in the list
    result <- data.frame(Team = team, Opponent = opponent,HomeGoals= home_goals,AwayGoals=away_goals,score = paste(home_goals,'-',away_goals))
    results <- c(results, list(result))
  }
}

# Combine the results into a data frame
result_df <- do.call(rbind, results)

# Print the resulting data frame
print(result_df)


# Initialize an empty list to store results
ProbResult <- data.frame(
  Team = character(),
  Opponent = character(),
  HomeWin = numeric(),
  Draw = numeric(),
  OpponentWin = numeric(),
  stringsAsFactors = FALSE
)


# Iterate over all teams
for (team in teams) {
  # Iterate over all opponents
  for (opponent in opponents) {
    # Skip if team equals opponent
    if (team == opponent) {
      next
    }
    
    # Call the simulate_match2 function and store the result
    result3 <- simulate_match2(pois_model2, team, opponent, data = SPLnew,max_goals = 6)
    # Calculate Home Win, Draw, and Opponent Win
    home_win <- sum(result3[lower.tri(result)])
    draw <- sum(diag(result3))
    opponent_win <- sum(result3[upper.tri(result3)])
    
    # Append the result to the data frame
    ProbResult <- rbind(ProbResult, data.frame(
      Team = team,
      Opponent = opponent,
      HomeWin = home_win,
      Draw = draw,
      OpponentWin = opponent_win
    ))
  }
}

FinalResult <-result_df %>% inner_join(ProbResult, by= c('Team','Opponent'))
head(FinalResult)

HomeTeamOutcomes<- ggplot(FinalResult, aes(x = Team, y = HomeWin, size = HomeWin, color = HomeGoals)) +
  geom_point() +
  labs(title = "Simulated Match Outcomes",
       x = "Teams",
       y = "Home Win Probability") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_gradient(low = "red", high = "green", name = "Home Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

AwayTeamOutcomes <- ggplot(FinalResult, aes(x = Opponent, y = OpponentWin, size = OpponentWin, color = AwayGoals)) +
  geom_point() +
  labs(title = "Simulated Match Outcomes",
       x = "Teams",
       y = "Away Win Probability") +
  scale_size_continuous(range = c(3, 10),name="Away win") +
  scale_color_gradient(low = "red", high = "green", name = "Away Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

HomeTeamOutcomes/ AwayTeamOutcomes


#Fitting team specific model
attacking_strength_home <- unique(SPLnew[SPLnew$Team == "Celtic" & SPLnew$HomeTeam == 1, "Team_AttackStrength"])
defensive_weakness_home <- unique(SPLnew[SPLnew$Team == "Celtic" & SPLnew$HomeTeam == 1, "Team_DefenceWeekness"])
defensive_weakness_home <- unique(SPLnew[SPLnew$Team == "Celtic" & SPLnew$HomeTeam == 1, "Team_DefenceWeekness"])

attacking_strength_away <- unique(SPLnew[SPLnew$Opponent == "Hibernian" & SPLnew$HomeTeam == 1, "Opponent_AttackStrength"])
defensive_weakness_away <- unique(SPLnew[SPLnew$Opponent == "Hibernian" & SPLnew$HomeTeam == 1, "Opponent_DefenceWeekness"])


Celtic <- round(predict(pois_model2, 
                        data.frame(HomeTeam='1',Team="Celtic", 
                                   Opponent="Hibernian",
                                   Team_AttackStrength=attacking_strength_home,
                                   Team_DefenceWeekness=defensive_weakness_home,
                                   Opponent_AttackStrength=attacking_strength_away,
                                   Opponent_DefenceWeekness=defensive_weakness_away), type="response"),0)

Hibernian <- round(predict(pois_model2, 
                           data.frame(HomeTeam='0', Team="Hibernian", 
                                      Opponent="Celtic",
                                      Team_AttackStrength=attacking_strength_away,
                                      Team_DefenceWeekness=defensive_weakness_away,
                                      Opponent_AttackStrength=attacking_strength_home,
                                      Opponent_DefenceWeekness=defensive_weakness_home), type="response"),0)

# cat("Celtic('Home')",Celtic,"-",Hibernian,"Hibernian('Away')")
# cat("Celtic('Home')",Celtic,"-",Hibernian,"Hibernian('Away')")
score<-paste(Celtic,"-",Hibernian)
score

HomeAwayGoalmatrix <- simulate_match2(pois_model2, team = "Celtic", opponent = "Hibernian",max_goals=10)


CelticVsHibernian <- data.frame(Home = "Celtic",
                                Opponent= "Hibernian",
                                Win = sum(HomeAwayGoalmatrix[lower.tri(HomeAwayGoalmatrix)]),
                                Draw = sum(diag(HomeAwayGoalmatrix)),
                                Loss = sum(HomeAwayGoalmatrix[upper.tri(HomeAwayGoalmatrix)]),
                                scoreline = score,
                                HomeWinOdds = 1/sum(HomeAwayGoalmatrix[lower.tri(HomeAwayGoalmatrix)]),
                                DrawOdds = 1/sum(diag(HomeAwayGoalmatrix)),
                                AwayWinOdds= 1/sum(HomeAwayGoalmatrix[upper.tri(HomeAwayGoalmatrix)])
)

# generating probability matrix and table of max 4 goal for specific team and opponent

Ran_Mir2 <- simulate_match2(pois_model2, team = "Rangers", opponent = "St Mirren", df_att_def = SPLnew_stren,max_goals=4)

result_df <- as.data.frame(as.table(Ran_Mir2))
names(result_df) <- c("hgoal", "agoal", "probability")

# Add team names for home and away
result_df$home <- "Rangers"
result_df$away <- "St Mirren"

result_df$hgoal <- as.numeric(factor(result_df$hgoal, levels = unique(result_df$hgoal)))-1
result_df$agoal <- as.numeric(factor(result_df$agoal, levels = unique(result_df$agoal)))-1

result_df <- result_df %>%
  mutate(
    probability = round(probability, 4))

# Reorder columns
result_df <- result_df[, c("home", "away", "hgoal", "agoal", "probability")]

# Display the result
print(result_df)

#Heatmap of score line
ggplot(result_df, aes(x = hgoal, y = agoal, fill = probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Probability Heatmap of scorlines Rangers vs Gretna", x = "Home Goals(Rangers scores)", y = "Away Goals(Gretna scores)") +
  theme_minimal()

## ************************************************** ##

# train vs test
SPLlatest <- SPL.indep %>%
  mutate(Points = case_when(
    FTR == 'H' & Home == 1 ~ 3,
    FTR == 'A' & Home == 1 ~ 0,
    FTR == 'H' & Home == 0 ~ 0,
    FTR == 'A' & Home == 0 ~ 3,
    TRUE ~ 1
  ))

SPLlatest$Home <- as.factor(SPLlatest$Home)

SPLlatest <- SPLlatest %>%
  rename(HomeTeam = Home)

SPLnew2 <- dplyr::select(SPLnew_Winner2,  SRC,  Season)

SPLlatest2 <- SPLlatest %>% left_join(SPLnew2,by="SRC")

##### filtering data based upon recent seasons and calculate strength and weakness similarly we did in the previous section ############
SPL_recent <- subset(SPLlatest2,Season %in% c('2022-2023','2021-2022','2020-2021'))

avg_goals_per_home_game <- mean(SPL_recent$Goals[SPL_recent$HomeTeam == 1], na.rm = TRUE)
avg_goals_per_away_game <- mean(SPL_recent$Goals[SPL_recent$HomeTeam == 0], na.rm = TRUE)
average_concede_per_away <- avg_goals_per_home_game
average_concede_per_home <- avg_goals_per_away_game


# Iterate over each match and calculate strengths
for (i in 1:nrow(SPL_recent)) {
  match <- SPL_recent[i, ]
  team_i <- match$Team
  home <- match$HomeTeam
  FTR <- match$FTR
  
  # Filter data for the specific team and home/away
  team_data_home <- SPL_recent[SPL_recent$Team == team_i & SPL_recent$HomeTeam == 1, ]
  team_data_away <- SPL_recent[SPL_recent$Team == team_i & SPL_recent$HomeTeam == 0, ]
  
  
  # Calculate average strengths
  avg_strength_X_home_scored <- mean(team_data_home$Goals, na.rm = TRUE)
  avg_strength_X_home_conceded <- mean(ifelse(team_data_home$FTR=='A',abs(team_data_home$GD + team_data_home$Goals),abs(team_data_home$GD - team_data_home$Goals)), na.rm = TRUE)
  avg_strength_X_away_scored <- mean(team_data_away$Goals, na.rm = TRUE)
  avg_strength_X_away_conceded <- mean(ifelse(team_data_away$FTR=='H',abs(team_data_away$GD + team_data_away$Goals),abs(team_data_away$GD - team_data_away$Goals)), na.rm = TRUE)

  # Assign strengths to the original dataframe
  SPL_recent$AttackStrength[i] <- ifelse(home == 1, avg_strength_X_home_scored / avg_goals_per_home_game, avg_strength_X_away_scored / avg_goals_per_away_game) 
  SPL_recent$DefenceWeekness[i] <- ifelse(home == 1, avg_strength_X_home_conceded / average_concede_per_home, avg_strength_X_away_conceded / average_concede_per_away)
}

head(SPL_recent)

SPLnew_stren2 <- SPL_recent %>%
  group_by(Team,HomeTeam) %>%
  summarise(
    AttackStrength = mean(AttackStrength, na.rm = TRUE),
    DefenceWeekness = mean(DefenceWeekness, na.rm = TRUE)
  )



# Iterate through each row and calculate values
for (i in 1:nrow(SPL_recent)) {
  home<-SPL_recent$HomeTeam[i]
  team <- SPL_recent$Team[i]
  opponent <- SPL_recent$Opponent[i]
  
  if(home==1) {
    # Calculate Team's AttackStrength and DefenceWeekness
    SPL_recent$Team_AttackStrength[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == team & SPLnew_stren2$HomeTeam == 1, "AttackStrength"])
    SPL_recent$Team_DefenceWeekness[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == team & SPLnew_stren2$HomeTeam == 1, "DefenceWeekness"])
    
    # Calculate Opponent's AttackStrength and DefenceWeekness
    SPL_recent$Opponent_AttackStrength[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == opponent & SPLnew_stren2$HomeTeam == 0, "AttackStrength"])
    SPL_recent$Opponent_DefenceWeekness[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == opponent & SPLnew_stren2$HomeTeam == 0, "DefenceWeekness"])
  }
  else
  {
    SPL_recent$Team_AttackStrength[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == team & SPLnew_stren2$HomeTeam == 0, "AttackStrength"])
    SPL_recent$Team_DefenceWeekness[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == team & SPLnew_stren2$HomeTeam == 0, "DefenceWeekness"])
    
    # Calculate Opponent's AttackStrength and DefenceWeekness
    SPL_recent$Opponent_AttackStrength[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == opponent & SPLnew_stren2$HomeTeam == 1, "AttackStrength"])
    SPL_recent$Opponent_DefenceWeekness[i] <- as.numeric(SPLnew_stren2[SPLnew_stren2$Team == opponent & SPLnew_stren2$HomeTeam == 1, "DefenceWeekness"])
    
  }
}


### Spliting Train and Test data for prediction ###############################
set.seed(123)
SPL_train <- subset(SPL_recent,Season %in% c('2021-2022','2020-2021')) #%in% %in% c('2021-2022','2020-2021','2019-2020','2018-2019','2017-2018','2016-2017','2015-2016','2014-2015','2013-2014'
SPL_test <- subset(SPL_recent,Season %in% c( '2022-2023'))
team <- as.data.frame (unique(SPL_test$Team))
head(SPL_train)
### Fitting model###################################
train_fit <- glm(Goals ~ HomeTeam + Team_AttackStrength + Opponent_DefenceWeekness +I(Opponent_DefenceWeekness * Team_AttackStrength)
                      +ShotOnTarg + Corner,  family=poisson(link=log), data=SPL_train)
summary(train_fit)
###Feature importance###################
vip(train_fit)

##prediction all goal and check r quare
pred1 <- predict(train_fit,SPL_test,type='response')
r2 <- cor(SPL_test$Goals,pred1)^2
r2

### prediction based upon team (home and away) , their strength and draft into a dataframe or table
simulate_match_test_all <- function(foot_model, test_data) {
  results <- data.frame()
  
  for (i in 1:nrow(test_data)) {
    team <- test_data[i, "Team"]
    opponent <- test_data[i, "Opponent"]
    Shot <- test_data[i, "ShotOnTarg"]
    corner <- test_data[i, "Corner"]
    #redc <- test_data[i, "RedC"]
    TAS <- test_data[i, "Team_AttackStrength"]
    ODW <- test_data[i, "Opponent_DefenceWeekness"]
  
  home_goals_avg <- predict(foot_model,
                            newdata = data.frame(HomeTeam = '1',# Team=team,
                                                 #Opponent = opponent,
                                                 Team_AttackStrength = TAS,
                                                 Opponent_DefenceWeekness = ODW,
                                                 ShotOnTarg=Shot,
                                                 Corner=corner
                                                #RedC=redc
                                                 ),
                            type = "response")
  
  away_goals_avg <- predict(foot_model,
                            newdata = data.frame(HomeTeam = '0',# Team=team,
                                                 #Opponent = opponent,
                                                 Team_AttackStrength = TAS,
                                                 Opponent_DefenceWeekness = ODW,
                                                 ShotOnTarg=Shot,
                                                 Corner=corner
                                                 #RedC=redc
                                                 ),
                            type = "response")
  
  #predicted_home_goals <- rpois(1, home_goals_avg)
  #predicted_away_goals <- rpois(1, away_goals_avg)
  
  if (test_data[i, "HomeTeam"] == 1) {
    result <- data.frame(Team = team,
                         Opponent = opponent,
                         goal_pred = round(home_goals_avg,0),
                         hometeam = 1)
  } else {
    result <- data.frame(Team = team,
                         Opponent = opponent,
                         goal_pred = round(away_goals_avg,0),
                         hometeam = 0)
  }
  
  results <- rbind(results, result)
  }
  
  return(results)
}

Test_pred<-simulate_match_test_all(train_fit, test_data = SPL_test)
  head(SPL_test,25)
  head(Test_pred,25)
  SPL_test2 <- dplyr::select(SPL_test,c("Date","Goals","FTR"))
  finaldata <- cbind(Test_pred,SPL_test2)
  
  head(finaldata)
  
  finaldata_new <- data.frame(
    
    Date = finaldata$Date,
    HomeTeam = ifelse(finaldata$hometeam == 1, as.character(finaldata$Team), as.character(finaldata$Opponent)),
    AwayTeam = ifelse(finaldata$hometeam == 0, as.character(finaldata$Team), as.character(finaldata$Opponent)),
    FTHG_pred = ifelse(finaldata$hometeam == 1, finaldata$goal_pred, 0),
    FTAG_pred = ifelse(finaldata$hometeam == 0, finaldata$goal_pred, 0),
    FTHG_Actual = ifelse(finaldata$hometeam == 1, finaldata$Goals, 0),
    FTAG_Actual = ifelse(finaldata$hometeam == 0, finaldata$Goals, 0),
    FTR_Actual = finaldata$FTR)
  
  head(finaldata_new)
  
  finaldata_new2 <- finaldata_new %>%
    group_by(Date,HomeTeam,AwayTeam,FTR_Actual) %>%
    summarise(
      FTHG_pred = sum(FTHG_pred, na.rm = TRUE),
      FTAG_pred = sum(FTAG_pred, na.rm = TRUE),
      FTHG_Actual = sum(FTHG_Actual, na.rm = TRUE),
      FTAG_Actual = sum(FTAG_Actual, na.rm = TRUE)
    )
  
  head(finaldata_new2)
  
  finaldata_new2 <- finaldata_new2 %>%
    mutate(
      FTR_pred = case_when(
        FTHG_pred > FTAG_pred ~ 'H',
        FTHG_pred < FTAG_pred ~ 'A',
        TRUE ~ 'D'  
      ),
      GD_pred= FTHG_pred - FTAG_pred,
      GD_Actual = FTHG_Actual - FTAG_Actual
    )
  
  # Plot histogram
  Home_goal_pred <- hist(finaldata_new2$FTHG_pred, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  Away_goal_pred <- hist(finaldata_new2$FTAG_pred, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  goal_diff_pred <- hist(finaldata_new2$GD_pred, xlim = c(-10, 10), breaks = (-100:100) - 0.5)
  Match_results <- barplot(table(finaldata_new2$FTR_pred)/length(finaldata_new2$HomeTeam), ylim = c(0, 1))
  
  # Plot histogram
  Home_goal_pred <- hist(finaldata_new2$FTHG_Actual, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  Away_goal_actual <- hist(finaldata_new2$FTAG_Actual, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  goal_diff_actual <- hist(finaldata_new2$GD_Actual, xlim = c(-10, 10), breaks = (-100:100) - 0.5)
  Match_results <- barplot(table(finaldata_new2$FTR_Actual)/length(finaldata_new2$HomeTeam), ylim = c(0, 1))
 

  
  # Modify simulate_match3 function
  simulate_match3new <- function(foot_model, test_data, max_goals = 10) {
    home_team <- ifelse(test_data$HomeTeam == 1, as.character(test_data$Team), as.character(test_data$Opponent))
    away_team <- ifelse(test_data$HomeTeam == 0, as.character(test_data$Team), as.character(test_data$Opponent))
    shot <- test_data$ShotOnTarg
    corner <- test_data$Corner
    TAS <- test_data$Team_AttackStrength
    ODW <- test_data$Opponent_DefenceWeekness
   
    
    # Simulate the match for the current row
    match_simul <- simulate_match3(foot_model, TAS, ODW, shot, corner, max_goals)
    
    return(data.frame( Date = test_data$Date,
      HomeTeam = home_team, 
                      AwayTeam = away_team, 
                      HomeWin = ifelse(test_data$HomeTeam == 1, sum(match_simul[lower.tri(match_simul)]),0), 
                      Draw = sum(diag(match_simul)), 
                      AwayWin = ifelse(test_data$HomeTeam == 0,sum(match_simul[upper.tri(match_simul)]),0)))
  }
  ###Simulation for probability based upon fitted model
  simulate_match3 <- function(foot_model, TAS, ODW, shot, corner, max_goals = 10) {
    home_goals_avg <- predict(foot_model,
                              data.frame(HomeTeam = "1", 
                                         Team_AttackStrength = TAS,
                                         Opponent_DefenceWeekness = ODW,
                                         ShotOnTarg = shot, 
                                         Corner = corner),
                              type = "response")
    
    away_goals_avg <- predict(foot_model,
                              data.frame(HomeTeam = "0", 
                                         Team_AttackStrength = TAS,
                                         Opponent_DefenceWeekness = ODW, 
                                         ShotOnTarg = shot, 
                                         Corner = corner),
                              type = "response")
    
    dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg)
  }
  
  
  
  probdata <- list()
  
  # Loop over rows in the test data
  for (i in 1:nrow(SPL_test)) {
    # Simulate match for the current row
    match_simul2 <- simulate_match3new(train_fit, SPL_test[i, ], max_goals = 10)
    
    # Store the result in the list
    probdata <- c(probdata, list(match_simul2))
  }
  
  # Combine results into a single data frame
  final_probdata <- do.call(rbind, probdata)
  
  head(final_probdata)
  
  final_probdata2 <- final_probdata %>%
    group_by(Date,HomeTeam,AwayTeam) %>%
    summarise(
      HomeWin = sum(HomeWin, na.rm = TRUE),
      Draw = mean(Draw, na.rm = TRUE),
      AwayWin = sum(AwayWin, na.rm = TRUE)
    )
  

  
  head(finaldata_new2)
  SPLpred <- finaldata_new2 %>% inner_join(final_probdata2, by = join_by(Date,HomeTeam,AwayTeam))
  
head(SPLpred)
  
  HomeTeamOutcomes<- ggplot(SPLpred, aes(x = HomeTeam, y = HomeWin, size = HomeWin, color = FTHG_pred)) +
    geom_point() +
    labs(title = "Simulated Match Outcomes",
         x = "Teams",
         y = "Home Win Probability") +
    scale_size_continuous(range = c(3, 10),name="Home win") +
    scale_color_gradient(low = "red", high = "green", name = "Home Goals Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  AwayTeamOutcomes <- ggplot(SPLpred, aes(x = AwayTeam, y = AwayWin, size = AwayWin, color = FTAG_pred)) +
    geom_point() +
    labs(title = "Simulated Match Outcomes",
         x = "Teams",
         y = "Away Win Probability") +
    scale_size_continuous(range = c(3, 10),name="Away win") +
    scale_color_gradient(low = "red", high = "green", name = "Away Goals Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  HomeTeamOutcomes / AwayTeamOutcomes
  
  ## home results
  count_data <- SPLpred %>%
    gather(key = "Type", value = "Result", FTR_Actual,FTR_pred) %>%
    group_by(HomeTeam, Result, Type) %>%
    summarise(Count = n())
  
  # Plot
  ggplot(count_data, aes(x = HomeTeam, y = Count, fill = Result)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Type ~ ., scales = "free_y", space = "free") +
    labs(title = "Predicted vs. Actual Results on Home Turf",
         x = "Team",
         y = "Count") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ## Away results
  count_data_away <- SPLpred %>% rename(c(Actual='FTR_Actual',Predicted='FTR_pred'))  %>%
   gather(key = "Type", value = "Result", Actual,Predicted) %>%
    group_by(AwayTeam, Result, Type) %>%
    summarise(Count = n()) 
  
  #Plot
  ggplot(count_data_away, aes(x = AwayTeam, y = Count, fill = Result)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Type ~ ., scales = "free_y", space = "free") +
    labs(title = "Predicted vs. Actual Results on Away Turf",
         x = "Team",
         y = "Count") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  # Print the confusion matrix
  
  confusion_matrix <- table(finaldata_new2$FTR_Actual, finaldata_new2$FTR_pred)
  
  print(confusion_matrix)
  
  # Optionally, visualize it using a heatmap
  library(ggplot2)
  ggplot(data = as.data.frame.table(confusion_matrix),
         aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 1,color='white') +
    theme_minimal() +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted",
         fill = "Count")
  
  
  confusion_df <- as.data.frame.matrix(confusion_matrix)
  # Calculate True Positives, True Negatives, False Positives, False Negatives
  TP <- confusion_matrix[3, 3]  # True Positives for "H" class
  TN <- sum(diag(confusion_matrix)) - TP  # Sum of diagonal excluding TP gives True Negatives
  FP <- sum(confusion_matrix[, 3]) - TP  # Sum of column "H" excluding TP gives False Positives
  FN <- sum(confusion_matrix[3, ]) - TP  # Sum of row "H" excluding TP gives False Negatives
  
  # Calculate accuracy
  accuracy <- (TP + TN) / sum(confusion_matrix)
  
  # Print the accuracy
  cat("Accuracy:", accuracy*100,"%\n")

