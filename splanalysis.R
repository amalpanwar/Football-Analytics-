library(readxl)
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

SPLnew <- SPL.indep %>%
  mutate(Points = case_when(
    FTR == 'H' & Home == 1 ~ 3,
    FTR == 'A' & Home == 1 ~ 0,
    FTR == 'H' & Home == 0 ~ 0,
    FTR == 'A' & Home == 0 ~ 3,
    TRUE ~ 1
  ))

SPLnew$Home <- as.factor(SPLnew$Home)

SPLnew <- SPLnew %>%
  rename(HomeTeam = Home)

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
  
  
  # Calculate average strengths
  avg_strength_X_home_scored <- mean(team_data_home$Goals, na.rm = TRUE)
  avg_strength_X_home_conceded <- mean(ifelse(team_data_home$FTR=='A',abs(team_data_home$GD + team_data_home$Goals),abs(team_data_home$GD - team_data_home$Goals)), na.rm = TRUE)
  avg_strength_X_away_scored <- mean(team_data_away$Goals, na.rm = TRUE)
  avg_strength_X_away_conceded <- mean(ifelse(team_data_away$FTR=='H',abs(team_data_away$GD + team_data_away$Goals),abs(team_data_away$GD - team_data_away$Goals)), na.rm = TRUE)
  
  # Update strengths based on home or away match
  #strengths$HomeAttackStrength <- strengths$HomeAttackStrength + avg_strength_X_home_scored / avg_goals_per_game
  #strengths$HomeDefenceWeekness <- strengths$HomeDefenceStrength + avg_strength_X_home_conceded / avg_goals_per_game
  #strengths$AwayAttackStrength <- strengths$AwayAttackStrength + avg_strength_X_away_scored / avg_goals_per_game
  #strengths$AwayDefenceWeekness <- strengths$AwayDefenceStrength + avg_strength_X_away_conceded / avg_goals_per_game
  
  # Assign strengths to the original dataframe
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

SPLnew_stren  %>%
  ggplot(aes(x = AttackStrength, y = DefenceWeekness)) +
  geom_point(alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = Team)) +
  theme_minimal() +
  labs(title = "Attack vs Defense",
       x = "Attack Strength",
       y = "Defence Weekness") +
  facet_wrap(~ factor(HomeTeam, levels = c("0", "1"), labels = c("Away", "Home")))

SPLnew_Winner <- SPLnew %>%
  group_by(SRC,Team,year(Date)) %>%
  summarise(
    TotalPoints = sum(Points, na.rm = TRUE)
  )
head(SPLnew_Winner)

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

SPLnew_Winner2 <- SPLnew_Winner2 %>%
  arrange(Season)


ggplot(data = SPLnew_Winner2, aes(x = Season, y = TotalPoints, fill = Team)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(SPLnew)

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

simulate_match(pois_model, "Rangers", "Celtic", max_goals=4)
rangVSgret<- simulate_match(pois_model, "Rangers", "Gretna", max_goals=4)

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


# looking at the odds for Chelsea v Sunderland
Ran_Mir <- simulate_match(pois_model, "Rangers", "St Mirren", max_goals=10)
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

model.0 = glm(Goals ~ HomeTeam + Team + Opponent, family=poisson(link=log), data=SPLnew)
model.1 = glm(Goals ~ HomeTeam + Team + Opponent + Shot + ShotOnTarg + Foul
              + Corner + AttackStrength,  family=poisson(link=log), data=SPLnew)
wei_model <- renewalCount(formula = Goals ~ HomeTeam + Team + Opponent, data = SPLnew,
                         dist = "weibull", weiMethod = "conv_dePril",
                         computeHessian = FALSE,
                         control = renewal.control(trace = 0,
                                                   method = "nlminb"))

compareToGLM(model.0, model.1)
summary(wei_model)

breaks_ <- 0:5
chiSq_gof(model.0,breaks=breaks_)
chiSq_gof(wei_model,breaks=breaks_)
compareToGLM(poisson_model = model.0, breaks = breaks_, weibull = wei_model)

head(SPLnew_Winner2)

SPLnew2 <- SPLnew_Winner2 %>% select(mpg, cyl)

