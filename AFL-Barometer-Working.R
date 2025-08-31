# Who is Your Club's Barometer? 
# I.e., who, when they play better than average, is correlated with your 
# team winning
# Joel Fernando, 2024

# Thoughts: 
# Should try add some more control variables this time e.g. Finals vs Regular Season, 
# Home or Away, Venue, Opponent (?), Season (?)

#-----Loading Packages
library(fitzRoy)
library(tidyverse)
library(purrr)
library(here)
library(lme4)
library(ggrepel)

#-----What Year is it?
current_year <- lubridate::year(Sys.Date())

#------Fetching Match Level Data
# Using the fitzRoy package
all_matches <- purrr::map(2010:current_year, 
                          ~ fitzRoy::fetch_results_afltables(season = .x)) |> 
  bind_rows() |> 
  janitor::clean_names()

#-------Data Cleaning Match Level Data
match_data <- all_matches |>
  mutate(
    winner = case_when(
      home_points > away_points ~ home_team, # Codifying the winner of each game
      home_points < away_points ~ away_team,
      home_points == away_points ~ "Draw"
    ), 
  )

#--------Fetching Player Level Data 
# AFL Data (which is where we get the rating points from) only starts in 2016
all_player_stats <- purrr::map(2016:current_year,
                               ~ fitzRoy::fetch_player_stats_afl(season = .x)) |>
  bind_rows() |>
  janitor::clean_names()

#--------Data Cleaning the Player Level Data
player_data <- all_player_stats |> 
  select(utc_start_time, round_number = round_round_number, # Selecting relevant columns 
         first_name = player_player_player_given_name, 
         surname = player_player_player_surname,
         team_name, 
         player_position = player_player_position, 
         captain = player_player_player_captain,
         games_played, rating_points, 
         home_team = home_team_name, 
         away_team = away_team_name) |> 
  mutate(utc_start_time = str_replace(utc_start_time, "T.*", "")) |> # Convering the utc time to a date to extract the season so I can join with the player datafram
  mutate(date = as.Date(utc_start_time, format = "%Y-%m-%d")) |> 
  mutate(season = lubridate::year(date)) |> 
  relocate(c(date, season)) |> 
  select(-utc_start_time)

# Function to ensure all team names are the same
# Can left join to convert
team_names <- data.frame(
  player_data_names = unique(player_data$away_team) |> sort(), 
  match_data_names = c("Adelaide","Brisbane Lions", "Carlton", "Collingwood", 
                       "Essendon", "Fremantle", "Geelong", "Gold Coast", 
                       "GWS", "Hawthorn", "Melbourne", "North Melbourne",
                       "Port Adelaide", "Richmond", "St Kilda", "Sydney", 
                       "West Coast", "Footscray"))

# Ensuring the same naming across dataframes - this will help join the match
# and player dataframes together
match_data_join <- left_join(match_data, team_names, 
                        by = c("winner" = "match_data_names"))  |>
  mutate(across(c(home_team, away_team, winner), 
                ~ case_match(.x,
                  "Adelaide" ~ "Adelaide Crows",
                  "Geelong" ~ "Geelong Cats",
                  "Gold Coast" ~ "Gold Coast SUNS",
                  "GWS" ~ "GWS GIANTS",
                  "Sydney" ~ "Sydney Swans",
                  "West Coast" ~ "West Coast Eagles",
                  "Footscray" ~ "Western Bulldogs",
                  .default = .x
                ))) # Converting match data team names to player data team names

# Merge player and match dataframes
player_match <-
  left_join(player_data,
            match_data_join,
            by = c("date",
                   "home_team",
                   "away_team",
                   "season")) |> 
  filter(!is.na(rating_points)) |> 
  mutate(name = paste(first_name, surname)) |> 
  relocate(name, .after = surname)

# Distribution of player ratings
ggplot(player_match, aes(x = rating_points)) + 
  geom_density()

# Data check
# colSums(is.na(player_match |> select(where(is.numeric))))
# Some NAs for round number, b/c of opening round in 2024, last round is different across dataframes -> no big issue
# Some NAs for rating points - mostly random games and players in 2016 and some emergencies in 2014 games -> can remove these

# player rating summaries 
player_rating_summary <- player_match |> 
  mutate(name = stringr::str_to_title(name)) |> # To fix duplicate names like de Goey and De Goey
  mutate(name = ifelse(name == "Cam Ellis-Yolmen", "Cameron Ellis-Yolmen", name)) |> # One naming irregularity, see test_names to check other 
  summarise(mean_rating = mean(rating_points),
            median_rating = median(rating_points),
            max_rating = max(rating_points),
            min_rating = min(rating_points),
            games_played = n(),
            .by = name) |> 
  arrange(desc(mean_rating))

# Testing cases where there may be duplicates e.g. players with 3 words in their name (e.g. Tom De Koning) or a hyphen
test_names <- player_rating_summary |> 
  filter(stringr::str_count(name, " ") == 2 | stringr::str_detect(name, "-")) |> 
  arrange(name)

# Some odd naming irregularities e.g. Jordan de Goey vs Jordan De Goey, 
# Cam Ellis-Yolmen, Cameron Ellis Yolmen - might need to remove duplicates
# by indexing by player number and using just one format

#-------------------------------------------------------------------------------
#-----------------------------Summary Statistics--------------------------------
#-------------------------------------------------------------------------------

# Games played vs player ratings
ggplot(
  player_rating_summary |> filter(games_played >= 50)
  ,
  aes(x = games_played, y = mean_rating)
) +
  geom_point(colour = "blue") +
  ggrepel::geom_text_repel(aes(label = ifelse(mean_rating > 15, name, "")), vjust = -1) +
  ggrepel::geom_text_repel(aes(label = ifelse(mean_rating < 7 &
                                                games_played > 175, name, "")), vjust = -1) +
  labs(title = "Games Played vs Average Player Rating",
       subtitle = "Min. 50 games played, since 2016",
       caption = "Source: AFL") + 
  theme_minimal()

ggsave(here("graphs","games_played-vs-rating.png"))

# Distribution of player ratings - bit of a long left tail
mean_line <- mean(player_rating_summary$mean_rating)

ggplot(
  player_rating_summary,
  aes(x = mean_rating)
) + 
  geom_density() +
  geom_vline(xintercept = mean_line, linetype = "dashed", colour = "blue") + 
  labs(title = "Distribution of Average Player Ratings",
       subtitle = "Min. 50 games played, since 2016, blue line is average",
       caption = "Source: AFL") + 
  theme_minimal()

ggsave(here("graphs", "distribution-average-player-rating.png"))

#--------------------------------------
# Regression of player rating on games played
summary(lm(mean_rating ~ games_played, data = player_rating_summary))

#-------------------------------------------------------------------------------
# Calculating the barometer of each team
# - could add games played as a control variable
# - position listed as control variable
# - is there a way to control for opposition strength? ladder position?

# Dataframe to calculate how many games each player has played (since 2016)
games_played <- player_match |> 
  filter(date > "2022-01-01") |> # Use the last 4 years of data
  select(date, name) |> 
  arrange(date) |> 
  mutate(games_played = row_number(), .by = name)
  
# position categories
forward <- c("HFFL", "FF", "CHF", "FPL", "FPR", "HFFR")
midfield <- c("WR", "WL", "C", "R", "RR")
defender <- c("FB", "HBFR", "BPR", "CHB", "HBFL", "BPL")
ruck <- c("RK")
sub <- c("SUB")
bench <- c("INT")

# Which position does a player primarily play - based off the team sheet
player_primary_position <- player_match |> 
  filter(date > "2022-01-01") |> # Use the last 4 years of data
  select(date, name, player_position) |> 
  mutate(position = case_when(
    player_position %in% forward ~ "forward",
    player_position %in% midfield ~ "midfielder", # Reducing the categories down 
    player_position %in% defender ~ "defender", 
    player_position %in% ruck ~ "ruck", 
    player_position %in% sub ~ "sub",
    player_position %in% bench ~ "bench",
    .default = NA
  )) |> 
  mutate(position_count = n(), .by = c(name, position)) |> 
  slice_max(order_by = position_count, by = name, with_ties = FALSE) |> # Keep the player in the position they have been most listed as
  select(name, position)
  
# Start with the dataframe - select the appropriate columns from player_match
# Need to create the appropriate dummy variables and controls
df_player_barometer <- player_match |> 
  filter(date > "2022-01-01") |> # Use the last 4 years of data
  select(date, game, season, round_number = round_number.x, name, rating_points,
         player_team_name = team_name, player_position, 
         round_type, winner) |> 
  mutate(
    team_won = ifelse(player_team_name == winner, 1, 0), # Dumm¬y variable for if a players team was the winner
    final = ifelse(round_type == "Finals", 1, 0) # Dummy variable for if the game is a final
         ) |> 
  replace_na(list(final = 0)) |> # Somes of the latest round is yet to have updated data 
  left_join(games_played, by = c("date", "name")) |>  # Joining cumulative games played total 
  left_join(player_primary_position, by = "name") |> 
  select(date, game, season, round_number, name, rating_points, player_team_name, 
         team_won, final, games_played, position) |> 
  mutate(dummy = 1) |> 
  pivot_wider(names_from = position, 
              values_from = dummy, # Adding dummy variables for position
              values_fill = 0) |> 
  group_by(name, player_team_name) |> 
  filter(n() >= 30) |> # Only want players who have played at least 30 games for their club 
  ungroup() |> 
  mutate(std_rating_points = scale(rating_points), .by = c(name, player_team_name)) # Standardising player rating points (per club) to test what a standard deviation increase in their performance has on their team's probability of winning 


# Testing regression specifications 
testing <- df_player_barometer |> 
  filter(name %in% c("Callum Wilkie", "Jack Steele", "Rowan Marshall"))

# Simple regression
reg1 <- lm(team_won ~ rating_points*name, testing)
summary(reg1)

# Adding some control variables
# reg2 <- glm(team_won ~ rating_points + final + games_played, testing,
#             link = "logit")
# summary(reg2)

#-------------------------------------------------------------------------------
# Nest the data for each team individually 
nested_data <- df_player_barometer |> 
  group_by(player_team_name) |> 
  nest()

# Run the model for each team
model_output <- nested_data %>%
  mutate(model = map(data, ~ lm(team_won ~ rating_points * name, data = .x)),
         tidy_model = map(model, broom::tidy))

barometer_terms <- model_output %>%
  select(player_team_name, tidy_model) %>%
  unnest(tidy_model) %>%
  filter(str_detect(term, "rating_points:name")) %>%
  mutate(player_id = str_remove(term, "rating_points:name"))

# Biggest barometer for each team 
max_barometer_team <- barometer_terms |> 
  arrange(desc(estimate)) |> 
  group_by(player_team_name) |> 
  slice_max(order_by = estimate, n = 1) |> 
  ungroup() |> 
  relocate(player_id, .after = player_team_name)

#-------------------------------------------------------------------------------
# PROBIT Model 
# Nest the data for each team individually 
nested_data_probit <- df_player_barometer %>%
  group_by(player_team_name) %>%
  nest()

# Run the probit model for each team
model_output_probit <- nested_data_probit %>%
  mutate(
    model = map(data, ~ glm(team_won ~ rating_points * name, 
                            data = .x, 
                            family = binomial(link = "probit"))),
    tidy_model = map(model, broom::tidy)
  )

barometer_terms_probit <- model_output_probit %>%
  select(player_team_name, tidy_model) %>%
  unnest(tidy_model) %>%
  filter(str_detect(term, "rating_points:name")) %>%
  mutate(player_id = str_remove(term, "rating_points:name"))

# Biggest barometer for each team
max_barometer_team_probit <- barometer_terms_probit %>%
  arrange(desc(estimate)) %>%
  group_by(player_team_name) %>%
  slice_max(order_by = estimate, n = 1) %>%
  ungroup() %>%
  relocate(player_id, .after = player_team_name)

# Notes: 
# While the estimates for rating_points:name are useful to compare within 
# teams, they are not useful for comparing across teams. Because the reference
# player (i.e. the intercept) is different 

#----------------Biggest Barometers in the AFL--------------------
# This allows us to compare estimates across teams, by using a single reference
# player (intercept) rather than running the regression at the team level 
# and having different references players for each team. This can also include 
# team-specific fixed effects (i.e. controlling) for the probability that a 
# team will win. 

# Probit model: 
#----------------
model_all <- glm(team_won ~ player_team_name + rating_points * name, 
                 data = df_player_barometer, 
                 family = binomial(link = "probit"))

# Get tidy summary of coefficients
tidy_coefs <- broom::tidy(model_all)

# Player names and their current team 
player_team <- df_player_barometer |> 
  arrange(desc(date)) |> 
  slice(1, .by = c(name, player_team_name)) |> 
  select(name, player_team_name)

# Filter for interaction terms: rating_points:name
interaction_terms <- tidy_coefs %>%
  filter(str_detect(term, "rating_points:name")) |> 
  mutate(name = str_remove(term, "rating_points:name")) |> 
  left_join(player_team, by = "name") |> 
  slice_max(order_by = estimate, by = player_team_name, n = 1) |> 
  arrange(player_team_name)

# Notes: 
# So these coefficients can be compared across teams, so it appears Reuben 
# Ginbey is the biggest barometer in the AFL; though, I wonder if this is only
# because WC have won such few games in the last 4 years. 

# A mixed model 
#--------------
# Notes: 
# Using a mixed model
# The mixed model models players and team effects as random draws from a 
# distribution (e.g. normally distributed) as opposed to specific parameters
# that are estimated for each player, team, player rating point slope in the 
# probit model. 
library(Matrix)
library(lme4)
# Fit logistic mixed model with:
# - Fixed effect for rating_points
# - Random intercepts and slopes (rating_points) by player (name)
# - Random intercepts by team
model_mixed <- glmer(
  team_won ~ rating_points + 
    (1 + rating_points || name) +  # player-specific intercept & slope
    (1 | player_team_name),        # team-specific intercept
  data = df_player_barometer,
  family = binomial(link = "logit")
)

# Extracting fixed effects 
fixef(model_mixed)

# Extracing random effects (player and team coefficients)
ranef_list <- ranef(model_mixed)

# Player random effects (intercept and slope)
player_effects <- ranef_list$name
head(player_effects)

# Team random intercepts
team_effects <- ranef_list$player_team_name
head(team_effects)

# Fixed effects
fixed_intercept <- fixef(model_mixed)["(Intercept)"]
fixed_slope <- fixef(model_mixed)["rating_points"]

# Player-specific effects
player_slopes <- player_effects %>%
  mutate(
    player_name = rownames(player_effects),
    total_intercept = fixed_intercept + `(Intercept)`,
    total_slope = fixed_slope + rating_points
  ) %>%
  select(player_name, total_intercept, total_slope)

head(player_slopes)

#----------------------------------------
# The above approach results in a singular matrix because the baseline 
# effect of a player playing and the effect of their rating points is 
# perfectly negatively correlated. We need to separate these two effects, which
# are: 
# 1. The effect of a player playing e.g. the inclusion of a key forward may 
#    have structural benefits for the team's winning chances even if that player
#.   does not play well. 
# 2. The effect of a player's performance on the team's chances of winning. This
#    is the effect we want to capture. 

# Within-player centering (separates presence vs performance)
# For each player, compute their mean rating points and then compute the 
# difference between rating points and their mean for each game 
df_player_barometer_mean <- df_player_barometer |> 
  mutate(
    rp_mean = mean(rating_points, na.rm = T),
    rp_centred = rating_points - rp_mean,
    .by = name
  )

# Marcus Bontempelli has the highest rating points over the period 
dta <- df_player_barometer_mean |> 
  slice(1, .by = name) |> 
  select(name, rp_mean) |> 
  arrange(desc(rp_mean))

# Running some data checks to test for players that: 
# a. have played too few games 
# b. no within-player spread in their rating points i.e. sd(rating_points) ~ 0
# c. no variation in outcomes i.e. var(team_won) ~ 0 e.g. always win when they play
player_checks <- df_player_barometer_mean |> 
  mutate(
    n = n(),
    sd_rating_points = sd(rp_centred, na.rm = T),
    var_win = var(team_won, na.rm = T),
    .by = name
  ) |> 
  slice(1, .by = name) |> 
  select(name, rp_mean, n, sd_rating_points, var_win)

ggplot(player_checks, aes(x = rp_mean, y = sd_rating_points)) + 
  geom_point() + 
  geom_text(data = player_checks |> filter(rp_mean > 15 & sd_rating_points < 5.5),
             aes(label = name), nudge_x = 1) +
  geom_text(data = player_checks |> filter(rp_mean > 16),
             aes(label = name), nudge_y = -0.1, nudge_x = -0.8) + 
  geom_text(data = player_checks |> filter(sd_rating_points > 7.6),
             aes(label = name), nudge_x = 1.3) + 
  labs(title = "Mean Rating Points and SD Rating Points")

ggsave(here("graphs", "mean_sd_rating_points.png"))
  

# Fitting the mixed model with uncorrelated intercepts (effect 1) and slopes 
# (effect 2)
m <- glmer(
  team_won ~ rp_centred + rp_mean +                  # within & between effects
    (1 + rp_centred || name) +                       # player intercepts AND player-specific slope (uncorrelated)
    (1 | player_team_name),                    # team baseline
  data = df_player_barometer_mean,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

df_fit <- df_player_barometer_mean |>
  dplyr::mutate(
    rp_c_z    = as.numeric(scale(rp_centred)),
    rp_mean_z = as.numeric(scale(rp_mean))
  )

m2 <- glmer(
  team_won ~ rp_c_z + rp_mean_z + (1 + rp_c_z || name) + (1 | player_team_name),
  data = df_fit, family = binomial(link="logit"),
  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)
isSingular(m2)

# nest players within teams (often more stable)
m3 <- glmer(
  team_won ~ rp_c_z + rp_mean_z + (1 + rp_c_z || name) + (1 | player_team_name/name),
  data = df_fit, family = binomial(link="logit"),
  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

isSingular(m3)

# OR drop team intercept if nesting is too strong

# m3b: 
# rp_c_z - (standardised) effect of rating points on team performance 
# rp_mean_z - average effect of player's performance on team win (standardised)
# intercept (1) - effect of player just being in the team 
# rp_c_z * name - slope effect; barometer term - impact of that player playing well on team performance
# Note: Cannot include team FE because of correlation between player-specific 
# intercepts - run into a singularity issue. This means we cannot compare slopes
# across teams 
m3b <- glmer(
  team_won ~ rp_c_z + rp_mean_z + 
    (1 + rp_c_z || name),
  data = df_fit, family = binomial(link="logit"),
  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

isSingular(m3b)

fixed_slope <- fixef(m3b)["rp_c_z"]
fixed_slope

ranef_players <- ranef(m3b)$name  # player-level random effects
head(ranef_players)

player_slopes <- ranef_players %>%
  rownames_to_column("player") %>%
  mutate(total_slope = rp_c_z + fixed_slope) %>%
 # select(player, total_slope) %>%
  arrange(desc(total_slope)) |>   # rank from largest to smallest
  rename(intercept = `(Intercept)`) |> 
  mutate(total_effect = intercept + total_slope)

# Interpretation of player slopes: 
# Intercept: Measures the player's baseline contribution of being in the team 
# Total slope:  player_i slope + average fixed effect slope = 'who moves the 
#               needle the most when they play above their norm. 
# Intercept + total slope: The total effect of that player playing (and 
#               playing well) on the team's probability of winning 

# Elite players may not have the highest slope because elite players can have 
# a low slope, this doesn't mean they don't affect the team's performance but 
# could reflect that their performance is always good so it is a 'given' they 
# perform well and then the probability of a team winning shifts by the 
# performance of other (perhaps more volatile) players

#---------
# Including team fixed effects (but removing player-specific slopes). This will
# allow us to compare results across teams but can no longer estimate the baseline
# contribution of a player just by being in the team (as this was previously)
# captured in the intercept term. 
m_teamFE <- glmer(
  team_won ~ rp_c_z + rp_mean_z + factor(player_team_name) +  # team fixed effects
    (0 + rp_c_z || name),                                     # player-specific slope only
  data = df_fit,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

isSingular(m_teamFE) # Not singular 

# Extracting the fixed effect of rp_c_z. This is the average slope across 
# all players
fixed_slope <- fixef(m_teamFE)["rp_c_z"]
fixed_slope

# Extracting the player random slopes 
ranef_players <- ranef(m_teamFE)$name   # player-level random slopes
head(ranef_players)

# Computing the total slope per player 
player_slopes <- ranef_players %>%
  rownames_to_column("player") %>%
  mutate(total_slope = rp_c_z + fixed_slope) %>%  # total effect
  select(player, total_slope) %>%
  arrange(desc(total_slope))  # rank from highest to lowest

#------------------------------------------------------------------------------
# Now reducing the dataframe to the last two seasons ; min 20  games played
# Want to capture more of the recent barometer effect 
recent_df <- df_player_barometer_mean %>%
  filter(season >= max(season) - 1)  # assuming 'season' is numeric

# --- 2. Keep players with at least 15 games ---
player_counts <- recent_df %>%
  group_by(name) %>%
  summarise(n_games = n())

recent_df <- recent_df %>%
  left_join(player_counts, by = "name") %>%
  filter(n_games >= 15) |> 
  dplyr::mutate(
    rp_c_z    = as.numeric(scale(rp_centred)),
    rp_mean_z = as.numeric(scale(rp_mean))
  )


# --- 3. Fit the mixed model ---
# Player slopes only, team fixed effects
m_teamFE_recent <- glmer(
  team_won ~ rp_c_z + rp_mean_z + factor(player_team_name) +
    (0 + rp_c_z || name),
  data = recent_df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# --- 4. Extract player slopes ---
fixed_slope <- fixef(m_teamFE_recent)["rp_c_z"]

ranef_players <- ranef(m_teamFE_recent)$name %>%
  rownames_to_column("player") %>%
  mutate(total_slope = rp_c_z + fixed_slope) %>%
  select(player, total_slope) %>%
  arrange(desc(total_slope))

# --- 5. Optional: join back with games played for context ---
player_slopes <- ranef_players %>%
  left_join(player_counts, by = c("player" = "name"))

# --- 6. View top players ---
head(player_slopes, 10)

# newdata <- data.frame(
#   rp_c_z = seq(-2, 2, length.out = 5),
#   rp_mean_z = 0,
#   player_team_name = "St Kilda",
#   name = "Nasiah Wanganeen-Milera"
# )
# 
# plot(predict(m_teamFE_recent, newdata, type = "response"))

#---------------- Fitting the original model with player-specific intercepts
# but no team fixed effects but this time with the more recent dataframe 
m_playerFE_recent <- glmer(
  team_won ~ rp_c_z + rp_mean_z + 
    (1 + rp_c_z || name),   # player intercept + slope
  data = recent_df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

fixed_effects <- fixef(m_playerFE_recent)
ranef_players <- ranef(m_playerFE_recent)$name %>%
  rownames_to_column("name") %>%
  mutate(
    total_slope = rp_c_z + fixed_effects["rp_c_z"],
    total_intercept = `(Intercept)` + fixed_effects["(Intercept)"]
  ) %>%
  select(name, total_intercept, total_slope)

# Standardise intercepts at the team level 
team_player <- recent_df |> 
  slice(1, .by = c(name, player_team_name)) |> 
  select(name, player_team_name)

# Joining with effects df
team_ranef_players <- left_join(ranef_players, team_player, by = "name") |> 
  mutate(team_mean_intercept = mean(total_intercept), .by = player_team_name) |> 
  mutate(intercept_std = total_intercept - team_mean_intercept,
         total_effect = total_slope + intercept_std)

# Plotting results 
ggplot(team_ranef_players, aes(x = intercept_std, y = total_slope, 
                         color = total_effect, size = total_effect)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = name), hjust = 0, vjust = 1.2, size = 3) +
  scale_color_viridis_c(option = "plasma") +  # nice color gradient
  labs(
    x = "Player baseline effect (intercept_std, relative to team)",
    y = "Above-average performance impact (total_slope)",
    color = "Total effect (intercept + slope)",
    size = "Total effect (intercept + slope)",
    title = "AFL Barometer: Player baseline vs performance impact (recent form)"
  ) +
  theme_minimal()

# Interpretation: 
# x-axis: Baseline contribution relative to team mates
# y-axis: How much a player's above-average performance shifts team win prob. 
# Point size + colour -> total effect - larger, brighter = bigger overall impact in typical above-average game
# Top-right quadrant - players who are both consistently important for their team and whose above average games move the needle
# Bottom-right quadrant - Elite players whose above-average performance doesn't add much beyond their baseline 
# Top-left quadrant: Players who can swing outcomes when performing above their usual level, despite modest baseline

# Largest slope (largest barometer for each team)
top_team_barometer <- team_ranef_players |> 
  slice_max(order_by = total_slope, n = 1, by = player_team_name) |> 
  arrange(player_team_name)

# Largest player effect for each team
top_team_player_effect <- team_ranef_players |> 
  slice_max(order_by = intercept_std, n = 1, by = player_team_name) |> 
  arrange(player_team_name)

# Pick a team to examine, e.g., "Essendon"
team_name <- "St Kilda"

team_stats <- team_ranef_players %>%
  filter(player_team_name == team_name) %>%
  arrange(desc(total_effect))

# Scatter plot: intercept_std vs total_slope, size/color by total_effect
ggplot(team_stats, aes(x = intercept_std, y = total_slope, 
                       color = total_effect, size = total_effect)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = name), hjust = 0, vjust = 1.2, size = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    x = "Player baseline effect (relative to team)",
    y = "Above-average performance impact",
    color = "Total effect",
    size = "Total effect",
    title = paste("Player effects for", team_name)
  ) +
  theme_minimal()

#---- Computing each team's most valuable player (i.e. no longer barometer)
# --- 1. Filter recent data as before (last 2 seasons, min 15 games) ---



