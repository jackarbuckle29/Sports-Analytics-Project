####################################################
########## Function File for Play By Play ##########
####################################################



epa_calculator <- function(data){
  #'
  #' This function calculates the average EPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Identify Teams
  teams <- unique(na.omit(data$posteam))
  teams <- teams[order(teams)]
  # Create empty vectors to store results
  run_epa <- run_freq <- pass_epa <- pass_freq <- rep(NA, length(teams))
  rz_run_epa <- rz_run_freq <- rz_pass_epa <- rz_pass_freq <- rep(NA, length(teams))
  games <- rep(NA, length(teams))
  # For each team
  for(i in 1:length(teams)){
    # Identify selected team
    sel_team <- teams[i]
    # Calculate total run epa
    run_epa[i] <- sum(data$epa[which(data$play_type == "Rush" & 
                                       data$posteam == sel_team)], na.rm = T)
    # Calculate total run plays
    run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                     data$posteam == sel_team),])
    # Calculate total pass epa
    pass_epa[i] <- sum(data$epa[which(data$play_type == "pass" & 
                                        data$posteam == sel_team)], na.rm = T)
    # Calculate total pass plays
    pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                      data$posteam == sel_team),])
    # Calculate total red-zone run EPA
    rz_run_epa[i] <- sum(data$epa[which(data$play_type == "Rush" & 
                                          data$posteam == sel_team &
                                          data$yardline_100 <= 20)], na.rm = T)
    # Calculate total red-zone run plays
    rz_run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                        data$posteam == sel_team &
                                        data$yardline_100 <= 20),])
    # Calculate total red-zone pass EPA
    rz_pass_epa[i] <- sum(data$epa[which(data$play_type == "pass" & 
                                           data$posteam == sel_team &
                                           data$yardline_100 <= 20)], na.rm = T)
    # Calculate total red-zone pass plays
    rz_pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                         data$posteam == sel_team &
                                         data$yardline_100 <= 20),])
    # Calculate total number of games played
    games[i] <- length(unique(na.omit(data$game_id[which(data$posteam == sel_team)])))
  }
  
  # Calculate averages per play
  avg_run_epa <- run_epa/run_freq
  avg_pass_epa <- pass_epa/pass_freq
  avg_rz_run_epa <- rz_run_epa/rz_run_freq
  avg_rz_pass_epa <- rz_pass_epa/rz_pass_freq
  # Calculate play type frequency per play
  runs_per_game <- run_freq/games
  pass_per_game <- pass_freq/games
  rz_runs_per_game <- rz_run_freq/games
  rz_pass_per_game <- rz_pass_freq/games
  
  # Join results columns
  res_dat <- cbind.data.frame(teams, run_epa, run_freq, pass_epa, pass_freq, 
                              rz_run_epa, rz_run_freq, rz_pass_epa, rz_pass_freq,
                              avg_run_epa, avg_pass_epa, avg_rz_run_epa, avg_rz_pass_epa, games,
                              runs_per_game, pass_per_game, rz_runs_per_game, rz_pass_per_game)
  # Name results
  names(res_dat) <- c("team", "Rush_epa", "Rush_freq", "pass_epa", "pass_freq", 
                      "rz_run_epa", "rz_run_freq", "rz_pass_epa", "rz_pass_freq",
                      "avg_run_epa", "avg_pass_epa", "avg_rz_run_epa", "avg_rz_pass_epa", "games",
                      "Rushs_per_game", "pass_per_game", "rz_runs_per_game", "rz_pass_per_game")
  
  res_dat[is.na(res_dat)] <- 0
  # Return Results
  return(res_dat)
}



def_epa_calculator <- function(data){
  #'
  #' This function calculates the average EPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Identify Teams
  teams <- unique(na.omit(data$defteam))
  teams <- teams[order(teams)]
  run_epa <- run_freq <- pass_epa <- pass_freq <- rep(NA, length(teams))
  rz_run_epa <- rz_run_freq <- rz_pass_epa <- rz_pass_freq <- rep(NA, length(teams))
  games <- rep(NA, length(teams))
  
  for(i in 1:length(teams)){
    sel_team <- teams[i]
    
    run_epa[i] <- -sum(data$epa[which(data$play_type == "Rush" & 
                                        data$defteam == sel_team)], na.rm = T)
    
    run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                     data$defteam == sel_team),])
    
    pass_epa[i] <- -sum(data$epa[which(data$play_type == "pass" & 
                                         data$defteam == sel_team)], na.rm = T)
    
    pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                      data$defteam == sel_team),])
    
    rz_run_epa[i] <- -sum(data$epa[which(data$play_type == "Rush" & 
                                           data$defteam == sel_team &
                                           data$yardline_100 <= 20)], na.rm = T)
    
    rz_run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                        data$defteam == sel_team &
                                        data$yardline_100 <= 20),])
    
    rz_pass_epa[i] <- -sum(data$epa[which(data$play_type == "pass" & 
                                            data$defteam == sel_team &
                                            data$yardline_100 <= 20)], na.rm = T)
    
    rz_pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                         data$defteam == sel_team &
                                         data$yardline_100 <= 20),])
    games[i] <- length(unique(na.omit(data$game_id[which(data$defteam == sel_team)])))
  }
  
  
  avg_run_epa <- run_epa/run_freq
  avg_pass_epa <- pass_epa/pass_freq
  avg_rz_run_epa <- rz_run_epa/rz_run_freq
  avg_rz_pass_epa <- rz_pass_epa/rz_pass_freq
  runs_per_game <- run_freq/games
  pass_per_game <- pass_freq/games
  rz_runs_per_game <- rz_run_freq/games
  rz_pass_per_game <- rz_pass_freq/games
  
  
  res_dat <- cbind.data.frame(teams, run_epa, run_freq, pass_epa, pass_freq, 
                              rz_run_epa, rz_run_freq, rz_pass_epa, rz_pass_freq,
                              avg_run_epa, avg_pass_epa, avg_rz_run_epa, avg_rz_pass_epa, games,
                              runs_per_game, pass_per_game, rz_runs_per_game, rz_pass_per_game)
  names(res_dat) <- c("team", "Rush_epa", "Rush_freq", "pass_epa", "pass_freq", 
                      "rz_run_epa", "rz_run_freq", "rz_pass_epa", "rz_pass_freq",
                      "avg_run_epa", "avg_pass_epa", "avg_rz_run_epa", "avg_rz_pass_epa", "games",
                      "Rushs_per_game", "pass_per_game", "rz_runs_per_game", "rz_pass_per_game")
  res_dat[is.na(res_dat)] <- 0
  return(res_dat)
}



wpa_calculator <- function(data){
  #'
  #' This function calculates the average WPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Identify Teams
  teams <- unique(na.omit(data$posteam))
  teams <- teams[order(teams)]
  # Create empty vectors to store results
  run_wpa <- run_freq <- pass_wpa <- pass_freq <- rep(NA, length(teams))
  rz_run_wpa <- rz_run_freq <- rz_pass_wpa <- rz_pass_freq <- rep(NA, length(teams))
  games <- rep(NA, length(teams))
  # For each team
  for(i in 1:length(teams)){
    # Identify selected team
    sel_team <- teams[i]
    # Calculate total run WPA
    run_wpa[i] <- sum(data$wpa[which(data$play_type == "Rush" & 
                                       data$posteam == sel_team)], na.rm = T)
    # Calculate total run plays
    run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                     data$posteam == sel_team),])
    # Calculate total pass WPA
    pass_wpa[i] <- sum(data$wpa[which(data$play_type == "pass" & 
                                        data$posteam == sel_team)], na.rm = T)
    # Calculate total pass plays
    pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                      data$posteam == sel_team),])
    # Calculate total red-zone run WPA
    rz_run_wpa[i] <- sum(data$wpa[which(data$play_type == "Rush" & 
                                          data$posteam == sel_team &
                                          data$yardline_100 <= 20)], na.rm = T)
    # Calculate total red-zone run plays
    rz_run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                        data$posteam == sel_team &
                                        data$yardline_100 <= 20),])
    # Calculate total red-zone pass WPA
    rz_pass_wpa[i] <- sum(data$wpa[which(data$play_type == "pass" & 
                                           data$posteam == sel_team &
                                           data$yardline_100 <= 20)], na.rm = T)
    # Calculate total red-zone pass plays
    rz_pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                         data$posteam == sel_team &
                                         data$yardline_100 <= 20),])
    # Calculate total number of games played
    games[i] <- length(unique(na.omit(data$game_id[which(data$posteam == sel_team)])))
  }
  
  # Calculate averages per play
  avg_run_wpa <- run_wpa/run_freq
  avg_pass_wpa <- pass_wpa/pass_freq
  avg_rz_run_wpa <- rz_run_wpa/rz_run_freq
  avg_rz_pass_wpa <- rz_pass_wpa/rz_pass_freq
  # Calculate play type frequency per play
  runs_per_game <- run_freq/games
  pass_per_game <- pass_freq/games
  rz_runs_per_game <- rz_run_freq/games
  rz_pass_per_game <- rz_pass_freq/games
  
  # Join results columns
  res_dat <- cbind.data.frame(teams, run_wpa, run_freq, pass_wpa, pass_freq, 
                              rz_run_wpa, rz_run_freq, rz_pass_wpa, rz_pass_freq,
                              avg_run_wpa, avg_pass_wpa, avg_rz_run_wpa, avg_rz_pass_wpa, games,
                              runs_per_game, pass_per_game, rz_runs_per_game, rz_pass_per_game)
  # Name results
  names(res_dat) <- c("team", "Rush_wpa", "Rush_freq", "pass_wpa", "pass_freq", 
                      "rz_run_wpa", "rz_run_freq", "rz_pass_wpa", "rz_pass_freq",
                      "avg_run_wpa", "avg_pass_wpa", "avg_rz_run_wpa", "avg_rz_pass_wpa", "games",
                      "Rushs_per_game", "pass_per_game", "rz_runs_per_game", "rz_pass_per_game")
  res_dat[is.na(res_dat)] <- 0
  # Return Results
  return(res_dat)
}



def_wpa_calculator <- function(data){
  #'
  #' This function calculates the average WPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Identify Teams
  teams <- unique(na.omit(data$defteam))
  teams <- teams[order(teams)]
  run_wpa <- run_freq <- pass_wpa <- pass_freq <- rep(NA, length(teams))
  rz_run_wpa <- rz_run_freq <- rz_pass_wpa <- rz_pass_freq <- rep(NA, length(teams))
  games <- rep(NA, length(teams))
  
  for(i in 1:length(teams)){
    sel_team <- teams[i]
    
    run_wpa[i] <- -sum(data$wpa[which(data$play_type == "Rush" & 
                                        data$defteam == sel_team)], na.rm = T)
    
    run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                     data$defteam == sel_team),])
    
    pass_wpa[i] <- -sum(data$wpa[which(data$play_type == "pass" & 
                                         data$defteam == sel_team)], na.rm = T)
    
    pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                      data$defteam == sel_team),])
    
    rz_run_wpa[i] <- -sum(data$wpa[which(data$play_type == "Rush" & 
                                           data$defteam == sel_team &
                                           data$yardline_100 <= 20)], na.rm = T)
    
    rz_run_freq[i] <- nrow(data[which(data$play_type == "Rush" & 
                                        data$defteam == sel_team &
                                        data$yardline_100 <= 20),])
    
    rz_pass_wpa[i] <- -sum(data$wpa[which(data$play_type == "pass" & 
                                            data$defteam == sel_team &
                                            data$yardline_100 <= 20)], na.rm = T)
    
    rz_pass_freq[i] <- nrow(data[which(data$play_type == "pass" & 
                                         data$defteam == sel_team &
                                         data$yardline_100 <= 20),])
    games[i] <- length(unique(na.omit(data$game_id[which(data$defteam == sel_team)])))
  }
  
  
  avg_run_wpa <- run_wpa/run_freq
  avg_pass_wpa <- pass_wpa/pass_freq
  avg_rz_run_wpa <- rz_run_wpa/rz_run_freq
  avg_rz_pass_wpa <- rz_pass_wpa/rz_pass_freq
  runs_per_game <- run_freq/games
  pass_per_game <- pass_freq/games
  rz_runs_per_game <- rz_run_freq/games
  rz_pass_per_game <- rz_pass_freq/games
  
  
  res_dat <- cbind.data.frame(teams, run_wpa, run_freq, pass_wpa, pass_freq, 
                              rz_run_wpa, rz_run_freq, rz_pass_wpa, rz_pass_freq,
                              avg_run_wpa, avg_pass_wpa, avg_rz_run_wpa, avg_rz_pass_wpa, games,
                              runs_per_game, pass_per_game, rz_runs_per_game, rz_pass_per_game)
  names(res_dat) <- c("team", "Rush_wpa", "Rush_freq", "pass_wpa", "pass_freq", 
                      "rz_run_wpa", "rz_run_freq", "rz_pass_wpa", "rz_pass_freq",
                      "avg_run_wpa", "avg_pass_wpa", "avg_rz_run_wpa", "avg_rz_pass_wpa", "games",
                      "Rushs_per_game", "pass_per_game", "rz_runs_per_game", "rz_pass_per_game")
  res_dat[is.na(res_dat)] <- 0
  return(res_dat)
}


game_calc <- function(home_team, away_team, week,
                      teams, game_res, week_res, week_teams, model_dat){
  #'
  #' This function calculates summary statistics for a single game based on
  #' deviation from average values
  #' 
  #' @param home_team The home team for the game
  #' @param away_team The away team for the game
  #' @param week The game week of interest
  #' @param teams A vector of team names, in alphabetical order
  #' @param game_res The game results data frame
  #' @param week_res The absolute weekly metric data frame
  #' @param week_teams The list containing the teams playing in each week
  #' @param model_dat The season level metrics
  #'
  

  
  # Pull out home team stats
  home_stats_g <- week_res[[week]][week_teams[[week]] == home_team,1:12]
  home_stats <- model_dat[teams == home_team,1:12]
  # Pull out away team stats
  away_stats_g <- week_res[[week]][week_teams[[week]] == away_team,1:12]
  away_stats <- model_dat[teams == away_team,1:12]
  
  ##### Create plot based on expected v actual
  # Convert to single data frame
  
  home_db <- cbind.data.frame(rep(home_team, ncol(home_stats)), names(home_stats),
                              t(home_stats), t(home_stats_g), c(rep("stat", 4), rep("freq", 4),
                                                                rep("stat", 4)))
  away_db <- cbind.data.frame(rep(away_team, ncol(away_stats)), names(away_stats), 
                              t(away_stats), t(away_stats_g),  c(rep("stat", 4), rep("freq", 4),
                                                                 rep("stat", 4)))
  names(home_db) <- names(away_db) <- c("team", "statistic", "value", "value_g", "type")
  res_db <- rbind.data.frame(home_db, away_db)
  # Split into frequency and statistics
  res_db_1 <- res_db[res_db$type == "stat",]
  res_db_2 <- res_db[res_db$type == "freq",]
  # Create statistics plot
  g_1 <- ggplot(res_db_1, aes(x = value, y= value_g, color = statistic)) +
    geom_point(alpha = 0.5) +
    geom_text_repel(aes(label = statistic), size = 5, box.padding = 0.5,
                    point.padding = 0.5, max.overlaps = 20) + # Set labels 
    dark_theme_bw() +
    theme(legend.position="none", # Turn off legend
          panel.grid.major = element_blank(), # Remove grid
          panel.grid.minor = element_blank(), # Remove grid
          panel.border = element_blank(), # Remove grid
          panel.background = element_blank()) +  # Remove grid
    labs(x= "Expected Values", y ="Actual Values", # Set labels
         title = paste(home_team, ": ", game_res$home_score[game_res$home_team == home_team &
                                                        game_res$week == week], " v ",
                       away_team, ": ", game_res$away_score[game_res$away_team == away_team &
                                                              game_res$week == week], sep = ""),
         subtitle = paste("Week: ", week, sep = "")) +
    facet_grid(~team) +
    geom_abline(slope = 1, intercept =  0) +
    xlim(min(c(res_db_1$value, res_db_1$value_g)), max(c(res_db_1$value, res_db_1$value_g))) +
    ylim(min(c(res_db_1$value, res_db_1$value_g)), max(c(res_db_1$value, res_db_1$value_g)))
  
  # Create frequency plot
  g_2 <- ggplot(res_db_2, aes(x = value, y= value_g, color = statistic)) +
    geom_point(alpha = 0.5) +
    geom_text_repel(aes(label = statistic), size = 5, box.padding =  0.5,
                    point.padding = 0.5,
                    max.overlaps = 20) + # Set labels 
    dark_theme_bw() +
    theme(legend.position="none", # Turn off legend
          panel.grid.major = element_blank(), # Remove grid
          panel.grid.minor = element_blank(), # Remove grid
          panel.border = element_blank(), # Remove grid
          panel.background = element_blank()) +  # Remove grid
    labs(x= "Expected Values", y ="Actual Values", # Set labels
         title = paste(home_team, ": ", game_res$home_score[game_res$home_team == home_team &
                                                              game_res$week == week], " v ",
                       away_team, ": ", game_res$away_score[game_res$away_team == away_team &
                                                              game_res$week == week], sep = ""),
         subtitle = paste("Week: ", week, sep = "")) +
    facet_grid(~team) +
    geom_abline(slope = 1, intercept =  0) +
    xlim(min(c(res_db_2$value, res_db_2$value_g)), max(c(res_db_2$value, res_db_2$value_g))) +
    ylim(min(c(res_db_2$value, res_db_2$value_g)), max(c(res_db_2$value, res_db_2$value_g)))
  # Return list of results
  return(list(stat_plot = g_1, freq_plot = g_2, res_db = res_db))
}






epa_game_assign <- function(teams, mean_vec, dev_dat, game_res){
  #'
  #' This function assigns epa values to home and away teams for 
  #' each game
  #'
  #'
  #'
  
  results_db_h <- results_db_a <- as.data.frame(matrix(NA, nrow = nrow(game_res), ncol = ncol(dev_dat)))
  for(i in 1:nrow(game_res)){
    results_db_h[i,] <- dev_dat[teams == game_res$home_team[i],]
    results_db_a[i,] <- dev_dat[teams == game_res$away_team[i],]
  }
  names(results_db_h) <- names(results_db_a) <- colnames(dev_dat)
  calc_db <- as.data.frame(matrix(NA, nrow = nrow(game_res), ncol = 8))
  names(calc_db) <- c("home_runs", "home_passes", "home_run_epa", "home_pass_epa",
                      "away_runs", "away_passes", "away_run_epa", "away_pass_epa")
  
  for(i in 1:nrow(game_res)){
    calc_db$home_runs[i] <- results_db_h$runs_per_game[i] + 
      results_db_a$runs_per_game_def[i] + 
      mean_vec[colnames(dev_dat) == "Rushs_per_game"]
    calc_db$home_passes[i] <- results_db_h$pass_per_game[i] +
      results_db_a$pass_per_game_def[i] +
      mean_vec[colnames(dev_dat) == "pass_per_game"]
    calc_db$home_run_epa[i] <- results_db_h$avg_run_epa[i] -
      results_db_a$avg_run_epa_def[i] +
      mean_vec[colnames(dev_dat) == "avg_run_epa"]
    calc_db$home_pass_epa[i] <- results_db_h$avg_pass_epa[i] -
      results_db_a$avg_pass_epa_def[i] +
      mean_vec[colnames(dev_dat == "avg_pass_epa")]
    calc_db$away_runs[i] <- results_db_a$runs_per_game[i] + 
      results_db_h$runs_per_game_def[i] + 
      mean_vec[colnames(dev_dat) == "Rushs_per_game"]
    calc_db$away_passes[i] <- results_db_a$pass_per_game[i] +
      results_db_h$pass_per_game_def[i] +
      mean_vec[colnames(dev_dat) == "pass_per_game"]
    calc_db$away_run_epa[i] <- results_db_a$avg_run_epa[i] -
      results_db_h$avg_run_epa_def[i] +
      mean_vec[colnames(dev_dat) == "avg_run_epa"]
    calc_db$away_pass_epa[i] <- results_db_a$avg_pass_epa[i] -
      results_db_h$avg_pass_epa_def[i] +
      mean_vec[colnames(dev_dat) == "avg_pass_epa"]
  }
  calc_db$home_total_run_epa <- calc_db$home_runs * calc_db$home_run_epa
  calc_db$home_total_pass_epa <- calc_db$home_passes * calc_db$home_pass_epa
  calc_db$away_total_run_epa <- calc_db$away_runs * calc_db$away_run_epa
  calc_db$away_total_pass_epa <- calc_db$away_passes * calc_db$away_pass_epa
  
  calc_db$home_points <- calc_db$home_total_run_epa + calc_db$home_total_pass_epa
  calc_db$away_points <- calc_db$away_total_run_epa + calc_db$away_total_pass_epa

  return(calc_db)
}





create_football_pitch <- function(plot_dat, grass_colour= "#538032", line_colour = "#ffffff",
                                  background_colour = "#538032", goal_colour = "#000000", shadow_text = TRUE,
                                  offset = 1, title ="", team_1 = "", team_2 = "", text_size =3,
                                  post_cols_1 = c("white", "white", "white"), post_cols_2 = c("white", "white", "white")){
  #'
  #' This function creates an NFL pitch plot
  #'
  #' @param plot_dat A data frame with a column for color and an alpha vectors.
  #'  100 rows corresponding to yards on a pitch
  #' 
  #'
  #' 
  #'
  #'
  theme_blankPitch = function(size = 12) {
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.background = element_rect(fill=background_colour, colour = NA),
          legend.key = element_rect(color = background_colour, fill = background_colour),
          legend.key.size = unit(1.2, "lines"),
          legend.title = element_text(size = size, face="bold", hjust= 0),
          strip.background = element_rect(colour = background_colour, fill=background_colour, size =0.5),
          panel.background =element_rect(fill = background_colour, colour = background_colour),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0,0,0,0), "lines"),
          plot.title=element_text(size= size *1.2),
          strip.text.y = element_text(colour=background_colour, size= size, angle= 270),
          strip.text.x = element_text(size = size))
  }
  # Create pitch dimensions 
  ymin <- 0
  ymax <- 6800
  xmin <- 0
  xmax <- 11000
  
  post_colour_1a <- post_cols_1[1]
  post_colour_2a <- post_cols_1[2]
  post_colour_3a <- post_cols_1[3]
  post_colour_1b <- post_cols_2[1]
  post_colour_2b <- post_cols_2[2]
  post_colour_3b <- post_cols_2[3]
  
  
  p <- ggplot() + 
    xlim(c(-200, xmax + 200))+
    ylim(c(-200, ymax + 200)) +
    theme_blankPitch() + # Add theme
    geom_rect(aes(xmin = 0, xmax = xmax, ymin =0, ymax = ymax), fill =grass_colour, color = line_colour) 
  
  p <-p + geom_rect(aes(xmin = 500 + ((1-1)*100) +offset, xmax = 500 + (1*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
                    fill = plot_dat$col_vec[1], alpha = plot_dat$alpha_vec[1]) +
    geom_rect(aes(xmin = 500 + ((2-1)*100) +offset, xmax = 500 + (2*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[2], alpha = plot_dat$alpha_vec[2]) +
    geom_rect(aes(xmin = 500 + ((3-1)*100) +offset, xmax = 500 + (3*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[3], alpha = plot_dat$alpha_vec[3]) +
    geom_rect(aes(xmin = 500 + ((4-1)*100) +offset, xmax = 500 + (4*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[4], alpha = plot_dat$alpha_vec[4]) +
    geom_rect(aes(xmin = 500 + ((5-1)*100) +offset, xmax = 500 + (5*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[5], alpha = plot_dat$alpha_vec[5]) +
    geom_rect(aes(xmin = 500 + ((6-1)*100) +offset, xmax = 500 + (6*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[6], alpha = plot_dat$alpha_vec[6]) +
    geom_rect(aes(xmin = 500 + ((7-1)*100) +offset, xmax = 500 + (7*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[7], alpha = plot_dat$alpha_vec[7]) +
    geom_rect(aes(xmin = 500 + ((8-1)*100) +offset, xmax = 500 + (8*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[8], alpha = plot_dat$alpha_vec[8]) +
    geom_rect(aes(xmin = 500 + ((9-1)*100) +offset, xmax = 500 + (9*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[9], alpha = plot_dat$alpha_vec[9]) +
    geom_rect(aes(xmin = 500 + ((10-1)*100) +offset, xmax = 500 + (10*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[10], alpha = plot_dat$alpha_vec[10]) +
    geom_rect(aes(xmin = 500 + ((11-1)*100) +offset, xmax = 500 + (11*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[11], alpha = plot_dat$alpha_vec[11]) +
    geom_rect(aes(xmin = 500 + ((12-1)*100) +offset, xmax = 500 + (12*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[12], alpha = plot_dat$alpha_vec[12]) +
    geom_rect(aes(xmin = 500 + ((13-1)*100) +offset, xmax = 500 + (13*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[13], alpha = plot_dat$alpha_vec[13]) +
    geom_rect(aes(xmin = 500 + ((14-1)*100) +offset, xmax = 500 + (14*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[14], alpha = plot_dat$alpha_vec[14]) +
    geom_rect(aes(xmin = 500 + ((15-1)*100) +offset, xmax = 500 + (15*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[15], alpha = plot_dat$alpha_vec[15]) +
    geom_rect(aes(xmin = 500 + ((16-1)*100) +offset, xmax = 500 + (16*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[16], alpha = plot_dat$alpha_vec[16]) +
    geom_rect(aes(xmin = 500 + ((17-1)*100) +offset, xmax = 500 + (17*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[17], alpha = plot_dat$alpha_vec[17]) +
    geom_rect(aes(xmin = 500 + ((18-1)*100) +offset, xmax = 500 + (18*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[18], alpha = plot_dat$alpha_vec[18]) +
    geom_rect(aes(xmin = 500 + ((19-1)*100) +offset, xmax = 500 + (19*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[19], alpha = plot_dat$alpha_vec[19]) +
    geom_rect(aes(xmin = 500 + ((20-1)*100) +offset, xmax = 500 + (20*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[20], alpha = plot_dat$alpha_vec[20]) +
    geom_rect(aes(xmin = 500 + ((21-1)*100) +offset, xmax = 500 + (21*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[21], alpha = plot_dat$alpha_vec[21]) +
    geom_rect(aes(xmin = 500 + ((22-1)*100) +offset, xmax = 500 + (22*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[22], alpha = plot_dat$alpha_vec[22]) +
    geom_rect(aes(xmin = 500 + ((23-1)*100) +offset, xmax = 500 + (23*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[23], alpha = plot_dat$alpha_vec[23]) +
    geom_rect(aes(xmin = 500 + ((24-1)*100) +offset, xmax = 500 + (24*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[24], alpha = plot_dat$alpha_vec[24]) +
    geom_rect(aes(xmin = 500 + ((25-1)*100) +offset, xmax = 500 + (25*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[25], alpha = plot_dat$alpha_vec[25]) +
    geom_rect(aes(xmin = 500 + ((26-1)*100) +offset, xmax = 500 + (26*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[26], alpha = plot_dat$alpha_vec[26]) +
    geom_rect(aes(xmin = 500 + ((27-1)*100) +offset, xmax = 500 + (27*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[27], alpha = plot_dat$alpha_vec[27]) +
    geom_rect(aes(xmin = 500 + ((28-1)*100) +offset, xmax = 500 + (28*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[28], alpha = plot_dat$alpha_vec[28]) +
    geom_rect(aes(xmin = 500 + ((29-1)*100) +offset, xmax = 500 + (29*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[29], alpha = plot_dat$alpha_vec[29]) +
    geom_rect(aes(xmin = 500 + ((30-1)*100) +offset, xmax = 500 + (30*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[30], alpha = plot_dat$alpha_vec[30]) +
    geom_rect(aes(xmin = 500 + ((31-1)*100) +offset, xmax = 500 + (31*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[31], alpha = plot_dat$alpha_vec[31]) +
    geom_rect(aes(xmin = 500 + ((32-1)*100) +offset, xmax = 500 + (32*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[32], alpha = plot_dat$alpha_vec[32]) +
    geom_rect(aes(xmin = 500 + ((33-1)*100) +offset, xmax = 500 + (33*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[33], alpha = plot_dat$alpha_vec[33]) +
    geom_rect(aes(xmin = 500 + ((34-1)*100) +offset, xmax = 500 + (34*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[34], alpha = plot_dat$alpha_vec[34]) +
    geom_rect(aes(xmin = 500 + ((35-1)*100) +offset, xmax = 500 + (35*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[35], alpha = plot_dat$alpha_vec[35]) +
    geom_rect(aes(xmin = 500 + ((36-1)*100) +offset, xmax = 500 + (36*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[36], alpha = plot_dat$alpha_vec[36]) +
    geom_rect(aes(xmin = 500 + ((37-1)*100) +offset, xmax = 500 + (37*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[37], alpha = plot_dat$alpha_vec[37]) +
    geom_rect(aes(xmin = 500 + ((38-1)*100) +offset, xmax = 500 + (38*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[38], alpha = plot_dat$alpha_vec[38]) +
    geom_rect(aes(xmin = 500 + ((39-1)*100) +offset, xmax = 500 + (39*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[39], alpha = plot_dat$alpha_vec[39]) +
    geom_rect(aes(xmin = 500 + ((40-1)*100) +offset, xmax = 500 + (40*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[40], alpha = plot_dat$alpha_vec[40]) +
    geom_rect(aes(xmin = 500 + ((41-1)*100) +offset, xmax = 500 + (41*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[41], alpha = plot_dat$alpha_vec[41]) +
    geom_rect(aes(xmin = 500 + ((42-1)*100) +offset, xmax = 500 + (42*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[42], alpha = plot_dat$alpha_vec[42]) +
    geom_rect(aes(xmin = 500 + ((43-1)*100) +offset, xmax = 500 + (43*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[43], alpha = plot_dat$alpha_vec[43]) +
    geom_rect(aes(xmin = 500 + ((44-1)*100) +offset, xmax = 500 + (44*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[44], alpha = plot_dat$alpha_vec[44]) +
    geom_rect(aes(xmin = 500 + ((45-1)*100) +offset, xmax = 500 + (45*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[45], alpha = plot_dat$alpha_vec[45]) +
    geom_rect(aes(xmin = 500 + ((46-1)*100) +offset, xmax = 500 + (46*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[46], alpha = plot_dat$alpha_vec[46]) +
    geom_rect(aes(xmin = 500 + ((47-1)*100) +offset, xmax = 500 + (47*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[47], alpha = plot_dat$alpha_vec[47]) +
    geom_rect(aes(xmin = 500 + ((48-1)*100) +offset, xmax = 500 + (48*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[48], alpha = plot_dat$alpha_vec[48]) +
    geom_rect(aes(xmin = 500 + ((49-1)*100) +offset, xmax = 500 + (49*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[49], alpha = plot_dat$alpha_vec[49]) +
    geom_rect(aes(xmin = 500 + ((50-1)*100) +offset, xmax = 500 + (50*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[50], alpha = plot_dat$alpha_vec[50]) +
    geom_rect(aes(xmin = 500 + ((51-1)*100) +offset, xmax = 500 + (51*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[51], alpha = plot_dat$alpha_vec[51]) +
    geom_rect(aes(xmin = 500 + ((52-1)*100) +offset, xmax = 500 + (52*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[52], alpha = plot_dat$alpha_vec[52]) +
    geom_rect(aes(xmin = 500 + ((53-1)*100) +offset, xmax = 500 + (53*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[53], alpha = plot_dat$alpha_vec[53]) +
    geom_rect(aes(xmin = 500 + ((54-1)*100) +offset, xmax = 500 + (54*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[54], alpha = plot_dat$alpha_vec[54]) +
    geom_rect(aes(xmin = 500 + ((55-1)*100) +offset, xmax = 500 + (55*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[55], alpha = plot_dat$alpha_vec[55]) +
    geom_rect(aes(xmin = 500 + ((56-1)*100) +offset, xmax = 500 + (56*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[56], alpha = plot_dat$alpha_vec[56]) +
    geom_rect(aes(xmin = 500 + ((57-1)*100) +offset, xmax = 500 + (57*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[57], alpha = plot_dat$alpha_vec[57]) +
    geom_rect(aes(xmin = 500 + ((58-1)*100) +offset, xmax = 500 + (58*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[58], alpha = plot_dat$alpha_vec[58]) +
    geom_rect(aes(xmin = 500 + ((59-1)*100) +offset, xmax = 500 + (59*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[59], alpha = plot_dat$alpha_vec[59]) +
    geom_rect(aes(xmin = 500 + ((60-1)*100) +offset, xmax = 500 + (60*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[60], alpha = plot_dat$alpha_vec[60]) +
    geom_rect(aes(xmin = 500 + ((61-1)*100) +offset, xmax = 500 + (61*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[61], alpha = plot_dat$alpha_vec[61]) +
    geom_rect(aes(xmin = 500 + ((62-1)*100) +offset, xmax = 500 + (62*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[62], alpha = plot_dat$alpha_vec[62]) +
    geom_rect(aes(xmin = 500 + ((63-1)*100) +offset, xmax = 500 + (63*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[63], alpha = plot_dat$alpha_vec[63]) +
    geom_rect(aes(xmin = 500 + ((64-1)*100) +offset, xmax = 500 + (64*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[64], alpha = plot_dat$alpha_vec[64]) +
    geom_rect(aes(xmin = 500 + ((65-1)*100) +offset, xmax = 500 + (65*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[65], alpha = plot_dat$alpha_vec[65]) +
    geom_rect(aes(xmin = 500 + ((66-1)*100) +offset, xmax = 500 + (66*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[66], alpha = plot_dat$alpha_vec[66]) +
    geom_rect(aes(xmin = 500 + ((67-1)*100) +offset, xmax = 500 + (67*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[67], alpha = plot_dat$alpha_vec[67]) +
    geom_rect(aes(xmin = 500 + ((68-1)*100) +offset, xmax = 500 + (68*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[68], alpha = plot_dat$alpha_vec[68]) +
    geom_rect(aes(xmin = 500 + ((69-1)*100) +offset, xmax = 500 + (69*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[69], alpha = plot_dat$alpha_vec[69]) +
    geom_rect(aes(xmin = 500 + ((70-1)*100) +offset, xmax = 500 + (70*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[70], alpha = plot_dat$alpha_vec[70]) +
    geom_rect(aes(xmin = 500 + ((71-1)*100) +offset, xmax = 500 + (71* 100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[71], alpha = plot_dat$alpha_vec[71]) +
    geom_rect(aes(xmin = 500 + ((72-1)*100) +offset, xmax = 500 + (72*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[72], alpha = plot_dat$alpha_vec[72]) +
    geom_rect(aes(xmin = 500 + ((73-1)*100) +offset, xmax = 500 + (73*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[73], alpha = plot_dat$alpha_vec[73]) +
    geom_rect(aes(xmin = 500 + ((74-1)*100) +offset, xmax = 500 + (74*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[74], alpha = plot_dat$alpha_vec[74]) +
    geom_rect(aes(xmin = 500 + ((75-1)*100) +offset, xmax = 500 + (75*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[75], alpha = plot_dat$alpha_vec[75]) +
    geom_rect(aes(xmin = 500 + ((76-1)*100) +offset, xmax = 500 + (76*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[76], alpha = plot_dat$alpha_vec[76]) +
    geom_rect(aes(xmin = 500 + ((77-1)*100) +offset, xmax = 500 + (77*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[77], alpha = plot_dat$alpha_vec[77]) +
    geom_rect(aes(xmin = 500 + ((78-1)*100) +offset, xmax = 500 + (78*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[78], alpha = plot_dat$alpha_vec[78]) +
    geom_rect(aes(xmin = 500 + ((79-1)*100) +offset, xmax = 500 + (79*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[79], alpha = plot_dat$alpha_vec[79]) +
    geom_rect(aes(xmin = 500 + ((80-1)*100) +offset, xmax = 500 + (80*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[80], alpha = plot_dat$alpha_vec[80]) +
    geom_rect(aes(xmin = 500 + ((81-1)*100) +offset, xmax = 500 + (81*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[81], alpha = plot_dat$alpha_vec[81]) +
    geom_rect(aes(xmin = 500 + ((82-1)*100) +offset, xmax = 500 + (82*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[82], alpha = plot_dat$alpha_vec[82]) +
    geom_rect(aes(xmin = 500 + ((83-1)*100) +offset, xmax = 500 + (83*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[83], alpha = plot_dat$alpha_vec[83]) +
    geom_rect(aes(xmin = 500 + ((84-1)*100) +offset, xmax = 500 + (84*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[84], alpha = plot_dat$alpha_vec[84]) +
    geom_rect(aes(xmin = 500 + ((85-1)*100) +offset, xmax = 500 + (85*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[85], alpha = plot_dat$alpha_vec[85]) +
    geom_rect(aes(xmin = 500 + ((86-1)*100) +offset, xmax = 500 + (86*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[86], alpha = plot_dat$alpha_vec[86]) +
    geom_rect(aes(xmin = 500 + ((87-1)*100) +offset, xmax = 500 + (87*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[87], alpha = plot_dat$alpha_vec[87]) +
    geom_rect(aes(xmin = 500 + ((88-1)*100) +offset, xmax = 500 + (88*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[88], alpha = plot_dat$alpha_vec[88]) +
    geom_rect(aes(xmin = 500 + ((89-1)*100) +offset, xmax = 500 + (89*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[89], alpha = plot_dat$alpha_vec[89]) +
    geom_rect(aes(xmin = 500 + ((90-1)*100) +offset, xmax = 500 + (90*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[90], alpha = plot_dat$alpha_vec[90]) +
    geom_rect(aes(xmin = 500 + ((91-1)*100) +offset, xmax = 500 + (91*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[91], alpha = plot_dat$alpha_vec[91]) +
    geom_rect(aes(xmin = 500 + ((92-1)*100) +offset, xmax = 500 + (92*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[92], alpha = plot_dat$alpha_vec[92]) +
    geom_rect(aes(xmin = 500 + ((93-1)*100) +offset, xmax = 500 + (93*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[93], alpha = plot_dat$alpha_vec[93]) +
    geom_rect(aes(xmin = 500 + ((94-1)*100) +offset, xmax = 500 + (94*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[94], alpha = plot_dat$alpha_vec[94]) +
    geom_rect(aes(xmin = 500 + ((95-1)*100) +offset, xmax = 500 + (95*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[95], alpha = plot_dat$alpha_vec[95]) +
    geom_rect(aes(xmin = 500 + ((96-1)*100) +offset, xmax = 500 + (96*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[96], alpha = plot_dat$alpha_vec[96]) +
    geom_rect(aes(xmin = 500 + ((97-1)*100) +offset, xmax = 500 + (97*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[97], alpha = plot_dat$alpha_vec[97]) +
    geom_rect(aes(xmin = 500 + ((98-1)*100) +offset, xmax = 500 + (98*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[98], alpha = plot_dat$alpha_vec[98]) +
    geom_rect(aes(xmin = 500 + ((99-1)*100) +offset, xmax = 500 + (99*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[99], alpha = plot_dat$alpha_vec[99]) +
    geom_rect(aes(xmin = 500 + ((100-1)*100) +offset, xmax = 500 + (100*100) -offset, ymin = 0 + offset, ymax = ymax - offset),
              fill = plot_dat$col_vec[99], alpha = plot_dat$alpha_vec[99]) 
  
  
  # Add Lines
  p <- p + 
    geom_segment(aes(x = 500, xend = 500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 1500, xend = 1500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 2500, xend = 2500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 3500, xend = 3500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 4500, xend = 4500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 5500, xend = 5500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 6500, xend = 6500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 7500, xend = 7500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 8500, xend = 8500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 9500, xend = 9500, y = ymin, yend = ymax), colour = line_colour) +
    geom_segment(aes(x = 10500, xend = 10500, y = ymin, yend = ymax), colour = line_colour) 
  # Add yard markers
  i <- 0
  p <- p + 
    geom_segment(aes(x= 500 + ((1)*100), xend = 500 + ((1)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((1)*100), xend = 500 + ((1)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((1)*100), xend = 500 + ((1)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((1)*100), xend = 500 + ((1)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((2)*100), xend = 500 + ((2)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((2)*100), xend = 500 + ((2)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((2)*100), xend = 500 + ((2)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((2)*100), xend = 500 + ((2)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((3)*100), xend = 500 + ((3)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((3)*100), xend = 500 + ((3)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((3)*100), xend = 500 + ((3)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((3)*100), xend = 500 + ((3)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((4)*100), xend = 500 + ((4)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((4)*100), xend = 500 + ((4)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((4)*100), xend = 500 + ((4)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((4)*100), xend = 500 + ((4)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((5)*100), xend = 500 + ((5)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((5)*100), xend = 500 + ((5)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((5)*100), xend = 500 + ((5)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((5)*100), xend = 500 + ((5)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((6)*100), xend = 500 + ((6)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((6)*100), xend = 500 + ((6)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((6)*100), xend = 500 + ((6)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((6)*100), xend = 500 + ((6)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5)  +
    
    geom_segment(aes(x= 500 + ((7)*100), xend = 500 + ((7)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((7)*100), xend = 500 + ((7)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((7)*100), xend = 500 + ((7)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((7)*100), xend = 500 + ((7)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((8)*100), xend = 500 + ((8)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((8)*100), xend = 500 + ((8)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((8)*100), xend = 500 + ((8)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((8)*100), xend = 500 + ((8)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((9)*100), xend = 500 + ((9)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((9)*100), xend = 500 + ((9)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((9)*100), xend = 500 + ((9)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((9)*100), xend = 500 + ((9)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((10)*100), xend = 500 + ((10)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((10)*100), xend = 500 + ((10)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((10)*100), xend = 500 + ((10)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((10)*100), xend = 500 + ((10)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) + 
    
    geom_segment(aes(x= 500 + ((11)*100), xend = 500 + ((11)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((11)*100), xend = 500 + ((11)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((11)*100), xend = 500 + ((11)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((11)*100), xend = 500 + ((11)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((12)*100), xend = 500 + ((12)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((12)*100), xend = 500 + ((12)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((12)*100), xend = 500 + ((12)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((12)*100), xend = 500 + ((12)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((13)*100), xend = 500 + ((13)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((13)*100), xend = 500 + ((13)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((13)*100), xend = 500 + ((13)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((13)*100), xend = 500 + ((13)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((14)*100), xend = 500 + ((14)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((14)*100), xend = 500 + ((14)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((14)*100), xend = 500 + ((14)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((14)*100), xend = 500 + ((14)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((15)*100), xend = 500 + ((15)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((15)*100), xend = 500 + ((15)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((15)*100), xend = 500 + ((15)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((15)*100), xend = 500 + ((15)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((16)*100), xend = 500 + ((16)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((16)*100), xend = 500 + ((16)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((16)*100), xend = 500 + ((16)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((16)*100), xend = 500 + ((16)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((17)*100), xend = 500 + ((17)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((17)*100), xend = 500 + ((17)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((17)*100), xend = 500 + ((17)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((17)*100), xend = 500 + ((17)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((18)*100), xend = 500 + ((18)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((18)*100), xend = 500 + ((18)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((18)*100), xend = 500 + ((18)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((18)*100), xend = 500 + ((18)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((19)*100), xend = 500 + ((19)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((19)*100), xend = 500 + ((19)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((19)*100), xend = 500 + ((19)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((19)*100), xend = 500 + ((19)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((20)*100), xend = 500 + ((20)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((20)*100), xend = 500 + ((20)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((20)*100), xend = 500 + ((20)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((20)*100), xend = 500 + ((20)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((21)*100), xend = 500 + ((21)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((21)*100), xend = 500 + ((21)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((21)*100), xend = 500 + ((21)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((21)*100), xend = 500 + ((21)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((22)*100), xend = 500 + ((22)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((22)*100), xend = 500 + ((22)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((22)*100), xend = 500 + ((22)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((22)*100), xend = 500 + ((22)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((23)*100), xend = 500 + ((23)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((23)*100), xend = 500 + ((23)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((23)*100), xend = 500 + ((23)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((23)*100), xend = 500 + ((23)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((24)*100), xend = 500 + ((24)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((24)*100), xend = 500 + ((24)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((24)*100), xend = 500 + ((24)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((24)*100), xend = 500 + ((24)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((25)*100), xend = 500 + ((25)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((25)*100), xend = 500 + ((25)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((25)*100), xend = 500 + ((25)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((25)*100), xend = 500 + ((25)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((26)*100), xend = 500 + ((26)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((26)*100), xend = 500 + ((26)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((26)*100), xend = 500 + ((26)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((26)*100), xend = 500 + ((26)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((27)*100), xend = 500 + ((27)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((27)*100), xend = 500 + ((27)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((27)*100), xend = 500 + ((27)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((27)*100), xend = 500 + ((27)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((28)*100), xend = 500 + ((28)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((28)*100), xend = 500 + ((28)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((28)*100), xend = 500 + ((28)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((28)*100), xend = 500 + ((28)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((29)*100), xend = 500 + ((29)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((29)*100), xend = 500 + ((29)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((29)*100), xend = 500 + ((29)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((29)*100), xend = 500 + ((29)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((30)*100), xend = 500 + ((30)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((30)*100), xend = 500 + ((30)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((30)*100), xend = 500 + ((30)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((30)*100), xend = 500 + ((30)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((31)*100), xend = 500 + ((31)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((31)*100), xend = 500 + ((31)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((31)*100), xend = 500 + ((31)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((31)*100), xend = 500 + ((31)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((32)*100), xend = 500 + ((32)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((32)*100), xend = 500 + ((32)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((32)*100), xend = 500 + ((32)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((32)*100), xend = 500 + ((32)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((33)*100), xend = 500 + ((33)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((33)*100), xend = 500 + ((33)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((33)*100), xend = 500 + ((33)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((33)*100), xend = 500 + ((33)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((34)*100), xend = 500 + ((34)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((34)*100), xend = 500 + ((34)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((34)*100), xend = 500 + ((34)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((34)*100), xend = 500 + ((34)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((35)*100), xend = 500 + ((35)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((35)*100), xend = 500 + ((35)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((35)*100), xend = 500 + ((35)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((35)*100), xend = 500 + ((35)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((36)*100), xend = 500 + ((36)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((36)*100), xend = 500 + ((36)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((36)*100), xend = 500 + ((36)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((36)*100), xend = 500 + ((36)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((37)*100), xend = 500 + ((37)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((37)*100), xend = 500 + ((37)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((37)*100), xend = 500 + ((37)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((37)*100), xend = 500 + ((37)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((38)*100), xend = 500 + ((38)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((38)*100), xend = 500 + ((38)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((38)*100), xend = 500 + ((38)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((38)*100), xend = 500 + ((38)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((39)*100), xend = 500 + ((39)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((39)*100), xend = 500 + ((39)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((39)*100), xend = 500 + ((39)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((39)*100), xend = 500 + ((39)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((40)*100), xend = 500 + ((40)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((40)*100), xend = 500 + ((40)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((40)*100), xend = 500 + ((40)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((40)*100), xend = 500 + ((40)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((41)*100), xend = 500 + ((41)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((41)*100), xend = 500 + ((41)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((41)*100), xend = 500 + ((41)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((41)*100), xend = 500 + ((41)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((42)*100), xend = 500 + ((42)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((42)*100), xend = 500 + ((42)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((42)*100), xend = 500 + ((42)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((42)*100), xend = 500 + ((42)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((43)*100), xend = 500 + ((43)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((43)*100), xend = 500 + ((43)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((43)*100), xend = 500 + ((43)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((43)*100), xend = 500 + ((43)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((44)*100), xend = 500 + ((44)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((44)*100), xend = 500 + ((44)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((44)*100), xend = 500 + ((44)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((44)*100), xend = 500 + ((44)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((45)*100), xend = 500 + ((45)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((45)*100), xend = 500 + ((45)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((45)*100), xend = 500 + ((45)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((45)*100), xend = 500 + ((45)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((46)*100), xend = 500 + ((46)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((46)*100), xend = 500 + ((46)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((46)*100), xend = 500 + ((46)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((46)*100), xend = 500 + ((46)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((47)*100), xend = 500 + ((47)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((47)*100), xend = 500 + ((47)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((47)*100), xend = 500 + ((47)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((47)*100), xend = 500 + ((47)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((48)*100), xend = 500 + ((48)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((48)*100), xend = 500 + ((48)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((48)*100), xend = 500 + ((48)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((48)*100), xend = 500 + ((48)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((49)*100), xend = 500 + ((49)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((49)*100), xend = 500 + ((49)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((49)*100), xend = 500 + ((49)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((49)*100), xend = 500 + ((49)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((50)*100), xend = 500 + ((50)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((50)*100), xend = 500 + ((50)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((50)*100), xend = 500 + ((50)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((50)*100), xend = 500 + ((50)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((51)*100), xend = 500 + ((51)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((51)*100), xend = 500 + ((51)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((51)*100), xend = 500 + ((51)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((51)*100), xend = 500 + ((51)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((52)*100), xend = 500 + ((52)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((52)*100), xend = 500 + ((52)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((52)*100), xend = 500 + ((52)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((52)*100), xend = 500 + ((52)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((53)*100), xend = 500 + ((53)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((53)*100), xend = 500 + ((53)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((53)*100), xend = 500 + ((53)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((53)*100), xend = 500 + ((53)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((54)*100), xend = 500 + ((54)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((54)*100), xend = 500 + ((54)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((54)*100), xend = 500 + ((54)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((54)*100), xend = 500 + ((54)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((55)*100), xend = 500 + ((55)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((55)*100), xend = 500 + ((55)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((55)*100), xend = 500 + ((55)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((55)*100), xend = 500 + ((55)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((56)*100), xend = 500 + ((56)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((56)*100), xend = 500 + ((56)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((56)*100), xend = 500 + ((56)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((56)*100), xend = 500 + ((56)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((57)*100), xend = 500 + ((57)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((57)*100), xend = 500 + ((57)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((57)*100), xend = 500 + ((57)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((57)*100), xend = 500 + ((57)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((58)*100), xend = 500 + ((58)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((58)*100), xend = 500 + ((58)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((58)*100), xend = 500 + ((58)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((58)*100), xend = 500 + ((58)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((59)*100), xend = 500 + ((59)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((59)*100), xend = 500 + ((59)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((59)*100), xend = 500 + ((59)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((59)*100), xend = 500 + ((59)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((60)*100), xend = 500 + ((60)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((60)*100), xend = 500 + ((60)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((60)*100), xend = 500 + ((60)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((60)*100), xend = 500 + ((60)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((61)*100), xend = 500 + ((61)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((61)*100), xend = 500 + ((61)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((61)*100), xend = 500 + ((61)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((61)*100), xend = 500 + ((61)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((62)*100), xend = 500 + ((62)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((62)*100), xend = 500 + ((62)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((62)*100), xend = 500 + ((62)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((62)*100), xend = 500 + ((62)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((63)*100), xend = 500 + ((63)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((63)*100), xend = 500 + ((63)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((63)*100), xend = 500 + ((63)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((63)*100), xend = 500 + ((63)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((64)*100), xend = 500 + ((64)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((64)*100), xend = 500 + ((64)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((64)*100), xend = 500 + ((64)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((64)*100), xend = 500 + ((64)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((65)*100), xend = 500 + ((65)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((65)*100), xend = 500 + ((65)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((65)*100), xend = 500 + ((65)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((65)*100), xend = 500 + ((65)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((66)*100), xend = 500 + ((66)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((66)*100), xend = 500 + ((66)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((66)*100), xend = 500 + ((66)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((66)*100), xend = 500 + ((66)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5)  +
    
    geom_segment(aes(x= 500 + ((67)*100), xend = 500 + ((67)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((67)*100), xend = 500 + ((67)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((67)*100), xend = 500 + ((67)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((67)*100), xend = 500 + ((67)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((68)*100), xend = 500 + ((68)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((68)*100), xend = 500 + ((68)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((68)*100), xend = 500 + ((68)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((68)*100), xend = 500 + ((68)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((69)*100), xend = 500 + ((69)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((69)*100), xend = 500 + ((69)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((69)*100), xend = 500 + ((69)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((69)*100), xend = 500 + ((69)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((70)*100), xend = 500 + ((70)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((70)*100), xend = 500 + ((70)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((70)*100), xend = 500 + ((70)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((70)*100), xend = 500 + ((70)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((71)*100), xend = 500 + ((71)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((71)*100), xend = 500 + ((71)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((71)*100), xend = 500 + ((71)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((71)*100), xend = 500 + ((71)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((72)*100), xend = 500 + ((72)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((72)*100), xend = 500 + ((72)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((72)*100), xend = 500 + ((72)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((72)*100), xend = 500 + ((72)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((73)*100), xend = 500 + ((73)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((73)*100), xend = 500 + ((73)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((73)*100), xend = 500 + ((73)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((73)*100), xend = 500 + ((73)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((74)*100), xend = 500 + ((74)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((74)*100), xend = 500 + ((74)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((74)*100), xend = 500 + ((74)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((74)*100), xend = 500 + ((74)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((75)*100), xend = 500 + ((75)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((75)*100), xend = 500 + ((75)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((75)*100), xend = 500 + ((75)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((75)*100), xend = 500 + ((75)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((76)*100), xend = 500 + ((76)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((76)*100), xend = 500 + ((76)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((76)*100), xend = 500 + ((76)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((76)*100), xend = 500 + ((76)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((77)*100), xend = 500 + ((77)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((77)*100), xend = 500 + ((77)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((77)*100), xend = 500 + ((77)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((77)*100), xend = 500 + ((77)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((78)*100), xend = 500 + ((78)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((78)*100), xend = 500 + ((78)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((78)*100), xend = 500 + ((78)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((78)*100), xend = 500 + ((78)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((79)*100), xend = 500 + ((79)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((79)*100), xend = 500 + ((79)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((79)*100), xend = 500 + ((79)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((79)*100), xend = 500 + ((79)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((80)*100), xend = 500 + ((80)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((80)*100), xend = 500 + ((80)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((80)*100), xend = 500 + ((80)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((80)*100), xend = 500 + ((80)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((81)*100), xend = 500 + ((81)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((81)*100), xend = 500 + ((81)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((81)*100), xend = 500 + ((81)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((81)*100), xend = 500 + ((81)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((82)*100), xend = 500 + ((82)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((82)*100), xend = 500 + ((82)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((82)*100), xend = 500 + ((82)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((82)*100), xend = 500 + ((82)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((83)*100), xend = 500 + ((83)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((83)*100), xend = 500 + ((83)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((83)*100), xend = 500 + ((83)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((83)*100), xend = 500 + ((83)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((84)*100), xend = 500 + ((84)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((84)*100), xend = 500 + ((84)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((84)*100), xend = 500 + ((84)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((84)*100), xend = 500 + ((84)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((85)*100), xend = 500 + ((85)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((85)*100), xend = 500 + ((85)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((85)*100), xend = 500 + ((85)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((85)*100), xend = 500 + ((85)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((86)*100), xend = 500 + ((86)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((86)*100), xend = 500 + ((86)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((86)*100), xend = 500 + ((86)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((86)*100), xend = 500 + ((86)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((87)*100), xend = 500 + ((87)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((87)*100), xend = 500 + ((87)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((87)*100), xend = 500 + ((87)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((87)*100), xend = 500 + ((87)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((88)*100), xend = 500 + ((88)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((88)*100), xend = 500 + ((88)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((88)*100), xend = 500 + ((88)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((88)*100), xend = 500 + ((88)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) + 
    
    geom_segment(aes(x= 500 + ((89)*100), xend = 500 + ((89)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((89)*100), xend = 500 + ((89)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((89)*100), xend = 500 + ((89)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((89)*100), xend = 500 + ((89)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((90)*100), xend = 500 + ((90)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((90)*100), xend = 500 + ((90)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((90)*100), xend = 500 + ((90)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((90)*100), xend = 500 + ((90)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((91)*100), xend = 500 + ((91)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((91)*100), xend = 500 + ((91)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((91)*100), xend = 500 + ((91)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((91)*100), xend = 500 + ((91)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((92)*100), xend = 500 + ((92)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((92)*100), xend = 500 + ((92)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((92)*100), xend = 500 + ((92)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((92)*100), xend = 500 + ((92)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((93)*100), xend = 500 + ((93)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((93)*100), xend = 500 + ((93)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((93)*100), xend = 500 + ((93)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((93)*100), xend = 500 + ((93)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((94)*100), xend = 500 + ((94)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((94)*100), xend = 500 + ((94)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((94)*100), xend = 500 + ((94)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((94)*100), xend = 500 + ((94)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((95)*100), xend = 500 + ((95)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((95)*100), xend = 500 + ((95)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((95)*100), xend = 500 + ((95)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((95)*100), xend = 500 + ((95)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((96)*100), xend = 500 + ((96)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((96)*100), xend = 500 + ((96)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((96)*100), xend = 500 + ((96)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((96)*100), xend = 500 + ((96)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((97)*100), xend = 500 + ((97)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((97)*100), xend = 500 + ((97)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((97)*100), xend = 500 + ((97)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((97)*100), xend = 500 + ((97)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((98)*100), xend = 500 + ((98)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((98)*100), xend = 500 + ((98)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((98)*100), xend = 500 + ((98)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((98)*100), xend = 500 + ((98)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((99)*100), xend = 500 + ((99)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((99)*100), xend = 500 + ((99)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((99)*100), xend = 500 + ((99)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((99)*100), xend = 500 + ((99)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) +
    
    geom_segment(aes(x= 500 + ((100)*100), xend = 500 + ((100)*100), y = ymin , yend = ymin + 100), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((100)*100), xend = 500 + ((100)*100), y = (ymax/2) - 500 , yend = (ymax/2) - 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((100)*100), xend = 500 + ((100)*100), y = (ymax/2) + 500 , yend = (ymax/2) + 600), colour = line_colour, linetype = 11, alpha =0.5) +
    geom_segment(aes(x= 500 + ((100)*100), xend = 500 + ((100)*100), y = ymax- 100 , yend = ymax ), colour = line_colour, linetype = 11, alpha =0.5) 
  
  # Add goal posts
  p <- p + 
    geom_segment(aes(x = 250, xend = -50, y=(ymax/2) + 330, yend = (ymax/2) + 405), colour = line_colour) +
    geom_segment(aes(x = 250, xend = -50, y=(ymax/2) - 330, yend = (ymax/2) - 405), colour = line_colour) +
    geom_segment(aes(x = xmax - 250, xend = xmax + 50, y=(ymax/2) + 330, yend = (ymax/2) + 405), colour = line_colour) +
    geom_segment(aes(x = xmax - 250, xend = xmax + 50, y=(ymax/2) - 330, yend = (ymax/2) - 405), colour = line_colour) +
    geom_segment(aes(x =  250 , xend = 250, y = (ymax/2) + 330, yend = (ymax/2) - 330), colour = line_colour) +
    geom_segment(aes(x = xmax - 250 , xend = xmax- 250, y = (ymax/2) + 330, yend = (ymax/2) - 330), colour = line_colour) +
    geom_segment(aes(x = 250, xend= 100, y = (ymax/2), yend= (ymax/2)), colour = line_colour) +
    geom_segment(aes(x = xmax - 250, xend= xmax- 100 , y = (ymax/2), yend= (ymax/2)), colour = line_colour)
  
  # Add yard markers
  p <- p +
    # 10 Yards
    geom_text(aes(x = 1200, y =250, label = "<1"), colour = "white", size =5) +
    geom_text(aes(x = 1700, y =250, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 1200, y = ymax - 250, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 1700, y = ymax - 250, label = "1"), colour = "white", size =5, angle = 180) +
    # 20 Yards
    geom_text(aes(x = 2200, y =250, label = "<2"), colour = "white", size =5) +
    geom_text(aes(x = 2700, y =250, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 2200, y = ymax - 250, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 2700, y = ymax - 250, label = "2"), colour = "white", size =5, angle = 180) +
    # 30 Yards
    geom_text(aes(x = 3200, y =250, label = "<3"), colour = "white", size =5) +
    geom_text(aes(x = 3700, y =250, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 3200, y = ymax - 250, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 3700, y = ymax - 250, label = "3"), colour = "white", size =5, angle = 180) +
    # 40 Yards
    geom_text(aes(x = 4200, y =250, label = "<4"), colour = "white", size =5) +
    geom_text(aes(x = 4700, y =250, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 4200, y = ymax - 250, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 4700, y = ymax - 250, label = "4"), colour = "white", size =5, angle = 180) +
    # 50 Yards
    geom_text(aes(x = 5300, y =250, label = "5"), colour = "white", size =5) +
    geom_text(aes(x = 5700, y =250, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 5300, y = ymax - 250, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 5700, y = ymax - 250, label = "5"), colour = "white", size =5, angle = 180) +
    # 60 Yards
    geom_text(aes(x = 6300, y =250, label = "4"), colour = "white", size =5) +
    geom_text(aes(x = 6800, y =250, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 6300, y = ymax - 250, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 6800, y = ymax - 250, label = "<4"), colour = "white", size =5, angle = 180) +
    # 70 Yards
    geom_text(aes(x = 7300, y =250, label = "3"), colour = "white", size =5) +
    geom_text(aes(x = 7800, y =250, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 7300, y = ymax - 250, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 7800, y = ymax - 250, label = "<3"), colour = "white", size =5, angle = 180) +
    # 80 Yards
    geom_text(aes(x = 8300, y =250, label = "2"), colour = "white", size =5) +
    geom_text(aes(x = 8800, y =250, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 8300, y = ymax - 250, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 8800, y = ymax - 250, label = "<2"), colour = "white", size =5, angle = 180) +
    # 90 Yards
    geom_text(aes(x = 9300, y =250, label = "1"), colour = "white", size =5) +
    geom_text(aes(x = 9800, y =250, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 9300, y = ymax - 250, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 9800, y = ymax - 250, label = "<1"), colour = "white", size =5, angle = 180) 
  
  # Plot Labels
  p <- p + geom_text(aes(x = (xmax/2), y=ymax+200), label = paste(title), 
                     colour = "white", size =6) 
  p <- p + geom_text(aes(x = (xmin + 270), y=ymax/2), label = paste("Own               End Zone"), 
                     colour = "white",  size =6, angle = 90) 
  p <- p + geom_text(aes(x = (xmax - 270), y=ymax/2), label = paste("Opponent          End Zone"), 
                     colour = "white",  size =6, angle = 270) 
  
  return(p)
}

map2color <- function(x, pal, limits = NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x, seq(limits[1], limits[2], length.out =length(pal) +1), all.inside = TRUE)]
}

create_epa_plot <- function(data, col_vec = c("blue", "red"), full_dat){
  mypal <- colorRampPalette(col_vec)(200)
  col_vec <- map2color(data$ep_vec, mypal, limits = c(min(full_dat$ep_vec), max(full_dat$ep_vec)))
  alpha_vec <- rep(0.75, length(col_vec))
  plot_dat <- cbind.data.frame(col_vec, alpha_vec)
  return(plot_dat)
}




























































































