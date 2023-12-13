##################################################
########## Sports Analytics Function V1 ##########
##################################################
#
#
#
#
#

# Create map to color function
map2color <- function(x, pal, limits = NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x, seq(limits[1], limits[2], length.out =length(pal) +1), all.inside = TRUE)]
}



#### Create team summary function ####
create_team_summary <- function(pbp, team_logos){
  #'
  #' This function calculates team summaries for
  #' offense and defense
  #' 
  #' @param pbp Play by play data
  #' @param team_logos The team logos dataset
  #'
  #'
  
  
  teams_to_use <- c("Boston College", "Clemson", "Florida State", "Louisville", "NC State" ,
                    "Notre Dame", "Syracuse", "Wake Forest",  "Duke", "Georgia Tech" ,
                    "Miami" , "North Carolina", "Pittsburgh", "Virginia", "Virginia Tech" ,
                    "Indiana", "Maryland", "Michigan", "Michigan State", "Penn State",
                    "Rutgers", "Illinois",  "Minnesota", "Nebraska", "Northwestern", 
                    "Ohio State", "Purdue", "Wisconsin", "Baylor", "Iowa State",
                    "Kansas", "Kansas State", "Oklahoma", "Oklahoma State",  "TCU",
                    "Texas", "Texas Tech",  "Air Force", "Army", "Navy",  "BYU",
                    "New Mexico State", "Connecticut", "Arizona", "Arizona State",
                    "UCLA", "Colorado",  "Oregon",  "Oregon State", "USC",
                    "Stanford", "Utah", "Washington", "Washington State",
                    "Florida", "Georgia", "Kentucky",  "Missouri", "South Carolina",
                    "Tennessee", "Vanderbilt",  "Alabama", "Arkansas", "Auburn", 
                    "Louisiana", "Mississippi State",  "Ole Miss",  "Texas A&M",
                    "Cincinnati")
  
  team_logos <- team_logos[team_logos$school %in% teams_to_use,]
  
  # Create data frame to store offense and defense result
  off_res <- def_res <- as.data.frame(matrix(NA, nrow = nrow(team_logos),
                                             ncol = 8))
  # Create vector to store number of games played
  games <- rep(NA, nrow(team_logos))
  # Set names on dataset
  names(off_res) <- names(def_res) <- c("pass_n", "pass_yards",
                                        "pass_epa", "pass_wpa",
                                        "rush_n", "rush_yards", 
                                        "rush_epa", "rush_wpa")
  
  # Loop through teams
  for(i in 1:nrow(team_logos)){
    
    # Extract offensive statistics
    temp <- pbp[pbp$pos_team == team_logos$school[i],]
    
    # Store number of games
    games[i] <- length(unique(temp$game_id))
    # Store number of passes
    off_res$pass_n[i] <- sum(temp$playtype == "Pass", na.rm = TRUE)
    # Store pass yards
    off_res$pass_yards[i] <- sum(temp$yards_gained[temp$playtype == "Pass"],
                                 na.rm = TRUE)
    # Store pass EPA
    off_res$pass_epa[i] <- sum(temp$EPA[temp$playtype == "Pass"],
                               na.rm = TRUE)
    # Store pass WPA
    off_res$pass_wpa[i] <- sum(temp$wpa[temp$playtype == "Pass"],
                               na.rm = TRUE)
    
    # Store number of rushes
    off_res$rush_n[i] <- sum(temp$playtype == "Rush", na.rm = TRUE)
    # Store rush yards
    off_res$rush_yards[i] <- sum(temp$yards_gained[temp$playtype == "Rush"],
                                 na.rm = TRUE)
    # Store rush EPA
    off_res$rush_epa[i] <- sum(temp$EPA[temp$playtype == "Rush"],
                               na.rm = TRUE)
    # Store rush WPA
    off_res$rush_wpa[i] <- sum(temp$wpa[temp$playtype == "Rush"],
                               na.rm = TRUE)
    
    # Extract Defensive Statistics
    tempd <- pbp[pbp$def_pos_team == team_logos$school[i],]
    
    
    # Store number of passes
    def_res$pass_n[i] <- sum(tempd$playtype == "Pass", na.rm = TRUE)
    # Store pass yards
    def_res$pass_yards[i] <- sum(tempd$yards_gained[tempd$playtype == "Pass"],
                                 na.rm = TRUE)
    # Store pass EPA
    def_res$pass_epa[i] <- sum(tempd$EPA[tempd$playtype == "Pass"],
                               na.rm = TRUE)
    # Store pass WPA
    def_res$pass_wpa[i] <- sum(tempd$wpa[tempd$playtype == "Pass"],
                               na.rm = TRUE)
    
    # Store number of rushes
    def_res$rush_n[i] <- sum(tempd$playtype == "Rush", na.rm = TRUE)
    # Store rush yards
    def_res$rush_yards[i] <- sum(tempd$yards_gained[tempd$playtype == "Rush"],
                                 na.rm = TRUE)
    # Store rush EPA
    def_res$rush_epa[i] <- sum(tempd$EPA[tempd$playtype == "Rush"],
                               na.rm = TRUE)
    # Store rush WPA
    def_res$rush_wpa[i] <- sum(tempd$wpa[tempd$playtype == "Rush"],
                               na.rm = TRUE)
  }
  
  # Calculate offensive per play statistics
  off_res$pass_yards_per_play <- off_res$pass_yards/off_res$pass_n
  off_res$pass_epa_per_play <- off_res$pass_ep/off_res$pass_n
  off_res$pass_wpa_per_play <- off_res$pass_wpa/off_res$pass_n
  
  off_res$rush_yards_per_play <- off_res$rush_yards/off_res$rush_n
  off_res$rush_epa_per_play <- off_res$rush_epa/off_res$rush_n
  off_res$rush_wpa_per_play <- off_res$rush_wpa/off_res$rush_n
  
  # Calculate defensive per play statistics
  def_res$pass_yards_per_play <- def_res$pass_yards/def_res$pass_n
  def_res$pass_epa_per_play <- def_res$pass_epa/def_res$pass_n
  def_res$pass_wpa_per_play <- def_res$pass_wpa/def_res$pass_n
  
  def_res$rush_yards_per_play <- def_res$rush_yards/def_res$rush_n
  def_res$rush_epa_per_play <- def_res$rush_epa/def_res$rush_n
  def_res$rush_wpa_per_play <- def_res$rush_wpa/def_res$rush_n
  
  # Fix names of defensive stats
  names(def_res) <- paste(names(def_res), "_def", sep ="")
  
  # Join Results
  res <- cbind.data.frame(team_logos, games, off_res, def_res)
  
  # Return results
  return(res)
}



#### Create Blank Pitch ####

create_blank_pitch <- function(grass_colour= "#538032",
                               line_colour = "#ffffff",
                               background_colour = "#538032",
                               goal_colour = "#000000",
                               shadow_text = TRUE,
                               offset = 1,
                               text_size =3,
                               post_cols_1 = c("white", "blue", "gold"),
                               post_cols_2 = c("white", "white", "white")){
  #'
  #' This function creates a blank pitch plot which can then be modified
  #'
  #'
  #'
  #'
  #'
  
  # Create Pitch Theme
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
  
  # Set pitch parameters
  ymin <- 0
  ymax <- 6800
  xmin <- 0
  xmax <- 11000
  
  # Set post colors
  post_colour_1a <- post_cols_1[1]
  post_colour_2a <- post_cols_1[2]
  post_colour_3a <- post_cols_1[3]
  post_colour_1b <- post_cols_2[1]
  post_colour_2b <- post_cols_2[2]
  post_colour_3b <- post_cols_2[3]
  
  # Create basic pitch
  p <- ggplot() + 
    xlim(c(-200, xmax + 200))+
    ylim(c(-200, ymax + 200)) +
    theme_blankPitch() + # Add theme
    geom_rect(aes(xmin = 0, xmax = xmax, ymin =0, ymax = ymax), fill =grass_colour, color = line_colour) 
  
  
  # Create data for lines
  seg_data <- data.frame(x = c(c(500, 1500, 2500, 3500, 4500,
                                 5500, 6500, 7500, 8500, 9500,
                                 10500),c(rep(500 + ((1)*100), 4),
                                          rep(500 + ((2)*100), 4),
                                          rep(500 + ((3)*100), 4),
                                          rep(500 + ((4)*100), 4),
                                          rep(500 + ((5)*100), 4),
                                          rep(500 + ((6)*100), 4),
                                          rep(500 + ((7)*100), 4),
                                          rep(500 + ((8)*100), 4),
                                          rep(500 + ((9)*100), 4),
                                          rep(500 + ((10)*100), 4),
                                          rep(500 + ((11)*100), 4),
                                          rep(500 + ((12)*100), 4),
                                          rep(500 + ((13)*100), 4),
                                          rep(500 + ((14)*100), 4),
                                          rep(500 + ((15)*100), 4),
                                          rep(500 + ((16)*100), 4),
                                          rep(500 + ((17)*100), 4),
                                          rep(500 + ((18)*100), 4),
                                          rep(500 + ((19)*100), 4),
                                          rep(500 + ((20)*100), 4),
                                          rep(500 + ((21)*100), 4),
                                          rep(500 + ((22)*100), 4),
                                          rep(500 + ((23)*100), 4),
                                          rep(500 + ((24)*100), 4),
                                          rep(500 + ((25)*100), 4),
                                          rep(500 + ((26)*100), 4),
                                          rep(500 + ((27)*100), 4),
                                          rep(500 + ((28)*100), 4),
                                          rep(500 + ((29)*100), 4),
                                          rep(500 + ((30)*100), 4),
                                          rep(500 + ((31)*100), 4),
                                          rep(500 + ((32)*100), 4),
                                          rep(500 + ((33)*100), 4),
                                          rep(500 + ((34)*100), 4),
                                          rep(500 + ((35)*100), 4),
                                          rep(500 + ((36)*100), 4),
                                          rep(500 + ((37)*100), 4),
                                          rep(500 + ((38)*100), 4),
                                          rep(500 + ((39)*100), 4),
                                          rep(500 + ((40)*100), 4),
                                          rep(500 + ((41)*100), 4),
                                          rep(500 + ((42)*100), 4),
                                          rep(500 + ((43)*100), 4),
                                          rep(500 + ((44)*100), 4),
                                          rep(500 + ((45)*100), 4),
                                          rep(500 + ((46)*100), 4),
                                          rep(500 + ((47)*100), 4),
                                          rep(500 + ((48)*100), 4),
                                          rep(500 + ((49)*100), 4),
                                          rep(500 + ((50)*100), 4),
                                          rep(500 + ((51)*100), 4),
                                          rep(500 + ((52)*100), 4),
                                          rep(500 + ((53)*100), 4),
                                          rep(500 + ((54)*100), 4),
                                          rep(500 + ((55)*100), 4),
                                          rep(500 + ((56)*100), 4),
                                          rep(500 + ((57)*100), 4),
                                          rep(500 + ((58)*100), 4),
                                          rep(500 + ((59)*100), 4),
                                          rep(500 + ((60)*100), 4),
                                          rep(500 + ((61)*100), 4),
                                          rep(500 + ((62)*100), 4),
                                          rep(500 + ((63)*100), 4),
                                          rep(500 + ((64)*100), 4),
                                          rep(500 + ((65)*100), 4),
                                          rep(500 + ((66)*100), 4),
                                          rep(500 + ((67)*100), 4),
                                          rep(500 + ((68)*100), 4),
                                          rep(500 + ((69)*100), 4),
                                          rep(500 + ((70)*100), 4),
                                          rep(500 + ((71)*100), 4),
                                          rep(500 + ((72)*100), 4),
                                          rep(500 + ((73)*100), 4),
                                          rep(500 + ((74)*100), 4),
                                          rep(500 + ((75)*100), 4),
                                          rep(500 + ((76)*100), 4),
                                          rep(500 + ((77)*100), 4),
                                          rep(500 + ((78)*100), 4),
                                          rep(500 + ((79)*100), 4),
                                          rep(500 + ((80)*100), 4),
                                          rep(500 + ((81)*100), 4),
                                          rep(500 + ((82)*100), 4),
                                          rep(500 + ((83)*100), 4),
                                          rep(500 + ((84)*100), 4),
                                          rep(500 + ((85)*100), 4),
                                          rep(500 + ((86)*100), 4),
                                          rep(500 + ((87)*100), 4),
                                          rep(500 + ((88)*100), 4),
                                          rep(500 + ((89)*100), 4),
                                          rep(500 + ((90)*100), 4),
                                          rep(500 + ((91)*100), 4),
                                          rep(500 + ((92)*100), 4),
                                          rep(500 + ((93)*100), 4),
                                          rep(500 + ((94)*100), 4),
                                          rep(500 + ((95)*100), 4),
                                          rep(500 + ((96)*100), 4),
                                          rep(500 + ((97)*100), 4),
                                          rep(500 + ((98)*100), 4),
                                          rep(500 + ((99)*100), 4),
                                          rep(500 + ((100)*100), 4))),
                         xend = c(c(500, 1500, 2500, 3500, 4500,
                                    5500, 6500, 7500, 8500, 9500,
                                    10500),c(rep(500 + ((1)*100), 4),
                                             rep(500 + ((2)*100), 4),
                                             rep(500 + ((3)*100), 4),
                                             rep(500 + ((4)*100), 4),
                                             rep(500 + ((5)*100), 4),
                                             rep(500 + ((6)*100), 4),
                                             rep(500 + ((7)*100), 4),
                                             rep(500 + ((8)*100), 4),
                                             rep(500 + ((9)*100), 4),
                                             rep(500 + ((10)*100), 4),
                                             rep(500 + ((11)*100), 4),
                                             rep(500 + ((12)*100), 4),
                                             rep(500 + ((13)*100), 4),
                                             rep(500 + ((14)*100), 4),
                                             rep(500 + ((15)*100), 4),
                                             rep(500 + ((16)*100), 4),
                                             rep(500 + ((17)*100), 4),
                                             rep(500 + ((18)*100), 4),
                                             rep(500 + ((19)*100), 4),
                                             rep(500 + ((20)*100), 4),
                                             rep(500 + ((21)*100), 4),
                                             rep(500 + ((22)*100), 4),
                                             rep(500 + ((23)*100), 4),
                                             rep(500 + ((24)*100), 4),
                                             rep(500 + ((25)*100), 4),
                                             rep(500 + ((26)*100), 4),
                                             rep(500 + ((27)*100), 4),
                                             rep(500 + ((28)*100), 4),
                                             rep(500 + ((29)*100), 4),
                                             rep(500 + ((30)*100), 4),
                                             rep(500 + ((31)*100), 4),
                                             rep(500 + ((32)*100), 4),
                                             rep(500 + ((33)*100), 4),
                                             rep(500 + ((34)*100), 4),
                                             rep(500 + ((35)*100), 4),
                                             rep(500 + ((36)*100), 4),
                                             rep(500 + ((37)*100), 4),
                                             rep(500 + ((38)*100), 4),
                                             rep(500 + ((39)*100), 4),
                                             rep(500 + ((40)*100), 4),
                                             rep(500 + ((41)*100), 4),
                                             rep(500 + ((42)*100), 4),
                                             rep(500 + ((43)*100), 4),
                                             rep(500 + ((44)*100), 4),
                                             rep(500 + ((45)*100), 4),
                                             rep(500 + ((46)*100), 4),
                                             rep(500 + ((47)*100), 4),
                                             rep(500 + ((48)*100), 4),
                                             rep(500 + ((49)*100), 4),
                                             rep(500 + ((50)*100), 4),
                                             rep(500 + ((51)*100), 4),
                                             rep(500 + ((52)*100), 4),
                                             rep(500 + ((53)*100), 4),
                                             rep(500 + ((54)*100), 4),
                                             rep(500 + ((55)*100), 4),
                                             rep(500 + ((56)*100), 4),
                                             rep(500 + ((57)*100), 4),
                                             rep(500 + ((58)*100), 4),
                                             rep(500 + ((59)*100), 4),
                                             rep(500 + ((60)*100), 4),
                                             rep(500 + ((61)*100), 4),
                                             rep(500 + ((62)*100), 4),
                                             rep(500 + ((63)*100), 4),
                                             rep(500 + ((64)*100), 4),
                                             rep(500 + ((65)*100), 4),
                                             rep(500 + ((66)*100), 4),
                                             rep(500 + ((67)*100), 4),
                                             rep(500 + ((68)*100), 4),
                                             rep(500 + ((69)*100), 4),
                                             rep(500 + ((70)*100), 4),
                                             rep(500 + ((71)*100), 4),
                                             rep(500 + ((72)*100), 4),
                                             rep(500 + ((73)*100), 4),
                                             rep(500 + ((74)*100), 4),
                                             rep(500 + ((75)*100), 4),
                                             rep(500 + ((76)*100), 4),
                                             rep(500 + ((77)*100), 4),
                                             rep(500 + ((78)*100), 4),
                                             rep(500 + ((79)*100), 4),
                                             rep(500 + ((80)*100), 4),
                                             rep(500 + ((81)*100), 4),
                                             rep(500 + ((82)*100), 4),
                                             rep(500 + ((83)*100), 4),
                                             rep(500 + ((84)*100), 4),
                                             rep(500 + ((85)*100), 4),
                                             rep(500 + ((86)*100), 4),
                                             rep(500 + ((87)*100), 4),
                                             rep(500 + ((88)*100), 4),
                                             rep(500 + ((89)*100), 4),
                                             rep(500 + ((90)*100), 4),
                                             rep(500 + ((91)*100), 4),
                                             rep(500 + ((92)*100), 4),
                                             rep(500 + ((93)*100), 4),
                                             rep(500 + ((94)*100), 4),
                                             rep(500 + ((95)*100), 4),
                                             rep(500 + ((96)*100), 4),
                                             rep(500 + ((97)*100), 4),
                                             rep(500 + ((98)*100), 4),
                                             rep(500 + ((99)*100), 4),
                                             rep(500 + ((100)*100), 4))),
                         y = c(rep(ymin, 11),c(rep(c(ymin, (ymax/2) - 500, (ymax/2) + 500, ymax- 100),100))),
                         yend = c(rep(ymax, 11), c(rep(c(ymin + 100, (ymax/2) - 600, (ymax/2) + 600,ymax ), 100))))
  
  # Add lines to pitch
  p <- p + geom_segment(data = seg_data, aes( x = x, xend = xend,
                                              y = y, yend = yend),
                        color = line_colour)
  
  
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
    geom_text(aes(x = 1200, y =280, label = "<1"), colour = "white", size =5) +
    geom_text(aes(x = 1700, y =280, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 1200, y = ymax - 280, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 1700, y = ymax - 280, label = "1"), colour = "white", size =5, angle = 180) +
    # 20 Yards
    geom_text(aes(x = 2200, y =280, label = "<2"), colour = "white", size =5) +
    geom_text(aes(x = 2700, y =280, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 2200, y = ymax - 280, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 2700, y = ymax - 280, label = "2"), colour = "white", size =5, angle = 180) +
    # 30 Yards
    geom_text(aes(x = 3200, y =280, label = "<3"), colour = "white", size =5) +
    geom_text(aes(x = 3700, y =280, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 3200, y = ymax - 280, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 3700, y = ymax - 280, label = "3"), colour = "white", size =5, angle = 180) +
    # 40 Yards
    geom_text(aes(x = 4200, y =280, label = "<4"), colour = "white", size =5) +
    geom_text(aes(x = 4700, y =280, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 4200, y = ymax - 280, label = "0>"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 4700, y = ymax - 280, label = "4"), colour = "white", size =5, angle = 180) +
    # 50 Yards
    geom_text(aes(x = 5300, y =280, label = "5"), colour = "white", size =5) +
    geom_text(aes(x = 5700, y =280, label = "0"), colour = "white", size =5) +
    geom_text(aes(x = 5300, y = ymax - 280, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 5700, y = ymax - 280, label = "5"), colour = "white", size =5, angle = 180) +
    # 60 Yards
    geom_text(aes(x = 6300, y =280, label = "4"), colour = "white", size =5) +
    geom_text(aes(x = 6800, y =280, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 6300, y = ymax - 280, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 6800, y = ymax - 280, label = "<4"), colour = "white", size =5, angle = 180) +
    # 70 Yards
    geom_text(aes(x = 7300, y =280, label = "3"), colour = "white", size =5) +
    geom_text(aes(x = 7800, y =280, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 7300, y = ymax - 280, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 7800, y = ymax - 280, label = "<3"), colour = "white", size =5, angle = 180) +
    # 80 Yards
    geom_text(aes(x = 8300, y =280, label = "2"), colour = "white", size =5) +
    geom_text(aes(x = 8800, y =280, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 8300, y = ymax - 280, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 8800, y = ymax - 280, label = "<2"), colour = "white", size =5, angle = 180) +
    # 90 Yards
    geom_text(aes(x = 9300, y =280, label = "1"), colour = "white", size =5) +
    geom_text(aes(x = 9800, y =280, label = "0>"), colour = "white", size =5) +
    geom_text(aes(x = 9300, y = ymax - 280, label = "0"), colour = "white", size =5, angle = 180) +
    geom_text(aes(x = 9800, y = ymax - 280, label = "<1"), colour = "white", size =5, angle = 180) 
  
  # Return pitch
  return(p)
}



create_zone_versus_plot <- function( logos = team_logos,
                              t1 = "Notre Dame",
                              t2 = "Cincinnati",
                              res_1 = nd_by_yards,
                              res_2 = cin_by_yards,
                              type = "EPA",
                              title =""){
  #'
  #' This function creates a plot to view performance by different areas
  #' of the field
  #'
  #' @param logos The data frame containing team logos
  #' @param t1 The first team to use as offense
  #' @param t2 The second team to use as defense
  #' @param res_1 The first teams summary data by zone
  #' @param res_2 The second teams summary data by zone
  #' @param type The type of stat to compare, default = "EPA"
  #'
  #'
  #'
  
  # Set initial plot parameters
  ymin <- 0
  ymax <- 6800
  xmin <- 0
  xmax <- 11000
  offset <- 1
  
  # Create Initial Plot
  p <- create_blank_pitch()
  # Add colored grid
  mypal <- colorRampPalette( c("blue", "red"))(200)
  
  
  if(type == "EPA"){
    
    use_dat_1 <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_epa_per_play, res_1$rush_epa_per_play))
    use_dat_2 <- cbind.data.frame(c(rev(res_2$pass_n_def), rev(res_2$rush_n_def)),
                                  c(rev(res_2$pass_epa_per_play_def), 
                                    rev(res_2$rush_epa_per_play_def)))
    names(use_dat_1) <- c("freq_off", "epa_off")
    names(use_dat_2) <- c("freq_def", "epa_def")
    epa_diff <- (use_dat_1$epa_off + use_dat_2$epa_def)/2
    use_dat <- cbind.data.frame(use_dat_1, use_dat_2, epa_diff)
    use_dat$alpha <- 0.75
    use_dat$col_vec <- map2color(use_dat$epa_diff, mypal, 
                                 limits = c(-max(abs(use_dat$epa_diff)), 
                                            max(abs(use_dat$epa_diff))))
    
    
  } else if (type == "Yards"){
    
    use_dat_1 <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_yards_per_play, res_1$rush_yards_per_play))
    use_dat_2 <- cbind.data.frame(c(rev(res_2$pass_n_def), rev(res_2$rush_n_def)),
                                  c(rev(res_2$pass_yards_per_play_def), 
                                    rev(res_2$rush_yards_per_play_def)))
    names(use_dat_1) <- c("freq_off", "epa_off")
    names(use_dat_2) <- c("freq_def", "epa_def")
    epa_diff <- (use_dat_1$epa_off + use_dat_2$epa_def)/2
    use_dat <- cbind.data.frame(use_dat_1, use_dat_2, epa_diff)
    use_dat$alpha <- 0.75
    use_dat$col_vec <- map2color(use_dat$epa_diff, mypal, 
                                 limits = c(min(use_dat$epa_diff), 
                                            max(use_dat$epa_diff)))
  } else if (type == "WPA"){
    use_dat_1 <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_wpa_per_play, res_1$rush_wpa_per_play))
    use_dat_2 <- cbind.data.frame(c(rev(res_2$pass_n_def), rev(res_2$rush_n_def)),
                                  c(rev(res_2$pass_wpa_per_play_def), 
                                    rev(res_2$rush_wpa_per_play_def)))
    names(use_dat_1) <- c("freq_off", "epa_off")
    names(use_dat_2) <- c("freq_def", "epa_def")
    epa_diff <- (use_dat_1$epa_off + use_dat_2$epa_def)/2
    use_dat <- cbind.data.frame(use_dat_1, use_dat_2, epa_diff)
    use_dat$alpha <- 0.75
    use_dat$col_vec <- map2color(use_dat$epa_diff, mypal, 
                                 limits = c(-max(abs(use_dat$epa_diff)), 
                                            max(abs(use_dat$epa_diff))))
  }
  
  plot_dat <- use_dat
  # Add color to plot
  p <- p + geom_rect(aes(xmin = 500 + ((1-1)*100) +offset, xmax = 500 + (20*100) -offset, ymin = ymax/2, ymax = ymax - offset),
                     fill = plot_dat$col_vec[1], alpha =  plot_dat$alpha[1]) +
    geom_rect(aes(xmin = 500 + ((21-1)*100) +offset, xmax = 500 + (50*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[2], alpha =plot_dat$alpha[2]) +
    geom_rect(aes(xmin = 500 + ((51-1)*100) +offset, xmax = 500 + (80*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[3], alpha = plot_dat$alpha[3]) +
    geom_rect(aes(xmin = 500 + ((81-1)*100) +offset, xmax = 500 + (100*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[4], alpha = plot_dat$alpha[4]) +
    
    geom_rect(aes(xmin = 500 + ((1-1)*100) +offset, xmax = 500 + (20*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[5], alpha = plot_dat$alpha[5]) +
    geom_rect(aes(xmin = 500 + ((21-1)*100) +offset, xmax = 500 + (50*100) -offset, ymin = ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[6], alpha = plot_dat$alpha[6]) +
    geom_rect(aes(xmin = 500 + ((51-1)*100) +offset, xmax = 500 + (80*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[7], alpha = plot_dat$alpha[7]) +
    geom_rect(aes(xmin = 500 + ((81-1)*100) +offset, xmax = 500 + (100*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[8], alpha = plot_dat$alpha[8]) 
    
  
  
  
  p <- p + geom_text(aes(x = (xmin + 260), y=ymax/2), label = paste("Rush               Pass"), 
                     colour = "white",  size =6, angle = 90) 
  
  
  # Add text
  p <- p + geom_text(aes(x = ((15-1)*100), y = ymax * 0.75),
                     label = paste(round(plot_dat$epa_diff[1],3)),
                     colour = "white", size =4) + 
    geom_text(aes(x = ((40-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa_diff[2],3)),
              colour = "white", size =4) + 
    geom_text(aes(x = ((70-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa_diff[3],3)),
              colour = "white", size =4) + 
    geom_text(aes(x = ((95-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa_diff[4],3)),
              colour = "white", size =4) + 
    
    geom_text(aes(x = ((15-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa_diff[5],3)),
              colour = "white", size =4) + 
    geom_text(aes(x = ((40-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa_diff[6],3)),
              colour = "white", size =4) + 
    geom_text(aes(x = ((70-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa_diff[7],3)),
              colour = "white", size =4) + 
    geom_text(aes(x =  ((95-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa_diff[8],3)),
              colour = "white", size =4) 
   
  
  # Add logos to indicate teams
  logos_use <- rbind.data.frame(team_logos[which(team_logos$school == t1),],
                                team_logos[which(team_logos$school == t2),])
  logos_use$x <- c(xmax* 0.25 +250, xmax * 0.75 - 250)
  logos_use$y <- c(ymax/2, ymax/2)
  p <- p + geom_image(data = logos_use, aes(x = x, y = y),
                      image = logos_use$logos, asp = 16/9)
  
  # Add Plot Labels
  p <- p + geom_text(aes(x = (xmax/2), y=ymax+200), 
                     label = paste("Average",
                                   type, "per play"), 
                     colour = "white", size =6) 
  
  # Add plot arrow
  p <- p + geom_segment(aes(x = xmax/2 - 1000, y = ymax/2, xend = xmax/2 + 1000,
                       yend = ymax/2), arrow = arrow())
  
  # Return Plot
  return(p)
  
}














create_zone_plot <- function( logos = team_logos,
                              t1 = "Notre Dame",
                              res_1 = nd_by_yards,
                              type = "EPA",
                              title ="",
                              off_def = "off"){
  #'
  #' This function creates a plot to view performance by different areas
  #' of the field
  #'
  #' @param logos The data frame containing team logos
  #' @param t1 The first team to use as 
  #' @param res_1 The first teams summary data by zone
  #' @param type The type of stat to compare, default = "EPA"
  #' @param off_def Plot offense or defense
  #'
  #'
  
  # Set initial plot parameters
  ymin <- 0
  ymax <- 6800
  xmin <- 0
  xmax <- 11000
  offset <- 1
  
  # Create Initial Plot
  p <- create_blank_pitch()
  # Add colored grid
  mypal <- colorRampPalette( c("blue", "red"))(200)
  
  
  if(type == "EPA"){
    if(off_def == "off"){
      use_dat <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_epa_per_play, res_1$rush_epa_per_play))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits =c(-max(abs(use_dat$epa)), 
                                             max(abs(use_dat$epa))))
      
    } else {
      use_dat <- cbind.data.frame(c(rev(res_1$pass_n_def), rev(res_1$rush_n_def)),
                                  c(rev(res_1$pass_epa_per_play_def),
                                    rev(res_1$rush_epa_per_play_def)))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits = c(-max(abs(use_dat$epa)), 
                                              max(abs(use_dat$epa))))
    }
   
    
  } else if (type == "Yards"){
    if(off_def == "off"){
      use_dat <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_yards_per_play, res_1$rush_yards_per_play))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits = c(min(use_dat$epa), 
                                              max(use_dat$epa)))
    } else {
      use_dat <- cbind.data.frame(c(rev(res_1$pass_n_def), rev(res_1$rush_n_def)),
                                  c(rev(res_1$pass_yards_per_play_def), 
                                    rev(res_1$rush_yards_per_play_def)))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits = c(min(use_dat$epa), 
                                              max(use_dat$epa)))
    }
    
    
  } else if (type == "WPA"){
    if(off_def == "off"){
      use_dat <- cbind.data.frame(c(res_1$pass_n, res_1$rush_n),
                                  c(res_1$pass_wpa_per_play, res_1$rush_wpa_per_play))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits = c(-max(abs(use_dat$epa)), 
                                              max(abs(use_dat$epa))))
    } else {
      use_dat <- cbind.data.frame(c(rev(res_1$pass_n_def), rev(res_1$rush_n_def)),
                                  c(rev(res_1$pass_wpa_per_play_def),
                                    rev(res_1$rush_wpa_per_play_def)))
      
      names(use_dat) <- c("freq", "epa")
      
      
      use_dat$alpha <- 0.75
      use_dat$col_vec <- map2color(use_dat$epa, mypal, 
                                   limits = c(-max(abs(use_dat$epa)), 
                                              max(abs(use_dat$epa))))
    }

  }
  
  plot_dat <- use_dat
  # Add color to plot
  p <- p + geom_rect(aes(xmin = 500 + ((1-1)*100) +offset, xmax = 500 + (20*100) -offset, ymin = ymax/2, ymax = ymax - offset),
                     fill = plot_dat$col_vec[1], alpha =  plot_dat$alpha[1]) +
    geom_rect(aes(xmin = 500 + ((21-1)*100) +offset, xmax = 500 + (50*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[2], alpha =plot_dat$alpha[2]) +
    geom_rect(aes(xmin = 500 + ((51-1)*100) +offset, xmax = 500 + (80*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[3], alpha = plot_dat$alpha[3]) +
    geom_rect(aes(xmin = 500 + ((81-1)*100) +offset, xmax = 500 + (100*100) -offset, ymin = ymax/2, ymax = ymax - offset),
              fill = plot_dat$col_vec[4], alpha = plot_dat$alpha[4]) +
    
    geom_rect(aes(xmin = 500 + ((1-1)*100) +offset, xmax = 500 + (20*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[5], alpha = plot_dat$alpha[5]) +
    geom_rect(aes(xmin = 500 + ((21-1)*100) +offset, xmax = 500 + (50*100) -offset, ymin = ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[6], alpha = plot_dat$alpha[6]) +
    geom_rect(aes(xmin = 500 + ((51-1)*100) +offset, xmax = 500 + (80*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[7], alpha = plot_dat$alpha[7]) +
    geom_rect(aes(xmin = 500 + ((81-1)*100) +offset, xmax = 500 + (100*100) -offset, ymin =  ymin + offset, ymax = ymax/2),
              fill = plot_dat$col_vec[8], alpha = plot_dat$alpha[8]) 
  
  
  
  
  p <- p + geom_text(aes(x = (xmin + 260), y=ymax/2), label = paste("Rush               Pass"), 
                     colour = "white",  size =6, angle = 90) 
  
  
  # Add text
  p <- p + geom_text(aes(x = ((15-1)*100), y = ymax * 0.75),
                     label = paste(round(plot_dat$epa[1],3),"\n",
                                   "Plays:",plot_dat$freq[1]),
                     colour = "white", size =4) + 
    geom_text(aes(x = ((40-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa[2],3),"\n",
                            "Plays:",plot_dat$freq[2]),
              colour = "white", size =4) + 
    geom_text(aes(x = ((70-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa[3],3),"\n",
                            "Plays:",plot_dat$freq[3]),
              colour = "white", size =4) + 
    geom_text(aes(x = ((95-1)*100), y = ymax * 0.75),
              label = paste(round(plot_dat$epa[4],3),"\n",
                            "Plays:",plot_dat$freq[4]),
              colour = "white", size =4) + 
    
    geom_text(aes(x = ((15-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa[5],3),"\n",
                            "Plays:",plot_dat$freq[5]),
              colour = "white", size =4) + 
    geom_text(aes(x = ((40-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa[6],3),"\n",
                            "Plays:",plot_dat$freq[6]),
              colour = "white", size =4) + 
    geom_text(aes(x = ((70-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa[7],3),"\n",
                            "Plays:",plot_dat$freq[7]),
              colour = "white", size =4) + 
    geom_text(aes(x =  ((95-1)*100), y = ymax * 0.25),
              label = paste(round(plot_dat$epa[8],3),"\n",
                            "Plays:",plot_dat$freq[8]),
              colour = "white", size =4) 
  
  
  # Add logos to indicate teams
  logos_use <- rbind.data.frame(team_logos[which(team_logos$school == t1),])
  logos_use$x <- c(xmax* 0.25 +250)
  logos_use$y <- c(ymax/2)
  p <- p + geom_image(data = logos_use, aes(x = x, y = y),
                      image = logos_use$logos, asp = 16/9)
  
  if (off_def == "off"){
    s1 <- "Offensive"
  } else {
    s1 <- "Defensive"
  }
  
  # Add Plot Labels
  p <- p + geom_text(aes(x = (xmax/2), y=ymax+200), 
                     label = paste(s1, ":", "Average",
                                   type, "per play"), 
                     colour = "white", size =6) 
  
  # Add plot arrow
  p <- p + geom_segment(aes(x = xmax/2 - 1000, y = ymax/2, xend = xmax/2 + 1000,
                            yend = ymax/2), arrow = arrow())
  
  # Return Plot
  return(p)
  
}


create_simplex_play_type <- function(pbp_data){
  #'
  #' This function creates a simpler play type to used for analysis for the college
  #' football dataset
  #'
  #' @param pbp_data
  #'
  #' @return A vector which contains the simple play type for the data
  #'
  
  # Create empty vector to store results
  simplex_play_type <- rep(NA, nrow(pbp_data))
  # Identify field goals
  simplex_play_type[which(pbp_data$play_type %in% c("Blocked Field Goal", "Field Goal Good",
                                                    "Field Goal Missed", "Missed Field Goal Return",
                                                    "Missed Field Goal Return Touchdown"))] <- "Field Goal"
  # Identify punts
  simplex_play_type[which(pbp_data$play_type %in% c("Blocked Punt", "Blocked Punt Touchdown",
                                                    "Punt","Punt Return Touchdown","Punt Touchdown"))] <- "Punt"
  # Identify Kick-offs
  simplex_play_type[which(pbp_data$play_type %in% c("Kickoff", "Kickoff Return (Offense)",
                                                    "Kickoff Return Touchdown"))] <- "Kickoff"
  # Identify Passes
  simplex_play_type[which(pbp_data$play_type %in% c("Pass Incompletion", "Pass Interception Return",
                                                    "Pass Reception", "Passing Touchdown", "Sack",
                                                    "Interception Return Touchdown"))] <- "Pass"
  # Identify Rushes
  simplex_play_type[which(pbp_data$play_type %in% c("Rush","Rushing Touchdown"))] <- "Rush"
  
  
  simplex_play_type[which(pbp_data$play_type %in% c("Fumble Recovery (Opponent)", "Fumble Recovery (Opponent) Touchdown",
                                                    "Fumble Recovery (Own)",      "Fumble Recovery (Own) Touchdown"))] <- "Fumble"
  # Return simple play type
  return(simplex_play_type)
}







































































