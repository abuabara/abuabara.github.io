#####################################################
# Simulation of the Monty Hall Problem
# Demonstrates that switching gives you better odds
# than staying with your initial guess
# https://en.wikipedia.org/wiki/Monty_Hall_problem

#####################################################
# Corey Chivers, 2012
# https://bayesianbiologist.com/2012/02/03/monty-hall-by-simulation/

rm(list=ls())

monty <- function(strat = 'stay',
                  N = 3,
                  print_games = TRUE)
{
  doors <- 1:3 # initialize the doors behind one of which is a good prize
  win   <- 0   # to keep track of number of wins
  
  for(i in 1:N)
  {
    prize <- floor(runif(1,1,4)) # randomize which door has the good prize
    guess <- floor(runif(1,1,4)) # guess a door at random
    
    ## reveal one of the doors you didn't pick which has a bum prize
    if(prize != guess)
      reveal <- doors[-c(prize,guess)]
    else
      reveal <- sample(doors[-c(prize,guess)],1)
    
    ## stay with your initial guess or switch
    if(strat == 'switch')
      select <- doors[-c(reveal,guess)]
    if(strat == 'stay')
      select <- guess
    if(strat =='random')
      select <- sample(doors[-reveal],1)
    
    ## count up your wins
    if(select == prize)
    {
      win <- win + 1
      outcome <- 'Winner!'
    } else
      outcome <- 'Losser!'
    
    if(print_games)
      cat(paste('Guess: ', guess,
                '\nRevealed: ', reveal,
                '\nSelection: ', select,
                '\nPrize door: ', prize,
                '\n',outcome,'\n\n',sep=''))
  }
  cat(paste('Using the ',strat,' strategy, your win percentage was ',win/N*100,'%\n',sep='')) # print the win percentage of your strategy
}

monty(strat="stay")
monty(strat="switch")
monty(strat="random")


#####################################################
# Martina Sladekova, 2022
# https://martinasladek.co.uk/content/blog/2022_feb_solving-the-monty-hall-problem-with-r#full-code

rm(list=ls())

library(dplyr)
library(ggplot2)

n_iter        <- 1000
switch_choice <- rep(c(TRUE, FALSE), each = n_iter/2)
doors         <- c(1,2,3)
monty_df      = NULL

for (i in switch_choice) {
  
  winning_door            <- sample(doors, 1)
  players_original_choice <- sample(doors, 1)
  losing_doors            <- setdiff(doors, winning_door)
  
  door_open <- ifelse(players_original_choice == winning_door,
                      sample(losing_doors, 1),
                      setdiff(losing_doors, players_original_choice))
  
  does_player_switch <- i
  
  players_final_choice <- ifelse(does_player_switch, 
                                 setdiff(doors, c(door_open, players_original_choice)), 
                                 players_original_choice)
  
  monty_df_i <- data.frame(
    switched = does_player_switch, 
    won = players_final_choice == winning_door
  )
  
  monty_df <- rbind.data.frame(monty_df_i, monty_df)
  
}

monty_df %>% 
  dplyr::mutate(
    switched = factor(switched, levels = c(TRUE, FALSE),  labels = c("Switched", "Did not switch")), 
    won = factor(won, levels = c(TRUE, FALSE), labels = c("Won ", "Lost "))
  ) %>% 
  
  dplyr::group_by(switched, won) %>%  
  dplyr::summarise(n = n(), perc = n / (nrow(.)/2) * 100) %>% 
  
  ggplot2::ggplot(., aes(x = switched, y = perc, fill = won)) + 
  geom_col(position = position_dodge(width = 0.5), width = 0.3, alpha = .90) + 
  scale_fill_manual(values = c("#172541","#e2ad00")) + 
  labs(x = "\nDid the player switch their choice?", fill = "Did the \nplayer win? ", y = "Percent (%)\n") +
  coord_cartesian(ylim = c(0,100)) + 
  theme_minimal() 

#####################################################
# Chris Teeter, 2020
# https://www.cteeter.ca/blog/2020-01-30-monty-hall-problem-simulation-R/

rm(list=ls())

require(tidyverse)

# Create function to simulate problem, and print and plot results --------------
MontyHall <- function(switch = TRUE, n = 1000, seasons = 1) {
  
  # Setup data frame to record results --------------------------------------------------------------------
  outcome <- data.frame(Season = numeric(),
                        Strategy = character(),
                        Prize = character(),
                        Instances = numeric(),
                        Percent = numeric())
  
  # Allow for multiple 'seasons' of playing the game n times. ---------------
  for (s in 1:seasons) {
    
    # Setup vector to record the prize results of each iteration of the game (1 = car, 0 = goat) --------
    game_result <- NULL
    
    for (p in 1:n) {
      
      # Set up components of the game -------------------------------------------
      prizes <- c('Car', 'Goat1', 'Goat2')
      door_setup <- sample(prizes, 3)
      contestant_choice <- sample(1:3, 1)
      door_reveal <- ifelse(door_setup[contestant_choice] == 'Car', 
                            sample(door_setup[-c(contestant_choice)], 1), 
                            door_setup[-c(contestant_choice, which(door_setup == 'Car'))])
      if (switch) {
        contestant_prize <- door_setup[-c(contestant_choice, which(door_setup == door_reveal))]
        game_result[p] <- ifelse(contestant_prize == 'Car', 1, 0)
      } else if (!switch) {
        contestant_prize <- door_setup[contestant_choice]
        game_result[p] <- ifelse(contestant_prize == 'Car', 1, 0) }
    }
    
    # Add results from this 'season' to the results data frame ------------------------
    tmp_outcome <- data.frame(Season = rep(s, 2),
                              Strategy = ifelse(switch, rep('Switch', 2), rep('Stay', 2)),
                              Prize = c('Car', 'Goat'), 
                              Instances = c(sum(game_result), sum(!game_result)),
                              Percent = c(round(sum(game_result)/n, 4)*100, round(sum(!game_result)/n, 4)*100))
    
    outcome <- rbind(outcome, tmp_outcome)
  }
  
  # Generate Summary Statistics for Plotting --------------------------------
  outcome_summary <- outcome %>%
    group_by(Prize) %>%
    summarise(Mean = round(mean(Percent, na.rm = T), 4),
              StDev = round(sd(Percent, na.rm = T), 4),
              CI = round((qnorm(0.975)*StDev)/sqrt(seasons), 4))
  
  # Plot Results ------------------------------------------------------------
  outcome_plot <- ggplot(outcome_summary, aes(x = Prize, y = Mean)) +
    { if (seasons > 1) geom_errorbar(aes(ymin = (Mean - CI), ymax = (Mean + CI)), color = 'black', size =  1, width = 0) } +
    geom_point(size = 4, stroke = 1.75, pch = 21, color = 'firebrick3', fill = 'white') +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    labs(x = "Contestant's Prize", y = if(seasons > 1) { 'Relative Frequency [+/- 95% CI]' } else { 'Relative Frequency' }, 
         subtitle = paste0("Results from ", {if(seasons > 1) { paste0(seasons, " runs of ") }}, n, 
                           " iterations of the Monty Hall Problem\nin which the contestant always",
                           ifelse(switch, " SWITCHES from their initial choice.\n", " STAYS with their initial choice.\n"))) +
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 1))
  
  return(list("data" = outcome, "summary" = outcome_summary, "plot" = outcome_plot))
}

MontyHall(switch = F, n = 1250, seasons = 4)
