library(tidyverse)

blinds <- c(-100, -80, -60, -40, -20, -10, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 10, 20, 40, 60, 80, 100)

probs <- c(0.005, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, 0.06, 0.07, 0.07, 0.085, 0.09, 
           0.085, 0.07, 0.07, 0.06, 0.06, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005)

ev_noncashout <- sum(blinds*probs)

ev_cashout <- as_tibble(blinds) %>%
  dplyr::mutate(value = ifelse(value > 10, value*0.99, value))

ev_cashout <- sum(ev_cashout$value*probs)



cashout_simulator <- function(hands, players) {
  
  simulations = data.frame(nrow = 0)
  simulations_cashout = data.frame(nrow = 0)
  
  #non-cashout simulations
  for (i in 1:players) {
    
    data <- sample(blinds, hands, replace = TRUE, probs) %>%
      as_tibble()
    names(data)[1] <- i
    simulations <- cbind(simulations, cumsum(data))
    simulations$nrow <- NULL
  }
  
  #cashout simulations
  for (i in 1:players) {
    
    data <- sample(blinds, hands, replace = TRUE, probs) %>%
      as_tibble() %>%
      dplyr::mutate(value = ifelse(value > 10, value*0.99, value))
    names(data)[1] <- i
    simulations_cashout <- cbind(simulations_cashout, cumsum(data))
    simulations_cashout$nrow <- NULL
  }
  
  simulations_cashout$ID <- seq.int(nrow(simulations_cashout))
  
  simulations.long.cashout <- simulations_cashout %>% 
    gather(key, value, -ID) %>%
    dplyr::mutate(cashout = rep("Cashout", n()),
                  EV = ev_cashout)
  

  simulations$ID <- seq.int(nrow(simulations))
  
  simulations.long <- simulations %>% 
    gather(key, value, -ID) %>%
    dplyr::mutate(cashout = rep("Non-cashout", n()),
                  key = as.numeric(key) + players,
                  EV = ev_noncashout) #this is a hack to unmatch key-values between the frames
  
  simulations_all <- rbind(simulations.long.cashout, simulations.long)
  

  #Plot all "players"
  plot <- simulations_all %>%
    ggplot(aes(ID, value, group=key)) + #group here required for complex animation
    geom_line(aes(group = key, color = cashout), alpha = .25, size = 0.1) +
    # geom_line(data = confidence, aes(x = ID, y = lower), linetype="dashed", color="black", size=0.5, inherit.aes=F) +
    # geom_line(data = confidence, aes(x = ID, y = upper), linetype="dashed", color="black", size=0.5, inherit.aes=F) +
    # geom_abline(intercept = 0, slope = winrate, linetype="dashed", size=1) +
    # geom_abline(intercept = 0, slope = tail(cumEV_cashout, 1) / hands, linetype="dashed", color ="red") + #cumEV last row (how much won at end, divided by the total number of hands, i.e. slope)
    # geom_abline(intercept = 0, slope = tail(cumEV, 1) / hands, linetype = "dashed", color ="blue") +
    # geom_abline(intercept = 0, slope = ev_cashout, linetype = "dashed", color ="red", size = 0.8) +
    # geom_abline(intercept = 0, slope = ev_noncashout, linetype = "dashed", color = "blue", size = 0.8) +
    geom_abline(aes(intercept = 0, slope = EV, group = cashout, color = cashout), linetype = "dashed", size = 0.8) +
    xlab("Hands played") + ylab("Big blinds won") +
    theme_bw(base_size=14) +
    scale_color_manual(values = c("red", "blue")) +
    #labs(color = NULL) +
    guides(color ="none") +
    facet_wrap("cashout")
    
    return(plot)
}
  
