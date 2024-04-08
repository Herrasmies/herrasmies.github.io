library(tidyverse)
library(gganimate)
library(transformr)
library(truncnorm)
library(tidyverse)

coinflips <- tibble(kruunut = c(30.40036, 43.80205, 48.04004, 
                                   49.38021, 49.804, 49.93802,
                                   49.9804),
                    klaavat = c(69.59964, 56.19795, 51.95996,
                                   50.61979, 50.196, 50.06198, 
                                   50.0196),
                    eurot = c(19.59964, 61.97950, 195.9964, 
                                 619.7950, 1959.964, 6197.950, 
                                 19599.64),
                    suhde = kruunut/klaavat,
                    sample = c(100, 1000, 10000, 100000,
                               1000000, 10000000, 100000000))


#TEE ALLA OLEVA UUDESTAAN HYÖDYNTÄEN BINOMIJAKAUMAA!!!
coinflips <- data.frame()
rounds <- seq(100, 100000, 100)

for (i in 1:length(rounds)) {

  cumEV = cumsum(rep(0, rounds[i]))
  cumSD = sqrt(cumsum(rep(1^2, rounds[i]))) #first convert to variance, then convert the sum back to SD // betsize = 1
  lower <- qnorm(0.025, cumEV, cumSD)
  upper <- qnorm(0.975, cumEV, cumSD)
  
  prop1 = ((rounds[i]/2) - tail(upper, 1)) / rounds[i]
  prop2 = 1-prop1
  euros = tail(upper, 1)

  
  test <- tibble(kruunat = prop1, klaavat = prop2, n = rounds[i], euros = euros)
  
  coinflips <- rbind(coinflips, test)
  
}



coinflips %>%
  gather(key, value, kruunut, klaavat) %>%
  ggplot(aes(sample, value, color = key)) +
  geom_line() +
  geom_point() +
  theme_bw()



