# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
Q <- qnorm(1-0.05/2) # 95% CL

brexit_polls %>% ggplot(aes(enddate, spread, color = poll_type)) +
    geom_smooth(method = "loess", span = 0.4) + geom_point()

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3) + geom_point()
# Exercise 2

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

head(brexit_polls)
tail(brexit_polls)

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
tail(june_polls)
june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize))
tail(june_polls)
june_polls <- june_polls %>% mutate(se_spread = 2*se_x_hat)
tail(june_polls)
june_polls <- june_polls %>% mutate(lower = spread-Q*se_spread, upper = spread+Q*se_spread) 
tail(june_polls)
june_polls <- june_polls %>% mutate(hit = ifelse(lower <= d & upper >= d, 1, 0))
head(june_polls)
june_polls %>% summarize(mean(hit))

test1 <- june_polls %>% mutate(hit1 = ifelse(lower <= 0 & upper >= 0, 1, 0)) %>% summarize(mean(hit1))
test1 
   
test2 <- june_polls %>% mutate(hit2 = ifelse(lower >= 0 & upper >= 0, 1, 0)) %>% summarize(mean(hit2))
test2 
    
test3 <- june_polls %>% group_by(pollster) #%>% arrange(desc(hit))
tail(test3)

june_polls %>% group_by(pollster) %>% summarize(avg = mean(hit), n = n())
boxplot(june_polls$spread~june_polls$poll_type)

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            lower = spread-Q*se_spread, upper = spread+Q*se_spread,
            diff = upper - lower)
combined_by_type
    
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit
tel_hitrate <- sum(brexit_hit$poll_type == "Telephone" & brexit_hit$hit == TRUE) / sum(brexit_hit$poll_type == "Telephone")
onl_hitrate <- sum(brexit_hit$poll_type == "Online" & brexit_hit$hit == TRUE) / sum(brexit_hit$poll_type == "Online")
tel_hitrate
onl_hitrate
sum(brexit_hit$poll_type == "Telephone")
sum(brexit_hit$poll_type == "Telephone" & brexit_hit$hit == TRUE)
sum(brexit_hit$poll_type == "Online")
sum(brexit_hit$poll_type == "Online" & brexit_hit$hit == TRUE)
#two_by_two <- tibble(poll_type = brexit_hit$poll_type, hit = brexit_hit$hit) 
two_by_two <- tibble(
  hit = c(TRUE, FALSE),
  Telephone = c(10, 32),
  Online = c(48, 37)
)
two_by_two
two_by_two %>% select(-hit) %>% chisq.test()
two_by_two[[1,2]]
two_by_two[[2,2]]
sum(two_by_two[[2]])
    
odds_t = (two_by_two[[1,2]] / sum(two_by_two[[2]])) / (two_by_two[[2,2]] / sum(two_by_two[[2]]))
odds_t 
odds_o = (two_by_two[[1,3]] / sum(two_by_two[[3]])) / (two_by_two[[2,3]] / sum(two_by_two[[3]]))
odds_o
odds_o/odds_t

N <- 1500     # Number of voters for exercise 1
N_remain <- p*N
se <- sqrt(p*(1-p)*N)

#brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
#head(brexit_polls)

#brexit_polls %>% summarize(se = 2*sqrt(x_hat*(1-x_hat)/median(samplesize)))
#de <- brexit_polls %>% summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>% pull(d_hat)
#xe <- mean(brexit_polls$x_hat)
#sd(brexit_polls$spread)
#sd(brexit_polls$x_hat)

#Q <- qnorm(1-0.05/2)
#X_hat <- brexit_polls[1,"x_hat"] 
#SE_hat <- sqrt(X_hat*(1-X_hat)/brexit_polls[1,"samplesize"])
#ci <- c(X_hat-Q*SE_hat, X_hat + Q*SE_hat)
#ci
