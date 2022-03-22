library(tidyverse)
library(magrittr)

# this data was scraped from 100 consecutive games in 2001-2002
load(file='jData_0102_100games.rda')

# dropping 15 games with more than 4 clues that were not read -- I suspect this may have been more common
# in earlier years of the show, although it could just be that the data hasn't been recorded
missing <- jDatatest$clue %>% 
  sapply(function(x) sum(!complete.cases(x))) # summing count of rows with missing clues
jDatatest$clue   %<>% `[`(which(missing <= 4))
jDatatest$player %<>% `[`(which(missing <= 4))
jDatatest$game   %<>% `[`(which(missing <= 4),)

### visual 1 -- relationship between Coryat score and true post-DJ score
jDatatest$player %>%
  do.call(what = 'rbind') %>% # aggregating all player-level data into a single df
  ggplot(aes(y = coryat.score, x = dj.score, color = fj.score/dj.score)) +
  geom_point(size = 2.5) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = 'black') +
  labs(title = 'Coryat score vs. true post-DJ score',
       x = 'Score after Double Jeopardy round',
       y = 'Coryat score',
       col = 'Ratio of final score \n to post-DJ score')

### visual 2 -- Final Jeopardy wagers for players in each standing
jDatatest$player %>%
  lapply(function(x) {
    x %>%
      `[`(order(x$dj.score, decreasing = TRUE),) %>% # ordering players in each game by post-DJ score
      mutate(dj.rank = 1:3) # adding post-DJ rank column
  }) %>%
  do.call(what = 'rbind') %>% # aggregating into a single df
  ggplot(aes(x = fj.wager, fill = as.character(dj.rank))) +
  geom_histogram(binwidth = 1000, show.legend = FALSE) +
  facet_wrap('dj.rank') + # separating histograms by rank
  theme_bw() +
  labs(title = 'Histogram of Final Jeopardy wagers by post-Double Jeopardy standing',
       x = 'Final Jeopardy wager (intervals of $1,000)',
       y = 'Count')

### visual 3 -- Relationship between large wagers and final placement
jDatatest$player %>%
  lapply(function(x) {
    x %>%
      `[`(order(x$fj.score, decreasing = TRUE),) %>% # ordering players in each game by final score
      mutate(final.rank = 1:3) # adding final rank column
  }) %>%
  do.call(what = 'rbind') %>%
  mutate(bigwager = ifelse(fj.wager/dj.score > 0.9, 'Yes', 'No')) %>% # 84/255 ~ 33% of players wagered more than 90% of their money
  #filter(bigwager == 1) %>% # only looking at players who made these big wagers
  ggplot(aes(fill = as.character(final.rank), x = bigwager)) +
  geom_bar() +
  theme_bw() +
  labs(title = 'Relationship between large Final Jeopardy wagers and final placement',
       x = 'Did the player wager more than 90% of their pre-Final Jeopardy money?',
       y = '',
       fill = 'Final placement')
