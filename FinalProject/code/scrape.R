# scrape.R - Script for scraping game information from the J! Archive and converting to useful format
# load the three packages & save the four functions below the hashes to the environment, then this code will run

# providing list of game IDs (from the J! Archive site)
game.ids <- c(1062:1066,1072:1076,1083:1092,1136:1140,1165:1169,1180:1184,1218:1222,1385:1389,1434:1438,1441:1445,1449:1453,1718:1753,1101:1103,1094)

jDatatest <- getJData(game.ids)

#################################################################################################################

library(tidyverse)
library(rvest)
library(magrittr)

getJData <- function(game.ids) {
  
  # initializing output object
  jData <- vector('list', 3)
  
  # initializing elements of the object
  names(jData)      <- c('clue', 'player', 'game')
  jData$clue        <- vector('list', length(game.ids))
  jData$player      <- vector('list', length(game.ids))
  jData$game        <- tibble(game.id = integer(),
                              show.num = integer(),
                              show.date = Sys.Date(),
                              fj.category = character(),
                              fj.clue = character())
  
  # vector for assigning each player to an ID; contains names of all players with duplicates for
  # returning champions, for now
  playernames <- vector('character')
  
  # populating the output object (beyond my understanding of apply/vectorization so I'll use a loop)
  for (i in 1:length(game.ids)) {
    
    # scraping data from web
    rawclue <- paste0("https://www.j-archive.com/showgame.php?game_id=",game.ids[i]) %>% 
      read_html
    rawscores <- paste0("https://www.j-archive.com/showscores.php?game_id=",game.ids[i]) %>% 
      read_html
    
    # reading into useful format. lots of code, using functions I've written below
    jData$clue[[i]]   <- getClueData(clue.data = rawclue,
                                     scores.data = rawscores)
    jData$player[[i]] <- getPlayerData(clue.data = rawclue,
                                       scores.data = rawscores,
                                       getClueData.output = jData$clue[[i]])
    jData$game[i,]    <- getGameData(clue.data = rawclue,
                                     game.id = game.ids[i])
    
    # adding to player names vector
    playernames %<>% union(jData$player[[i]]$name)
  }
  
  # assigning ID to each player
  player.id <- data.frame(name = playernames,
                            id = 1:length(playernames))
  
  # adding IDs to player object
  for (i in 1:length(game.ids)) {
    jData$player[[i]]$player.id <- player.id$id %>%
      `[`(c(which(player.id$name == jData$player[[i]]$name[1]),
            which(player.id$name == jData$player[[i]]$name[2]),
            which(player.id$name == jData$player[[i]]$name[3])))
  }
  
  # output
  jData$game %<>% as.data.frame # I don't like tibbles
  jData
}

getClueData <- function(clue.data, scores.data) {
  ##### 1: getting elements with relevant information (but in a terrible format, largely) from web pages
  
  # elements containing info about each clue -- 
  #   category & value, order it came up in the round, is Daily Double & wager
  jj.clues <- clue.data %>%
    html_element("#jeopardy_round > table.round") %>% 
    html_table %>%
    as.data.frame
  
  # same info from DJ round
  dj.clues <- clue.data %>%
    html_element("#double_jeopardy_round > table.round") %>% 
    html_table %>%
    as.data.frame
  
  # info about score progression after each clue, in order
  jj.scores <- scores.data %>%
    html_element("#jeopardy_round > table") %>%
    html_table %>%
    as.data.frame
  
  # same info from DJ round
  dj.scores <- scores.data %>%
    html_element("#double_jeopardy_round > table") %>%
    html_table %>%
    as.data.frame
  
  ##### 2: turning these tables into a usable format. this is best accomplished by constructing one
  #####    table from each source and then joining the two tables
  
  ### i: creating table from clue page
  
  # initializing an empty data table
  out1 <- data.frame(matrix(nrow = 60, ncol = 7))
  colnames(out1) <- c('round',
                      'category',
                      'value',
                      'clue',
                      'order', # between 1-60, unique for each game -- order this clue came up in the game
                      'is.dd', # 1 if this clue is a Daily Double, 0 otherwise
                      'dd.wager') # if is.dd == TRUE, then the wager; -1 otherwise (to distinguish from 
                                 # a theoretically possible zero wager)
  
  # now, filling one column at a time
  
  # 1. round
  out1$round <- c(rep('JJ',30),
                  rep('DJ',30))
  
  # 2. categories
  out1$category <- c(jj.clues[2*1:6,1] %>% rep(5), # these are located in the first column in a predictable place
                     dj.clues[2*1:6,1] %>% rep(5))
  
  # 3. dollar values
  out1$value <- 200 * c(1,2,3,4,5,2,4,6,8,10) %>%
    rep(each = 6)
  
  # 4. clues - weird formatting here; the 30 clues are contained in inconsistent places
  
  # finding which rows the clues are saved in
  jj.clue.rows <- which(jj.clues[, 5] != '') %>%
    union(which(jj.clues[, 6] != '')) %>%
    union(which(jj.clues[,13] != '')) %>%
    union(which(jj.clues[,14] != '')) %>%
    setdiff(1) # the first row should not be included but generally is
  dj.clue.rows <- which(dj.clues[, 5] != '') %>%
    union(which(dj.clues[, 6] != '')) %>%
    union(which(dj.clues[,13] != '')) %>%
    union(which(dj.clues[,14] != '')) %>%
    setdiff(1)
  
  # finding & populating the clues (or blanks) in each of these rows
  for (j in 1:5) {
    
    # finding blank columns where valid, to be used to find clues & DD indicators
    jj.blank.cols <- union(which(jj.clues[jj.clue.rows[j],] == ''),
                           which(is.na(jj.clues[jj.clue.rows[j],]))) %>% 
      sort %>%
      head(6)
    jj.blank.cols %<>% `[`(jj.blank.cols <= ncol(jj.clues)-3) # if the 6th blank is this far across then the clue in the 6th category cannot have been read.
                                                              # keeping it in will mess up the formulas below
    # filling in the table
    out1$clue[(6*j-5):(6*j)] <- ifelse(jj.clues[jj.clue.rows[j],jj.blank.cols+2] %>% substring(1,1) %in% as.character(1:9),
           yes = jj.clues[jj.clue.rows[j],jj.blank.cols+3], # clues are stored here if they are not missing
           no = NA) %>% # indicates that clues are missing
      as.character %>%
      c(rep('NA', 6-length(jj.blank.cols)))
    out1$order[(6*j-5):(6*j)] <- ifelse(out1$clue[(6*j-5):(6*j)] != 'NA',
                                        yes = jj.clues[jj.clue.rows[j],jj.blank.cols+2],
                                        no = NA) %>%
      as.numeric
    out1$is.dd[(6*j-5):(6*j)] <- ifelse(out1$clue[(6*j-5):(6*j)] != 'NA',
                                        yes = ifelse(jj.clues[jj.clue.rows[j],jj.blank.cols+1] %>%
                                                       substring(1,2) == 'DD',
                                                     yes = 1,
                                                     no = 0),
                                        no = 0)
    out1$dd.wager[(6*j-5):(6*j)] <- ifelse(out1$is.dd[(6*j-5):(6*j)] == 1,
                                           yes = jj.clues[jj.clue.rows[j],jj.blank.cols+1] %>%
                                             substring(6) %>%
                                             gsub(pattern=",", replacement=""),
                                           no = -1) %>%
      as.integer
    
    # repeating for DJ round
    dj.blank.cols <- union(which(dj.clues[dj.clue.rows[j],] == ''),
                           which(is.na(dj.clues[dj.clue.rows[j],]))) %>% 
      sort %>%
      head(6)
    dj.blank.cols %<>% `[`(dj.blank.cols <= ncol(dj.clues)-3)
    out1$clue[(6*(j+5)-5):(6*(j+5))] <- ifelse(dj.clues[dj.clue.rows[j],dj.blank.cols+2] %>% substring(1,1) %in% as.character(1:9),
                                            yes = dj.clues[dj.clue.rows[j],dj.blank.cols+3],
                                            no = NA) %>%
      as.character %>%
      c(rep('NA', 6-length(dj.blank.cols)))
    out1$order[(6*(j+5)-5):(6*(j+5))] <- ifelse(out1$clue[(6*(j+5)-5):(6*(j+5))] != 'NA',
                                                yes = dj.clues[dj.clue.rows[j],dj.blank.cols+2],
                                                no = NA) %>%
      as.numeric
    out1$is.dd[(6*(j+5)-5):(6*(j+5))] <- ifelse(out1$clue[(6*(j+5)-5):(6*(j+5))] != 'NA',
                                                yes = ifelse(dj.clues[dj.clue.rows[j],dj.blank.cols+1] %>%
                                                               substring(1,2) == 'DD',
                                                             yes = 1,
                                                             no = 0),
                                                no = 0)
    out1$dd.wager[(6*(j+5)-5):(6*(j+5))] <- ifelse(out1$is.dd[(6*(j+5)-5):(6*(j+5))] == 1,
                                                   yes = dj.clues[dj.clue.rows[j],dj.blank.cols+1] %>%
                                                     substring(6) %>%
                                                     gsub(pattern=",", replacement=""),
                                                   no = -1) %>%
      as.integer
    
    }

  # consolidating missing indicators
  out1$clue[which(out1$clue == 'NA')] <- NA
  # using better order for DJ round
  out1$order[out1$round == 'DJ'] <- out1$order[out1$round == 'DJ'] + 30

  
  ### ii. creating table from scores page
  
  # initializing empty data table
  out2 <- data.frame(matrix(nrow = 1 + max(na.omit(jj.scores[,1] + max(na.omit(dj.scores[,1])))), # table will be constructed using lags, then first row will be dropped
                            ncol = 4))
  names(out2) <- c('order', # same as above
                   'p1', #  1 if player 1 (player on left) answered clue correctly;
                         # -1 if they answered incorrectly;
                         #  0 otherwise
                   'p2',
                   'p3')

  # filling in
  out2[1,] <- rep(0,4)
  out2[2:max(na.omit(jj.scores[,1]+1)),] <- jj.scores[1:max(na.omit(jj.scores[,1]))+1,1:4]
  out2[max(na.omit(jj.scores[,1]+2)):nrow(out2),] <- dj.scores[1:max(na.omit(dj.scores[,1]))+1,1:4]
  
  # adjustments
  out2[max(na.omit(jj.scores[,1]+2)):nrow(out2),'order'] <- out2[max(na.omit(jj.scores[,1]+2)):nrow(out2),'order'] + 30 # better order for DJ round
  out2$p1 %<>% gsub(pattern='\\$', replacement="") %>% # dropping dollar signs
    gsub(pattern=",", replacement="") %>% # dropping commas
    as.integer
  out2$p2 %<>% gsub(pattern='\\$', replacement="") %>% gsub(pattern=",", replacement="") %>% as.integer
  out2$p3 %<>% gsub(pattern='\\$', replacement="") %>% gsub(pattern=",", replacement="") %>% as.integer
  
  # defining the ternary output in terms of the difference between the values and their lags
  out2$p1 <- ifelse(out2$p1 - lag(out2$p1) == 0,
                    yes = 0,
                    no = ifelse(out2$p1 - lag(out2$p1) > 0,
                                yes = 1,
                                no = -1))
  out2$p2 <- ifelse(out2$p2 - lag(out2$p2) == 0,
                    yes = 0,
                    no = ifelse(out2$p2 - lag(out2$p2) > 0,
                                yes = 1,
                                no = -1))
  out2$p3 <- ifelse(out2$p3 - lag(out2$p3) == 0,
                    yes = 0,
                    no = ifelse(out2$p3 - lag(out2$p3) > 0,
                                yes = 1,
                                no = -1))
  
  # dropping first row
  out2 %<>% `[`(-1,)
  
  ### iii. joining these two tables
  out <- out1 %>%
    left_join(out2, by = 'order') %>%
    arrange('order')
  
  ##### output
  out
}

getPlayerData <- function(clue.data, scores.data, getClueData.output) {
  
  # getting HTML elements
  players <- clue.data %>%
    html_element("#contestants_table") %>%
    html_table %>%
    `[`(1,2) %>%
    as.character
  
  finalscores <- clue.data %>%
    html_element("#final_jeopardy_round > table:nth-child(4)") %>%
    html_table
  
  # separating into different strings for each player
  p3 <- players %>% 
    substring(first = 1,
              last = players %>%
                gregexpr(pattern = 'from') %>%
                unlist %>%
                `[`(1) - 2)
  p2 <- players %>% 
    substring(first = players %>%
                gregexpr(pattern = '\n') %>%
                unlist %>%
                `[`(1) + 19,
              last = players %>%
                gregexpr(pattern = 'from') %>%
                unlist %>%
                `[`(2) - 2)
  p1 <- players %>% 
    substring(first = players %>%
                gregexpr(pattern = '\n') %>%
                unlist %>%
                `[`(2) + 19,
              last = players %>%
                gregexpr(pattern = 'from') %>%
                unlist %>%
                `[`(3) - 2)
  
  # initializing output object
  out <- data.frame(matrix(nrow = 3, ncol = 10))
  names(out) <- c('player.id',
                  'position',
                  'name',
                  'occupation',
                  'jj.score',
                  'dj.score',
                  'coryat.score',
                  'fj.wager',
                  'fj.score',
                  'fj.correct')
  
  # filling in values
  out$position <- c('p1','p2','p3')
  
  out$name <- sapply(c(p1, p2, p3),
                     function(n) {n %>% substring(first = 1,
                                                  last = n %>%
                                                    gregexpr(pattern = ',') %>%
                                                    unlist - 1)})
  
  out$occupation <- sapply(c(p1, p2, p3),
                           function(n) {n %>% substring(first = n %>%
                                                          gregexpr(pattern = ',') %>%
                                                          unlist + 2)})
  
  out$jj.score <- with(getClueData.output[1:30,] %>%
                         `[`(complete.cases(getClueData.output[1:30,]),),
                       c(sum(p1 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value)),
                         sum(p2 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value)),
                         sum(p3 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value))))
  
  out$dj.score <- with(getClueData.output %>%
                         `[`(complete.cases(getClueData.output),),
                       c(sum(p1 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value)),
                         sum(p2 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value)),
                         sum(p3 * ifelse(is.dd == 1,
                                         yes = dd.wager,
                                         no = value))))
  
  out$coryat.score <- with(getClueData.output %>%
                             `[`(complete.cases(getClueData.output),),
                           c(sum(p1 * value),
                             sum(p2 * value),
                             sum(p3 * value)))
  
  out$fj.score <- finalscores %>%
    `[`(2,) %>%
    gsub(pattern=",", replacement="") %>%
    gsub(pattern="\\$", replacement="") %>%
    as.integer
  
  out$fj.correct <- ifelse(out$fj.score >= out$dj.score, 1, 0)
  
  out$fj.wager <- abs(out$fj.score - out$dj.score)
  
  # output
  out
}

getGameData <- function(clue.data, game.id) {
  
  # elements containing relevant information
  title <- clue.data %>%
    html_element("#game_title > h1") %>%
    as.character
  
  final <- clue.data %>%
    html_element("#final_jeopardy_round > table.final_round") %>%
    html_table %>%
    as.data.frame
  
  # filling in values
  show.num <- title %>% 
    substring(first = title %>%
                gregexpr(pattern = '#') %>%
                unlist + 1,
              last = title %>%
                gregexpr(pattern = '-') %>%
                unlist - 2) %>%
    as.integer
  
  show.date <- title %>%
    substring(first = title %>%
                gregexpr(pattern = 'day') %>%
                unlist + 5,
              last = title %>%
                gregexpr(pattern = '/h') %>%
                unlist - 2) %>%
    as.Date('%B %d, %Y')
  
  fj.category <- final[1,1]
  
  fj.clue <- final[4,1]
  
  # output
  tibble(game.id, show.num, show.date, fj.category, fj.clue)
}
