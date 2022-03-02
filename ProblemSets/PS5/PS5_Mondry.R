library(tidyverse)
library(rvest)
library(magrittr)
library(spotifyr)

### scraping - from the J! Archive site
# I will attempt to construct a df for the first round of the Jeopardy game #1564, which aired 5/23/1991

# we need two different pages to get this info -- one page contains the categories and clues, while
# another page contains the responses and information about which player(s) responded

jclues.r <- read_html("https://www.j-archive.com/showgame.php?game_id=5105")
jclues <- jclues.r %>% html_element("#jeopardy_round > table.round") %>% html_table()

jresp.r <- read_html("https://www.j-archive.com/showgameresponses.php?game_id=5105")
jresp <- jresp.r %>% html_element("#jeopardy_round > table.round") %>% html_table()

# these tables are nasty but they have all the information we need in a format we can work with
# since the format is not at all useful, I'll reconstruct the table from scratch

# first, initializing an empty table
jtable <- data.frame(matrix(nrow = 30, ncol = 7))
colnames(jtable) <- c('category',
                      'value',
                      'clue',
                      'order', # between 1-30, order this clue came up in the round
                      'is.dd', # 1 if this clue is a Daily Double, 0 otherwise
                      'dd.wager', # if is.dd == TRUE, then the wager; -1 otherwise (to distinguish from
                                  # a theoretically possible zero wager)
                      'cor.player') # name of the correct respondent

# now, filling one column at a time

# 1. categories - the 6 categories i are stored in jclues[2i,1]
jtable$category <- jclues[2 * 1:6, 1] %>% # getting these 6 categories
  unlist %>% rep(5) # there are 5 clues for each

# 2. value - we can fill in without using the table
jtable$value <- c(100,200,300,400,500) %>% # this was before values were doubled
  rep(each = 6) # there are 6 categories for each

# 3. clue - we can use the weird format to help us -- e.g. the 30 clues are contained in the
#    first column, in the cells below each of the last 30 blank cells in that column.
#    we'll create a vector of these row numbers, which will be useful later
jclues.loc <- which(jclues[,1] == '') %>% tail(30)
# filling in the column now
jtable$clue <- jclues[jclues.loc+1,1] %>% `[[`(1)

# 4. order - this information is located in the same row as the blank spaces, in the third column
jtable$order <- jclues[jclues.loc,3] %>% `[[`(1)

# 5-6. is.dd and dd.wager - this information is located in the same row as the blank spaces, in the second column
jtable[,c('is.dd','dd.wager')] <- 
  sapply(1:30, # to vectorize substr()
         function(i) {
           if (substring(jclues[jclues.loc[i],2], # this value will begin with "DD: $" rather than "$" if
                         first = 1,                 # the clue is a daily double
                         last = 2) == 'DD') {
             return(c(1, # is.dd = 1
                      substring(jclues[jclues.loc[i],2],
                                first = 6))) # dd.wager given by value
           } else {
             return(c(0,-1)) # not a daily double
           }
         }) %>% t

# 7. cor.player - we need a new vector for the jresp table, and this one's format is slightly less predictable --
#    it varies based on how many players attempt to answer a clue. A simple rule which works for this game & the other
#    few I checked relies on the locations of the last 29 blank rows and the location 3 rows below the row length of the table.
#
#    The correct respondent is located two rows above the blank rows in the first column, except for with the final category
#    (i.e, the 6th, 12th, ..., 30th clues), where they are located three rows above
jresp.loc <- which(jresp[,1] == '') %>% tail(29) %>% c(nrow(jresp)+3)
jresp.loc[6*1:5] <- jresp.loc[6*1:5]-1 # moving this index up one for the final category
# filling in the column now
jtable$cor.player <- jresp[jresp.loc-2,1] %>% `[[`(1)

# all done
jtable

########################################################################################################################################################

### using an API - Spotify data with spotifyr package
# I will pull various audio features from some of the most popular recordings of Faure's Requiem on Spotify, in order to see if any of them are ~weird~
# in a quantifiable way

# first, searching for albums; from here, we can get album IDs and look up features -- returning top 100 results (we can only search 50 at a time)
search <- rbind(search_spotify('faure requiem', type='album', limit = 50),
                search_spotify('faure requiem', type='album', limit = 50, offset = 50))
# I should make sure I'm looking at albums that only have the Requiem. This mass has 7 movements, so we should be somewhere around that. Also looking
# at album names to make sure no other piece is listed. We'll look at the top 10 such albums.
search[,c('name','total_tracks')]
keep <- search[c(1, 13, 19, 22, 25, 29, 39, 41, 45, 60),]
keep[,c('name','total_tracks')]
# looks good

# I'll assign an artist name to each one; I don't have a catch-all way to do this, because there is no singular convention for the order of naming artists.
# looking at all listed artists in each selected album
keep$artists
# naming them in a way that is meaningful to me -- generally, conductor, then ensemble
artists <- c('Equilbey, Accentus',
             'Rutter, Oxford Schola Cantorum',
             'Cluytens',
             'Kuentz',
             'Boulanger, NY Phil',
             'Davis, MDR Leipzig Radio Chorus',
             "Ledger, King's College, Cambridge",
             'Bolton, Sinfonieorchester Basel',
             'Hartung, Cologne New Philharmonic',
             'Corboz, Berne Symphony')

# putting together a more meaningful (to me) table of information
faure <- data.frame(id = keep$id, # Spotify ID so we can look up features
                    artists = artists,
                    year = substr(keep$release_date, start = 1, stop = 4))

# since the Spotify API only includes features for tracks, not albums, we will get features for all tracks and then average for each album
# looking up Spotify IDs for all tracks in each album
track.ids <- lapply(1:10, function(i) faure$id[i] %>% get_album_tracks %>% '['('id'))

# filling in average features for each album, in one step (loop seemed like the most understandable way to do this)
for (i in 1:10) {
faure[i, 4:16] <- # filling the results directly into the df
  track.ids[[i]] %>% sapply(get_track_audio_features) %>% # getting features for each track
  '['(-c(12:16)) %>% # features 12-16 are character, so we will omit
  sapply(mean) # finding mean of all tracks across each album
}

# getting labels from one track
names(faure)[4:16] <- faure$id[1] %>% get_album_tracks %>% '['('id') %>% '[['(1) %>% '['(1) %>% get_track_audio_features %>% names %>% '['(-c(12:16))

# all done
faure
