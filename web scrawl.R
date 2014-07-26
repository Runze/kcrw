library(RCurl)
library(XML)
library(stringr)
library(lubridate)
setwd('/Users/Runze/Documents/Github/kcrw')

#define dates to loop over
start = as.Date('2013-01-01')
end = as.Date('2014-06-17')
date = seq(start, end, by = 'days')

part1 = 'http://newmedia.kcrw.com/tracklists/search.php?'
part2 = '&search_type=0&date_from='
part3 = '&host=&date_to=&artist=&channel=Simulcast&label='

#crawl daily playlists
playlists = list()
l = 1
for (i in 1:length(date)) {
  d = date[i]
  yy = substr(d, 1, 4)
  mm = substr(d, 6, 7)
  dd = substr(d, 9, 10)
  date_str = paste(mm, '%2F', dd, '%2F', yy, sep = '')
  
  cat('now processing', as.character(d), '\n')
  
  for (s in seq(0, 100, 50)) {
    if (s == 0) {
      start_str = ''
    }
    else {
      start_str = paste('start=', s, sep = '')
    }
    link = paste0(part1, start_str, part2, date_str, part3)
    url = getURL(link, followlocation =  T)
    
    na_check = str_locate(url, 'Playlist information is not available for the selected criteria')
    if (!is.na(na_check[1])) {
      break
    }
    else {
      #get main table
      html = htmlTreeParse(url, useInternalNodes = T)
      tables = readHTMLTable(html, header = F)
      
      tables[sapply(tables, is.null)] = NULL
      if (length(tables) > 0) {
        #find the largest table
        dims = as.numeric(lapply(tables, function(t) dim(t)[2]))
        index = which(dims == max(dims))                   
        table = data.frame(tables[[index]])
        
        #add new column for date
        table$date = d
        playlists[[l]] = table
        l = l + 1 
      }
    }
  }
}

#check the number of columns in the returned tables
play_ncol = as.numeric(lapply(playlists, ncol))
table(play_ncol) #all 7

#function to clean the returned tables
clean_play = function(play) {
  play = play[-6] #the last column is for purchase information
  names(play) = c('time', 'artist', 'song', 'album', 'label', 'date')
  play = play[, c('date', 'time', 'artist', 'song', 'album', 'label')]
  play = subset(play, !is.na(artist))
  play = play[-grep('DJ Comment', play$artist, ignore.case = T), ]
}

playlists_clean = lapply(playlists, clean_play)
playlists_df = do.call(rbind.data.frame, playlists_clean)

#create date and time variable
playlists_df$date = as.character(playlists_df$date)
playlists_df$time = as.character(playlists_df$time)
datetime = paste(playlists_df$date, playlists_df$time)
playlists_df$datetime = strptime(datetime, '%Y-%m-%d %I:%M %p')
playlists_df = playlists_df[, c('date', 'time', 'datetime', 'artist', 'song', 'album', 'label')]
playlists_df = playlists_df[order(playlists_df$datetime), ]

#create cutoff time for day and night
cutoff_time1 = strptime(paste(playlists_df$date, '6:00 am'), '%Y-%m-%d %I:%M %p')
cutoff_time2 = strptime(paste(playlists_df$date, '6:00 pm'), '%Y-%m-%d %I:%M %p')

playlists_df$daytime = playlists_df$datetime >= cutoff_time1 & playlists_df$datetime < cutoff_time2

#create month indicator
yr = year(playlists_df$date)
mo = month(playlists_df$date)
bom = yr * 10000 + mo * 100 + 1 #beginning of month
playlists_df$bom = ymd(bom)

#clean up data
playlists_df = subset(playlists_df, artist != '' & song != '' & song != 'Interview')

#generate monthly data for artists and save them in the data folder for the shiny app
#all time
artists_mo_alltime = aggregate(song ~ artist + bom, data = playlists_df, length)
names(artists_mo_alltime)[3] = 'freq'
save(artists_mo_alltime, file = 'playlist/data/artists_mo_alltime.RData')

#daytime only
artists_mo_day = aggregate(song ~ artist + bom, data = subset(playlists_df, daytime == T), length)
names(artists_mo_day)[3] = 'freq'
save(artists_mo_day, file = 'playlist/data/artists_mo_day.RData')

#nighttime only
artists_mo_night = aggregate(song ~ artist + bom, data = subset(playlists_df, daytime == F), length)
names(artists_mo_night)[3] = 'freq'
save(artists_mo_night, file = 'playlist/data/artists_mo_night.RData')

#generate monthly data for songs and save them in the data folder for the shiny app
#all time
songs_mo_alltime = aggregate(album ~ artist + song + bom, data = playlists_df, length)
names(songs_mo_alltime)[4] = 'freq'
save(songs_mo_alltime, file = 'playlist/data/songs_mo_alltime.RData')

#daytime only
songs_mo_day = aggregate(album ~ artist + song + bom, data = subset(playlists_df, daytime == T), length)
names(songs_mo_day)[4] = 'freq'
save(songs_mo_day, file = 'playlist/data/songs_mo_day.RData')

#nighttime only
songs_mo_night = aggregate(album ~ artist + song + bom, data = subset(playlists_df, daytime == F), length)
names(songs_mo_night)[4] = 'freq'
save(songs_mo_night, file = 'playlist/data/songs_mo_night.RData')