library(shiny)
library(ggplot2)

load(file = 'data/artists_mo_alltime.RData')
load(file = 'data/artists_mo_day.RData')
load(file = 'data/artists_mo_night.RData')
load(file = 'data/songs_mo_alltime.RData')
load(file = 'data/songs_mo_day.RData')
load(file = 'data/songs_mo_night.RData')

shinyServer(function(input, output) {  
  output$header1 = renderUI({
    if (input$top_or_spec == 'Top Chart') {
      h1 = paste('Top', input$top_range, 'Artists', sep = ' ')
    }
    else {
      h1 = paste('Top 10 Play History for', input$artist_input, sep = ' ')
    }
    HTML('<h3>', h1, '<h3>')
  })

  output$header2 = renderUI({
    h2 = paste('During', input$beg_range, '-', input$end_range, sep = ' ')
    
    if (input$tod == 1) {
      h3 = '(Daytime Only)'
    }
    else if (input$tod == 2) {
      h3 = '(Nighttime Only)'
    }
    else {
      h3 = '(All Day)'
    }
    
    HTML('<h4>', h2, h3, '<h4>')
  })
  
  output$plot = renderPlot({
    #set data
    beg_range = as.Date(paste(1, input$beg_range), '%d %b %Y')
    end_range = as.Date(paste(1, input$end_range), '%d %b %Y')
    
    if (input$top_or_spec == 'Top Chart') {
      #create top x chart
      #subset data based on time-of-day input
      if (input$tod == 1) {
        dat = subset(artists_mo_day, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      else if (input$tod == 2) {
        dat = subset(artists_mo_night, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      else {
        dat = subset(artists_mo_alltime, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      
      #collapse to count the total played times over the selected time range per artist
      dat_overall = aggregate(freq ~ artist, data = dat, sum)
      
      #rank the data per played times
      dat_overall$rank = rank(-dat_overall$freq)
      dat_overall = subset(dat_overall, select = -freq)
      dat_overall = dat_overall[order(dat_overall$rank), ]
      
      #merge the overall rank back to the monthly data
      dat = merge(dat, dat_overall, by = 'artist')
      
      #subset to only the top range as specifed by the user
      dat = subset(dat, rank < input$top_range + 1)
      dat = dat[order(dat$rank), ]
      
      #relevel artist factors (so that the legends on the plot will be ordered properly per played times)
      new_lev = dat_overall$artist
      dat$artist = factor(dat$artist, levels = new_lev)
      
      #plot
      ggplot(dat, aes(x = bom, y = freq, col = artist)) + geom_line() + 
        guides(col = guide_legend(ncol = 6, title = NULL)) +
        theme(legend.position = 'bottom') +
        xlab('Month') + ylab('Played Times')
    }
    else {
      #create artist-specific charts showing their popular songs
      #subset data based on time-of-day input
      if (input$tod == 1) {
        dat = subset(songs_mo_day, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      else if (input$tod == 2) {
        dat = subset(songs_mo_night, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      else {
        dat = subset(songs_mo_alltime, as.Date(bom) >= beg_range & as.Date(bom) <= end_range)
      }
      #subset to this particular artist
      dat = dat[grep(input$artist_input, dat$artist, ignore.case = T), ]
      
      if (nrow(dat) > 0) {
        #wrap long song names
        dat$song = gsub(' \\(', '\n\\(', dat$song)
        
        #collapse to count the total played times over the selected time range per song
        dat_overall = aggregate(freq ~ song, data = dat, sum)
        
        #rank the data per played times
        dat_overall$rank = rank(-dat_overall$freq)
        dat_overall = subset(dat_overall, select = -freq)
        dat_overall = dat_overall[order(dat_overall$rank), ]
        
        #merge the overall rank back to the monthly data
        dat = merge(dat, dat_overall, by = 'song')
        
        #subset to only the top 10 songs
        dat = subset(dat, rank < 11)
        dat = dat[order(dat$rank), ]
        
        #relevel song factors (so that the legends on the plot will be ordered properly per played times)
        new_lev = dat_overall$song
        dat$song = factor(dat$song, levels = new_lev)
        
        #plot
        ggplot(dat, aes(x = bom, y = freq, col = song)) + geom_point() + geom_line() +
          guides(col = guide_legend(ncol = 4, title = NULL)) +
          theme(legend.position = 'bottom') +
          xlab('Month') + ylab('Played Times')
      }
    }
  })
})