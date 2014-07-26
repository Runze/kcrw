library(shiny)

shinyUI(fluidPage(
  titlePanel('Most Played Artists on KCRW in 1/1/13 - 6/17/14'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('beg_range', 
                  label = '1. Choose beginning month.',
                  choices = list('Jan 2013', 'Feb 2013', 'Mar 2013', 'Apr 2013',
                                 'May 2013', 'Jun 2013', 'Jul 2013', 'Aug 2013',
                                 'Sep 2013', 'Oct 2013', 'Nov 2013', 'Dec 2013',
                                 'Jan 2014', 'Feb 2014', 'Mar 2014', 'Apr 2014',
                                 'May 2014', 'Jun 2014'),
                  selected = 'Jan 2013'),
      
      selectInput('end_range', 
                  label = '2. Choose ending month.',
                  choices = list('Jan 2013', 'Feb 2013', 'Mar 2013', 'Apr 2013',
                                 'May 2013', 'Jun 2013', 'Jul 2013', 'Aug 2013',
                                 'Sep 2013', 'Oct 2013', 'Nov 2013', 'Dec 2013',
                                 'Jan 2014', 'Feb 2014', 'Mar 2014', 'Apr 2014',
                                 'May 2014', 'Jun 2014'),
                  selected = 'Jun 2014'),
      
      radioButtons('tod', label = '3. Choose time of day',
                   choices = list('Daytime' = 1, 'Nighttime' = 2,
                                  'All day' = 3), selected = 3),
      
      selectInput('top_or_spec', 
                  label = '4. Choose to see top charts or artist-specific charts.',
                  choices = list('Top Chart', 'Artist-Specific'),
                  selected = 'Top Chart'),
      
      sliderInput('top_range', 
                  label = '- To see the top artists, choose top range',
                  min = 0, max = 50, value = 20),
      
      textInput('artist_input', label = '- Or to see a specific artist, enter the name (case-insensitive)', 
                value = ''),
      
      h6(a('www.runzemc.com', href='http://www.runzemc.com'))
    ),
    
    mainPanel(
      htmlOutput('header1'),
      htmlOutput('header2'),
      plotOutput('plot')
    )
  )
))