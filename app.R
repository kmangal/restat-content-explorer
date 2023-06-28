# ReStat Content Explorer - RShiny App
# Developer: Kunal Mangal, HKS PhD Student, kmangal@g.harvard.edu

rm(list = ls())

library(shiny)
library(shinyjs)

library(readr)
library(stringr)

library(ggplot2)
library(scales)
library(data.table)
library(DBI)
library(pool)
library(dplyr)
library(TTR)
library(RMySQL)
library(twitteR)
library(magick)

library(config)

'%nin%' <- function(x,y)!('%in%'(x,y))

db <- config::get("db", file = "config/demo.yml")

# Disable twitter functionality in demo version
#twitterconfig <- config::get("twitter", file = "config/config.yml")

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = db$dbname,
  host = db$host,
  username = db$uid,
  password = db$pwd
)

# Disable twitter functionality in demo version
# setup_twitter_oauth(consumer_key = twitterconfig$consumerkey,
#                     access_token = twitterconfig$access_token,
#                     consumer_secret = twitterconfig$consumer_secret,
#                     access_secret = twitterconfig$access_secret)
# 
# # The setup prompts whether to cache this information locally or not. 1 = yes, 2 = no.
# 1

make_blank_output <- function(ylabel) {
  
  year <- seq(1920, 2011, 1)
  word_freq <- rep(0, length(year))
  blank_df <- data.frame(year, word_freq)
  
  out <- ggplot(blank_df, aes(x = year, y = word_freq)) +
    xlab("Publication Year") +
    ylab(ylabel) +
    scale_x_continuous(breaks = seq(1920, 2010, 10)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme_bw() + 
    theme(axis.text = element_text(size = 12), 
          axis.title=element_text(size=16))
  
  return(out)
}


blank_output <- make_blank_output("Fraction of articles")


make_wordlist <- function(input) {
  if(input == '') {
    return("")
  }
  
  input.lower <- str_to_lower(input)
  wordlist <- unlist(strsplit(input.lower, ","))
  wordlist <- trimws(wordlist)  
  return(wordlist)
  
}

get_data <- function(wordlist, graphtype) {
  
  if (length(wordlist) == 1) {
    if (wordlist == "") {
      return(data.frame())
      
    }
  }
  
  wordlistq <- paste0("('", paste(wordlist, collapse = "', '"), "')")
  query <- paste0("SELECT word, year, ", graphtype, " as yvar FROM wordstab WHERE word IN ", wordlistq)
  dfq <- dbGetQuery(pool, query)
  return(dfq)
}

check_word <- function(wordlist, df) {
  if (length(wordlist) == 1) {
    if (wordlist == "") {
      return("")
    }
  }
  
  nwords <- length(wordlist)
  dfwords <- unique(df$word)
  
  if (length(dfwords) == nwords) {
    return("")
  }
  
  if (length(dfwords) == nwords - 1) {
    notfound <- wordlist[wordlist %nin% dfwords]
    return(paste("'", notfound, "' not found", sep= ""))
  }
  
  notfound <- wordlist[wordlist %nin% dfwords]
  notfounds <- paste(notfound, collapse = "' and '")
  return(paste("'", notfounds, "' not found", sep= ""))
  
}

plot_word_freq <- function(wordlist, dfq, graphtype, smoothing) {
  
  # If input is blank, return a blank graph
  if (length(wordlist) == 1) {
    if (wordlist == "") {
      return(blank_output)
    }
  }

  if (nrow(dfq) == 0){
    return(blank_output)
  }
  
  nwords <- length(wordlist)
  
  dfallyears <- data.frame(year = rep(seq(1920, 2010, 1), nwords))
  dfallyears$year <- sort(dfallyears$year)
  dfallyears$word <- rep(wordlist, 91)
  df <- base::merge(dfq, dfallyears, all = T)
  
  df$yvar[is.na(df$yvar)] <- 0
  
  if (smoothing == 0) {
    df$yvar_filtered <- df$yvar
  }
  else {
    df <- df %>% group_by(word) %>% mutate(yvar_filtered = runMean(yvar, 2 * smoothing + 1))
  }
  
  
  if (graphtype == "fracarticle") {
    ylabtext <- "Fraction of articles"
  }
  else {
    ylabtext <- "Fraction of words"
    
  }
  
  output <- ggplot(df, aes(x = year, y = yvar_filtered, colour = word)) +
    xlab("Publication Year") +
    ylab(ylabtext) +
    scale_x_continuous(breaks = seq(1920, 2010, 10)) +
    scale_y_continuous(labels = scales::percent) +
    geom_point(size = 2) + 
    geom_line(linewidth = 1) +
    scale_colour_brewer("", palette="Set1") +
    theme_bw() + 
    theme(axis.text = element_text(size = 12), 
          axis.title=element_text(size=16),
          legend.text=element_text(size=12),
          legend.position = "top")
  
  return(output)
  
}

# Define server logic
server <- function(input, output, session) {
  
  # Disable tweet functionality in demo version
  
  # # Functionality for the "Tweet this graph" link
  # observeEvent(input$tweetserver, {
  #   
  #   # We will first tweet the graph with a description on the @restat100 account
  #   # We'll then open up a tweet intent for the user to reply to that tweet, so
  #   # the can add some additional commentary
  #   
  #   wordlist <- make_wordlist(input$searchtext)
  #   wordlistq <- paste(wordlist, collapse = ' ')
  #   readquery <- paste0("SELECT tweetid FROM tweets WHERE wordlist = '", wordlistq, 
  #                   "' AND graphtype = '", input$graphtype, 
  #                   "' AND smoothing = ", input$smoothing)
  #   
  #   df <- dbGetQuery(pool, readquery)
  #   
  #   if (nrow(df) == 0) {
  #     # Make a new tweet
  #     
  #     png('tweet.png', width = 600, height = 400)
  #     print(plot_word_freq(wordlist, dfq(), 
  #                          input$graphtype, 
  #                          as.numeric(input$smoothing)))
  #     dev.off()
  #     
  #     if (input$graphtype == 'fracarticle') {
  #       s <- 'Fraction of published articles that mention the '
  #     }
  #     if (input$graphtype == 'fracword') {
  #       s <- 'Fraction of published words that include the '
  #     }
  #     
  #     nwords <- length(wordlist)
  #     max.wordlength <- max(unlist(lapply(strsplit(wordlist, ' '), length)))
  #     
  #     if (nwords == 1 & max.wordlength == 1) {
  #       w <- 'word '
  #     }
  #     if (nwords > 1  & max.wordlength == 1) {
  #       w <- 'words '
  #     }
  #     if (nwords == 1 & max.wordlength > 1) {
  #       w <- 'phrase '
  #     }
  #     if (nwords > 1 & max.wordlength > 1) {
  #       w <- 'phrases '
  #     }
  #     
  #     if (nwords == 1) {
  #       wl <- paste('"', wordlist, '"', sep = '')
  #     }
  #     else if (nwords == 2) {
  #       wl <- paste('"', wordlist[1], '" and "', wordlist[2], '"', sep = "")
  #     }
  #     else {
  #       wl <- paste(wordlist[-nwords], collapse = '," "', sep = '')
  #       wl <- paste('"', wl, '," and "', wordlist[nwords], '"', sep = '')
  #     }
  #     
  #     tweet.text <- paste(s, w, wl, sep = "")
  #     
  #     # Post tweet
  #     tw <- updateStatus(tweet.text, mediaPath = 'tweet.png')
  #     
  #     # Update database
  #     writequery <- paste0("INSERT INTO tweets (tweetid, wordlist, graphtype, smoothing) VALUES ('", 
  #                          tw$id, "', '", 
  #                          wordlistq, "', '", 
  #                          input$graphtype, "', '",
  #                          input$smoothing, "')")
  #     
  #     numrowsaffected <- dbExecute(pool, writequery)
  #     
  #     session$sendCustomMessage("tweetintent", tw$id)
  #     
  #   }
  #   else {
  #     # serve old tweet
  #     session$sendCustomMessage("tweetintent", df$tweetid)
  #     
  #   }
  #   
  # })
  
  plot_with_logo <- function(wordlist, dfq, graphtype, smoothing) {
    png('out.png', width = 600, height = 400)
    print(plot_word_freq(wordlist, dfq, 
                         graphtype, 
                         smoothing))
    dev.off()
    
    plot <- image_read('out.png')
    logo<- image_read("www/logo_v4.png") 
    
    final_plot <- image_append(image_scale(c(plot, logo), "600"), stack = TRUE)
    return(final_plot)
  }
  
  # generate_main_plot <- function(wordlist, dfq, graphtype, smoothing) {
  #   outfile <- tempfile(fileext='.png')
  # 
  #   final_plot <- plot_with_logo(wordlist, dfq, graphtype, smoothing)
  #   image_write(final_plot, outfile)
  #   return(list(src = outfile,
  #               alt = "Error generating plot"))
  # }
  
  # Handles the "Download this graph" link
  output$downloadPlot <- downloadHandler(
    filename = "download.png",
    content = function(file) {
      final_plot <- plot_with_logo(wordlist(), dfq(), input$graphtype, as.numeric(input$smoothing))
      image_write(final_plot, file)
    },
    contentType = 'image/png'
  )  
  
  wordlist <- reactive({
    input$submit
    isolate(make_wordlist(input$searchtext))
  })
  
  dfq <-  reactive({
    input$submit
    isolate(get_data(wordlist(), input$graphtype))
  })
  
  
  # Adds warning text in case word is not found in database
  output$warning <- renderText({ 
    input$submit
    isolate(check_word(wordlist(), dfq()))
  })
  
  # Renders plot after user clicks the "plot" button  
  output$plot <- renderPlot({
    input$submit
    isolate(plot_word_freq(wordlist(), dfq(), input$graphtype, as.numeric(input$smoothing)))
  })
  
  # Old code for handling GET requests, which would allow users to share graphs via links
  #
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['searchtext']]) & !is.null(query[['graphtype']])) {
  #     #updateTextInput(session, "searchtext", value = query[['searchtext']])
  #     output$plot <- renderPlot({
  #       plot_word_freq(query[['searchtext']], query[['graphtype']], 1)
  #     })
  #   }
  #   
  # })
  
}

# Run the application 

html <- read_file("www/template.html")
ui <- htmlTemplate(text_ = html)
shinyApp(ui = ui, server = server)