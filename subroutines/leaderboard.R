observeEvent(input$update_leaderboard, {
  if (verbose) print("in update_leaderboard observeEvent")
  
  output_files <- list.files(wd_output)
  leaderboard <- dplyr::bind_rows(lapply(output_files, FUN=function(f){
    user <- gsub("^[0-9][_]", "", f)
    user <- gsub("[.]csv", "", user)
    file_f <- read.csv(file.path(wd_output,f))
    data.frame("user" = user, "levels_completed" = sum(file_f$status == TRUE))
  }))
  leaderboard <- leaderboard %>% 
    filter(user != "" & levels_completed>0) %>% 
    arrange(desc(levels_completed))
  
  if(values$username == ""){ msg <- ""
  }else{
    rank <- which(leaderboard$user == isolate(values$username))
    if(length(rank)==0){  msg <- paste(isolate(values$username), "is not yet ranked")
    }else{                msg <- paste(isolate(values$username), "ranking:", rank, "of", nrow(leaderboard))
    }
    
    values$ranking     <- msg
  }
  
  values$leaderboard <- leaderboard
})

output$leaderboard <- renderDataTable({
  if (verbose) print("in leaderboard renderDataTable")
  leaderboard <- values$leaderboard
  leaderboard
})

output$ranking <- renderText({
  if (verbose) print("in ranking renderText")
  ranking <- values$ranking
  ranking
})
