library(dplyr)
options(stringsAsFactors = F)

verbose <- T
wd_output <- 'output'
labels <- c("elephant", "rhinoceros")
img_width <- 1200    #this determines image size- height is et to 100%
  
#===== functions =====
#lookup plot for a given round
lookup_plot <- function(data_record, round_num){
  file_current <- file.path("img",data_record$file[round_num])
  
  # Return a list containing the filename
  return(list(src = file_current,
              contentType = 'image/png',
              width = img_width,      
              alt = data_record$file[round_num]))
}

hash_name <- function(username, n = 5){
  if(nchar(username) == 0) return(1)
  vals <- (1:26)
  names(vals) <- letters
  track <- sum(sapply(strsplit(username,"")[[1]], FUN = function(s){
    if(!s %in% letters) return(0)
    vals[names(vals)==s]
  }))
  return((track %% n) + 1)
}

#==================================
server <- function(input, output, session) {
  values <- reactiveValues(round_num = 1,
                           username = NULL,
                           data_record = NULL,
                           leaderboard = NULL,
                           output_file = NULL)
  
  # only elements related to multiple tabs are included here
  
  observe({
    # when username is updated, update filename and load any saved data
    if (verbose) print("in update username observe")
    username <- tolower(input$username) # this is the trigger
    dig <- hash_name(username)     #outputs a deterministic digit between 0 and 9 for the username
    
    input_file  <- file.path("template", paste0("t", dig, ".csv"))
    output_file <- file.path(wd_output, paste0(dig, "_", username, ".csv"))
    
    if (!file.exists(input_file)) stop("Error: can't find file ", input_file)
    if (!file.exists(output_file)) file.copy(from = input_file, to=output_file)
    
    values$username    <- username
    values$input_file  <- input_file
    values$output_file <- output_file
    
    data_record <- read.csv(file=output_file)
    for(l in labels){if(!l %in% names(data_record)) data_record[[l]] <- NA}
    values$data_record <- data_record
    write.csv(data_record, file=output_file)
    round_num   <- min(which(data_record$status==FALSE))
    if(round_num==Inf)  round_num <- 1
    values$round_num <- round_num
  })
  
  source("subroutines/play.R", local=TRUE)
  
  
  output$records <- renderDataTable({
    if (verbose) print("in records renderDataTable")
    values$data_record
  })
  
  output$status_text <- renderText({
    if (verbose) print("in status_text renderText")
    username <-  values$username
    round_num <- values$round_num
    if(is.null(username) | username==""){ return("Please enter username to continue.")
    }else{                 return(paste("Round:", round_num))
    }
  })
  
 
  source("subroutines/leaderboard.R", local=TRUE)
  
}
