# img <- png::readPNG(file.path("img", "img1.png"))
# dim(img)

# login text output
output$login_text <- renderText({
  if (verbose) print("in login_text renderText")
  username    <- values$username        #trigger 1
  data_record <- values$data_record     #trigger 2
  
  completed <- sum(data_record$status == TRUE)
  msg <- paste("Welcome ", username, "!")
  if(is.null(username) | username==""){ return("Please enter username to continue.")
  }else if(completed > 0){              return(paste(msg, "You've completed",completed, "rounds."))
  }else{                                return(paste(msg))
  }
})


# buttons to change iterate through files
observeEvent(input$previous_button, {
  if (verbose) print("in previous_button observeEvent")
  if(values$username == "" | is.null(values$data_record)) return(NULL)    # don't do anything if theres no username
  
  current_round <- isolate(values$round_num)
  data_record   <- isolate(values$data_record)
  
  data_record$status[current_round]     <- TRUE
  
  write.csv(data_record, isolate(values$output_file), row.names = F)
  values$data_record <- data_record
  
  new_round <- ((current_round - 1) %% (nrow(data_record)))
  if(new_round == 0) new_round <- nrow(data_record)
  values$round_num <- new_round
})

observeEvent(input$next_button, {
  if (verbose) print("in next_button observeEvent")
  # don't do anything if theres no username
  if(values$username == "" | is.null(values$data_record)) return(NULL)    # don't do anything if theres no username
  
  current_round <- isolate(values$round_num)
  data_record   <- isolate(values$data_record)
  
  data_record$status[current_round]     <- TRUE
  
  write.csv(data_record, isolate(values$output_file), row.names = F)
  values$data_record <- data_record
  
  new_round <- ((current_round + 1) %% (nrow(data_record)))
  if(new_round == 0) new_round <- nrow(data_record)
  values$round_num <- new_round
})

# if username or round_num changes, update interface
observe({
  username    <- values$username      #this is a trigger
  round_num   <- values$round_num     #this is another trigger
  
  # update dropdown selection to ""
  updateSelectInput(session, "label",
                    choices = c("", labels),
                    selected = ""
  )
})


output$labeled <- renderText({
  if (verbose) print("in labeled renderText")
  
  round_num   <- values$round_num     #this is a trigger
  #t2          <- input$apply_label    #another trigger
  data_record <- values$data_record
  
  if(is.null(data_record)) return(NULL)    # don't do anything if theres no username
  
  # name labels already applied
  #data_record <- read.csv(file.path("output", "4_w.csv")); round_num <-1
  data_record_round   <- data_record[round_num,]
  applied <- unlist(lapply(labels, FUN=function(l){
    if(is.na(data_record_round[[l]])) return(NULL)
    return(l)
  }))
  if(is.null(applied)) return("no labels applied")
  lab_applied <- paste("Labels applied:", toString(applied))
  return(lab_applied)
})

# apply label to selected area
observeEvent(input$apply_label, {
  if (verbose) print("in apply_label observeEvent")
  
  round_num  <- values$round_num    
  label      <- input$label
  brush_data <- input$image_dblclick
  
  if(is.null(brush_data)){ label_output <- NA
  }else{
    x_range <- img_width
    y_range <- brush_data$range$bottom
    label_output <- paste0("xmin:", 
                           round(brush_data$xmin/x_range, 4), ", xmax:", 
                           round(brush_data$xmax/x_range, 4), ", ymin:", 
                           round(brush_data$ymin/y_range, 4), ", ymax:", 
                           round(brush_data$ymax/y_range, 4))
  }
  
  
  # name labels already applied
  if(is.null(values$data_record)) return(NULL)
  if((!label %in% names(data_record))) return(NULL)
  data_record   <- isolate(values$data_record)
  num_label     <- length(data_record[[label]][round_num])
  data_record[[label]][round_num] <- label_output
  data_record$status[round_num]     <- TRUE
  
  write.csv(data_record, isolate(values$output_file), row.names = F)
  values$data_record <- data_record
})

# update image to label
output$ref_plot <- renderImage({
  if (verbose) print("in ref_plot renderImage")
  
  lookup_plot(values$data_record,
              values$round_num)
}, deleteFile = FALSE)


output$selected_area <- renderImage({
  if (verbose) print("in selected_area renderImage")
  
  data_record <- values$data_record
  round_num   <- values$round_num
  
  # name labels already applied
  data_record_round   <- data_record[round_num,]
  applied <- unlist(lapply(labels, FUN=function(l){
    if(is.na(data_record_round[[l]])) return(NULL)
    return(l)
  }))
  
  outfile <- file.path("img", "tmp.png")
  if(is.null(applied) | length(applied) == 0){
    placeholder <- array(0, dim=c(1,1,1))
    png::writePNG(placeholder, outfile)
    return(list(src = outfile))
  }
  
  # focus on selected area
  file_current <- file.path("img", data_record_round$file)
  current_img <- png::readPNG(file_current)

  x_range <- dim(current_img)[2]
  y_range <- dim(current_img)[1]

  imgs <- lapply(applied, FUN=function(i){
    coords <- data_record_round[[i]]   # coords <- "xmin:0.57, xmax:0.5917, ymin:0.1002, ymax:0.1403"
    coords <- gsub("[a-z]*[:]", "", coords)
    coords <- as.numeric(strsplit(coords, ",")[[1]])
    xmin <- round(coords[1]*x_range)
    xmax <- round(coords[2]*x_range)
    ymin <- round(coords[3]*y_range)
    ymax <- round(coords[4]*y_range)
    
    selected_img <- current_img[ymin:ymax,,]
    selected_img <- selected_img[,xmin:xmax,]
    #grid::grid.raster(selected_img)
    
    return(selected_img)
  })
  if (verbose) print("files grabbed")
  
  # pad y-direction (all files hould have max_y, x variable, 4)
  max_y <- max(unlist(lapply(imgs, FUN= function(i){dim(i)[1]})))
  ind_x <- cumsum(unlist(lapply(imgs, FUN= function(i){dim(i)[2]})))
  if (verbose) print(max_y)
  
  combined_img <- array(1, dim = c(max_y, max(ind_x), 3))
  for(i in 1:length(ind_x)){
    im <- imgs[[i]]
    combined_img[(1:dim(im)[1]), (ind_x[i] - dim(im)[2] + 1):ind_x[i], 1:3] <- im
  }
  if (verbose) print("images combined")
  
  # Generate a png
  png::writePNG(combined_img, outfile)

  # pixel width of image to display
  scaled_w <- 
    if(dim(combined_img)[1] > 100){ 100/dim(combined_img)[1]*dim(combined_img)[2]
    }else if(dim(combined_img)[2] > 300){ 300
    }else{  dim(combined_img)[2]}
  
  # Return a list
  list(src = outfile,
       width = scaled_w)
})
