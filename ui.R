## ui.R ##
library(shiny)


# how many labels will we need to add? will it change over time?
# leaderboard does not currently have any information that is useful. change this.

#====== setup tabs ===========
tab_login       <- tabPanel('login',
                            sidebarLayout(
                              sidebarPanel(textInput("username", label = h5("username:"), value = ""),
                                           helpText("Example: first.last"),
                                           width = 3
                              ),
                              mainPanel(
                                h5(textOutput("login_text"))
                              )
                            )
)
  
tab_leaderboard <- tabPanel('leaderboard',
                            actionButton("update_leaderboard", label = "Update leaderboard"),
                            fluidRow(
                              column(12,
                                     br(),
                                     h3(textOutput("ranking")),
                                     dataTableOutput("leaderboard")
                              )
                            )
)

tab_play <- tabPanel("play",
                     fluidRow(
                       column(4,
                              h4(textOutput("status_text")),
                              fluidRow(
                                br(),
                                actionButton("previous_button", label = "Previous"), 
                                actionButton("next_button", label = "Next")
                              )
                       ),
                       column(5,
                              
                              selectInput("label", label = h4("Select label"), 
                                          choices = "", 
                                          selected = ""),
                              actionButton("apply_label", label = "Apply label to selected area")
                              
                       ),
                       column(3,
                              textOutput("labeled"),
                              imageOutput("selected_area", height = 20)
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(12,
                              imageOutput("ref_plot", height = "100%",
                                          brush = brushOpts(
                                            id = "image_dblclick",
                                            resetOnNew = TRUE
                                          )
                              )
                       )
                     )
)

tab_record <- tabPanel("record",
                       dataTableOutput("records") 
)


#======= combined ui =========
shinyUI(
  navbarPage("crowdsourcing app",
             tab_login,
             tab_play,
             tab_record,
             tab_leaderboard
  )
)
