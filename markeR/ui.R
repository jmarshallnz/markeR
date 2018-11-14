library(shiny)
library(shinyjs)

fluidPage(
  # Some JS magic to unfocus buttons on click
  tags$script(HTML("
     $(document).ready(function() {
        $('.bttn').on('click', function(){$(this).blur()});
      })
    ")),
  useShinyjs(),
  titlePanel("markeR: Arrr!"),
  h3(textOutput("student")),
  h4(textOutput("question")),
  fluidRow(

    column(9, 
      uiOutput("comments")
    ),
    
    column(1,
      uiOutput("marks")
    ),
    
    column(2,
      uiOutput("awards")      
    )
  ),
  fluidRow(
    column(8, selectizeInput("addcomment", label = NULL, choices = NULL, width = '100%', options=list(create=TRUE, placeholder="New Comment..."))),
    column(2, actionBttn("prev", "Prev", size="lg", block=TRUE, color='primary', style='unite')),
    column(2, actionBttn("next", "Next", size="lg", block=TRUE, color='primary', style='unite'))
  )
)
