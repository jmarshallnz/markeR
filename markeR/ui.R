library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

add_attribute <- function(x, ...) {
  x$attribs <- c(x$attribs, list(...))
  x
}

dashboardPage(
  dashboardHeader(title="markeR", titleWidth = 350),
  dashboardSidebar(width=350,
    h4(textOutput("question"), style="display:inline-block; padding-left: 5px"),
    actionButton("hideguide", label="Hide", class="btn-tiny", style="float: right;"),
    conditionalPanel(condition = "output.show_guide > 0", htmlOutput("markingguide")),
    tags$hr(),
    h4("Comments", style="padding-left: 5px"),
    checkboxGroupButtons("comments", choices = "", selected=NULL, status='light',
                         direction="vertical", individual=TRUE, width='350px'),
    selectizeInput("addcomment", label = NULL, choices = NULL, width = '100%', options=list(create=TRUE, placeholder="New Comment...")),
    tags$hr(),
    h4("Mark", style="padding-left: 5px"),
    add_attribute(radioGroupButtons("marks", status = "marks", choices = "", individual=TRUE, width='290px', justified=TRUE),id="markgroup"),
    add_attribute(checkboxGroupButtons("star", choiceNames = list(""), choiceValues = list("star"), checkIcon = list(yes = icon("star", class="fas"), no = icon("star-o", class="far")),
                         status="star"),id="markgroup"),
    tags$hr(),
    div(id = "pagebuttons", style="width:340px",
      actionButton("prev", "Prev", width='20%', style="display:inline-block;", class="btn-page"),
      actionButton("next", "Next", width='20%', style="display:inline-block;", class="btn-page"),
      actionButton("next_unmarked", "Unmarked", width='30%', style="display:inline-block; float:right", class="btn-page"))
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(HTML('
                  $(document).ready(function() {
                  $("header").find("nav").append(\'<div id="pageHeader" class="header-class"></div>\');
                  })
                 ')),
    tags$head(
      tags$link(rel = "stylesheet", type="text/css", href="custom.css")
    ),
    tags$style(type = "text/css", "#pdfviewer {height: calc(100vh - 54px) !important;}"),
    htmlOutput('pdfviewer')
  )
)
