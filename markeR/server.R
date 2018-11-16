library(shiny)
library(shinyWidgets)
library(dplyr)

#source("sample_db.R")
source("sample_csv.R")

# create some data for some questions
question_db = tibble(Question = 1:3, Marks = c(3,5,2), By=c(1,1,0.5), Guide="$$\\begin{aligned}\\bar{x} \\pm 2 \\frac{s}{\\sqrt{n}} &= 2.3 \\pm 2 \\frac{1.2}{\\sqrt{53}}\\\\\\\\ & = 2.3 \\pm 0.33\\\\\\\\ & = (1.97, 2.63)\\end{aligned}$$\n - 1 mark for calculation\n - 1 mark for answer\n - 1 mark for interpretation")
comments_db = tibble(Question = 1, Comments = c("Calculation error", "Use a prediction interval instead of a confidence interval", "Incorrect standard error", "Interpretation is for individuals"))

# Format of marks data.frame would be something like:
marks_db <- NULL
#data.frame(StudentID = NULL, StudentName = NULL, Question = NULL, Mark = NULL, Comments = NULL, Award=NULL)

# These are reactives that change per-student
current_student_id = "15969203" # make identifier a string for extensibility
current_student_name = "Joanne Bloggs"
current_question = 1

# These are reactives that change per-question/student
question_marks = NULL; # this is fetched from the database given the student and question
question_layout = NULL; # this is fetched from the database given the question

# database stuff goes here
# create database for noodles
#if (!create_database("noodle", columns)) {
#  cat("Unable to create database\n", file=stderr());
#}
#sim_columns <- c("n", "Ex", "Ex2", "year")
#if (!create_database("simulation", sim_columns, types=c("INT", "DOUBLE", "DOUBLE", "INT"))) {
#  cat("Unable to create database\n", file=stderr());
#}
#sim_start <- na.omit(read_rows("simulation", sim_columns))
#if (!is.null(sim_start) && nrow(sim_start) > 0) {
#  sim_start <- sim_start[nrow(sim_start),]
#  cat("read simulation info from database\n", file=stderr())
#} else {
#  cat("didn't read simulation info from database - creating new\n", file=stderr())
#  sim_start <- matrix(0, 1, length(sim_columns))
#  colnames(sim_start) = sim_columns;
#  sim_start <- as.data.frame(sim_start)
#}

read_question_layout <- function(question) {
  # get the question
  question_db %>% filter(Question == question) %>% select(marks=Marks, guide=Guide, by=By) %>% as.list()
}

read_comments_layout <- function(question) {
  # get the comments
  comments_db %>% filter(Question == question) %>% pull(Comments)
}

read_marks <- function(student_id, question) {
  # get the student/id/marks
  if (is.null(marks_db)) {
    # TODO: open the marks database and read...
    return(NULL)
  }
  marks_db %>%
    filter(StudentID == student_id, Question == question) %>%
    select(mark=Mark, award=Award, comments=Comments)
}

get_all_comments <- function() {
  comments_db %>% pull(Comments) %>% unique()
}

max_question <- function() {
  question_db %>% pull(Question) %>% max()
}

shinyServer(function(input, output, session) {

  # Where we are at currently. Will have marker_id in future
  student <- reactiveValues(id = current_student_id,
                            name = current_student_name,
                            pdf_url = "test.pdf")

  current <- reactiveValues(question = current_question,
                            marks = read_marks(current_student_id, current_question),
                            new_marks = read_marks(current_student_id, current_question))

  layout  <- reactiveValues(show_guide = TRUE,
                            question = read_question_layout(current_question),
                            comments = read_comments_layout(current_question))

  # All comments, so that we can update the choice list when the user has added one
  comments <- reactiveValues(all = get_all_comments())

  # Current student
  observe({
    cat("updating the pageHeader\n")
    header = paste(student$id, student$name)
    shinyjs::html("pageHeader", header)
  })
  output$pdfviewer <- renderText({
    return(paste('<iframe name="pdfviewer" style="width:100%; border:0; height:100%" src="', paste0(student$pdf_url, "#toolbar=0&navpanes=0"), '"></iframe>', sep = ""))
  })

  # Current question
  output$question = renderText({
    paste("Question", current$question)
  })

  # Current guide
  output$markingguide = renderUI({
    html <- markdown::markdownToHTML(text = layout$question$guide, fragment.only = TRUE)
    Encoding(html) <- "UTF-8"
    withMathJax(HTML(html))
  })

  # Hide/show the guide
  observeEvent(input$hideguide, {
    layout$show_guide <- !layout$show_guide
    updateActionButton(session, "hideguide", label = ifelse(layout$show_guide, "Hide", "Show"))
  })
  output$show_guide <- reactive({ return(if(layout$show_guide) 1 else 0) })
  outputOptions(output, "show_guide", suspendWhenHidden = FALSE)

  # Comment buttons
  output$comments = renderUI({
    comments = layout$comments
    selected = current$marks$comments
    checkboxGroupButtons("comments", choices = comments, selected=selected, status='light',
                      direction="vertical", individual=TRUE, width='350px', justified = TRUE)
  })

  # Add comment updater
  observeEvent(input$addcomment, {
    if (nchar(input$addcomment)) { #  nchar check, because emptying the text field results in "" choice.
      cat("Adding a new comment\n")
      # new comment - add to this question
      # TODO: UPDATE THE DATABASE HERE
      layout$comments <- c(layout$comments, input$addcomment)
      # and select it!
      current$marks$comments = c(current$marks$comments, input$addcomment)
    }
  })

  # Update all comments whenever a single comment is added
  observe({
   cat("all comments updated\n")
   layout$comments
   comments$all = union(comments$all, layout$comments)
   choices = setdiff(comments$all, layout$comments)
   updateSelectizeInput(session, "addcomment", selected = "", choices = choices, server = TRUE)
  })

  # Marks buttons
  observeEvent(input$marks, {
    # We need this as updateRadioGroupButtons et. al. don't update input$marks when updating/reset, even with renderUI
    # see https://stackoverflow.com/questions/40309649/shiny-updateradiobuttons-cannot-clear-selected-item for example
    cat("Clicked on a mark\n")
    # Note: This isn't enough - input$marks might be 2 from a previous question, whereby
    # clicking on 2 won't trigger this observer as input$marks isn't updated.
    # We need a way of *forcing* input$marks to be back to empty or _something_. Maybe it's only on deselect??
    current$new_marks$mark = input$marks # That isn't enough either, as it doesn't reset if you click the same thing
                                         # i.e. if input$marks doesn't change between items
  })
  observe({
    cat("Marks being updated\n")
#    layout$question
    marks = layout$question$marks
    by = layout$question$by
    selected = current$marks$mark
    marks = seq(0, marks, by=by)
    choices = c("X", marks)
    updateRadioGroupButtons(session, "marks", choices = choices, selected=selected, status='marks')
  })

  # Next/Prev buttons
  observe({
    shinyjs::toggleState("next", current$question < max_question())
    shinyjs::toggleState("prev", current$question > 1)
  })
  observeEvent(input$`next`, {
    if (current$question < max_question()) {
      # TODO: Write mark, award, layout to database
      cat("Going NEXT\n")
      cat("Current mark=", isolate(current$marks$mark), "\n")
      cat("Current new-mark=", isolate(current$marks$new_mark), "\n")
      cat("Current input-mark=", isolate(input$marks), "\n")
      cat("Current award=", isolate(current$marks$award), "\n")
      cat("Current input-award=", isolate(input$awards), "\n")
      # increment the question
      current$question = current$question + 1
      # read in the new layout
      layout$question = read_question_layout(current$question)
      layout$comments = read_comments_layout(current$question)
      layout$awards   = read_awards_layout()
      # and update marks
      current$marks = read_marks(student$id, current$question)
      current$new_marks = read_marks(student$id, current$question)
    }
  })
  observeEvent(input$prev, {
    if (current$question > 1) {
      cat("Going PREV\n")
      cat("Current mark=", isolate(current$marks$mark), "\n")
      cat("Current new-mark=", isolate(current$marks$new_mark), "\n")
      cat("Current input-mark=", isolate(input$marks), "\n")
      cat("Current award=", isolate(current$marks$award), "\n")
      cat("Current input-award=", isolate(input$awards), "\n")

      # decrement the question
      current$question = current$question - 1
      # read in the new layout
      layout$question = read_question_layout(current$question)
      layout$comments = read_comments_layout(current$question)
      layout$awards   = read_awards_layout()
      # and update marks
      current$marks = read_marks(student$id, current$question)
      current$new_marks = read_marks(student$id, current$question)
    }
  })

})
