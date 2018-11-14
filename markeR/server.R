library(shiny)
library(shinyWidgets)
library(dplyr)

#source("sample_db.R")
source("sample_csv.R")

# create some data for some questions
question_db = tibble(Question = 1:3, Marks = c(3,5,2), By=c(1,1,0.5))
comments_db = tibble(Question = 1, Comments = c("Calculation error", "Use a prediction interval instead of a confidence interval", "Incorrect standard error", "Interpretation is for individuals"))
awards_db = tibble(Awards = c("Excellent!", "Great work!", "Very nice!"))

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

read_layout <- function(question) {
  # get the question
  layout = question_db %>% filter(Question == question) %>% select(marks=Marks, by=By) %>% as.list()
  # get the comments
  layout$comments = comments_db %>% filter(Question == question) %>% pull(Comments)
  # get the awards
  layout$awards = awards_db %>% pull(Awards)
  # return
  layout
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
  current <- reactiveValues(student_id = current_student_id,
                            student_name = current_student_name,
                            question = current_question,
                            layout = read_layout(current_question),
                            marks = read_marks(current_student_id, current_question))

  # All comments, so that we can update the choice list when the user has added one
  comments <- reactiveValues(all = get_all_comments())

  # TODO: make the various buttons do something useful...
#  observeEvent(input$submit, {
#    sample <- get_sample()
#    if (sum(!is.na(sample)) > 0) {
#      # write the results to the database
#      v$samples <- rbind(v$samples, c(sample, current_year))
#      if (!write_row("noodle", columns, c(sample, current_year))) {
#        cat("Unable to write sample to database\n", file=stderr())
#      }
#    }
#
#    # reset our input controls
#    for (i in seq_along(vars))
#      updateNumericInput(session, vars[i], value=NA)
#  })

  # Current student
  output$student = renderText({
    paste(current$student_id, current$student_name)
  })

  # Current question
  output$question = renderText({
    paste("Question", current$question)
  })
  
  # Comment buttons
  output$comments = renderUI({
    comments = current$layout$comments
    selected = current$marks$comments
    checkboxGroupButtons("comments", choices = comments, selected=selected,
                      direction="vertical", size='lg', individual=TRUE, width='700px', justified = TRUE)
  })
  
  # Add comment updater

  # React to the add comment button
  observeEvent(input$addcomment, {
    # nchar check, because emptying the text field results in "" choice.
    if (nchar(input$addcomment)) {
  #    && !(input$addcomment %in% comments$all)) {
      cat("Adding a new comment\n")
      # new comment - add to this question.
      current$layout$comments <- c(current$layout$comments, input$addcomment)
      # and select it!
      current$marks$comments = c(current$marks$comments, input$addcomment)
    }
  })

  # Update all comments whenever a single comment is added
  observe({
   cat("all comments updated\n")
   current$layout$comments
   comments$all = union(comments$all, current$layout$comments)
   updateSelectizeInput(session, "addcomment", selected = "", choices = setdiff(comments$all, current$layout$comments), server = TRUE)
  })

  # Marks buttons
  output$marks = renderUI({
    marks = current$layout$marks
    by = current$layout$by
    selected = current$marks$mark
    radioGroupButtons("marks", choices = seq(0, marks, by=by), selected=selected, status = 'danger',
                      direction="vertical", size='lg', individual=TRUE, width='100%')
  })

  # Award buttons
  output$awards = renderUI({
    awards = current$layout$awards
    selected = current$marks$award
    cat("Selected award is:")
    print(selected)
    if (is.null(selected))
      selected = character(0)
    cat("\n")
    # TODO: Perhaps just replace these with a set of buttons and then detect them directly?
    radioGroupButtons("awards", choices = awards, selected=selected,
                      direction="vertical", size='lg', individual=TRUE, width='100%')
  })

  # Next/Prev buttons
  observe({
    shinyjs::toggleState("next", current$question < max_question())
    shinyjs::toggleState("prev", current$question > 1)
  })
  observeEvent(input$`next`, {
    if (current$question < max_question()) {
      current$question = current$question + 1
      current$layout = read_layout(current$question)
      current$marks = read_marks(current$student_id, current$question)
    }
  })
  observeEvent(input$prev, {
    if (current$question > 1) {
      current$question = current$question - 1
      current$layout = read_layout(current$question)
      current$marks = read_marks(current$student_id, current$question)
    }
  })
})
