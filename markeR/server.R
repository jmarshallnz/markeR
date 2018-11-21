library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)

source("questions_csv.R")
source("marks_csv.R")

# The list for this marker to mark. This would be randomised by student (but not by question)
mark_order <- read_marks() %>% select(StudentID, Question)

current_student_id = "15969203" # make identifier a string for extensibility
current_student_name = "Joanne Bloggs"
current_question = 1

shinyServer(function(input, output, session) {

  # Where we are at currently. Will have marker_id in future
#  student <- reactiveValues(id = mark_order$StudentID[1],
#                            details = get_student_details(mark_order$StudentID[1]),
#                            pdf_url = get_student_pdf(mark_order$StudentID[1]))
  student <- reactiveValues(info = get_student_details(mark_order$StudentID[1]))

  current <- reactiveValues(question = 1,
                            question_name = mark_order$Question[1],
                            marks = get_marks(mark_order$StudentID[1], mark_order$Question[1]),
                            new_marks = get_marks(mark_order$StudentID[1], mark_order$Question[1]))

  layout  <- reactiveValues(show_guide = TRUE,
                            question = read_question_layout(mark_order$Question[1]),
                            comments = get_comments_for_question(mark_order$Question[1]))

  # All comments, so that we can update the choice list when the user has added one
  comments <- reactiveValues(all = get_all_comments())

  # Current student
  observe({
    cat("updating the pageHeader\n")
    header = paste(student$info$id, student$info$name)
    shinyjs::html("pageHeader", header)
  })
  output$pdfviewer <- renderText({
    return(paste('<iframe name="pdfviewer" style="width:100%; border:0; height:100%" src="', paste0(student$info$pdf_url, "#toolbar=0&navpanes=0"), '"></iframe>', sep = ""))
  })

  # Current question
  output$question = renderText({
    paste("Question", current$question_name)
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
      # new comment - add to the database
      add_comment_to_question(current$question_name, input$addcomment)
      # refresh the comment list from the database
      layout$comments <- get_comments_for_question(current$question_name)
      # and select that comment for the student
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
    shinyjs::toggleState("next", current$question < nrow(mark_order))
    shinyjs::toggleState("prev", current$question > 1)
  })
  observeEvent(input$`next`, {
    if (current$question < nrow(mark_order)) {
      # TODO: Write mark, award, layout to database
      cat("Going NEXT\n")
      cat("Current mark=", isolate(current$marks$mark), "\n")
      cat("Current new-mark=", isolate(current$marks$new_mark), "\n")
      cat("Current input-mark=", isolate(input$marks), "\n")
      cat("Current award=", isolate(current$marks$award), "\n")
      cat("Current input-award=", isolate(input$awards), "\n")

      # increment the question and/or student
      current$question = current$question + 1
      current$question_name = mark_order$Question[current$question]
      student$info = get_student_details(mark_order$StudentID[current$question])
      cat("Got student details...\n")
      print(isolate(student))

      # read in the new layout
      layout$question = read_question_layout(current$question_name)
      layout$comments = get_comments_for_question(current$question_name)
      cat("Got layout...\n")
      # and update marks
      current$marks = get_marks(student$info$id, current$question_name)
      current$new_marks = get_marks(student$info$id, current$question_name)
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
      current$question_name = mark_order$Question[current$question]
      student$info = get_student_details(mark_order$StudentID[current$question])

      # read in the new layout
      layout$question = read_question_layout(current$question_name)
      layout$comments = get_comments_for_question(current$question_name)
      # and update marks
      current$marks = get_marks(student$info$id, current$question_name)
      current$new_marks = get_marks(student$info$id, current$question_name)
    }
  })

})
