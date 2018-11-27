library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)

source("questions_csv.R")
source("marks_csv.R")

# The list for this marker to mark. This would be randomised by student (but not by question)
mark_order <- read_marks() %>% select(StudentID, Question) %>% arrange(Question, StudentID)

shinyServer(function(input, output, session) {

  # Where we are at currently. Will have marker_id in future
  student <- reactiveValues(info = get_student_details(mark_order$StudentID[1]))

  current <- reactiveValues(question = 1,
                            question_name = mark_order$Question[1],
                            marks = get_marks(mark_order$StudentID[1], mark_order$Question[1]))

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
    if (is.null(comments)) {
      comments = character(0)
    }
    selected = current$marks$comments
    checkboxGroupButtons("comments", choices = comments, selected=selected, status='light',
                      direction="vertical", individual=TRUE, width='350px')
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
      current$marks$comments = c(input$comments, input$addcomment)
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
  observe({
    cat("Marks being updated\n")
    marks = layout$question$marks
    by = layout$question$by
    selected = current$marks$mark
    if (is.na(selected)) selected = NULL
    cat("selected = ", selected, "\n")
    marks = seq(0, marks, by=by)
    choices = c("X", marks)
    updateRadioGroupButtons(session, "marks", choices = choices, selected=selected, status='marks')
  })
  observe({
    cat("Award being updated\n")
    cat("Current award = ", isolate(current$marks$award), "\n")
    award = current$marks$award
    cat("award = ", award, "\n")
    updateCheckboxGroupButtons(session, "star", selected = award)
  })
  
  # Next/Prev buttons
  observe({
#    shinyjs::toggleState("next", current$question < nrow(mark_order))
    shinyjs::toggleState("prev", current$question > 1)
  })
  changed <- function(old, new) {
    if (is.null(new) && length(old) == 1 && nchar(old) == 0) return(FALSE) # NULL and empty are equivalent
    if (length(new) != length(old)) return(TRUE) # length has changed
    if (any(is.na(new) != is.na(old))) return(TRUE) # NA pattern has changed
    # NA pattern hasn't changed, so just compare them ignoring the NAs
    diff = any(new != old)
    # if there's a difference in value, diff will be TRUE. If there's not it'll be FALSE or NA
    !is.na(diff) && diff
  }
  observeEvent(input$`next`, {
    cat("Going NEXT\n")
    marks = list(marks = as.numeric(input$marks),
                 award = input$star,
                 comments = input$comments)
    diff = unlist(map2(current$marks, marks, changed))
    if (any(diff)) { # we don't care if there's only NAs
      cat("Something has changed...\n")
      set_marks(id = student$info$id, question = current$question_name,
                mark = input$marks, award = input$star, comments = input$comments)
    }

    if (current$question < nrow(mark_order)) {
      # increment the question and/or student
      current$question = current$question + 1
      current$question_name = mark_order$Question[current$question]
      student$info = get_student_details(mark_order$StudentID[current$question])

      # read in the new layout
      layout$question = read_question_layout(current$question_name)
      layout$comments = get_comments_for_question(current$question_name)
      # and update marks
      current$marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = get_marks(student$info$id, current$question_name)
    }
  })
  observeEvent(input$prev, {
    if (current$question > 1) {
      cat("Going PREV\n")
      marks = list(marks = as.numeric(input$marks),
                   award = input$star,
                   comments = input$comments)
      diff = unlist(map2(current$marks, marks, changed))
      if (any(diff)) { # we don't care if there's only NAs
        cat("Something has changed...\n")
        set_marks(id = student$info$id, question = current$question_name,
                  mark = input$marks, award = input$star, comments = input$comments)
      }

      # decrement the question
      current$question = current$question - 1
      current$question_name = mark_order$Question[current$question]
      student$info = get_student_details(mark_order$StudentID[current$question])

      # read in the new layout
      layout$question = read_question_layout(current$question_name)
      layout$comments = get_comments_for_question(current$question_name)
      # and update marks
      current$marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = get_marks(student$info$id, current$question_name)
    }
  })

})
