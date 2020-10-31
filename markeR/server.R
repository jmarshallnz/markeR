library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)

logfile <- "log.txt"
log <- function(...) { cat(..., file=logfile, append=TRUE) }

source("questions_csv.R")
source("marks_csv.R")

shinyServer(function(input, output, session) {

  paper <- reactiveValues(id = NULL,
                          markers = read_marks_for_paper(NULL) %>% pull(Marker) %>% unique())

  marker <- reactiveValues(id = "",
                           order = data.frame(StudentID = NULL, Question = NULL))

  # Where we are at currently. Will have marker_id in future
  student <- reactiveValues(info = NULL) #get_student_details(marker$order$StudentID[1]))

  # The current (and database) state of marking.
  # We use marks to store the marks temporarily, as input$foo doesn't necessarily update
  # immediately, so we need something to compare to.
  current <- reactiveValues(question = 1,
                             question_name = NULL, #marker$order$Question[1],
                             marks = NULL,
                             db_marks = NULL) #get_marks(marker$order$StudentID[1], marker$order$Question[1]))

  layout  <- reactiveValues(show_guide = TRUE,
                            question = NULL, #read_question_layout(marker$order$Question[1]),
                            comments = NULL) #get_comments_for_question(marker$order$Question[1]))

  # load the data based one the URL?
  observe({
    query = parseQueryString(session$clientData$url_search)
    log("called into query observer\n")
    if (!is.null(query[['p']]) && (is.null(paper$id) || query[['p']] != paper$id)) {
      paper$id = query[['p']];
      paper$markers = read_marks_for_paper(paper$id) %>% pull(Marker) %>% unique()
      marker$id = NULL; # force change in marker below.
      log('paper id has changed to:', paper$id, '\n')
      log('paper markers are:', paste(paper$markers, collapse=','), '\n')
      log("marker id has been set to NULL\n")
    }
    if (is.null(marker$id) || !is.null(query[['m']]) && query[['m']] != marker$id) {
      marker$id = query[['m']];
      log('marker id has changed to:', marker$id, '\n')

      if (!is.null(marker$id) && marker$id %in% paper$markers) {
        marker$order = read_marks_for_paper(paper$id) %>% filter(Marker == marker$id) %>% 
          filter(!is.na(PDFurl)) %>% arrange(Order) %>% select(StudentID, Question)

        # setup all the info above
        student$info = get_student_details(marker$order$StudentID[1],
                                           marker$order$Question[1],
                                           paper$id)
    
        current$question = 1
        current$question_name = marker$order$Question[1]
        current$marks = current$db_marks = get_marks(marker$order$StudentID[1], marker$order$Question[1], paper$id)
  
        layout$show_guide = TRUE
        layout$question = read_question_layout(marker$order$Question[1], paper$id)
        layout$comments = get_top_comments(marker$order$Question[1], paper$id)
        layout$all_comments <- get_all_comments_for_question(marker$order$Question[1], paper$id)
        layout$num_left = marker$order %>% slice(current$question:n()) %>% filter(Question == current$question_name) %>% nrow()

        update_ui()
      }
    }
  })

  # Current student
  observe({
    log("updating the pageHeader\n")
    header = paste(student$info$id, student$info$name, span(style="float:right;", layout$num_left))
    shinyjs::html("pageHeader", header)
  })
  output$pdfviewer <- renderText({
    if (length(student$info$pdf_url)) {
      return(paste('<iframe name="pdfviewer" style="width:100%; border:0; height:100%" src="', paste0(student$info$pdf_url, "#toolbar=0&navpanes=0"), '"></iframe>', sep = ""))
    } else {
      return('')
    }
  })

  # Current question
  output$question = renderText({
    paste("Question", current$question_name)
  })

  # Current guide
  output$markingguide = renderUI({
    if (length(layout$question$guide)) {
      html <- markdown::markdownToHTML(text = layout$question$guide, fragment.only = TRUE)
    } else {
      html <- ""
    }
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

  update_ui <- function() {
    log("update_ui: all comments being set\n")
    layout$all_comments
    choices = setdiff(layout$all_comments, layout$comments)
    updateSelectizeInput(session, "addcomment", selected = "", choices = choices, server = TRUE)

    log("update_ui: comments being set\n")
    comments = unique(c(layout$comments, current$marks$comments))
    if (is.null(comments)) {
      comments = character(0)
    }
    selected = current$marks$comments
    updateCheckboxGroupButtons(session, "comments", choices=comments,
                               selected=selected, status='light')

    log("update_ui: marks set to ", current$marks$mark, "\n")
    marks = layout$question$marks
    if (length(marks)) {
      by = layout$question$by
      selected = as.character(current$marks$mark)
      if (length(selected) && is.na(selected)) selected = NULL
      marks = seq(0, marks, by=by)
      choices = c("X", marks)
      updateRadioGroupButtons(session, "marks", choices = choices, selected=selected, status='marks')
    }

    log("update_ui: star set to ", current$marks$award, "\n")
    updateCheckboxGroupButtons(session, "star", selected = current$marks$award)
  }

  # Event observers
  observeEvent(input$addcomment, {
    if (nchar(input$addcomment)) { #  nchar check, because emptying the text field results in "" choice.
      log("input: add comment\n")
      # new comment - add to the database
      add_comment_to_question(current$question_name, input$addcomment, paper=paper$id)
      # and select that comment for the student
      current$marks$comments = c(input$comments, input$addcomment)
      set_comments(id = student$info$id, question = current$question_name, comments = current$marks$comments,
                   paper = paper$id)
      # refresh the comment list from the database
      layout$comments <- get_top_comments(current$question_name, paper=paper$id)
      layout$all_comments <- get_all_comments_for_question(current$question_name, paper=paper$id)
      
      update_ui()
    }
  })

  observeEvent(input$comments, {
    log("input: comments set\n")
    current$marks$comments = input$comments
    if (is.null(current$marks$comments)) current$marks$comments = character(0)
  }, ignoreNULL = FALSE)

  observeEvent(input$marks, {
    req(input$marks)
    log("input: marks set to ", input$marks, " (current=", current$marks$mark,")\n")
    current$marks$mark = input$marks
  })

  observeEvent(input$star, {
    log("input: star set to ", input$star, " (current=", current$marks$award,")\n")
    current$marks$award = input$star
    if (is.null(current$marks$award) || is.na(current$marks$award))
      current$marks$award = ""
  }, ignoreNULL = FALSE)

  # Next/Prev buttons
  observe({
#    shinyjs::toggleState("next", current$question < nrow(marker$order))
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

  if (0) {
  observe({
    different = any(unlist(map2(current$db_marks, current$marks, changed)))
    shinyjs::toggleState("reset", different)
    shinyjs::toggleState("save", different)
    shinyjs::toggleState("savenext", different)
  })
  }

  observeEvent(input$reset, {
    log("reset\n")
    current$marks = NA; # force reset
    current$marks = current$db_marks
    update_ui()
  })

  on_save <- function() {
    log("save\n")
    diff = unlist(map2(current$db_marks, current$marks, changed))
    log("diff=", diff, "\n")
    if (any(diff)) { # we don't care if there's only NAs
      log("Something has changed...\n")
      set_marks(id = student$info$id, question = current$question_name,
                mark = input$marks, award = input$star, comments = input$comments,
                paper = paper$id)
    }
    return(any(diff))
  }

  observeEvent(input$save, {
    if (on_save()) {
      current$db_marks = NA
      current$db_marks = current$marks
      update_ui()
    }
  })

  observeEvent(input$savenext, {
    log("save and next\n")
    on_save()
    if (current$question < nrow(marker$order)) {
      # increment the question and/or student
      current$question = current$question + 1
      current$question_name = marker$order$Question[current$question]
      student$info = get_student_details(marker$order$StudentID[current$question],
                                         marker$order$Question[current$question],
                                         paper = paper$id)

      # read in the new layout
      layout$question = read_question_layout(current$question_name, paper = paper$id)
      layout$comments = get_top_comments(current$question_name, paper = paper$id)
      layout$all_comments = get_all_comments_for_question(current$question_name, paper = paper$id)
      layout$num_left = marker$order %>% slice(current$question:n()) %>% filter(Question == current$question_name) %>% nrow()

      # and update marks
      current$marks = current$db_marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = current$db_marks = get_marks(student$info$id, current$question_name, paper = paper$id)

      update_ui()
    }
  })

  observeEvent(input$`next`, {
    log("Going NEXT\n")
    if (current$question < nrow(marker$order)) {
      # increment the question and/or student
      current$question = current$question + 1
      current$question_name = marker$order$Question[current$question]
      student$info = get_student_details(marker$order$StudentID[current$question],
                                         marker$order$Question[current$question],
                                         paper = paper$id)

      # read in the new layout
      layout$question = read_question_layout(current$question_name, paper = paper$id)
      layout$comments = get_top_comments(current$question_name, paper = paper$id)
      layout$all_comments = get_all_comments_for_question(current$question_name, paper = paper$id)
      layout$num_left = marker$order %>% slice(current$question:n()) %>% filter(Question == current$question_name) %>% nrow()

      # and update marks
      current$marks = current$db_marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = current$db_marks = get_marks(student$info$id, current$question_name, paper = paper$id)

      update_ui()
    }
  })
  observeEvent(input$next_unmarked, {
    log("Going NEXT unmarked\n")

    done <- FALSE
    while (!done && current$question < nrow(marker$order)) {
      # increment the question and/or student
      current$question = current$question + 1
      current$question_name = marker$order$Question[current$question]
      student$info = get_student_details(marker$order$StudentID[current$question],
                                         marker$order$Question[current$question],
                                         paper = paper$id)

      # read in the new layout
      layout$question = read_question_layout(current$question_name, paper = paper$id)
      layout$comments = get_top_comments(current$question_name, paper = paper$id)
      layout$all_comments = get_all_comments_for_question(current$question_name, paper = paper$id)
      layout$num_left = marker$order %>% slice(current$question:n()) %>% filter(Question == current$question_name) %>% nrow()
      
      # and update marks
      current$marks = current$db_marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = current$db_marks = get_marks(student$info$id, current$question_name, paper = paper$id)
      if (!length(current$marks$mark) || is.na(current$marks$mark))
        done = TRUE
    }
    update_ui()
  })
  observeEvent(input$prev, {
    log("Going PREV\n")
    if (current$question > 1) {

      # decrement the question
      current$question = current$question - 1
      current$question_name = marker$order$Question[current$question]
      student$info = get_student_details(marker$order$StudentID[current$question],
                                         marker$order$Question[current$question],
                                         paper = paper$id)

      # read in the new layout
      layout$question = read_question_layout(current$question_name, paper = paper$id)
      layout$comments = get_top_comments(current$question_name, paper = paper$id)
      layout$all_comments = get_all_comments_for_question(current$question_name, paper = paper$id)
      layout$num_left = marker$order %>% slice(current$question:n()) %>% filter(Question == current$question_name) %>% nrow()

      # and update marks
      current$marks = current$db_marks = NA; # force it to flag as update - apparently it can't otherwise detect the changes in the list...
      current$marks = current$db_marks = get_marks(student$info$id, current$question_name, paper = paper$id)

      update_ui()
    }
  })

})
