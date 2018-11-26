# load in the database reading stuff
source("marks_csv.R")
source("questions_csv.R")

collapse <- function(comments) {
  comments <- unlist(lapply(comments, function(x) { paste(x, collapse="\n") }))
  comments[is.na(comments)] <- ""
  comments
}

create_mark <- function(Mark, Total, Award) {
  mark_fraction = paste(Mark, Total, sep="/")
  ifelse(!is.na(Award) & Award == "star", paste(mark_fraction, "\\includegraphics[width=0.5cm]{star.png}"), mark_fraction)
}

create_feedback <- function(marks) {
  student = marks %>% select(StudentID, StudentName) %>% unique() %>% as.list()
  questions = marks %>% select(Question, Mark, Total, Award, Comments) %>% mutate(Comments = collapse(Comments))
  # add total row
  tab_data <- bind_rows(questions, tibble(Question = "Total", Mark = sum(questions$Mark), Total = sum(questions$Total), Comments = "")) %>%
    mutate(Mark = create_mark(Mark, Total, Award)) %>% select(Question, Mark, Comments)
  # create environment
  student.envir = new.env()
  student.envir$name = student$StudentName
  student.envir$id   = student$StudentID
  student.envir$marks = tab_data
  # render markdown
  rmarkdown::render("feedback.Rmd", output_format='pdf_document', output_file = paste0(student$StudentName, ".pdf"),
                    output_dir = "../test_assignment/feedback", envir=student.envir)
}

feedback <- function() {
  # read in the marks
  marks = read_marks() %>% left_join(read_questions() %>% select(-Comments, Total=Marks))
  marks %>% split(marks$StudentID) %>% lapply(create_feedback)
}
