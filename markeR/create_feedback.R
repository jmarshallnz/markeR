# load in the database reading stuff
source(here::here("markeR/marks_csv.R"))
source(here::here("markeR/questions_csv.R"))
library(fs)

collapse <- function(comments) {
  comments <- unlist(lapply(comments, function(x) { paste(x, collapse="\n") }))
  comments[is.na(comments)] <- ""
  comments
}

create_mark <- function(Mark, Total, Award) {
  mark_fraction = paste(Mark, Total, sep="/")
  ifelse(!is.na(Award) & Award == "star", paste(mark_fraction, "\\includegraphics[width=0.5cm]{star.png}"), mark_fraction)
}

create_feedback <- function(marks, out_dir, clean=TRUE) {
  student = marks %>% select(StudentID, StudentName, PDFurl) %>% unique() %>% as.list()
  questions = marks %>% select(Exercise, Question, Mark, Total, Award, Comments) %>% mutate(Comments = collapse(Comments)) %>%
    split(.$Exercise)
  # add total row
  tab_data <- lapply(questions, 
                     function(x) {
                       bind_rows(x, tibble(Question = "Total", Mark = sum(x$Mark), Total = sum(x$Total), Comments = "")) %>%
                         mutate(Mark = create_mark(Mark, Total, Award)) %>% select(Question, Mark, Comments)
                     })
  total = marks %>% select(Mark, Total) %>% summarise(Mark = sum(Mark), Total = sum(Total)) %>% as.list()
  # create environment
  student.envir = new.env()
  student.envir$name = student$StudentName
  student.envir$id   = student$StudentID
  student.envir$overall = total
  student.envir$marks = tab_data
  # render markdown
  student_outdir <- file.path(out_dir, path_dir(student$PDFurl))
  rmarkdown::render("feedback.Rmd", output_format='pdf_document', output_file = "feedback.pdf",
                    output_dir = student_outdir, envir=student.envir, clean=clean)
}

feedback <- function(our_dir) {
  # read in the marks
  marks = read_marks() %>% left_join(read_questions() %>% select(-Comments, Total=Marks)) %>%
    tidyr::extract(Question, into=c("Exercise", "Question"), regex="([0-9]+).([0-9]+)", remove=TRUE, convert=TRUE) %>%
    mutate(Question = as.character(Question)) %>%
    # HACK
    mutate(Mark = as.numeric(Mark))
  # HACK
  marks %>% split(.$StudentID) %>% lapply(create_feedback, out_dir = our_dir)
}

# dump marks out to spreadsheet
#read_marks(here::here("markeR/marks.csv")) %>% group_by(StudentID, StudentName) %>% mutate(Mark = as.numeric(Mark)) %>% summarise(Mark = sum(Mark)) %>% write_csv("227215_ass1.csv")

