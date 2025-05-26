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
  student = marks %>% filter(!is.na(PDFurl)) %>% select(StudentID, StudentName, PDFurl) %>% slice_head(n=1) %>% as.list()
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
  student.envir$id   = as.character(student$StudentID)
  student.envir$overall = total
  student.envir$marks = tab_data
  # render markdown
  student_outdir <- file.path(out_dir, path_dir(student$PDFurl))
  rmarkdown::render(file.path(out_dir, "feedback.Rmd"), output_format='pdf_document', output_file = "feedback.pdf",
                    output_dir = student_outdir, envir=student.envir, clean=clean)
}

feedback <- function(our_dir, marks_dir) {
  # read in the marks
  marks = read_marks(file.path(marks_dir, "marks.csv")) %>% left_join(read_questions(file.path(marks_dir, "questions.csv")) %>% select(-Comments, Total=Marks)) %>%
    tidyr::extract(Question, into=c("Exercise", "Question"), regex="([0-9]+).([0-9]+)", remove=TRUE, convert=TRUE) %>%
    mutate(Question = as.character(Question)) %>%
    # HACK
    mutate(Mark = as.numeric(Mark))
    # HACK
  
  # filter out students that didn't submit
  marks <- marks %>% filter(!is.na(PDFurl))
  
  # Expand out the marks in the case the student hasn't submitted something
  complete_marks <- marks %>%
    complete(StudentID, nesting(Exercise,Question,Total), 
             fill = list(Mark = 0, Comments = list("Not submitted")))
  
  complete_marks %>% split(.$StudentID) %>% lapply(create_feedback, out_dir = our_dir)
}

if (0) {
  # compute bonus marks
  marks <- read_marks(here::here("feedback/193301_2020/193301_marks.csv"))
  bonus_marks <- marks %>%
    group_by(StudentID, StudentName) %>%
    summarise(Bonus = sum(Award == 'star', na.rm=TRUE)) %>% ungroup()
  # add another question on
  bonus_question <- marks %>%
    filter(Question == 1.1) %>%
    left_join(bonus_marks %>% select(StudentID, Bonus)) %>%
    mutate(Question = 3.1, Mark = Bonus, Comments = list(""), Award = NA) %>%
    select(-Bonus)
  write_marks(bind_rows(marks, bonus_question), here::here("feedback/193301_2020/marks.csv"))
  questions <- read_questions(here::here("feedback/193301_2020/193301_questions.csv"))
  bind_rows(questions,
            tibble(Question = 3.1, Marks = 5, By = 1, Guide = "", Comments = list(""))) %>%
    flatten_listcol(Comments) %>%
    write_csv(here::here("feedback/193301_2020/questions.csv"))
}
if (0) {
  old_dir <- setwd("feedback")
  feedback("227212_2021", "227212_2021")
  setwd(old_dir)
  old_dir <- setwd("feedback/227212_2021/227212_2021")
  zip("upload.zip", ".")
  file_move("upload.zip", "..")
  setwd(old_dir)
}
# dump marks out to spreadsheet
if (0) {
  read_marks(here::here("feedback/227212_2021/marks.csv")) %>%
    group_by(StudentID, StudentName) %>%
    mutate(Mark = as.numeric(Mark)) %>%
    summarise(Mark = sum(Mark)) %>%
    filter(!is.na(Mark)) %>%
#    mutate(Mark = Mark + Stars) %>%
#    ggplot(aes(x=Mark)) + geom_histogram()
    write_csv("feedback/227212_2021/227212_2021.csv")
}
