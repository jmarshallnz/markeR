## CSV based database for questions
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

flatten_listcol <- function(db, list_col, list_name) {
  lc = enquo(list_col)
  flatten <- function(x) { paste0(capture.output(dput(x)), collapse="") }
  db %>% mutate(!!quo_name(lc) := map_chr(!!lc, flatten))
}

unflatten_listcol <- function(db, list_col) {
  lc = enquo(list_col)
  mutate(db, !!quo_name(lc) := map(!!lc, . %>% (rlang::parse_expr)() %>% eval()))
}
# Testing shit - yay!
#packed = flatten_listcol(question_db, Comments)
#unpacked = unflatten_listcol(packed, Comments)
#all.equal(unpacked$Comments, question_db$Comments)

# first time creation
read_marks <- function() {
  if (file.exists("marks.csv")) {
    read_csv("marks.csv", col_types = cols()) %>% unflatten_listcol(Comments)
  } else if (file.exists("pre_marks.csv")) {
    read_csv("pre_marks.csv", col_types = cols()) %>% unflatten_listcol(Comments)
  } else {
    x = tibble(StudentID = c("123", "456"), StudentName = c("Andrew", "Brenda"), PDFurl = paste0(StudentName, ".pdf"))
    y = tibble(Question = c("1 (a)", "1 (b)", "1 (c)"), Marker = "Doug", Mark = NA, Award=NA, Order = NA, Comments = list(NULL))
    crossing(x,y)
  }
}

get_student_details <- function(id) {
  read_marks() %>% filter(StudentID == id) %>% select(id = StudentID, name = StudentName, pdf_url = PDFurl) %>% unique() %>% as.list()
}

get_marks <- function(id, question) {
  cat("Calling get_marks()")
  marks = read_marks() %>% filter(StudentID == id, Question == question) %>%
    select(mark = Mark, award = Award, comments = Comments) %>%
    as.list
  marks$comments = unlist(marks$comments)
  if (is.null(marks$comments)) marks$comments = character(0) # hmm, is this best??
  marks$mark = as.numeric(marks$mark)
  marks$award = as.character(marks$award)
  if (is.null(marks$award) || is.na(marks$award)) marks$award = ""
  cat("Marks = ", marks$mark, "\n")
  cat("Award = ", marks$award, "\n")
  cat("Comments = ")
  print(marks$comments)
  cat("\n")
  cat("length(marks)=", length(marks), "\n")
  marks
}

set_marks <- function(id, question, mark, award, comments) {
  marks_db = read_marks()
  row = which(marks_db$StudentID == id & marks_db$Question == question)
  cat("Setting: mark=", mark, "\n")
  print(str(mark))
  cat("Setting: award=", award, "\n")
  if (is.null(award)) award = ""
  print(str(award))
  if (is.null(comments)) comments = "" # empty
  cat("Setting: comments=")
  print(comments)
  cat("\n")
  cat("Setting: list(comments)=")
  print(list(comments))
  cat("\n")
  marks_db$Mark[row] = mark
  marks_db$Award[row] = award
  marks_db$Comments[row] = list(comments)
  write_csv(marks_db %>% flatten_listcol(Comments), "marks.csv")
}
