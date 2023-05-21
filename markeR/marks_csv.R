## CSV based database for questions
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

logfile <- "log.txt"
log <- function(...) { cat(..., file=logfile, append=TRUE) }
log_list <- function(x) {
  for (i in seq_len(length(x)))
    log(x[[i]], "\n")
}

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
read_marks <- function(filename = NULL) {
  if (is.null(filename))
    filename = "marks.csv"
  if (file.exists(filename)) {
    read_csv(filename, col_types = cols()) %>% unflatten_listcol(Comments)
  } else if (file.exists("pre_marks.csv")) {
    read_csv("pre_marks.csv", col_types = cols()) %>% unflatten_listcol(Comments)
  } else {
    x = tibble(StudentID = c("123", "456"), StudentName = c("Andrew", "Brenda"), PDFurl = paste0(StudentName, ".pdf"))
    y = tibble(Question = c("1 (a)", "1 (b)", "1 (c)"), Marker = "Doug", Mark = NA, Award=NA, Order = NA, Comments = list(NULL))
    crossing(x,y)
  }
}

write_marks <- function(marks_db, filename = NULL) {
  if (is.null(filename))
    filename = "marks.csv"
  write_csv(marks_db %>% flatten_listcol(Comments), filename)
}

marks_file <- function(paper) {
  if (!is.null(paper)) {
    file <- file.path('data', paper, 'marks.csv')
  } else {
    file <- "marks.csv"
  }
  file
}

read_marks_for_paper <- function(paper) {
  read_marks(marks_file(paper))
}

write_marks_for_paper <- function(marks, paper) {
  write_marks(marks, marks_file(paper))
}

get_student_details <- function(id, question, paper) {
  read_marks_for_paper(paper) %>%
    filter(StudentID == id, Question == question) %>%
    select(id = StudentID, name = StudentName, pdf_url = PDFurl) %>% unique() %>% as.list()
}

get_marks <- function(id, question, paper) {
  log("get_marks for student ", id, " question ", question, "\n")
  marks = read_marks_for_paper(paper) %>%
    filter(StudentID == id, Question == question) %>%
    select(mark = Mark, award = Award, comments = Comments) %>%
    as.list
  marks$comments = unlist(marks$comments)
  marks$comments = setdiff(marks$comments, "")
  if (is.null(marks$comments)) marks$comments = character(0) # hmm, is this best??
  marks$mark = as.numeric(marks$mark)
  marks$award = as.character(marks$award)
  if (is.null(marks$award) || is.na(marks$award)) marks$award = ""
  log("get_marks: Marks = ", marks$mark, "\n")
  log("get_marks: Award = ", marks$award, "\n")
  log("get_marks: Comments = ", "\n")
  log_list(marks$comments)
  log("get_marks: length(marks)=", length(marks), "\n")
  marks
}

get_top_comments <- function(question, paper, n=3) {
  log("Calling get_top_comments()\n")
  comments = read_marks_for_paper(paper) %>%
    filter(Question == question) %>%
    pull(Comments) %>% unlist()
  if (length(comments) > 0) {
    comments = data.frame(Comments = comments, stringsAsFactors = FALSE) %>% filter(Comments != "") %>% count(Comments) %>% slice_max(5, n, with_ties=FALSE) %>%
      pull(Comments)
  }
  log("found", length(comments), "comments\n")
  comments
}

set_marks <- function(id, question, mark, award, comments, paper) {
  marks_db = read_marks_for_paper(paper)
  log("set_marks for student ", id, " question ", question, "\n")
  row = which(marks_db$StudentID == id & marks_db$Question == question)
  log("set_marks row is ", row, "\n")
  log("set_marks: mark=", mark, "\n")
  print(str(mark))
  log("set_marks: award=", award, "\n")
  if (is.null(award)) award = ""
  if (is.null(comments)) comments = "" # empty
  log("set_marks: comments=")
  log_list(list(comments))
  marks_db$Mark[row] = mark
  marks_db$Award[row] = award
  marks_db$Comments[row] = list(comments)
  write_marks_for_paper(marks_db, paper)
}

set_comments <- function(id, question, comments, paper) {
  marks_db = read_marks_for_paper(paper)
  log("set_comments for student ", id, " question ", question, "\n")
  row = which(marks_db$StudentID == id & marks_db$Question == question)
  log("set_comments row is ", row, "\n")
  if (is.null(comments)) comments = "" # empty
  marks_db$Comments[row] = list(comments)
  write_marks_for_paper(marks_db, paper)
}
