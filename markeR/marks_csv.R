## CSV based database for questions
library(readr)
library(dplyr)
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
    read_csv("marks.csv") %>% unflatten_listcol(Comments)
  } else {
    x = tibble(StudentID = c("123", "456", "789"), StudentName = c("Alice", "Bob", "Cara"), PDFurl = paste0(StudentName, ".pdf"))
    y = tibble(Question = c("1 (a)", "1 (b)", "1 (c)"), Marker = "Doug", Mark = NA, Award=NA, Order = NA, Comments = list(NULL))
    crossing(x,y)
  }
}

get_student_details <- function(id) {
  read_marks() %>% filter(StudentID == id) %>% select(id = StudentID, name = StudentName, pdf_url = PDFurl) %>% unique() %>% as.list()
}

get_marks <- function(id, question) {
  cat("Calling get_marks()")
  print(id)
  print(question)
  read_marks() %>% filter(StudentID == id, Question == question) %>%
    select(mark = Mark, award = Award, comments = Comments) %>%
    as.list()
}
