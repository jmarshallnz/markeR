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
read_questions <- function(filename=NULL) {
  if (is.null(filename))
    filename = "questions.csv"
  if (file.exists(filename)) {
    read_csv(filename, col_types = cols()) %>% unflatten_listcol(Comments)
  } else {
    warning("Default questions in use")
    tibble(Question = c("1 (a)", "1 (b)", "1 (c)"), Marks = c(3,5,2), By=c(1,1,0.5), Guide="$$\\begin{aligned}\\bar{x} \\pm 2 \\frac{s}{\\sqrt{n}} &= 2.3 \\pm 2 \\frac{1.2}{\\sqrt{53}}\\\\\\\\ & = 2.3 \\pm 0.33\\\\\\\\ & = (1.97, 2.63)\\end{aligned}$$\n - 1 mark for calculation\n - 1 mark for answer\n - 1 mark for interpretation", Comments = list(c("Calculation error", "Use a prediction interval instead of a confidence interval", "Incorrect standard error", "Interpretation is for individuals")))
  }
}

read_question_layout <- function(question) {
  read_questions() %>%
    filter(Question == question) %>%
    select(marks=Marks, guide=Guide, by=By) %>%
    as.list()
}

# TODO: This isn't actually needed - just being used to test Next/Prev buttons...
max_question <- function() {
  read_questions() %>% pull(Question) %>% max()
}

get_all_comments_for_question <- function(question) {
  read_questions() %>%
    filter(Question == question) %>%
    pull(Comments) %>%
    unlist()
}

get_all_comments <- function() {
  read_questions() %>% pull(Comments) %>%
    unlist() %>%
    unique()
}

add_comment_to_question <- function(question, comment) {
  question_db = read_questions()
  comments = question_db %>% filter(Question == question) %>% pull(Comments) %>% unlist() %>% c(comment)
  question_db$Comments[question_db$Question == question] <- list(comments)
  write_csv(question_db %>% flatten_listcol(Comments), "questions.csv")
}
