# read in the markdown document, break it up into segments and comments and dump into the .csv file
library(readr)
library(stringr)
library(tibble)
source('markeR/questions_csv.R')

paper_name = "161324_A2"
year = 2025
base_path = file.path("processing_assignments", year, paper_name)
assignment = read_lines(file.path(base_path, 'solutions.Rmd'))
out_path = file.path(base_path, "data", paste(year, paper_name, sep="_"))
fs::dir_create(out_path)

# break up into questions
question_start = which(grepl("## Question (.*)", assignment))
question_end = c(question_start[-1]-1, length(assignment))
question_names = sub("## Question (.*)", "\\1", assignment)[question_start]

questions = list()
ans = lapply(seq_along(question_start), function(x) { questions[[x]] = assignment[(question_start[x]+1):question_end[x]] })
names(ans) = question_names

# find any comments
pull_comments <- function(x) {
  matches = gregexpr("\\[\\[(.*?)\\]\\]", x)
  get_comment <- function(match, text) {
    if (length(match) == 1 && match == -1) {
      NULL
    } else {
      substring(text, match+2, match+attr(match, 'match.length')-3)
    }
  }
  comments = lapply(seq_along(matches), function(i) { get_comment(matches[[i]], x[i]) })
  unlist(comments[lengths(comments) > 0])
}

pull_guide <- function(x) {
  matches = gregexpr("\\[\\[(.*?)\\]\\]", x)
  get_guide <- function(match, text) {
    if (length(match) == 1 && match == -1) {
      text
    } else {
      ""
    }
  }
  guides = lapply(seq_along(matches), function(i) { get_guide(matches[[i]], x[i]) })
  unlist(str_split(str_trim(paste(unlist(guides), collapse='\n')), '\n'))
}

comments = lapply(ans, pull_comments)
guide = lapply(ans, pull_guide)

# find number of marks for each question in square brackets
pull_marks <- function(x) {
  matches = gregexpr("\\[([0-9 ]+?)\\]", x)
  get_num <- function(match, text) {
    readr::parse_number(substring(text, match, match+attr(match, 'match.length')-1))
  }
  marks = lapply(seq_along(matches), function(i) { get_num(matches[[i]], x[i]) })
  sum(unlist(marks), na.rm=TRUE)
}

marks = unlist(lapply(guide, pull_marks))

# TODO: find by (I guess it'd be 0.5 if there are any 0.5's otherwise 1?)
by = 1

# compact guide down
guide = unlist(lapply(guide, function(x) { paste(x, collapse="\n")}))

if (0) {
  question_names = 1
  marks = 5
  by = 1
  guide = "Something goes here!"
  comments = list(NULL) 
}
# now create the question database
question_db = tibble(Question = question_names, Marks = marks, By=by, Guide=guide, Comments = comments)

# create data folder if not there
fs::dir_create(base_path, "data", paste0())
# now read in the original one and merge with this one
if (0) {
  foo <- read_csv(file.path(out_path, "questions.csv")) %>% unflatten_listcol(Comments)
  question_db$Comments <- foo$Comments
}
question_db
write_csv(question_db %>% flatten_listcol(Comments), file.path(out_path, "questions.csv"))
