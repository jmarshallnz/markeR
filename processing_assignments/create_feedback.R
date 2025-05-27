# load in the database reading stuff
source(here::here("markeR/marks_csv.R"))
source(here::here("markeR/questions_csv.R"))
library(fs)

collapse <- function(comments) {
  comments <- unlist(lapply(comments, function(x) { paste(x, collapse="\n") }))
  comments[is.na(comments)] <- ""
  comments
}

fill_empty_comments <- function(comments, mark, total) {
  good_job <- sample(c("Nice work!", "Good work!", "Great!", "Nice job!"),
                     size=length(comments), replace=TRUE)
  case_when(comments == "" & mark == total ~ good_job,
            comments == "" & mark == 0 ~ "See solutions",
            TRUE ~ comments)
}

create_mark <- function(Mark, Total, Award) {
  if (sum(Total) > 0) {
    mark_fraction = paste0(paste(Mark, Total, sep="/"), " ")
  } else {
    mark_fraction = ""
  }
  ifelse(!is.na(Award) & Award == "star", paste0(mark_fraction, "\\includegraphics[width=0.5cm]{star.png}"), mark_fraction)
}

create_feedback <- function(marks, out_dir, rmd_dir, clean=TRUE) {
  student = marks %>% filter(!is.na(PDFurl)) %>% select(StudentID, StudentName, PDFurl) %>% slice_head(n=1) %>% as.list()
  questions = marks %>% select(Exercise, Question, Mark, Total, Award, Comments) %>%
    mutate(Comments = collapse(Comments)) %>%
    mutate(Comments = fill_empty_comments(Comments, Mark, Total)) %>%
    split(.$Exercise)
  # add total row
  tab_data <- lapply(questions, 
                     function(x) {
                       if (sum(x$Total) > 0) {
                         tab <- bind_rows(x, tibble(Question = "Total", Mark = sum(x$Mark), Total = sum(x$Total), Comments = ""))
                       } else {
                         tab <- x
                       }
                       tab %>% mutate(Mark = create_mark(Mark, Total, Award)) %>% select(Question, Mark, Comments)
                     })
  print(tab_data)
  total = marks %>% select(Mark, Total) %>% summarise(Mark = sum(Mark), Total = sum(Total)) %>% as.list()
  # create environment
  student.envir = new.env()
  student.envir$name = student$StudentName
  student.envir$id   = as.character(student$StudentID)
  student.envir$overall = total
  student.envir$marks = tab_data
  # render markdown
  # do safely to feedback folder then move
  student_outdir <- file.path(out_dir, path_dir(student$PDFurl))
  temp_dir <- file.path(out_dir, "temp")
  rmarkdown::render(file.path(rmd_dir, "feedback.Rmd"), output_format='pdf_document', output_file = "feedback.pdf",
                    output_dir = temp_dir, envir=student.envir, clean=clean)
  fs::dir_create(student_outdir)
  fs::file_move(file.path(temp_dir, "feedback.pdf"), file.path(student_outdir, "feedback.pdf"))
  fs::dir_delete(temp_dir)
}

feedback <- function(rmd_dir, out_dir, marks_dir) {
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
  
  complete_marks %>% split(.$StudentID) %>% lapply(create_feedback, out_dir = out_dir, rmd_dir = rmd_dir)
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


# dump marks out to spreadsheet
get_feedback_ids <- function(file) {
  feedback_files <- list.files(file)
  ids <- gsub("([0-9]+) .*", "\\1", feedback_files)
  ids
}
student_id <- function(student_id) {
  if (is.numeric(student_id)) {
    sprintf("%08i", student_id)
  } else {
    student_id
  }
}

# Merge spreadsheet for 'upload marks' functionality
generate_upload_marks <- function(stream_csv_file, marker_csv_file, out_dir, max_total = NULL) {
  stream <- read_csv(stream_csv_file)
  marker <- read_csv(marker_csv_file) %>%
    group_by(StudentID, StudentName) %>%
    mutate(Mark = as.numeric(Mark)) %>%
    summarise(Mark = sum(Mark)) %>%
    filter(!is.na(Mark)) %>% as.data.frame() %>%
    mutate(StudentID = student_id(StudentID)) %>%
    ungroup() |>
    mutate(StudentID = as.character(StudentID))
  if (!is.null(max_total)) {
    # max_total is given, grade students as min(max_total, Mark).
    marker <- marker |>
      mutate(Mark = pmin(max_total, Mark))
  }
  stream %>% mutate(StudentID = as.character(`ID number`)) %>%
    left_join(marker, by=c("StudentID")) %>%
    mutate(Grade = if_else(is.na(Grade), Mark, as.numeric(Grade))) %>%
    filter(!is.na(Grade)) %>%
    select(one_of(names(stream))) %>%
    write_csv(file.path(out_dir, "upload_grades.csv"), na="")
}

if (0) {
  if (0) {
    read_csv('feedback/2024_233214_week11/marks.csv') |>
      mutate(Question = as.numeric(paste0('1.', Question))) |>
      write_csv('feedback/2024_233214_week11/marks.csv')
    read_csv('feedback/2024_233214_week11/questions.csv') |>
      mutate(Question = as.numeric(paste0('1.', Question))) |>
      write_csv('feedback/2024_233214_week11/questions.csv')
  }
  year <- 2025
  paper <- "161324_A1"
  base_path <- file.path("processing_assignments", year, paper)
  year_paper <- paste(year, paper, sep="_")
  feedback(rmd_dir = base_path, out_dir=file.path(base_path, "feedback"),
           marks_dir=file.path(base_path, "data", year_paper))
  zip(file.path(base_path, "feedback", "upload.zip"),
      file.path(base_path, "feedback", year_paper))

  # find the stream csv
  stream_csv_path <- list.files(base_path, "Grades.*.csv", full.names = TRUE)
  generate_upload_marks(stream_csv_file=stream_csv_path,
                        marker_csv_file=file.path(base_path, "data", year_paper, "marks.csv"),
                        out_dir=file.path(base_path, "feedback"),
                        max_total=NULL)
}

if (0) {
  all_marks <- read_marks(here::here("feedback/233214_2023_week12/marks.csv")) %>%
    group_by(StudentID, StudentName) %>%
    mutate(Mark = as.numeric(Mark)) %>%
    summarise(Mark = sum(Mark)) %>%
    filter(!is.na(Mark)) %>% as.data.frame()

#    mutate(Mark = Mark + Stars) %>%
#    ggplot(aes(x=Mark)) + geom_histogram()
    write_csv("feedback/233214_2023_week12/233214_2022_week12.csv")
}
