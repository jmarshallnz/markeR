library(tidyverse)
library(rmarkdown)
library(fs)

# list files
paper_path = "processing_assignments/2025/297101_homework2"

# find the zip file
zip_file = list.files(paper_path, "*.zip", full.names = TRUE)

# extract it
in_path = file.path(paper_path, "in")
unzip(zip_file, exdir=in_path)

# create output path
out_path = file.path(paper_path, "out")
fs::dir_create(out_path)

# crunch down any folder structure past the first one
dir_names <- list.files(in_path)

undir_files_completely <- function(path, in_path, out_path) {
  all_files <- file.path(path, list.files(path=file.path(in_path, path), recursive=TRUE))
  out_files <- str_replace_all(all_files, "/", "_")
  file_cp <- data.frame(from = file.path(in_path, all_files), to = file.path(out_path, out_files)) |>
    filter(from != to)
  walk2(file_cp$from, file_cp$to, fs::file_copy)
}

walk(dir_names, undir_files_completely, in_path=in_path, out_path=out_path)

students = list.files(path=out_path, pattern=".", ignore.case=TRUE, recursive = TRUE)

# find the stream csv
csv_path <- list.files(paper_path, "Grades.*.csv", full.names = TRUE)

stream <- read_csv(csv_path) |> rename(StudentName = `Full name`) |>
#  select(StudentID = `ID number`, StudentName, Identifier, Email = `Email address`) |>
  extract(Identifier, into='Identifier', regex = "([[:digit:]]+)")

student_work = data.frame(file_url = students) |>
  mutate(Identifier = gsub(".*?_([0-9]+)_assign.*", "\\1", file_url))

mark_data = stream %>% left_join(student_work) %>%
  dplyr::select(StudentID, StudentName, file_url) |>
  mutate(Mark = NA)

write_csv(mark_data |> arrange(file_url), file.path(paper_path, "marks.csv"))

# MARK IN EXCEL

marks <- read_csv(file.path(paper_path, "marks.csv"), col_types = 'c')

stream <- read_csv(csv_path) |> select(-Grade, -`Feedback comments`) |>
  left_join(marks |> select(StudentID, Grade = Mark, `Feedback comments` = Comment), by=join_by(`ID number` == StudentID)) |>
  mutate(Grade = as.numeric(Grade))

write_csv(stream, file.path(paper_path, "upload_grades.csv"), na = '')
