# Create the marks database.

# read in the students from a stream download
library(tidyverse)
library(readxl)
library(fs)
library(httr)

paper_name = "161324_A2"
year = 2025
base_path = file.path("processing_assignments", year, paper_name)
paper_folder = paste(year, paper_name, sep="_")
out_path = file.path(base_path, "data", paper_folder)
fs::dir_create(out_path)

# find the stream csv
csv_path <- list.files(base_path, "Grades.*.csv", full.names = TRUE)

stream <- read_csv(csv_path)

all = stream %>% mutate(StudentName = `Full name`) %>% select(StudentID = `ID number`, StudentName, Identifier) %>%
  extract(Identifier, into='Identifier', regex = "([[:digit:]]+)")

# TODO: Append the PDFs
path_to_pdfs = file.path(base_path, "out")
path_for_pdfs = paste(year, paper_name, sep="_")

student_pdfs = list.files(path = path_to_pdfs, pattern = ".pdf|.htm", recursive = TRUE)
student_ident  = gsub(".*?_([0-9]+)_assign.*", "\\1", student_pdfs)

copy_pdfs <- function(path_to_pdfs, pdfs, base_path, path_for_pdfs) {
  # create the subfolders we need
  paths <- fs::path_dir(pdfs)
  fs::dir_create(file.path(base_path, "www", path_for_pdfs, paths))
  fs::file_copy(file.path(path_to_pdfs, pdfs), file.path(base_path, "www", path_for_pdfs, pdfs), overwrite = TRUE)
  file.path(path_for_pdfs, pdfs)
}

pdf_urls = copy_pdfs(path_to_pdfs, student_pdfs, base_path, path_for_pdfs)
student_folders = data.frame(PDFurl = pdf_urls, Identifier = student_ident)

# TODO: Update this so it reads in the marks sheet (if present) for the mark/award/comment stuff...
mark_data = all %>% left_join(student_folders) %>%
  dplyr::select(StudentID, StudentName, PDFurl)

mark_data %>% filter(is.na(StudentID))

if (0) {
  # Try a fuzzy join
  library(fuzzyjoin)
  library(stringdist)
  
  mark_data <- all %>% stringdist_left_join(student_folders, max_dist = 1) %>%
    select(StudentID, StudentName = StudentName.x, PDFurl)
}

# Check for no match on student ID...
mark_data %>% filter(is.na(PDFurl))
mark_data %>% nrow()

# read in the questions
questions <- read_csv(file.path(out_path, "questions.csv")) %>%
  dplyr::select(Question) %>%
  separate(Question, into=c("Exercise", "Subquestion"), remove=FALSE,
           convert=TRUE) %>%
  mutate(Marker = LETTERS[Exercise]) %>%
  select(Question, Marker)

if (0) {
  # any specific processing goes here...
  # 324 A2
  questions <- questions |> mutate(Marker = if_else(Question %in% c(1.1, 2.1), "C", Marker))
}
marks <- crossing(mark_data, questions) %>%
  mutate(Mark = NA, Award = NA, Order = NA, Comments = list(NULL))

# Generate the order we want
final_marks = marks %>% mutate(Random = runif(n())) %>% group_by(Marker) %>%
  arrange(Question, Random) %>% mutate(Order = row_number()) %>%
  dplyr::select(-Random) %>% ungroup()

replace_if_na <- function(x, y) {
  case_when(is.na(x) & !is.na(y) ~ y,
            x != y ~ y,
            TRUE ~ x)
}
# TODO: Here is we're we'd read in our existing database and join things up
if (0) {
  source('markeR/marks_csv.R')
  
  # Stuff for updating marks with a new student submission:
  library(tidyverse)
  marks <- read_marks(file.path(out_path, "marks.csv"))
  # Check we have new ones to add:
  marks |> anti_join(final_marks |> select(StudentID, StudentName, PDFurl))
  # OK, now go and add in our PDFs from final_marks
  done <- marks |>
    left_join(final_marks |> dplyr::select(StudentID, PDFurl2=PDFurl) |> unique()) |>
    mutate(PDFurl = replace_if_na(PDFurl, PDFurl2)) |>
    dplyr::select(-PDFurl2)

  # Check it worked
  done |> anti_join(marks)
  
  # Write it back out
  done |> write_marks(file.path(out_path, "marks.csv"))
}

write_csv(final_marks %>% flatten_listcol(Comments), file.path(out_path, "marks.csv"))

# generate a new hash
hash = digest::sha1(paste(paper_folder, now()))
write_lines(hash, file=file.path(out_path, "hash"))
url = parse_url("https://shiny.massey.ac.nz/jcmarsha/markeR/")
url$query = list(p = paper_folder,
                  m = "A",
                  h = hash)
write_lines(build_url(url), file=file.path(base_path, "url"))
