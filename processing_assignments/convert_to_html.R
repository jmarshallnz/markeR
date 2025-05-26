library(tidyverse)
library(rmarkdown)

# list files
paper_path = "processing_assignments/2025/161324_A2"

# find the zip file
zip_file = list.files(paper_path, "*.zip", full.names = TRUE)

# extract it
in_path = file.path(paper_path, "in")
unzip(zip_file, exdir=in_path)

# crunch down any folder structure past the first one
dir_names <- list.files(in_path)

undir_files <- function(path, in_path) {
  all_files <- list.files(path=file.path(in_path, path), recursive=TRUE)
  out_files <- str_replace_all(all_files, "/", "_")
  file_cp <- data.frame(from = file.path(in_path, path, all_files), to = file.path(in_path, path, out_files)) |>
    filter(from != to)
  walk2(file_cp$from, file_cp$to, fs::file_copy)
  # remove any folders
  fs::dir_delete(list.dirs(file.path(in_path, path), recursive = FALSE))
}

walk(dir_names, undir_files, in_path=in_path)

out_path  = file.path(paper_path, "out")
docx_path = file.path(paper_path, "docx")

# Step 1: Find stuff in the 'new' folder that isn't in the old
if (0) {
  # Delete stuff we've already processed
  del_these <- intersect(list.files(path=in_path, include.dirs=TRUE),
                         list.files(path=out_path, include.dirs=TRUE))
  fs::dir_delete(file.path(base_path, del_these))
}

students = list.files(path=in_path, pattern=".Rmd$", ignore.case=TRUE, recursive = TRUE)

# ignore those that have html files
get_html_files <- function(in_path, ext=".htm|.pdf") {
  html = list.files(path=in_path, pattern=ext, ignore.case = TRUE, recursive = TRUE)
  # ignore the Generative AI use statemetn bullshit
  html <- html[!str_detect(html, "Generative AI")]
  html <- html[!str_detect(html, "Generative-AI")]
  # check for multiple files per student
  multiples <- ignore <- data.frame(dir = dirname(html), file = html) |>
    add_count(dir) |>
    filter(n > 1)
  
  if (nrow(multiples) > 0) {
    print(multiples)
#    stop("Need to do something here...")
    ignore <- multiples |> slice(1) |> pull(file)
    html <- setdiff(html, ignore)
  }

  html
}
html = get_html_files(in_path)

students <- students[!dirname(students) %in% dirname(html)]


# now load up rmarkdown and knit

cleanSearch <- function() {
  defaults <- c(".GlobalEnv", 
                paste0("package:", getOption("defaultPackages")),
                "Autoloads", "tools:rstudio",
                "package:base", "package:rmarkdown")
  currentList <- search()  
  deletes <- setdiff(currentList, defaults)
#  deletes <- names(sessionInfo()$otherPkgs)
  for (entry in deletes)
    detach(entry, character.only = TRUE)       
}

loadPackages <- function() {
  
  #  loadPackages()
  #  library(ggplot2)
#  library(tidyverse)
#  library(lubridate)
#  library(visreg)
  #  library(patchwork)
#  ggplot2::theme_set(ggplot2::theme_gray())
  #  cycle_counts_plot <- read_csv('https://www.massey.ac.nz/~jcmarsha/161122/cycle_counts_plot.csv')
 
}

knit_assignment <- function(path, base_path=base_path, out_path=out_path,
                            ignoreErrors = FALSE) {
  # break up assignment into path and filename
  dir = dirname(path)
  file = basename(path)
  out_file = file.path(out_path, dir, file)
  out_file = fs::path_ext_set(out_file, ".html")
  # comment out any fucking install.packages stuff Fucking students :(
  {
    con <- file(file.path(base_path, path), open="r")
    lines <- readLines(con)
    # comment out any install.packages stuff
    lines <- unlist(lapply(lines, function(x) { sub(pattern="install.packages", replacement="# install.packages\\1", x) }))
    close(con)
    # write the file back out again
    con <- file(file.path(base_path, path), open="w")
    writeLines(lines, con)
    close(con)
  }
  cleanSearch()
  # helpful packages most students forget
  loadPackages()
  tryCatch(
    # TODO: want this to be a complete clean render
    # ideally we need it to be executed by Rscript??
    {
    out_format <- html_document()
    out_format$knitr$opts_chunk$error = ignoreErrors
    rmarkdown::render(file.path(base_path, path), output_format=out_format, output_dir = file.path(out_path, dir),
                      envir = new.env())
    # Alternative is to just run it in a script environment directly. That would be:
      if (0) {
        
    test <- tempfile()
    commands <- c("library(rmarkdown)",
                  "library(tidyverse)",
                  "library(lubridate)",
                  "library(patchwork)",
                  "cycle_counts_plot <- read_csv('https://www.massey.ac.nz/~jcmarsha/161122/cycle_counts_plot.csv')",
                  "out_format <- html_document()",
                  paste0("out_format$knitr$opts_chunk$error = ", ifelse(ignoreErrors, "TRUE", "FALSE")),
                  paste0("rmarkdown::render(\"", file.path(base_path, path), "\",output_format=out_format, output_dir = \"", file.path(out_path, dir), "\")")
    )
    write_lines(commands, test)
    errors <- tempfile()
    xfun::Rscript(test, stdout = "", stderr = errors)
    read_lines(errors)
      }
    },
    error = function(e) { e }
  )
}

files=lapply(students, knit_assignment, base_path=in_path, out_path=out_path,
             ignoreErrors = FALSE)

library(tidyverse)
# find the odd ones
errors = which(lapply(files, function(x) { "error" %in% class(x) }) %>% unlist())

# PACKAGE INSTALLATION
if (0) {
  # TODO: A more efficient method would search for 'library(foo)' in the markdown files and collate a list. ATM we're running
  #       and re-running (and re-running) for each first error
  extract_bit <- function(x) {
    char <- as.character(unlist(x))
    foo <- grepl("library\\((.*)\\)", char)
    gsub("library\\((.*)\\)", "\\1", char[foo])
  }
  packages <- map(files[errors], extract_bit) %>% unlist() %>% unique()
  install.packages(packages)
}

# FIX THINGS UP HERE
files[errors] <- lapply(students[errors], knit_assignment, base_path=in_path, out_path=out_path)

# Once fixed, run
files[errors] <- lapply(students[errors], knit_assignment, base_path=in_path, out_path=out_path,
                        ignoreErrors = TRUE)

done <- dirname(students)

# now grab those already html and copy them across as well
no_md <- html[!dirname(html) %in% done]

# TODO: prefer HTML files over markdown?

# copy the HTML files across with no markdown
# create the output dir
lapply(file.path(out_path, dirname(no_md)), dir.create, recursive = TRUE)
file.copy(from=file.path(in_path, no_md), to = file.path(out_path, no_md))
done <- c(done, dirname(no_md))

# finally, find those folders that have neither .html or .Rmd files
all_files <- list.files(path=in_path, pattern="*", recursive = TRUE)

out_files <- list.files(path=out_path, pattern=".htm|.pdf", recursive=TRUE)
all_files[!dirname(all_files) %in% dirname(out_files)]

if (0) {
  # 24 missing, all are .docx. Let's try pandoc?
  docx = list.files(path=base_path, pattern=".docx", recursive = TRUE)

  # pandoc seems a bit shit. Let's instead dump these in a separate folder, convert with Windows PowerShell, then copy them back
  dir.create(docx_path)
  lapply(file.path(docx_path, dirname(docx)), dir.create)
  file.copy(from=file.path(base_path, docx), to = file.path(docx_path, docx))
  # now read all the .pdf files from there and copy them across again
  converted = list.files(path=docx_path, pattern=".pdf", recursive = TRUE)
  lapply(file.path(out_path, dirname(converted)), dir.create)
  file.copy(from=file.path(docx_path, converted), to = file.path(out_path, converted))
}

# check everything
out_files <- list.files(path=out_path, pattern=".htm|.pdf", recursive=TRUE)
all_files[!dirname(all_files) %in% dirname(out_files)]

# YAY, we're done!