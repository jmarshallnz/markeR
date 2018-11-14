filename <- function(table) {
  paste0(table, ".csv")
}

# fetch from the database if possible
read_rows <- function(table, columns) {
  values <- NULL

  try({
    values = read.csv(filename(table))
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(table, columns) {
  success <- FALSE

  try({
    # check if file exists
    if (!file.exists(filename(table))) {
      # otherwise, create it
      db <- matrix(NA, 1, length(columns))
      colnames(db) <- columns
      write.csv(db, filename(table), row.names=FALSE)
    }
    success <- TRUE
    }, silent=TRUE)

  success
}

# write row to the database if possible
write_row <- function(table, columns, values) {
  success <- FALSE

  try({
    db <- read.csv(filename(table))
    db <- rbind(db, values)
    write.csv(db, filename(table), row.names=FALSE)
    success <- TRUE
  }, silent=TRUE)

  success
}
