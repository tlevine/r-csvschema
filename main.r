exports <- list()

exports$primitives <- list()
exports$primitives$reference <- function(column) {
  results <- sapply(column, function(cell) {
    grepl('^[a-z]+://', cell) || file.exists(cell)
  })
  is.logical(results) & sum(results) == length(results)
}

exports$validate <- function(file, table, func = read.csv){
  table <- read.csv(file)
  table.schema <- func(schema)

  check.type <- function(colname, type) {
    exports$primitives$reference(type)(table[,colname])
  }

  if (!(names(table) == table.schema$colname)) {
    FALSE
  } else {
    results <- mapply(check.type, table.schema$colnames, table.schema$type)
    sum(results) == length(results)
  }
}
