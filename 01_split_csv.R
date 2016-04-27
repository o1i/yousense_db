# --- 1: Read the big csv (in chunks) save it according to event type
# As the original csv contains all events there cannot be a unified schema.
# Therefore we split the rows according to the event type they represent.

file.remove(file.path(sand_path, paste0(names(col_headers), '.csv')))
read_lines <- 0
emergency_counter <- 0
testcon <- file(file.path(data_path, file_name), open = "r")

while(length(test_data <- readLines(testcon, batchsize)) > 0 & 
      emergency_counter < break_limit){
  vectors <- strsplit(test_data, ',')
  categories <- sapply(vectors, function(v)v[3])
  # soll_laenge <- sapply(col_headers[categories], length)
  vectors_padded <- lapply(vectors, function(v_){
    c(v_, rep(NA, length(col_headers[[v_[3]]]) - length(v_)))
  })
  # ist_laenge <- sapply(vectors_padded, length)
  different_categories <- unique(categories)
  sapply(different_categories, function(name_){
    to_append <- data.frame(do.call(rbind, vectors_padded[categories == name_]))
    if(nrow(to_append) > 0){
      colnames(to_append) <- col_headers[[name_]]
      write.table(to_append, 
                  file = file.path(sand_path, paste0(name_, '.csv')), 
                  append = T,
                  sep = ',', na = 'NULL', row.names = F, 
                  col.names = !file.exists(file.path(sand_path, 
                                                     paste0(name_, '.csv'))))
      return(NULL)
    }
  })
  read_lines <- read_lines + length(test_data)
  emergency_counter <- emergency_counter + 1
  if(emergency_counter %% 10 == 0 | T) print(paste(read_lines, "lines read"))
  if(emergency_counter >= break_limit) print("reading terminated prematurely")
}
close(testcon)