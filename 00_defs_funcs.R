# ------------------------------------------------------------------------------
# --- Definitions, names, lists ------------------------------------------------
# ------------------------------------------------------------------------------

# Spaltennamen der einzelnen Events
names_prefix <- c("uid", "milisec", "eventtype")
col_headers <- list(
  app.heartbeat = names_prefix,
  app.start = names_prefix,
  call.incoming.answered = c(names_prefix,
                             'number',
                             'time_start',
                             'time_answer',
                             'time_end'),
  call.incoming.missed = c(names_prefix,
                           'number',
                           'time_start',
                           'time_answer',
                           'time_end'),
  call.outgoing = c(names_prefix,
                    'number',
                    'time_start',
                    'time_answer',
                    'time_end'),
  device.airplane.off = names_prefix,
  device.airplane.on  = names_prefix,
  device.cell.location = c(names_prefix,
                           'type',
                           'cid',
                           'lac',
                           'operator_numeric'),
  device.cell.service = c(names_prefix,
                          'state',
                          'operator_short',
                          'operator_long',
                          'roaming'),
  device.network = c(names_prefix,
                     'connected',
                     'roaming',
                     'wifi'),
  sensor.gps = c(names_prefix,
                 'time2',
                 'loc_y',
                 'loc_x',
                 'accuracy',
                 'altitude',
                 'bearing',
                 'speed'),
  sms.incoming = c(names_prefix,
                   'number',
                   'time',
                   'text_length',
                   'text_words',
                   'text_average_word_length',
                   'text_letters',
                   'text_numbers',
                   'text_puncts',
                   'text_whites',
                   'is_email',
                   'is_status_report',
                   'is_replace',
                   'is_mwi'),
  sms.outgoing = c(names_prefix,
                   'number',
                   'time',
                   'text_length',
                   'text_words',
                   'text_average_word_length',
                   'text_letters',
                   'text_numbers',
                   'text_puncts',
                   'text_whites'),
  user.prefs.pause = names_prefix  #,
  # device.battery.level = c(names_prefix,
  #                          'level',
  #                          'max_level',
  #                          'plugged',
  #                          'status')
)
  
# ------------------------------------------------------------------------------
# --- Functions ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# --- Connecting to DB ---------------------------------------------------------
disconnect <- function(){
  # This function disconnects from the database
  dbDisconnect(con)
  dbUnloadDriver(drv)
}

connect <- function(user = "burkhard", pw = "", dbname = "burkhard", 
                    host = "localhost", port = 5432){
  # This function connects to the database with the given credentials
  # It assigns the relevant variables to the global environment
  try(disconnect(), silent = T)
  drv <<- dbDriver("PostgreSQL")
  con <<- dbConnect(drv, dbname = dbname,
                    host = host, port = port,
                    user = user, password = pw)
}

# --- Importing a csv-file -----------------------------------------------------
import_csv <- function(name_, namelist_, filepath_){
  # takes the name of a csv file and a namelist_, which contains the expected
  #   variables. Then it
  # 1. creates a table in the db with the same name and the variables needed
  # 2. loads the csv into the db
  print(paste("Working on", name_))
  table_name <- gsub("\\.", "_", name_)
  col_defs <- paste(namelist_[[name_]], "varchar (50)", 
                        collapse = ", ")
  query <- paste(" DROP TABLE IF EXISTS", table_name, "CASCADE;
CREATE TABLE", table_name, "(
  ", col_defs, ")")
  dbGetQuery(con, query)
  query_copy <- paste(
    "COPY", table_name, "FROM", 
    paste0("'", filepath_, "/", name_,".csv'"), 
            " DELIMITER ',' NULL \'\\N\' CSV HEADER;")
  tryCatch({
    dbGetQuery(con, query_copy)
    },
    error = function(e) {
      print("Upload failed.")
    },
    warning = function(w){
      print("There was a warning.")
    }
  )
  return(NULL)
}
















