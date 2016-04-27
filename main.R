# This script takes the input data (in .csv format) as provided by Mattias
# Linnap and uses it to populate a database on the server.

# Anfangsdefinitonen
data_path <- "/project-data/userdata/rawdata"
sand_path <- "/project-data/userdata/sandkasten"
file_name <- "oliver.csv"
batchsize <- 500000
break_limit = 200

source("00_defs_funcs.R")

# --- Stuff is happening -------------------------------------------------------
# source("01_split_csv.R")




