## Obtain and import all required data ----

# Import all variables from the data directory into data.names
data.names <- paste0("data/", dir("data"))

# Import each .rdata file from ~/data directory into the workspace
for (i in data.names){
  load(i)
}
