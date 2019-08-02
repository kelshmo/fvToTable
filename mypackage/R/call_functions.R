source("R/process_functions.R")

# Data still requires manual curation but need a system to alert DAC when data is ready for processing and alert PEC team when new data is available. Manual curation occurs while the data is in the Staging `Folder`. A fileview points to all of the folders shared with the internal consortia. This script will run daily to detect when new data is visible to the consortia; i.e. data is moved from Staging to PEC `Folder`.  

pec_fv <- get_fv("syn18691012")

static_table <- get_fv("syn20555115")

new <- mod_table(pec_fv, static_table)

save <- store(new, "syn20555115")

