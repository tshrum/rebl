# metadata_functions
# 2024.03.14

# Adapted from function by Dr. Nicholas Gotelli, UVM
# Required functions: none

# This is a three step procedure to embed metadata into a raw data csv. It will
# be commented out with '#', so note that to load the data cleanly, you will 
# have to use read_csv('path', comment = '#')

# 1. get_metadata_template creates template in root fodler in .csv and opens it 
# for editing. Once metadata is there, just save and close it.

# 2. save_metadata_template writes those lines to the beginning of a specified
# csv file. They will be commented out with '#'.

# 3. remove_metadata_template deletes the template csv we made in the root
# folder.

# 4. explain_metadata just gives instructions because I keep forgetting what I
# named these functions.



# get_metadata_template ---------------------------------------------------


get_metadata_template <- function(file_name = NULL) {
  
  if (is.null(file_name)) {
    file_name <- "metadata_template.csv"
  }
  
  if (file.exists(file_name)) stop(
    '\n\n', file_name, " already exists! ",
    'Choose a new filename or remove existing template.\n\n') else {
      
      write.table(
        cat("# *** METADATA ***", "\n",
            "#------------------------------", "\n",
            "# Title: ","\n",
            "# Metadata timestamp: ", format(Sys.time(), "%d %B %Y %X"),"\n",
            "# Author: ", "\n",
            "# Author email: ", "\n",
            "#------------------------------", "\n",
            "# Description: ", "\n",
            "# Data Collection: ", "\n",
            "# Variables: ", "\n",
            "# Missing Data or Quality Issues: ", "\n",
            "# Ownership: ", "\n",
            "#------------------------------", "\n",
            "# Changelog", "\n",
            "# Date: , Changes:", "\n",
            "#------------------------------", "\n",
            "# *** END OF METADATA ***", "\n\n",
            file = file_name,
            row.names = "",
            col.names = "",
            sep = ""))
    }
  
  file.edit(file_name)
  
} 



# save_metadata_to_csv ----------------------------------------------------


# Combine metadata with a csv and save it. Has an option to remove the template
# too, but it is probably risky. Use remove_metadata_template() once you 
# confirm that it worked.

save_metadata_to_csv <- function(csv_file_path,
                                 metadata_file = 'metadata_template.csv',
                                 remove_template = FALSE) {
  
  if (any(grepl(" METADATA ", readLines(csv_file_path), ignore.case = FALSE))) {
    stop('\n\nMetadata already exists for .csv file! ', 
         'Edit or remove existing metadata.\n\n',
         call. = FALSE)
  }
  
  metadata <- readLines(metadata_file)
  csv_data <- readLines(csv_file_path)
  combined <- c(metadata, csv_data)
  
  writeLines(combined, csv_file_path)
  
  if (remove_template == TRUE) {
    rm(metadata_file)
  }
  
  cat('\n\n* Metadata saved to', csv_file_path, '*\n\n')
  
}



# remove_metadata_template ------------------------------------------------


# separate function to remove template. There is an option within 
# save_metadata_to_csv.R to do remove it, but it seems dangerous. Having this
# separate just in case. 

remove_metadata_template <- function(template_filepath = 'metadata_template.csv') {
  
  if (file.exists(template_filepath)) {
    
    file.remove(template_filepath)
    cat("\n\n*", template_filepath, "deleted *\n\n", sep = " ")
    
  } else {
    
    cat("\n\n* No file found *\n\n")
    
  }
}



# explain_metadata --------------------------------------------------------


explain_metadata <- function() {
  cat(
    '\nHow to use metadata functions:\n',
    '1. get_metadata_template()\n',
    '     This will open up a csv for you to write in metadata\n',
    '2. save_metadata_to_csv(csv_file_path)\n',
    '     Appends that metadata to a csv of your choosing, editing in place.\n',
    '3. remove_metadata_template()\n',
    '     Removes thet template with default name unless otherwise specified.\n'
  )
}



# Explain Instructions When Package is Loaded ------------------------------


cat(
  '\nHow to use metadata functions:\n',
  '1. get_metadata_template()\n',
  '     This will open up a csv for you to write in metadata\n',
  '2. save_metadata_to_csv(csv_file_path)\n',
  '     Appends that metadata to a csv of your choosing, editing in place.\n',
  '3. remove_metadata_template()\n',
  '     Removes the template with default name unless otherwise specified.\n',
  '\nTo see these instructions again, use explain_metadata()\n'
)
