# Meta_data
R snippet to extract Articles Meta-data from text file


if (!requireNamespace("tidyverse", quietly = TRUE)) {
  # If not, install tidyverse
  install.packages("tidyverse")
}

# Load tidyverse package
library(tidyverse)

# load data read lines of txt (insert text file path)
data <- readLines("Project_24.txt")

# convert to dataframe under one col
data <- data.frame(v1 = data)

#check data  head first 100 lines
head(data, 100)


# Function to extract information from the data
extract_info <- function(data_lines) {
  extracted_data <- data.frame(title = character(0), doi = character(0), abstract = character(0))
  current_title <- ""
  current_doi <- ""
  current_abstract <- ""
  # YOU CAN ADD AS MUCH META DATA AS YOU WANT TO EXTRACT 
  # MAKE SURE TO LOOP IT THROUGH DOWN THE FUNCTION AS else if STATEMENTS
  # current_author <- ""
  # current_journal <- ""
  
  data_lines <- tolower(data_lines)
  
  for (line in data_lines) {
    if (grepl("^title:", line)) {
      current_title <- gsub("^title:", "", line)
    } else if (grepl("^abstract:", line)) {
      current_abstract <- gsub("^abstract:", "", line)
    } else if (grepl("^doi: ", line)) {
      current_doi <- gsub("^doi: ", "", line)
    } else if (line == "") {
      extracted_data <- rbind(extracted_data, data.frame(title = current_title, doi = current_doi, abstract = current_abstract))
      current_title <- ""
      current_doi <- ""
      current_abstract <- ""
    }
  }
  
  return(extracted_data)
  
}



# Run the function to extract information from the data
data_info <- extract_info(data$v1)

# EXCLUDE EMPTY ROWS
data_info2 <- data_info[!data_info$title == "",]

# view the extracted information dataframe
view(data_info2[1:10,])
