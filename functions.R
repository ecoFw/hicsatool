

generate_text_file <- function(df, output_file) {
  # Ensure the data frame has the expected columns
  if (!all(c("Mitigation", "Practice", "Description", "Resource") %in% colnames(df))) {
    stop("Data frame must contain columns: 'Mitigation', 'Practice', 'Description', 'Resource'")
  }
  
  # Initialize a character vector to store the lines of the text file
  lines <- character()
  
  # Get unique mitigations
  mitigations <- unique(df$Mitigation)
  
  for (mit in mitigations) {
    # Add the mitigation as a section
    lines <- c(lines, paste0("## ", mit), "")
    
    # Get unique practices for the current mitigation
    practices <- unique(df[df$Mitigation == mit, "Practice"])
    
    for (prac in practices) {
      # Add the practice as a subsection
      lines <- c(lines, paste0("### ", prac), "")
      
      # Filter the data frame for the current mitigation and practice
      sub_df <- df[df$Mitigation == mit & df$Practice == prac, ]
      
      for (i in 1:nrow(sub_df)) {
        # Add the description and resource
        lines <- c(lines, paste0("**Description**: ", sub_df$Description[i]), "")
        lines <- c(lines, paste0("**Resource**: ", sub_df$Resource[i]), "")
        lines <- c(lines, "")  # Add a blank line for spacing
      }
    }
  }
  
  # Write the lines to the output file
  writeLines(lines, output_file)
}


generate_word_file <- function(df, output_file) {
  # Ensure the data frame has the expected columns
  if (!all(c("Mitigation", "Practice", "Description", "Resource") %in% colnames(df))) {
    stop("Data frame must contain columns: 'Mitigation', 'Practice', 'Description', 'Resource'")
  }
  
  # Create a new Word document
  doc <- read_docx()
  
  # Get unique mitigations
  mitigations <- unique(df$Mitigation)
  
  for (mit in mitigations) {
    # Add the mitigation as a section
    doc <- doc %>% 
      body_add_par(mit, style = "heading 1") %>% 
      body_add_par("")
    
    # Get unique practices for the current mitigation
    practices <- unique(df[df$Mitigation == mit, "Practice"])
    
    for (prac in practices) {
      # Add the practice as a subsection
      doc <- doc %>% 
        body_add_par(prac, style = "heading 2") %>% 
        body_add_par("")
      
      # Filter the data frame for the current mitigation and practice
      sub_df <- df[df$Mitigation == mit & df$Practice == prac, ]
      
      for (i in 1:nrow(sub_df)) {
        # Add the description and resource
        doc <- doc %>% 
          body_add_par(paste("Description:", sub_df$Description[i]), style = "Normal") %>% 
          body_add_par(paste("Resource:", sub_df$Resource[i]), style = "Normal") %>% 
          body_add_par("")
      }
    }
  }
  
  # Save the Word document
  print(doc, target = output_file)
}
