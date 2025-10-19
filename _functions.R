# Helper functions for CV generation

# Function to print publications from a bib file with links
print_publications <- function(bib_file, font_size = "\\small") {
  library(bib2df)
  
  pubs <- bib2df(bib_file) %>%
    arrange(desc(YEAR))
  
  # Print as a simple list with smaller font and reduced spacing
  cat(font_size, "\n")  # Options: \footnotesize, \small, \normalsize
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{0pt}\n")  # Remove space between items
  cat("\\setlength{\\parskip}{0pt}\n")  # Remove paragraph spacing
  
  for(i in 1:nrow(pubs)) {
    authors <- paste(pubs$AUTHOR[[i]], collapse = ", ")
    # Make my name bold
    authors <- gsub("F Barto\\\\v\\{s\\}", "\\\\textbf{F Barto\\\\v{s}}", authors)
    title <- pubs$TITLE[i]
    journal <- pubs$JOURNAL[i]
    year <- pubs$YEAR[i]
    volume <- ifelse(!is.na(pubs$VOLUME[i]), paste0(", ", pubs$VOLUME[i]), "")
    pages <- ifelse(!is.na(pubs$PAGES[i]), paste0(", ", pubs$PAGES[i]), "")
    
    # Create link if DOI or URL is present (will be used as item marker)
    link <- ""
    if(!is.na(pubs$DOI[i])) {
      doi_url <- paste0("https://doi.org/", pubs$DOI[i])
      link <- paste0("\\item[\\href{", doi_url, "}{\\textcolor{blue}{\\faFileText}}] ")
    } else if(!is.na(pubs$URL[i])) {
      link <- paste0("\\item[\\href{", pubs$URL[i], "}{\\textcolor{blue}{\\faFileText}}] ")
    } else {
      link <- "\\item "
    }
    
    cat(link, authors, " (", year, "). ", 
        title, ". \\textit{", journal, "}", volume, pages, ".\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")  # Reset to normal size
}

# Function to print software packages with links
print_software <- function(packages_df, font_size = "\\small") {
  # Print as a simple list with reduced spacing
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{0pt}\n")  # Remove space between items
  cat("\\setlength{\\parskip}{0pt}\n")  # Remove paragraph spacing
  
  for(i in 1:nrow(packages_df)) {
    pkg_name <- packages_df$name[i]
    pkg_url <- packages_df$url[i]
    pkg_desc <- packages_df$description[i]
    
    # Create the item with link marker or just bullet
    if(!is.na(pkg_url) && pkg_url != "") {
      # Detect if it's CRAN or GitHub
      if(grepl("cran.r-project.org", pkg_url)) {
        # CRAN package - use [CRAN] as bullet
        link <- paste0("\\item[\\href{", pkg_url, "}{[CRAN]}] ")
      } else if(grepl("github.com", pkg_url)) {
        # GitHub package - use GitHub icon as bullet
        link <- paste0("\\item[\\href{", pkg_url, "}{\\textcolor{awesome}{\\faGithub}}] ")
      } else {
        # Other link - use generic bullet
        link <- paste0("\\item[\\href{", pkg_url, "}{\\textcolor{awesome}{\\faLink}}] ")
      }
      cat(link, "\\textbf{", pkg_name, "}: ", pkg_desc, "\n", sep = "")
    } else {
      cat("\\item \\textbf{", pkg_name, "}: ", pkg_desc, "\n", sep = "")
    }
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")  # Reset to normal size
}

# Function to print service entries with better page breaks
print_service <- function(service_df, font_size = "\\small") {
  # Print as a simple list with reduced spacing that allows page breaks
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")  # Small space between items
  cat("\\setlength{\\parskip}{0pt}\n")  # Remove paragraph spacing
  
  for(i in 1:nrow(service_df)) {
    year <- service_df$year[i]
    title <- service_df$title[i]
    description <- service_df$description[i]
    
    # Format: [YEAR] Title: Description
    cat("\\item[", year, "] \\textbf{", title, "}: ", description, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")  # Reset to normal size
}

# Function to print teaching entries with better page breaks
print_teaching <- function(teaching_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(teaching_df)) {
    semester <- teaching_df$semester[i]
    title <- teaching_df$title[i]
    role <- teaching_df$role[i]
    code <- teaching_df$code[i]
    campus <- teaching_df$campus[i]
    
    # Format: [SEMESTER] Title: Role; Code; Campus
    cat("\\item[", semester, "] \\textbf{", title, "}: ", role, "; ", code, "; ", campus, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print supervision entries with better page breaks
print_supervision <- function(supervision_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(supervision_df)) {
    year <- supervision_df$year[i]
    type <- supervision_df$type[i]
    title <- supervision_df$title[i]
    description <- supervision_df$description[i]
    campus <- supervision_df$campus[i]
    
    # Format: [YEAR] Type: Title - Description; Campus
    cat("\\item[", year, "] \\textbf{", type, "}: ", title, " - ", description, "; ", campus, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print talks with better page breaks
print_talks <- function(talks_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(talks_df)) {
    when <- talks_df$when[i]
    title <- talks_df$title[i]
    details <- talks_df$details[i]
    where <- talks_df$where[i]
    
    details_where <- ifelse(details != "", paste0(details, "; ", where), where)
    
    # Format: [YEAR] Title: Details; Where
    cat("\\item[", when, "] \\textbf{", title, "}: ", details_where, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print conference talks with better page breaks
print_conference_talks <- function(talks_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(talks_df)) {
    year <- talks_df$year[i]
    title <- talks_df$title[i]
    conference <- talks_df$conference[i]
    place <- talks_df$place[i]
    
    # Format: [YEAR] Title: Conference; Place
    cat("\\item[", year, "] \\textbf{", title, "}: ", conference, "; ", place, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print workshops with better page breaks
print_workshops <- function(workshops_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(workshops_df)) {
    year <- workshops_df$year[i]
    title <- workshops_df$title[i]
    place <- workshops_df$place[i]
    
    # Format: [YEAR] Title: Place
    cat("\\item[", year, "] \\textbf{", title, "}: ", place, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print awards with better page breaks
print_awards <- function(awards_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(awards_df)) {
    year <- awards_df$year[i]
    award <- awards_df$award[i]
    
    # Check if there's a project/description column
    if("project" %in% names(awards_df)) {
      project <- awards_df$project[i]
    } else {
      project <- NA
    }
    
    from <- awards_df$from[i]
    
    # Format: [YEAR] Award: project; from or just from if project is empty
    # Check if project is empty, NA, or just whitespace
    if(!is.na(project) && nchar(trimws(project)) > 0) {
      cat("\\item[", year, "] \\textbf{", award, "}: ", project, "; ", from, "\n", sep = "")
    } else {
      cat("\\item[", year, "] \\textbf{", award, "}: ", from, "\n", sep = "")
    }
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print scholarships with project field
print_scholarships <- function(scholarships_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(scholarships_df)) {
    year <- scholarships_df$year[i]
    scholarship <- scholarships_df$scholarship[i]
    project <- scholarships_df$project[i]
    from <- scholarships_df$from[i]
    
    # Format: [YEAR] Scholarship: project; from or just from if project is empty
    # Check if project is empty, NA, or just whitespace
    if(!is.na(project) && nchar(trimws(project)) > 0) {
      cat("\\item[", year, "] \\textbf{", scholarship, "}: ", project, "; ", from, "\n", sep = "")
    } else {
      cat("\\item[", year, "] \\textbf{", scholarship, "}: ", from, "\n", sep = "")
    }
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print service & leadership entries with better page breaks
print_service_leadership <- function(service_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(service_df)) {
    when <- service_df$when[i]
    what <- service_df$what[i]
    role <- service_df$role[i]
    details <- service_df$details[i]
    
    # Format: [WHEN] What: Role \n Details
    cat("\\item[", when, "] \\textbf{", what, "}: ", role, " \\newline ", details, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print peer review journals
print_peer_review <- function(journals_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{2pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(journals_df)) {
    journal <- journals_df$journal[i]
    cat("\\item ", journal, "\n", sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}

# Function to print funding entries
print_funding <- function(funding_df, font_size = "\\small") {
  cat(font_size, "\n")
  cat("\\begin{itemize}\n")
  cat("\\setlength{\\itemsep}{3pt}\n")
  cat("\\setlength{\\parskip}{0pt}\n")
  
  for(i in 1:nrow(funding_df)) {
    year <- funding_df$year[i]
    type <- funding_df$type[i]
    title <- funding_df$title[i]
    
    # Check for optional fields
    amount <- if("amount" %in% names(funding_df)) funding_df$amount[i] else NA
    coauthors <- if("coauthors" %in% names(funding_df)) funding_df$coauthors[i] else NA
    agency <- funding_df$agency[i]
    
    # Build the output string
    # Format: [YEAR] Type: Title; amount; coauthors; agency
    output <- paste0("\\item[", year, "] \\textbf{", type, "}: ", title)
    
    # Add amount if present
    if(!is.na(amount) && nchar(trimws(amount)) > 0) {
      output <- paste0(output, "; ", amount)
    }
    
    # Add coauthors if present
    if(!is.na(coauthors) && nchar(trimws(coauthors)) > 0) {
      output <- paste0(output, "; ", coauthors)
    }
    
    # Add agency
    output <- paste0(output, "; ", agency, "\n")
    
    cat(output, sep = "")
  }
  
  cat("\\end{itemize}\n")
  cat("\\normalsize\n")
}
