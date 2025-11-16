# Export citations from BibTeX files to LaTeX format
# This script reads .bib files and exports formatted citations to a .tex file

library(bib2df)
library(dplyr)

# Function to format a single author name in APA style
format_single_author <- function(author) {
  # Remove extra whitespace
  author <- trimws(author)

  # Split by space to separate initials chunk from surname (including prefixes)
  parts <- strsplit(author, "\\s+")[[1]]

  if (length(parts) < 2) return(author)

  # According to assumption: first chunk = initials, remainder = surname (can include prefixes)
  initials_raw <- parts[1]
  surname <- paste(parts[-1], collapse = " ")

  # Add dots after initials if they don't have them
  if (!grepl("\\.", initials_raw)) {
    letters <- strsplit(initials_raw, "")[[1]]
    initials_formatted <- paste(paste0(letters, "."), collapse = " ")
  } else {
    initials_formatted <- initials_raw
  }

  # Combine as "Surname, Initials" (surname stays intact, including prefixes)
  formatted <- paste0(surname, ", ", initials_formatted)

  return(formatted)
}

# Function to format authors for LaTeX in APA style
format_authors <- function(author_list) {
  if (length(author_list) == 0) return("")

  # Ensure we have a clean character vector of individual authors
  authors <- author_list
  if (length(authors) == 1) {
    authors <- unlist(strsplit(authors, ","), use.names = FALSE)
  }
  authors <- trimws(authors)
  authors <- authors[authors != ""]

  # Format each author
  authors_formatted <- vapply(authors, format_single_author, character(1), USE.NAMES = FALSE)

  # Handle case with more than 7 authors (APA uses et al.)
  if (length(authors_formatted) > 7) {
    # First 6 authors, then ..., then last author
    authors_list <- c(authors_formatted[1:6], ". . .", authors_formatted[length(authors_formatted)])
    authors_str <- paste(authors_list, collapse = ", ")
  } else {
    # For 7 or fewer authors, list all with proper formatting
    if (length(authors_formatted) > 1) {
      # Join all but last with commas
      authors_str <- paste(authors_formatted[-length(authors_formatted)], collapse = ", ")
      # Add last author with ", & "
      authors_str <- paste0(authors_str, ", \\& ", authors_formatted[length(authors_formatted)])
    } else {
      authors_str <- authors_formatted[1]
    }
  }

  # Make Bartoš bold (after formatting to "Bartos, F.")
  authors_str <- gsub("Barto\\\\v\\{s\\}, F\\.", "\\\\textbf{Bartoš, F.}", authors_str)

  return(authors_str)
}

# Function to format a single publication entry in APA style
format_publication <- function(pub) {
  # Extract fields
  authors <- format_authors(pub$AUTHOR[[1]])
  year <- pub$YEAR

  # Extract month if available
  month_str <- ""
  if (!is.na(pub$MONTH)) {
    month_str <- paste0(", ", pub$MONTH)
  }

  title <- pub$TITLE
  journal <- pub$JOURNAL
  volume <- ifelse(!is.na(pub$VOLUME), pub$VOLUME, "")
  number <- ifelse(!is.na(pub$NUMBER), pub$NUMBER, "")
  pages <- ifelse(!is.na(pub$PAGES), pub$PAGES, "")

  # Format volume (italic), number (in parentheses), and pages
  volume_str <- ""
  if (volume != "") {
    volume_str <- paste0(", \\textit{", volume, "}")
    if (number != "") {
      volume_str <- paste0(volume_str, "(", number, ")")
    }
  }

  pages_str <- ifelse(pages != "", paste0(", ", pages), "")

  # Create DOI or URL string
  doi_str <- ""
  if (!is.na(pub$DOI)) {
    # Clean DOI - remove any "https://doi.org/" prefix if present
    doi <- pub$DOI
    doi <- gsub("^https?://doi\\.org/", "", doi)
    doi <- gsub("_", "\\\\_", doi, fixed = TRUE)
    doi_str <- paste0(" doi: ", doi)
  } else if (!is.na(pub$URL)) {
    doi_str <- paste0(" Retrieved from ", pub$URL)
  }

  # Construct the full citation in APA style
  citation <- paste0(
    "\\item ", authors, " (", year, month_str, "). ",
    title, ". \\textit{", journal, "}",
    volume_str, pages_str, ".", doi_str
  )

  return(citation)
}

# Function to export publications from a bib file to tex format
export_bib_to_tex <- function(bib_file, output_file, section_title = NULL) {
  # Read the bib file
  pubs <- bib2df(bib_file)
  if (nrow(pubs) > 0 && "YEAR" %in% names(pubs)) {
    pubs <- pubs[order(pubs$YEAR, decreasing = TRUE), ]
  }

  # Open output file
  con <- file(output_file, "w")

  # Write section title if provided
  if (!is.null(section_title)) {
    writeLines(paste0("\\subsection*{", section_title, "}"), con)
    writeLines("", con)
  }

  # Start itemize environment
  writeLines("\\begin{itemize}\\setlength{\\itemsep}{0pt}", con)

  # Format and write each publication
  if (nrow(pubs) > 0) {
    for (i in seq_len(nrow(pubs))) {
      citation <- format_publication(pubs[i, ])
      writeLines(citation, con)
    }
  }

  # End itemize environment
  writeLines("\\end{itemize}", con)

  close(con)

  cat("Exported", nrow(pubs), "citations to", output_file, "\n")
}

# Function to export all publications to a single tex file
export_all_publications <- function(output_file = "publications_export.tex") {
  # List of bib files and their section titles
  bib_files <- list(
    list(file = "publications-meta_analysis.bib", title = "Meta-Analysis & Publication Bias Methods"),
    list(file = "publications-statistics.bib", title = "Statistical Methods & Theory"),
    list(file = "publications-empirical.bib", title = "Empirical Research & Meta-Science"),
    list(file = "publications-all.bib", title = "Other Publications"),
    list(file = "publications-preprints.bib", title = "Other Preprints")
  )

  # Open output file
  con <- file(output_file, "w")

  # Write header
  writeLines("% Exported publications", con)
  writeLines(paste0("% Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
  writeLines("", con)

  close(con)

  # Export each bib file
  for (bib_info in bib_files) {
    if (file.exists(bib_info$file)) {
      # Read the bib file
      pubs <- bib2df(bib_info$file)
      if (nrow(pubs) > 0 && "YEAR" %in% names(pubs)) {
        pubs <- pubs[order(pubs$YEAR, decreasing = TRUE), ]
      }

      # Open file in append mode
      con <- file(output_file, "a")

      # Write section title
      writeLines(paste0("\\subsection*{", bib_info$title, "}"), con)
      writeLines("", con)

  # Start itemize environment
  writeLines("\\begin{itemize}\\setlength{\\itemsep}{0pt}", con)

      # Format and write each publication
      if (nrow(pubs) > 0) {
        for (i in seq_len(nrow(pubs))) {
          citation <- format_publication(pubs[i, ])
          writeLines(citation, con)
        }
      }

      # End itemize environment
      writeLines("\\end{itemize}", con)
      writeLines("", con)

      close(con)

      cat("Added", nrow(pubs), "citations from", bib_info$file, "\n")
    }
  }

  cat("\nAll publications exported to", output_file, "\n")
}

# Example usage:
# Export a single bib file
# export_bib_to_tex("publications-meta_analysis.bib", "meta_analysis_citations.tex", "Meta-Analysis & Publication Bias Methods")

# Export all publications to one file
export_all_publications("publications_export.tex")
