# Script to download publications from Google Scholar
# Author: František Bartoš
# Date: October 2025

# Install required packages if needed
if (!require("scholar")) install.packages("scholar")
if (!require("dplyr")) install.packages("dplyr")

library(scholar)
library(dplyr)

# ===== CONFIGURATION =====
# Replace with your Google Scholar ID
scholar_id <- "vAo-APsAAAAJ"

# Output file name
output_file <- "publications-scholar.bib"

# ===== DOWNLOAD PUBLICATIONS =====
cat("Downloading publications from Google Scholar...\n")
cat("Scholar ID:", scholar_id, "\n\n")

# Get publications
pubs <- get_publications(scholar_id)

cat("Found", nrow(pubs), "publications\n\n")

# Display summary
cat("Publications overview:\n")
print(pubs %>%
        select(title, author, journal, year, cites) %>%
        arrange(desc(year)) %>%
        head(10))

# ===== CONVERT TO BIBTEX =====
cat("\n\nConverting to BibTeX format...\n")

# Create BibTeX entries
bib_entries <- character(nrow(pubs))

for (i in 1:nrow(pubs)) {
  pub <- pubs[i, ]

  # Generate citation key: surname-year-firstword
  # Clean up author name and extract first author surname
  first_author <- strsplit(pub$author, ",")[[1]][1]
  first_author_clean <- gsub("[^A-Za-z]", "", first_author)
  first_author_clean <- tolower(first_author_clean)

  # Extract first meaningful word from title (skip prepositions)
  prepositions <- c("a", "an", "the", "in", "on", "at", "to", "for", "of",
                    "with", "by", "from", "and", "or", "but")
  title_words <- tolower(strsplit(pub$title, " ")[[1]])
  title_words <- gsub("[^a-z]", "", title_words)  # Remove punctuation
  title_words <- title_words[nchar(title_words) > 0]  # Remove empty strings

  # Find first non-preposition word
  first_word <- NULL
  for (word in title_words) {
    if (!(word %in% prepositions)) {
      first_word <- word
      break
    }
  }
  if (is.null(first_word)) first_word <- title_words[1]  # Fallback

  # Create citation key
  citekey <- paste0(first_author_clean, pub$year, first_word)

  # Make citekey unique if needed
  if (sum(grepl(paste0("^", citekey, "$"), bib_entries[1:i])) > 0) {
    suffix <- sum(grepl(paste0("^", citekey), bib_entries[1:i])) + 1
    citekey <- paste0(citekey, letters[suffix])
  }

  # Determine entry type
  entry_type <- if (!is.na(pub$journal) && pub$journal != "") {
    "article"
  } else {
    "misc"
  }

  # Function to convert special characters to LaTeX
  convert_to_latex <- function(text) {
    text <- gsub("š", "\\\\v{s}", text)
    text <- gsub("Š", "\\\\v{S}", text)
    text <- gsub("č", "\\\\v{c}", text)
    text <- gsub("Č", "\\\\v{C}", text)
    text <- gsub("ř", "\\\\v{r}", text)
    text <- gsub("Ř", "\\\\v{R}", text)
    text <- gsub("ž", "\\\\v{z}", text)
    text <- gsub("Ž", "\\\\v{Z}", text)
    text <- gsub("ý", "\\\\'{y}", text)
    text <- gsub("Ý", "\\\\'{Y}", text)
    text <- gsub("á", "\\\\'{a}", text)
    text <- gsub("Á", "\\\\'{A}", text)
    text <- gsub("é", "\\\\'{e}", text)
    text <- gsub("É", "\\\\'{E}", text)
    text <- gsub("í", "\\\\'{i}", text)
    text <- gsub("Í", "\\\\'{I}", text)
    text <- gsub("ó", "\\\\'{o}", text)
    text <- gsub("Ó", "\\\\'{O}", text)
    text <- gsub("ú", "\\\\'{u}", text)
    text <- gsub("Ú", "\\\\'{U}", text)
    text <- gsub("ů", "\\\\r{u}", text)
    text <- gsub("Ů", "\\\\r{U}", text)
    text <- gsub("ě", "\\\\v{e}", text)
    text <- gsub("Ě", "\\\\v{E}", text)
    text <- gsub("ď", "\\\\v{d}", text)
    text <- gsub("Ď", "\\\\v{D}", text)
    text <- gsub("ť", "\\\\v{t}", text)
    text <- gsub("Ť", "\\\\v{T}", text)
    text <- gsub("ň", "\\\\v{n}", text)
    text <- gsub("Ň", "\\\\v{N}", text)
    return(text)
  }

  # Build BibTeX entry with LaTeX conversion
  bib_entry <- paste0("@", entry_type, "{", citekey, ",\n")
  bib_entry <- paste0(bib_entry, "  title = {", convert_to_latex(pub$title), "},\n")
  bib_entry <- paste0(bib_entry, "  author = {", convert_to_latex(pub$author), "},\n")

  if (!is.na(pub$journal) && pub$journal != "") {
    bib_entry <- paste0(bib_entry, "  journal = {", convert_to_latex(pub$journal), "},\n")
  }

  if (!is.na(pub$number) && pub$number != "") {
    bib_entry <- paste0(bib_entry, "  number = {", convert_to_latex(pub$number), "},\n")
  }

  if (!is.na(pub$year) && pub$year != "") {
    bib_entry <- paste0(bib_entry, "  year = {", pub$year, "},\n")
  }

  # Remove trailing comma and close entry
  bib_entry <- paste0(bib_entry, "}\n")

  bib_entries[i] <- bib_entry
}

# ===== SAVE TO FILE =====
cat("\nSaving to", output_file, "...\n")

# Write all entries to file
writeLines(bib_entries, output_file)

cat("\n✓ Success! Downloaded", nrow(pubs), "publications to", output_file, "\n")
cat("\nYou can now use this file in your CV with:\n")
cat("  bibliography_entries(\"", output_file, "\")\n", sep = "")

# ===== ADDITIONAL INFORMATION =====
cat("\n\nProfile statistics:\n")
profile <- get_profile(scholar_id)
cat("  Name:", profile$name, "\n")
cat("  h-index:", profile$h_index, "\n")
cat("  i10-index:", profile$i10_index, "\n")
