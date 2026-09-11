# DEVELOPER WORKFLOW RULE:
# 1. All functions, internal variables, and objects in this package must be named in English using camelCase.
# 2. All documentation (including roxygen2), comments, and console messages must be written in English.

#' Check Translations Coverage
#'
#' This function validates if all labels and hints present in an XML form
#' have corresponding translations in the translation CSV file.
#'
#' @param translationFile Character. Path to the CSV file containing translations.
#' @param baseForm Character. Path to the base XML form file.
#'
#' @return Logical. TRUE if all translations are present, FALSE otherwise.
#' @export
#'
#' @examples
#' # checkTranslations("translations.csv", "base_form.xml")
checkTranslations <- function(translationFile, baseForm) {
  library(xml2)
  
  translations <- read.csv(translationFile)
  form <- read_xml(baseForm)
  ns <- xml_ns(form)
  
  # Extract texts from the form (both labels and hints)
  labels <- xml_find_all(form, "//h:label", ns)
  hints <- xml_find_all(form, "//h:hint", ns)
  
  formTexts <- unique(c(xml_text(labels), xml_text(hints)))
  formTexts <- formTexts[formTexts != "" & !is.na(formTexts)]
  
  # Check coverage in the translated columns of the CSV
  translatedTexts <- c(translations$label..Português, translations$label..English, translations$label..Español)
  translatedTexts <- unique(translatedTexts[translatedTexts != "" & !is.na(translatedTexts)])
  
  missingTexts <- setdiff(formTexts, translatedTexts)
  
  # Filter dynamic ODK references (e.g., jr:itext)
  missingTexts <- missingTexts[!grepl("jr:itext", missingTexts)]
  
  if(length(missingTexts) > 0) {
    warning("Texts or hints missing translation in the translation file: ", paste(missingTexts, collapse = ", "))
    return(FALSE)
  } else {
    cat("✓ All translations for questions, options, and hints are present\n")
    return(TRUE)
  }
}

#' Extract Translations from an XML Form
#'
#' This function extracts existing translations or raw labels/hints from an XML form
#' and saves them into a structured CSV file template for translation.
#'
#' @param baseForm Character. Path to the base XML form file.
#' @param outputFile Character. Path where the extracted translations CSV should be saved.
#'
#' @return Character. The path to the generated CSV file.
#' @export
#'
#' @examples
#' # extractTranslations("base_form.xml", "extracted_translations.csv")
extractTranslations <- function(baseForm, outputFile) {
  library(xml2)
  
  form <- read_xml(baseForm)
  ns <- xml_ns(form)
  
  # Initialize empty data frame for translations
  translationTable <- data.frame(
    list_name = character(),
    name = character(),
    label..Português = character(),
    label..English = character(),
    label..Español = character(),
    stringsAsFactors = FALSE
  )
  
  # Check if itext element already exists in head using namespace-agnostic search
  itextNode <- xml_find_first(form, "//h:head/*[local-name()='model']/*[local-name()='itext']", ns)
  
  if (!inherits(itextNode, "xml_missing")) {
    # Extract from existing itext structure using namespace-agnostic search
    translations <- xml_find_all(itextNode, ".//*[local-name()='translation']")
    
    # Temporary list to accumulate translations by ID
    textMap <- list()
    
    for (trans in translations) {
      lang <- xml_attr(trans, "lang")
      if (is.na(lang)) {
        lang <- xml_attr(trans, "default")
        if (!is.na(lang) && lang == "true") {
          lang <- "default"
        } else {
          lang <- "default"
        }
      }
      
      textNodes <- xml_find_all(trans, ".//*[local-name()='text']")
      for (textNode in textNodes) {
        textId <- xml_attr(textNode, "id")
        valueNode <- xml_find_first(textNode, ".//*[local-name()='value']")
        valueText <- xml_text(valueNode)
        
        if (is.null(textMap[[textId]])) {
          textMap[[textId]] <- list(pt = "", en = "", es = "")
        }
        
        # Match languages carefully to avoid overlap (e.g. "Português" containing "es")
        if (lang == "default" || grepl("pt|port", lang, ignore.case = TRUE)) {
          textMap[[textId]]$pt <- valueText
        } else if (grepl("en|eng", lang, ignore.case = TRUE)) {
          textMap[[textId]]$en <- valueText
        } else if (grepl("es|spa|espa", lang, ignore.case = TRUE)) {
          textMap[[textId]]$es <- valueText
        }
      }
    }
    
    if (length(textMap) > 0) {
      for (textId in names(textMap)) {
        translationTable <- rbind(translationTable, data.frame(
          list_name = "text",
          name = textId,
          label..Português = textMap[[textId]]$pt,
          label..English = textMap[[textId]]$en,
          label..Español = textMap[[textId]]$es,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    # Extract raw labels and hints from the body
    questions <- xml_find_all(form, "//h:body//*[h:label or h:hint]", ns)
    
    for (qNode in questions) {
      qName <- xml_attr(qNode, "ref")
      if (is.na(qName)) {
        qName <- xml_attr(qNode, "name")
      }
      if (is.na(qName)) next
      
      qNameClean <- basename(qName)
      
      # Extract Label
      labelNode <- xml_find_first(qNode, ".//h:label", ns)
      if (!inherits(labelNode, "xml_missing")) {
        labelText <- xml_text(labelNode)
        if (labelText != "" && !grepl("jr:itext", labelText)) {
          translationTable <- rbind(translationTable, data.frame(
            list_name = "text",
            name = qNameClean,
            label..Português = labelText,
            label..English = "",
            label..Español = "",
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Extract Hint
      hintNode <- xml_find_first(qNode, ".//h:hint", ns)
      if (!inherits(hintNode, "xml_missing")) {
        hintText <- xml_text(hintNode)
        if (hintText != "" && !grepl("jr:itext", hintText)) {
          translationTable <- rbind(translationTable, data.frame(
            list_name = "text",
            name = paste0(qNameClean, "_hint"),
            label..Português = hintText,
            label..English = "",
            label..Español = "",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Remove duplicates
  translationTable <- unique(translationTable)
  
  # Add default languages list metadata at the top
  languagesMetadata <- data.frame(
    list_name = c("languages", "languages", "languages"),
    name = c("default", "en", "es"),
    label..Português = c("Português", "Inglês", "Espanhol"),
    label..English = c("Portuguese", "English", "Spanish"),
    label..Español = c("Portugués", "Inglés", "Español"),
    stringsAsFactors = FALSE
  )
  
  finalTable <- rbind(languagesMetadata, translationTable)
  
  write.csv(finalTable, outputFile, row.names = FALSE, na = "")
  cat("✓ Translations successfully extracted to:", outputFile, "\n")
  return(outputFile)
}

#' Check Translations in an XLSForm Excel File
#'
#' This function reads the 'survey' and 'choices' sheets of an XLSForm Excel file
#' and checks for missing translations across language columns (e.g., label, hint, etc.).
#'
#' @param xlsformPath Character. Path to the XLSForm Excel file.
#'
#' @return Logical. TRUE if no missing translations are found, FALSE otherwise.
#' @export
#'
#' @examples
#' # checkXlsformTranslations("data/treeCensusForm.xlsx")
checkXlsformTranslations <- function(xlsformPath) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required to run this function. Please install it.")
  }
  
  if (!file.exists(xlsformPath)) {
    stop("The specified XLSForm file does not exist: ", xlsformPath)
  }
  
  wb <- openxlsx::loadWorkbook(xlsformPath)
  sheets <- openxlsx::sheets(wb)
  
  hasIssues <- FALSE
  
  for (sheetName in c("survey", "choices")) {
    if (!sheetName %in% sheets) next
    
    df <- openxlsx::readWorkbook(wb, sheet = sheetName)
    cols <- colnames(df)
    
    # Find translation columns (e.g., label::English, hint::Português, etc.)
    transCols <- cols[grepl("^(label|hint|constraint_message|required_message)::", cols, ignore.case = TRUE)]
    if (length(transCols) == 0) {
      # Try with dot notation or other common patterns
      transCols <- cols[grepl("^(label|hint|constraint_message|required_message)\\.", cols, ignore.case = TRUE)]
    }
    
    if (length(transCols) < 2) {
      cat(sprintf("ℹ Sheet '%s' does not have multiple translation columns to compare.\n", sheetName))
      next
    }
    
    cat(sprintf("Checking sheet '%s' with columns: %s\n", sheetName, paste(transCols, collapse = ", ")))
    
    # For each row, if at least one translation column is populated, all should be populated
    for (i in seq_len(nrow(df))) {
      rowVals <- df[i, transCols]
      populated <- !is.na(rowVals) & rowVals != ""
      
      if (any(populated) && !all(populated)) {
        missingCols <- transCols[!populated]
        presentCols <- transCols[populated]
        cat(sprintf("  ⚠ Row %d: Missing translation in [%s]. Present in [%s] (Value: '%s')\n",
                    i + 1, # +1 for header row in Excel
                    paste(missingCols, collapse = ", "),
                    presentCols[1],
                    df[i, presentCols[1]]))
        hasIssues <- TRUE
      }
    }
  }
  
  if (hasIssues) {
    warning("Some translations are missing in the XLSForm.")
    return(FALSE)
  } else {
    cat("✓ All translations in the XLSForm are complete and consistent!\n")
    return(TRUE)
  }
}
