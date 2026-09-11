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
  
  # Check if itext element already exists in head
  itextNode <- xml_find_first(form, "//h:head/model/itext | //h:head/d1:model/itext", ns)
  
  if (!inherits(itextNode, "xml_missing")) {
    # Extract from existing itext structure
    translations <- xml_find_all(itextNode, ".//translation")
    
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
      
      textNodes <- xml_find_all(trans, ".//text")
      for (textNode in textNodes) {
        textId <- xml_attr(textNode, "id")
        valueNode <- xml_find_first(textNode, ".//value")
        valueText <- xml_text(valueNode)
        
        if (is.null(textMap[[textId]])) {
          textMap[[textId]] <- list(pt = "", en = "", es = "")
        }
        
        if (lang == "default" || grepl("pt", lang, ignore.case = TRUE)) {
          textMap[[textId]]$pt <- valueText
        } else if (grepl("en", lang, ignore.case = TRUE)) {
          textMap[[textId]]$en <- valueText
        } else if (grepl("es", lang, ignore.case = TRUE)) {
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
