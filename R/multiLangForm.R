# DEVELOPER WORKFLOW RULE:
# 1. All functions, internal variables, and objects in this package must be named in English using camelCase.
# 2. All documentation (including roxygen2), comments, and console messages must be written in English.

#' Create a Multi-Language ODK Form
#'
#' This function takes a base XML form and a translation CSV file to generate
#' a multi-language ODK form with dynamic language selection.
#'
#' @param baseForm Character. Path to the base XML form file.
#' @param translationFile Character. Path to the CSV file containing translations.
#' @param outputFile Character. Path where the generated XML form should be saved.
#'
#' @return Character. The path to the generated output file.
#' @export
#'
#' @examples
#' # createMultiLangForm("base_form.xml", "translations.csv", "multi_lang_form.xml")
createMultiLangForm <- function(baseForm, translationFile, outputFile) {
  library(xml2)
  
  # Read base form
  form <- read_xml(baseForm)
  ns <- xml_ns(form)
  
  # Read translations
  translations <- read.csv(translationFile, stringsAsFactors = FALSE)
  
  # 1. ADD LANGUAGE SELECTION QUESTION
  bodyNode <- xml_find_first(form, "//h:body", ns)
  
  # Create language select
  selectLanguage <- xml_new_root("select1", ref = "/data/language", name = "language")
  
  labelNode <- xml_add_child(selectLanguage, "label")
  xml_text(labelNode) <- "Selecione o idioma / Select language / Seleccione el idioma"
  
  # Add options
  languages <- translations[translations$list_name == "languages", ]
  for(i in 1:nrow(languages)) {
    item <- xml_add_child(selectLanguage, "item")
    xml_add_child(item, "label", languages[i, "label..Português"])
    xml_add_child(item, "value", languages[i, "name"])
  }
  
  # Add as the first element of the body
  xml_add_child(bodyNode, selectLanguage, .where = 0)
  
  # 2. ADD ITEXT TO HEAD
  headNode <- xml_find_first(form, "//h:head", ns)
  itextNode <- xml_add_child(headNode, "itext")
  
  # For each translatable text
  texts <- translations[translations$list_name != "languages", ]
  for(i in 1:nrow(texts)) {
    textNode <- xml_add_child(itextNode, "text", id = texts[i, "name"])
    
    # Portuguese
    valuePt <- xml_add_child(textNode, "value", form = "default")
    xml_text(valuePt) <- texts[i, "label..Português"]
    
    # English
    valueEn <- xml_add_child(textNode, "value", form = "english", lang = "en")
    xml_text(valueEn) <- texts[i, "label..English"]
    
    # Spanish
    valueEs <- xml_add_child(textNode, "value", form = "spanish", lang = "es")
    xml_text(valueEs) <- texts[i, "label..Español"]
  }
  
  # 3. REPLACE LABELS AND HINTS WITH ITEXT REFERENCES
  # Find all elements that have a label or hint in the body (inputs, selects, etc.)
  questions <- xml_find_all(form, "//h:body//*[h:label or h:hint]", ns)
  for(qNode in questions) {
    qName <- xml_attr(qNode, "ref")
    if (is.na(qName)) {
      qName <- xml_attr(qNode, "name")
    }
    if (is.na(qName)) next
    
    # Clean the path to get only the field name
    qNameClean <- basename(qName)
    
    # Find corresponding translation for the label
    translationQ <- texts[texts$name == qNameClean, ]
    if(nrow(translationQ) > 0) {
      labelNode <- xml_find_first(qNode, ".//h:label", ns)
      if (!inherits(labelNode, "xml_missing")) {
        xml_remove(labelNode)
        newLabel <- xml_add_child(qNode, "label")
        xml_attr(newLabel, "ref") <- paste0("jr:itext('", qNameClean, "')")
      }
    }
    
    # Find corresponding translation for the hint
    hintName <- paste0(qNameClean, "_hint")
    translationHint <- texts[texts$name == hintName, ]
    if(nrow(translationHint) > 0) {
      hintNode <- xml_find_first(qNode, ".//h:hint", ns)
      if (!inherits(hintNode, "xml_missing")) {
        xml_remove(hintNode)
        newHint <- xml_add_child(qNode, "hint")
        xml_attr(newHint, "ref") <- paste0("jr:itext('", hintName, "')")
      }
    }
  }
  
  # 4. ADD DYNAMIC TRANSLATION TO THE MODEL
  modelNode <- xml_find_first(form, "//d1:model", ns)
  
  # Bind for language
  bindLang <- xml_add_child(modelNode, "bind")
  xml_attr(bindLang, "nodeset") <- "/data/language"
  xml_attr(bindLang, "type") <- "select1"
  
  # Instance for itext
  instanceNode <- xml_find_first(form, "//d1:instance", ns)
  itextInstance <- xml_add_child(instanceNode, "itext")
  
  write_xml(form, outputFile)
  return(outputFile)
}
