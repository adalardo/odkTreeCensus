validar_traducoes <- function(arquivo_traducoes, formulario_base) {
  library(xml2)
  
  trad <- read.csv(arquivo_traducoes)
  form <- read_xml(formulario_base)
  ns <- xml_ns(form)
  
  # Extrair textos do formulário
  labels <- xml_find_all(form, "//h:label", ns)
  textos_form <- unique(xml_text(labels))
  
  # Verificar cobertura
  textos_trad <- trad$label..Português[trad$list_name != "languages"]
  faltantes <- setdiff(textos_form, textos_trad)
  
  if(length(faltantes) > 0) {
    warning("Textos sem tradução: ", paste(faltantes, collapse = ", "))
    return(FALSE)
  } else {
    cat("✓ Todas as traduções estão presentes\n")
    return(TRUE)
  }
}