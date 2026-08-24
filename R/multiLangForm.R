criar_formulario_multi_idioma <- function(formulario_base, arquivo_traducoes, output_file) {
  library(xml2)
  
  # Ler formulário base
  form <- read_xml(formulario_base)
  ns <- xml_ns(form)
  
  # Ler traduções
  trad <- read.csv(arquivo_traducoes, stringsAsFactors = FALSE)
  
  # 1. ADICIONAR PERGUNTA DE SELEÇÃO DE IDIOMA
  body_node <- xml_find_first(form, "//h:body", ns)
  
  # Criar select de idioma
  select_idioma <- xml_new_root("select1", ref = "/data/language", name = "language")
  
  label_node <- xml_add_child(select_idioma, "label")
  xml_text(label_node) <- "Selecione o idioma / Select language / Seleccione el idioma"
  
  # Adicionar opções
  idiomas <- trad[trad$list_name == "languages", ]
  for(i in 1:nrow(idiomas)) {
    item <- xml_add_child(select_idioma, "item")
    xml_add_child(item, "label", idiomas[i, "label..Português"])
    xml_add_child(item, "value", idiomas[i, "name"])
  }
  
  # Adicionar como primeiro elemento do body
  xml_add_child(body_node, select_idioma, .where = 0)
  
  # 2. ADICIONAR ITEXT AO HEAD
  head_node <- xml_find_first(form, "//h:head", ns)
  itext_node <- xml_add_child(head_node, "itext")
  
  # Para cada texto traduzível
  textos <- trad[trad$list_name != "languages", ]
  for(i in 1:nrow(textos)) {
    text_node <- xml_add_child(itext_node, "text", id = textos[i, "name"])
    
    # Português
    value_pt <- xml_add_child(text_node, "value", form = "default")
    xml_text(value_pt) <- textos[i, "label..Português"]
    
    # Inglês
    value_en <- xml_add_child(text_node, "value", form = "english", lang = "en")
    xml_text(value_en) <- textos[i, "label..English"]
    
    # Espanhol
    value_es <- xml_add_child(text_node, "value", form = "spanish", lang = "es")
    xml_text(value_es) <- textos[i, "label..Español"]
  }
  
  # 3. SUBSTITUIR LABELS POR REFERÊNCIAS ITEXT
  questions <- xml_find_all(form, "//h:body//h:input", ns)
  for(q_node in questions) {
    q_name <- xml_attr(q_node, "name")
    
    # Encontrar tradução correspondente
    trad_q <- textos[textos$name == q_name, ]
    if(nrow(trad_q) > 0) {
      label_node <- xml_find_first(q_node, ".//h:label", ns)
      xml_remove(label_node)
      
      new_label <- xml_add_child(q_node, "label")
      xml_attr(new_label, "ref") <- paste0("jr:itext('", q_name, "')")
    }
  }
  
  # 4. ADICIONAR TRADUÇÃO DINÂMICA AO MODELO
  model_node <- xml_find_first(form, "//d1:model", ns)
  
  # Bind para idioma
  bind_lang <- xml_add_child(model_node, "bind")
  xml_attr(bind_lang, "nodeset") <- "/data/language"
  xml_attr(bind_lang, "type") <- "select1"
  
  # Instance para itext
  instance_node <- xml_find_first(form, "//d1:instance", ns)
  itext_instance <- xml_add_child(instance_node, "itext")
  
  write_xml(form, output_file)
  return(output_file)
}

# Uso
criar_formulario_multi_idioma("formulario_base.xml", "traducoes.csv", "formulario_multi_idioma.xml")