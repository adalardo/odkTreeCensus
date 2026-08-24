#install.packages("reticulate")
#library(reticulate)
#py_require()

# 1. Declara a dependência do pacote Python
reticulate::py_require("pyxform")

# 2. Importa o módulo Python e atribui ao objeto R 'pyxform'
pyxform <- reticulate::import("pyxform")

# 3. Lista os atributos do módulo importado
reticulate::py_list_attributes(pyxform)


if (!reticulate::py_module_available("pyxform")) {
    system("pipx install pyxform")
    #cat("pyxform not available. Install with: pip install pyxform")
    #stop("pyxform not available. Install with: pip install pyxform")
}

pyxform <- reticulate::import("pyxform")
reticulate::py_module_available("pyxform")
reticulate::py_list_attributes(pyxform)

test <- pyxform.xls2json("data/formForestGeo.xlsx")


survey <- pyxform$create_survey_from_xls("data/formForestGeo.xlsx")
xml_content <- survey$to_xml()
writeLines(xml_content, "formularioTest.xml")
str(xml_content)
