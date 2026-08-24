#' Convert XLSForm to ODK XML using pyxform
#'
#' This function converts an Excel XLSForm (.xlsx) into an ODK-compliant XML file
#' using the Python library \code{pyxform} via the \code{reticulate} package.
#' It also performs a basic integrity check on the generated XML file.
#'
#' @param xlsxPath Character. Path to the input Excel (.xlsx) file.
#' @param xmlPath Character. Path where the output XML file should be saved.
#'   If NULL, it will be saved in the same directory with a .xml extension.
#'
#' @return Character. The path to the generated XML file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' xmlForm("data/treeCensusForm.xlsx", "data/treeCensusForm.xml")
#' }
xmlForm <- function(xlsxPath, xmlPath = NULL) {
  # 1. Check and install reticulate package if missing
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    message("The 'reticulate' package is not installed. Installing now...")
    install.packages("reticulate")
  }

  # 2. Validate input file existence
  if (!file.exists(xlsxPath)) {
    stop("The specified Excel file does not exist: ", xlsxPath)
  }

  # Define default output path if not provided
  if (is.null(xmlPath)) {
    xmlPath <- sub("\\.xlsx$", ".xml", xlsxPath, ignore.case = TRUE)
  }

  # 3. Ensure pyxform is available in the Python environment
  tryCatch({
    reticulate::import("pyxform")
  }, error = function(e) {
    message("Python library 'pyxform' not found. Trying to install via reticulate...")
    tryCatch({
      # Se py_require estiver disponível (versões mais novas do reticulate), use-o para evitar avisos de venv efêmero
      if (exists("py_require", envir = asNamespace("reticulate"), inherits = FALSE)) {
        reticulate::py_require("pyxform")
      } else {
        reticulate::py_install("pyxform", pip = TRUE)
      }
    }, error = function(err) {
      stop("Could not install 'pyxform' automatically. Please install it manually in your Python environment.")
    })
  })

  # Import pyxform modules
  pyxformXls2json <- reticulate::import("pyxform.xls2json")
  pyxformBuilder <- reticulate::import("pyxform.builder")

  message("Converting ", xlsxPath, " to XML...")

  # 4. Run conversion using pyxform
  tryCatch({
    # Parse XLS to intermediate JSON structure
    surveyJson <- pyxformXls2json$parse_file_to_json(xlsxPath)
    
    # Create Survey element from dictionary
    survey <- pyxformBuilder$create_survey_element_from_dict(surveyJson)
    
    # Write XForm XML to file
    survey$print_xform_to_file(xmlPath)
    message("XML successfully generated at: ", xmlPath)
  }, error = function(e) {
    stop("Error during pyxform conversion: ", e$message)
  })

  # 5. Verify XML integrity and return problems if any
  if (file.exists(xmlPath)) {
    if (requireNamespace("xml2", quietly = TRUE)) {
      tryCatch({
        # Attempt to read the XML to check if it is well-formed
        xmlDoc <- xml2::read_xml(xmlPath)
        message("Integrity check: XML is valid and well-formed.")
      }, error = function(e) {
        warning("XML was generated but failed integrity validation: ", e$message)
      })
    } else {
      message("Install the 'xml2' package to enable advanced XML integrity validation.")
    }
  } else {
    stop("XML file was not found after the conversion process.")
  }

  return(invisible(xmlPath))
}
