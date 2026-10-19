# ====================================================================
# Funções auxiliares internas do pacote odkTreeCensus
# ====================================================================
#
# Este arquivo reúne as funções auxiliares (helpers) usadas
# internamente pelas funções públicas do pacote. Todas essas funções
# são de uso interno e, por convenção, usam o prefixo "." (ponto) no
# nome, sinalizando que não fazem parte da API pública do pacote.
#
# Organização:
#   1. Helpers de leitura de instâncias XML (usados por readInstancesXML)
#   2. Helper de conversão XLSForm -> XML (usado por xmlForm)
#   3. Helpers de mesclagem de CSV (usados por mergeCSV)


# --------------------------------------------------------------------
# 1. Helpers de leitura de instâncias XML (readInstancesXML)
# --------------------------------------------------------------------

#' Derive the form base name from a submission file name
#'
#' ODK Collect names submission files after the form plus a
#' `_YYYY-MM-DD_HH-MM-SS` timestamp, e.g.
#' `treeCensusForm_2026-10-06_16-38-57.xml`. This helper strips the
#' extension and the timestamp, returning `"treeCensusForm"`.
#'
#' @param fname A file name (basename) of a submission XML.
#' @return A character scalar with the derived base name.
#' @keywords internal
.deriveFormBase <- function(fname) {
  fname <- sub("\\.xml$", "", fname, ignore.case = TRUE)
  sub("_[0-9]{4}-[0-9]{2}-[0-9]{2}[_-][0-9]{2}-[0-9]{2}-[0-9]{2}.*$", "", fname)
}


#' Try to locate the XForm XML for a submission
#'
#' The most reliable key is the form id carried by the submission root
#' (`<data id="...">`), which is independent of the user-supplied
#' `formBase`. This function searches for `<formId>.xml` (and, as a
#' fallback, `<formBase>.xml`) in a few conventional locations: the
#' `instances` directory itself, a sibling `forms/` directory, the parent
#' directory of `instances`, the package `odk` directory and a local
#' `inst/odk` directory. As a last resort it searches recursively under
#' the parent directory of `instances`.
#'
#' @param instancesDir Path to the ODK Collect `instances` directory.
#' @param formBase Base name for the output CSVs (used as a fallback
#'   search key only).
#' @param formId Optional form id read from the submission root; when
#'   provided it takes precedence over `formBase` as the search key.
#' @param hint Optional explicit path to try first.
#' @return A path to the found XML, or NULL if none is found.
#' @keywords internal
.findFormFile <- function(instancesDir, formBase, formId = NULL, hint = NULL) {
  cand <- character(0)
  if (!is.null(hint) && nzchar(hint)) cand <- c(cand, hint)

  keys <- unique(c(formId, formBase))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) return(NULL)

  parent <- dirname(instancesDir)

  pkgDirs <- character(0)
  pkgDir <- system.file("odk", package = "odkTreeCensus")
  if (nzchar(pkgDir) && dir.exists(pkgDir)) pkgDirs <- c(pkgDirs, pkgDir)
  localOdk <- file.path(getwd(), "inst", "odk")
  if (dir.exists(localOdk)) pkgDirs <- c(pkgDirs, localOdk)

  for (nm in keys) {
    fname <- paste0(nm, ".xml")
    cand <- c(cand,
              file.path(instancesDir, fname),
              file.path(parent, "forms", fname),
              file.path(parent, fname),
              file.path(pkgDirs, fname))
  }

  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) return(cand[1])

  ## Last resort: recursive search under the parent of `instances`
  allxml <- list.files(parent, pattern = "\\.xml$",
                       recursive = TRUE, full.names = TRUE)
  if (length(allxml) > 0) {
    instancesNorm <- normalizePath(instancesDir, winslash = "/", mustWork = FALSE)
    allNorm <- normalizePath(allxml, winslash = "/", mustWork = FALSE)
    allxml <- allxml[!startsWith(allNorm, instancesNorm)]
    for (nm in keys) {
      hit <- allxml[basename(allxml) == paste0(nm, ".xml")]
      if (length(hit) > 0) return(hit[1])
    }
  }

  NULL
}


#' Detect repeat paths in an XForm (or, as a fallback, in an instance)
#'
#' A form's repeat structure is authoritative and must be read from the
#' **XForm**. This function first reads the `nodeset` of every
#' `<repeat>` element in the form body (preferred). If the form does
#' not contain such elements, it falls back to finding `jr:template`
#' markers inside the form's primary instance. As a last resort (when
#' passed a submission document instead of a form), it looks for
#' `jr:template` directly, which only works with old-style instances.
#'
#' @param formDoc An `xml2` document: the XForm (preferred) or, as a
#'   fallback, a submission instance.
#' @return A character vector of repeat paths relative to the `/data`
#'   root, ordered from the shallowest to the deepest.
#' @keywords internal
.detectRepeatPaths <- function(formDoc) {
  ## Strategy 1: <repeat nodeset="..."> elements in the form body
  reps <- xml2::xml_find_all(formDoc, "//*[local-name()='repeat']")
  nodesets <- xml2::xml_attr(reps, "nodeset")
  nodesets <- nodesets[!is.na(nodesets) & nzchar(nodesets)]
  if (length(nodesets) > 0) {
    paths <- sub("^/data/?", "", nodesets)
    paths <- unique(paths[nzchar(paths)])
    return(.sortByDepth(paths))
  }

  ## Strategy 2: jr:template in the primary instance of the form
  insts <- xml2::xml_find_all(formDoc, "//*[local-name()='instance']")
  prim  <- NULL
  for (inst in insts) {
    a <- xml2::xml_attrs(inst)
    if (!any(names(a) %in% c("id", "src"))) {
      prim <- inst
      break
    }
  }
  if (is.null(prim)) {
    ## Strategy 3 (fallback): jr:template anywhere in the passed document
    tpls <- xml2::xml_find_all(formDoc, "//*[@*[local-name()='template']]")
  } else {
    tpls <- xml2::xml_find_all(prim, ".//*[@*[local-name()='template']]")
  }
  if (length(tpls) == 0) return(character(0))

  paths <- vapply(tpls, function(n) {
    p <- xml2::xml_path(n)
    p <- sub("^.*?/data(?:/|$)", "", p)
    p
  }, character(1))
  paths <- unique(paths[nzchar(paths)])
  .sortByDepth(paths)
}


#' Sort repeat paths from shallowest to deepest
#'
#' @param paths Character vector of slash-separated paths.
#' @return The same paths reordered by depth (and then alphabetically),
#'   so that parents always come before their children.
#' @keywords internal
.sortByDepth <- function(paths) {
  if (length(paths) == 0) return(paths)
  d <- vapply(strsplit(paths, "/", fixed = TRUE), length, integer(1))
  paths[order(d, paths)]
}


#' Identify the "direct" child repeats of a given repeat level
#'
#' A repeat `rp2` is a direct child of `rp1` when `rp1` is a proper
#' prefix of `rp2` and there is no other repeat strictly between them.
#' Non-repeat groups (e.g. `cobertura`) are transparently skipped, so
#' `subquad/cobertura/rep_cover` is considered a direct child of
#' `subquad`.
#'
#' @param depthPath The repeat path of the current level (`""` for the
#'   root record).
#' @param repeatPaths All repeat paths in the form.
#' @return A character vector of direct child repeat paths.
#' @keywords internal
.childRepeats <- function(depthPath, repeatPaths) {
  desc <- if (!nzchar(depthPath)) {
    repeatPaths
  } else {
    repeatPaths[startsWith(repeatPaths, paste0(depthPath, "/"))]
  }
  if (length(desc) == 0) return(character(0))

  ## Candidate "intermediate" repeats MUST be strict descendants of
  ## depthPath. Ancestors of depthPath MUST be excluded: otherwise,
  ## for a deep rp such as "subquad/tree/sec_stem", the ancestor
  ## "subquad" would still be in the candidate pool, and
  ## startsWith(rp, "subquad/") would be TRUE, wrongly classifying
  ## that rp as a non-direct child of "subquad/tree". That bug made
  ## the walk skip deeper repeats entirely (e.g. sec_stem and
  ## sec_stem_miss), producing empty CSVs for them.
  candPool <- desc

  keep <- vapply(desc, function(rp) {
    cand <- setdiff(candPool, rp)
    if (length(cand) == 0) return(TRUE)
    !any(startsWith(rp, paste0(cand, "/")))
  }, logical(1))

  desc[keep]
}


#' Compute a repeat path relative to the current record level
#'
#' @param depthPath The repeat path of the current level (`""` for the
#'   root record).
#' @param rp An absolute (relative to `/data`) repeat path.
#' @return The portion of `rp` below `depthPath`.
#' @keywords internal
.relativePath <- function(depthPath, rp) {
  if (!nzchar(depthPath)) return(rp)
  pref <- paste0(depthPath, "/")
  if (startsWith(rp, pref)) substring(rp, nchar(pref) + 1L) else rp
}


#' Return the last segment of a slash-separated path
#'
#' @param path A slash-separated path.
#' @return The last element name.
#' @keywords internal
.lastSegment <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts[length(parts)]
}


#' Find all XML elements matching a slash-separated relative path
#'
#' Navigates from `node` along `relPath` (e.g. `"tree"`,
#' `"cobertura/rep_cover"`) and returns every element reached. When a
#' level matches multiple elements (e.g. several `cobertura` groups),
#' all combinations below are returned.
#'
#' @param node The starting XML element.
#' @param relPath A slash-separated path relative to `node`.
#' @return A list of matching XML elements (possibly empty).
#' @keywords internal
.findElementsAtPath <- function(node, relPath) {
  parts <- strsplit(relPath, "/", fixed = TRUE)[[1]]
  current <- list(node)
  for (p in parts) {
    nxt <- list()
    for (nd in current) {
      kids <- xml2::xml_children(nd)
      if (length(kids) == 0) next
      knames <- xml2::xml_name(kids)
      hits <- kids[knames == p]
      if (length(hits) > 0) {
        nxt <- c(nxt, as.list(hits))
      }
    }
    current <- nxt
    if (length(current) == 0) return(list())
  }
  current
}


#' Walk one record of an ODK instance and register it (plus any repeats)
#'
#' The accumulator `records` is a plain named list that is passed down
#' and returned back up by the recursion. Using a functional style avoids
#' the pitfalls of mutating a nested list through an environment, which
#' previously left the root table empty. The root record is stored under
#' the internal sentinel key `".root"` (see `ROOT_KEY` in
#' `readInstancesXML`).
#'
#' @param node The XML element representing the record.
#' @param thisKey The KEY assigned to this record.
#' @param parentKey The KEY of the parent record ("" for the root).
#' @param depthPath The repeat path of this record ("" for the root).
#' @param records Named list of lists of named character vectors
#'   (one entry per observation). Returned, updated.
#' @param repeatPaths Character vector of all repeat paths in the form.
#' @param instanceID Submission id, added to every row as a column.
#' @return The updated `records` list.
#' @keywords internal
.walkRecord <- function(node, thisKey, parentKey, depthPath,
                        records, repeatPaths, instanceID = "") {
  ## 1. Direct repeat children for this record (repeats may be nested
  ##    under non-repeat groups, e.g. cobertura/rep_cover).
  directRepeats <- .childRepeats(depthPath, repeatPaths)

  ## 2. Collect leaf values (stopping at ANY repeat boundary)
  rec <- .collectLeafValues(node, depthPath = depthPath, repeatPaths = repeatPaths)
  rec["KEY"] <- thisKey
  if (nzchar(parentKey)) rec["PARENT_KEY"] <- parentKey
  if (nzchar(instanceID)) rec["instanceID"] <- instanceID

  ## 3. Register this record. The root uses the internal sentinel
  ##    ".root" instead of "" to avoid ambiguous empty-string indexing.
  storagePath <- if (nzchar(depthPath)) depthPath else ".root"
  records[[storagePath]] <- c(records[[storagePath]], list(rec))

  ## 4. Recurse into direct repeat children
  if (length(directRepeats) > 0) {
    for (rp in directRepeats) {
      relPath <- .relativePath(depthPath, rp)
      matches <- .findElementsAtPath(node, relPath)
      rnName  <- .lastSegment(rp)
      for (i in seq_along(matches)) {
        childKey <- paste0(thisKey, "/", rnName, "[", i, "]")
        records <- .walkRecord(matches[[i]],
                               thisKey      = childKey,
                               parentKey    = thisKey,
                               depthPath    = rp,
                               records      = records,
                               repeatPaths  = repeatPaths,
                               instanceID   = instanceID)
      }
    }
  }

  records
}


#' Collect leaf values (dot-separated paths) under an XML element
#'
#' Recurses through the tree, producing a named character vector where
#' names are dot-separated paths (e.g. `"parcela.quadrat"`). Any
#' descendant whose absolute path is a repeat boundary (see
#' `repeatPaths`) is not entered: those elements belong to their own
#' records and are handled by `.walkRecord`.
#'
#' @param node The XML element (a record).
#' @param depthPath The repeat path of the current record (`""` for the
#'   root).
#' @param repeatPaths All repeat paths in the form.
#' @param namePrefix Dot-separated prefix for column names (internal).
#' @param pathPrefix Slash-separated prefix for path comparison
#'   (internal).
#' @return A named character vector.
#' @keywords internal
.collectLeafValues <- function(node, depthPath, repeatPaths,
                               namePrefix = "", pathPrefix = "") {
  out <- character(0)
  kids <- xml2::xml_children(node)
  for (k in kids) {
    nm <- xml2::xml_name(k)

    nameFull  <- if (nzchar(namePrefix)) paste0(namePrefix, ".", nm) else nm
    pathFull  <- if (nzchar(pathPrefix)) paste0(pathPrefix, "/", nm) else nm
    fullRepeat <- if (nzchar(depthPath)) paste0(depthPath, "/", pathFull) else pathFull

    if (fullRepeat %in% repeatPaths) next

    grand <- xml2::xml_children(k)
    if (length(grand) == 0) {
      val <- xml2::xml_text(k)
      out[nameFull] <- if (is.na(val)) "" else val
    } else {
      sub <- .collectLeafValues(k,
                                depthPath   = depthPath,
                                repeatPaths = repeatPaths,
                                namePrefix  = nameFull,
                                pathPrefix  = pathFull)
      out <- c(out, sub)
    }
  }
  out
}


# --------------------------------------------------------------------
# 2. Helper de conversão XLSForm -> XML (xmlForm)
# --------------------------------------------------------------------

#' @noRd
setupPyEnv <- function() {
  pkg_name <- "odkTreeCensus"
  # 1. Tenta o caminho de desenvolvimento (pasta local do projeto)
  dev_path <- file.path("inst", "python")

  # 2. Se a pasta local 'inst/python' existir (modo dev), usa ela.
  # Caso contrário, usa o caminho padrão do pacote instalado.
  if (dir.exists(dev_path)) {
    wheels_dir <- dev_path
  } else {
    wheels_dir <- system.file("python", package = pkg_name)
  }

  # Validação simples
  if (wheels_dir == "" || !dir.exists(wheels_dir)) {
    stop("Erro: A pasta 'inst/python' (dev) ou 'python' (instalado) não foi encontrada.")
  }

  # Caminho para salvar o virtualenv no cache do usuário
  venv_path <- file.path(tools::R_user_dir(pkg_name, "cache"), "python_env")

  # Instalação offline caso o ambiente Python não exista no cache
  if (!reticulate::virtualenv_exists(envname = venv_path)) {
    message("Criando ambiente Python offline...")
    reticulate::virtualenv_create(envname = venv_path)

    wheels <- list.files(wheels_dir, pattern = "\\.whl$", full.names = TRUE)

    if (length(wheels) == 0) {
      stop(paste("Nenhum arquivo .whl encontrado em:", wheels_dir))
    }

    # Instala os wheels sem usar a internet
    reticulate::py_install(
      packages = wheels,
      envname = venv_path,
      pip_options = c("--no-index", paste0("--find-links=", wheels_dir))
    )
  }

  # Ativa o virtualenv na sessão
  reticulate::use_virtualenv(virtualenv = venv_path, required = TRUE)
}


# --------------------------------------------------------------------
# 3. Helpers de mesclagem de CSV (mergeCSV)
# --------------------------------------------------------------------
#
# Estas funções eram definidas como closures locais dentro de mergeCSV.
# Ao serem promovidas para o nível do pacote, recebem por parâmetro
# tudo o que antes vinha capturado pelo ambiente de mergeCSV:
#   - csvDir, csvFiles, dataList, keyEnv, etc.

#' @noRd
.tidyName <- function(x) sub("^.*\\.", "", x)


#' @noRd
.round1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_real_, round(x, 1))
}


#' @noRd
.asMediaPath <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & nzchar(x)
  x[ok] <- file.path("media", x[ok])
  x
}


#' @noRd
.isEmptyCsvFile <- function(path) {
  sz <- suppressWarnings(file.size(path))
  if (is.na(sz) || sz <= 0L) return(TRUE)

  bytes <- tryCatch(readBin(path, what = "raw", n = sz),
                    error = function(e) raw(0))
  if (length(bytes) == 0L) return(TRUE)

  ## Strip a leading UTF-8 BOM (EF BB BF)
  if (length(bytes) >= 3L &&
      bytes[1L] == as.raw(0xEF) &&
      bytes[2L] == as.raw(0xBB) &&
      bytes[3L] == as.raw(0xBF)) {
    bytes <- bytes[-(1L:3L)]
  }
  if (length(bytes) == 0L) return(TRUE)

  ## Only whitespace bytes left -> treat as empty
  all(as.integer(bytes) %in% c(0x09L, 0x0AL, 0x0DL, 0x20L))
}


#' @noRd
.readOne <- function(f, csvDir) {
  path <- file.path(csvDir, f)

  if (.isEmptyCsvFile(path)) {
    return(data.frame())
  }

  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) character(0))
  lines <- lines[!is.na(lines)]
  if (length(lines) > 0L) {
    lines[1L] <- sub("^\xef\xbb\xbf", "", lines[1L], useBytes = TRUE)
  }
  lines <- lines[vapply(lines, function(x) nzchar(trimws(x)), logical(1))]

  if (length(lines) == 0L) {
    return(data.frame())
  }

  d <- tryCatch(
    utils::read.csv(text = paste(lines, collapse = "\n"),
                    stringsAsFactors = FALSE,
                    check.names      = FALSE),
    error = function(e) data.frame()
  )

  if (ncol(d) > 0L) names(d) <- .tidyName(names(d))
  d
}


#' @noRd
.getCol <- function(d, nm, default = NA) {
  if (nm %in% names(d)) return(d[[nm]])
  rep(default, nrow(d))
}


#' @noRd
.bindRows <- function(lst) {
  lst <- lst[vapply(lst, function(x) !is.null(x) && nrow(x) > 0L, logical(1))]
  if (length(lst) == 0L) return(data.frame())
  allCols <- unique(unlist(lapply(lst, names)))
  lst <- lapply(lst, function(d) {
    miss <- setdiff(allCols, names(d))
    for (m in miss) d[[m]] <- NA
    d[, allCols, drop = FALSE]
  })
  do.call(rbind, lst)
}


#' @noRd
.readCsvSafe <- function(path) {
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
}


#' @noRd
.baseName <- function(f) gsub("-.*|\\.csv", "", f)


#' @noRd
.readMergedBaseNames <- function(path) {
  if (!file.exists(path)) return(character(0))
  oldFiles <- .readCsvSafe(path)
  if (nrow(oldFiles) == 0L) return(character(0))
  colOld <- if ("fileNames" %in% names(oldFiles)) {
    as.character(oldFiles$fileNames)
  } else {
    as.character(oldFiles[[1L]])
  }
  colOld[!is.na(colOld) & nzchar(colOld)]
}


#' @noRd
.ancestorValue <- function(parentKey, field, keyEnv) {
  cur <- as.character(parentKey)
  hops <- 0L
  while (!is.na(cur) && nzchar(cur) && hops < 100L) {
    entry <- keyEnv[[cur]]
    if (is.null(entry)) break
    row <- entry$row
    if (field %in% names(row)) {
      v <- row[[field]][1]
      if (!is.na(v) && nzchar(as.character(v))) return(v)
    }
    if ("PARENT_KEY" %in% names(row)) {
      cur <- as.character(row$PARENT_KEY[1])
    } else {
      break
    }
    hops <- hops + 1L
  }
  NA_character_
}


#' @noRd
.fieldSelfOrAncestor <- function(d, field, keyEnv) {
  self <- if (field %in% names(d)) as.character(d[[field]]) else
    rep(NA_character_, nrow(d))
  pk <- if ("PARENT_KEY" %in% names(d)) as.character(d$PARENT_KEY) else
    rep(NA_character_, nrow(d))
  for (i in seq_along(self)) {
    if (is.na(self[i]) || !nzchar(self[i])) {
      self[i] <- as.character(.ancestorValue(pk[i], field, keyEnv))
    }
  }
  self
}


#' @noRd
.contextDf <- function(d, keyEnv) {
  n <- nrow(d)
  data.frame(
    date    = rep(format(Sys.Date()), n),
    plot    = .fieldSelfOrAncestor(d, "plot_name",   keyEnv),
    quad    = .fieldSelfOrAncestor(d, "quadrat",     keyEnv),
    subquad = .fieldSelfOrAncestor(d, "sel_subquad", keyEnv),
    team    = .fieldSelfOrAncestor(d, "equipe_nomes", keyEnv),
    stringsAsFactors = FALSE
  )
}


#' @noRd
.pick <- function(pattern, csvFiles) {
  csvFiles[grepl(pattern, csvFiles, ignore.case = TRUE)]
}


#' @noRd
.getOne <- function(f, dataList) dataList[[f]]


#' @noRd
.writeOneWithMerge <- function(df, dirOut, name) {
  path <- file.path(dirOut, name)

  oldExists <- file.exists(path)
  old <- if (oldExists) .readCsvSafe(path) else data.frame()
  nOld <- nrow(old)

  ## Drop duplicated rows from the incoming data itself
  if (nrow(df) > 0L) {
    df <- df[!duplicated(df), , drop = FALSE]
  }

  if (oldExists) {
    ## Keep everything that already exists and add only the new rows
    merged <- .bindRows(list(old, df))
    if (ncol(merged) == 0L) merged <- df
    if (nrow(merged) > 0L) {
      merged <- merged[!duplicated(merged), , drop = FALSE]
    }
    utils::write.csv(merged, path, row.names = FALSE, na = "")
    nTotal <- nrow(merged)
    nAdded <- max(0L, nTotal - nOld)
    message(sprintf("  - %s: mesclado (%d + %d = %d registro(s)).",
                    name, nOld, nAdded, nTotal))
    return(list(file = name, old = nOld, new = nAdded, total = nTotal,
                action = "merge"))
  }

  utils::write.csv(df, path, row.names = FALSE, na = "")
  nNew <- nrow(df)
  message(sprintf("  - %s: criado (%d registro(s)).", name, nNew))
  list(file = name, old = 0L, new = nNew, total = nNew, action = "create")
}
