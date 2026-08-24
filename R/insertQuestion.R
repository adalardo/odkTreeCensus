#' Inserir uma nova pergunta em um XLSForm
#'
#' Esta função insere uma nova pergunta (linha) na aba `survey` de um arquivo XLSForm (Excel),
#' permitindo especificar a posição exata por meio do nome de uma pergunta existente ou por um índice numérico.
#'
#' @param xlsformPath Caminho para o arquivo XLSForm (.xlsx) existente.
#' @param newQuestion Uma lista nomeada ou um `data.frame` de uma linha contendo as colunas da nova pergunta (ex: `type`, `name`, `label`, etc.).
#' @param afterName Caractere. O nome (`name`) da pergunta após a qual a nova pergunta deve ser inserida. Se não for encontrado ou for `NULL`, a função tentará usar o parâmetro `position`.
#' @param position Inteiro. A posição da linha (índice baseado em 1, relativo aos dados da aba `survey`) onde a pergunta deve ser inserida. Se `afterName` e `position` forem `NULL`, a pergunta será adicionada ao final.
#' @param outputPath Caminho para salvar o arquivo modificado. Se for `NULL` (padrão), o arquivo original em `xlsformPath` será sobrescrito.
#'
#' @return Retorna o caminho do arquivo salvo, invisivelmente.
#' @export
#'
#' @examples
#' \dontrun{
#' # Exemplo de nova pergunta como lista
#' nova_q <- list(
#'   type = "integer",
#'   name = "idade_anos",
#'   label = "Qual a sua idade em anos?",
#'   required = "yes"
#' )
#'
#' # Inserir após a pergunta "nome_completo"
#' insertQuestion(
#'   xlsformPath = "cadastro.xlsx",
#'   newQuestion = nova_q,
#'   afterName = "nome_completo",
#'   outputPath = "cadastro_atualizado.xlsx"
#' )
#' }
insertQuestion <- function(xlsformPath, newQuestion, afterName = NULL, position = NULL, outputPath = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("O pacote 'openxlsx' é necessário para executar esta função. Por favor, instale-o.")
  }

  if (!file.exists(xlsformPath)) {
    stop("O arquivo especificado em 'xlsformPath' não existe.")
  }

  # Carrega o workbook para preservar outras abas e formatações
  wb <- openxlsx::loadWorkbook(xlsformPath)
  
  # Verifica se a aba 'survey' existe
  if (!"survey" %in% names(wb)) {
    stop("A aba 'survey' não foi encontrada no arquivo XLSForm.")
  }

  # Lê os dados atuais da aba 'survey'
  survey_df <- openxlsx::readWorkbook(wb, sheet = "survey", detectDates = TRUE, skipEmptyRows = FALSE)

  # Converte a nova pergunta para data.frame se for lista
  if (is.list(newQuestion) && !is.data.frame(newQuestion)) {
    newQuestion <- as.data.frame(newQuestion, stringsAsFactors = FALSE)
  }

  # Alinha as colunas entre o data.frame original e a nova pergunta
  all_cols <- unique(c(names(survey_df), names(newQuestion)))
  
  # Adiciona colunas faltantes no survey_df com NA
  for (col in setdiff(all_cols, names(survey_df))) {
    survey_df[[col]] <- NA
  }
  # Adiciona colunas faltantes no newQuestion com NA
  for (col in setdiff(all_cols, names(newQuestion))) {
    newQuestion[[col]] <- NA
  }
  
  # Garante a mesma ordem de colunas
  newQuestion <- newQuestion[, names(survey_df), drop = FALSE]

  # Determina a posição de inserção (índice de linha nos dados)
  insert_idx <- NULL

  if (!is.null(afterName)) {
    if ("name" %in% names(survey_df)) {
      match_idx <- which(survey_df$name == afterName)
      if (length(match_idx) > 0) {
        insert_idx <- match_idx[1] # Insere logo após a primeira ocorrência encontrada
      } else {
        warning(paste("A pergunta de referência '", afterName, "' não foi encontrada. Usando parâmetro 'position' ou adicionando ao final.", sep = ""))
      }
    } else {
      warning("A coluna 'name' não foi encontrada na aba 'survey'. Não foi possível buscar por 'afterName'.")
    }
  }

  if (is.null(insert_idx)) {
    if (!is.null(position)) {
      insert_idx <- as.integer(position)
      # Garante limites aceitáveis
      if (insert_idx < 0) insert_idx <- 0
      if (insert_idx > nrow(survey_df)) insert_idx <- nrow(survey_df)
    } else {
      # Se ambos forem NULL, insere no final
      insert_idx <- nrow(survey_df)
    }
  }

  # Realiza a inserção da linha
  if (insert_idx == 0) {
    updated_survey <- rbind(newQuestion, survey_df)
  } else if (insert_idx >= nrow(survey_df)) {
    updated_survey <- rbind(survey_df, newQuestion)
  } else {
    updated_survey <- rbind(
      survey_df[1:insert_idx, , drop = FALSE],
      newQuestion,
      survey_df[(insert_idx + 1):nrow(survey_df), , drop = FALSE]
    )
  }

  # Limpa o conteúdo antigo da aba 'survey' para evitar resíduos se a tabela encolhesse (não é o caso, mas é boa prática)
  # E escreve os novos dados atualizados
  openxlsx::writeData(wb, sheet = "survey", x = updated_survey, startCol = 1, startRow = 1, colNames = TRUE)

  # Define o caminho de saída
  target_path <- if (is.null(outputPath)) xlsformPath else outputPath

  # Salva o arquivo
  openxlsx::saveWorkbook(wb, file = target_path, overwrite = TRUE)

  invisible(target_path)
}
