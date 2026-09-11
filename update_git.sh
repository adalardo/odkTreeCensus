#!/bin/bash

# Verifica o status do repositório
echo "=== Status atual do Git ==="
git status

# Pergunta ao usuário se deseja prosseguir
read -p "Deseja adicionar todas as alterações e continuar? (s/n): " confirmar
if [[ "$confirmar" != "s" && "$confirmar" != "S" ]]; then
    echo "Operação cancelada."
    exit 0
fi

# Adiciona todas as alterações
git add .

# Solicita a mensagem de commit
read -p "Digite a mensagem do commit: " mensagem
if [ -z "$mensagem" ]; then
    echo "Erro: A mensagem de commit não pode ser vazia."
    exit 1
fi

# Realiza o commit
git commit -m "$mensagem"

# Detecta o branch atual automaticamente
BRANCH_ATUAL=$(git branch --show-current)

# Envia para o repositório remoto
echo "Enviando alterações para o branch '$BRANCH_ATUAL' no GitHub..."
git push origin "$BRANCH_ATUAL"

echo "=== Atualização concluída com sucesso! ==="
