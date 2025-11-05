# Questão 1

# Commit é uma operação de atualização do código para o repositório.

# Questão 2 - Criado

# Questão 3 - Clonado

# Questão 4

### Ativando pacotes necessários

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Definindo pasta de trabalho

setwd("/home/aluno/Área de Trabalho/AnaliseExploratoria_")

# lendo base

base = read_excel("Base_trabalho.xlsx")

##### Usando o dicionário

base = base |>
  mutate(sexo = case_when(
    sexo == 1 ~ "Masculino",
    sexo == 0 ~ "Feminino",
    TRUE ~ NA_character_
  ))

base = base |>
  mutate(filhos = case_when(
    filhos == 1 ~ "Sim",
    filhos == 0 ~ "Não",
    TRUE ~ NA_character_
  ))

base = base |>
  mutate(escolaridade = case_when(
    escolaridade == 1 ~ "Fundamental",
    escolaridade == 2 ~ "Médio",
    escolaridade == 3 ~ "Superior",
    TRUE ~ NA_character_
  ))

base = base |>
  mutate(casado = case_when(
    casado == 1 ~ "Sim",
    casado == 0 ~ "Não",
    TRUE ~ NA_character_
  ))

# Essa próxima etapa não está no dicionário, mas segui a lógica binária
# das variáveis anteriores;

base = base |>
  mutate(reincidente = case_when(
    reincidente == 1 ~ "Sim",
    reincidente == 0 ~ "Não",
    TRUE ~ NA_character_
  ))


# Questão 5
## A

base$escolaridade = as.factor(base$escolaridade)
base$reincidente = as.factor(base$reincidente)
base$filhos = as.factor(base$filhos)
base$casado = as.factor(base$casado)
base$sexo = as.factor(base$sexo)
class(base$escolaridade)
class(base$reincidente)
class(base$filhos)
class(base$casado)
class(base$sexo)

## B
# Com a benção divina e o bom coração de vocês, não há dados faltantes

## C - Histograma básico variável idade

graf1 = ggplot(base, aes(x = idade)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_text(stat = "bin", binwidth = 5, aes(label = after_stat(count)),
            vjust = -0.5, color = "black") +    # Adiciona rótulo nos dados
  labs(
    title = "Frequência por Idade",
    x = "Idade",
    y = "Frequência"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
  )

## D - Boxplot tempo preso

graf2 = ggplot(base, aes(y = tempo_preso)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    title = "Análise de Outliers",
    y = "Tempo Preso"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       # Centraliza o título
    axis.text.x = element_blank(),           # Remove os valores do eixo X
    axis.ticks.x = element_blank()           # Remove as marcações do eixo X
  )

## E - Periculosidade

graf3 = ggplot(base, aes(x = escolaridade, y = score_periculosidade)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    title = "Periculosidade por Escolaridade",
    x = "Escolaridade",
    y = "Score de Periculosidade"
  ) +
  theme_minimal() +
  theme()

## F - Reincidência

graf4 = ggplot(base, aes(x = reincidente)) +
  geom_bar(fill = "steelblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, color = "black") +    # Adiciona rótulo nos dados
  labs(
    title = "Distribuição de Reincidência",
    x = "Reincidente",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
  )

## G - Salvando em PDF

# Abrir arquivo PDF
pdf("graficos.pdf", width = 8, height = 6)

grid.arrange(graf1)
grid.arrange(graf2)
grid.arrange(graf3)
grid.arrange(graf4)

# Fechar o PDF
dev.off()
