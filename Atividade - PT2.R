library(readxl)
library(ggplot2)

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


resumo_periculosidade = c(summary(base$score_periculosidade))

resumo_periculosidade = as.data.frame(resumo_periculosidade)

resumo_idade = c(summary(base$idade))

resumo_idade = as.data.frame(resumo_idade)

resumo_tempo_preso = c(summary(base$tempo_preso))

resumo_tempo_preso = as.data.frame(resumo_tempo_preso)


# Gráfico de dispersão entre tempo_preso e score_periculosidade
graf1 = ggplot(base, aes(x = tempo_preso, y = score_periculosidade, color = sexo)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(
    title = "Tempo Preso x Score de Periculosidade por Sexo",
    x = "Tempo Preso",
    y = "Score de Periculosidade",
    color = "Sexo"
  ) +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")

correlacao_tempo_periculosidade = cor(x = base$tempo_preso, y = base$score_periculosidade)

estatisticas = data.frame(
  Variavel = c("score_periculosidade", "idade", "tempo_preso"),
  Variancia = c(
    var(base$score_periculosidade, na.rm = TRUE),
    var(base$idade, na.rm = TRUE),
    var(base$tempo_preso, na.rm = TRUE)
  ),
  Desvio_Padrao = c(
    sd(base$score_periculosidade, na.rm = TRUE),
    sd(base$idade, na.rm = TRUE),
    sd(base$tempo_preso, na.rm = TRUE)
  ),
  Amplitude = c(
    max(base$score_periculosidade, na.rm = TRUE) - min(base$score_periculosidade, na.rm = TRUE),
    max(base$idade, na.rm = TRUE) - min(base$idade, na.rm = TRUE),
    max(base$tempo_preso, na.rm = TRUE) - min(base$tempo_preso, na.rm = TRUE)
  )
)

estatisticas

