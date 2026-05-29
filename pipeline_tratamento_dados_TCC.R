# ======================================================
# 📊 PIPELINE COMPLETO DE LIMPEZA E PADRONIZAÇÃO COVID-19
# ======================================================

# Pacotes necessários
# install.packages("data.table")
library(data.table)
library(tidyverse)
library(lubridate)
library(readr)



# FUNÇÕES NECESSÁRIAS 

# para escala de Y
addUnits <- function(n) {
  ifelse(n < 1e3, n,
         ifelse(n < 1e6, paste0(round(n / 1e3), "k"),
                ifelse(n < 1e9, paste0(round(n / 1e6), "M"),
                       ifelse(n < 1e12, paste0(round(n / 1e9), "B"),
                              ifelse(n < 1e15, paste0(round(n / 1e12), "T"), "too big!")))))
}


# ------------------------------------------------------
# 1️⃣ LER TODOS OS ARQUIVOS DO PAINEL COVID-19
# ------------------------------------------------------
# setwd("C:/Users/milly/OneDrive/Área de Trabalho/Dados_covid_att") # ajuste o caminho se necessário
# 
# arquivos <- list.files(pattern = "^HIST_PAINEL_COVIDBR.*\\.csv$")
# dados_covid <- rbindlist(lapply(arquivos, fread), use.names = TRUE, fill = TRUE)
# 
# setwd("C:/Users/milly/Downloads/tcc_camilly")
# ------------------------------------------------------
# 2️⃣ FILTRAR APENAS BRASIL (Nível Nacional)
# ------------------------------------------------------
# # A base contém dados por município e estado; aqui filtramos o total nacional
# 
# dados_brasil <- dados_covid %>%
#   filter(regiao == "Brasil") %>%
#   select(data, casosNovos, obitosNovos) %>%
#   mutate(data = as.Date(data))
# 
# 
# 
# # ------------------------------------------------------
# # 3️⃣ LIMPEZA DOS DADOS
# # ------------------------------------------------------
# 
# # 🔹 Corrigir valores negativos (duplicidades, revisões de secretarias)
# # dados_brasil <- dados_brasil %>%
# #   mutate(
# #     casosNovos = ifelse(casosNovos < 0, 0, casosNovos),
# #     obitosNovos = ifelse(obitosNovos < 0, 0, obitosNovos)
# #   )
# # 
# # # 🔹 Remover valores extremos absurdos (ex: > 2 milhões em um dia)
# # limite_casos <- quantile(dados_brasil$casosNovos, 0.999, na.rm = TRUE)
# # dados_brasil <- dados_brasil %>%
# #   mutate(
# #     casosNovos = ifelse(casosNovos > limite_casos, casosNovos / 7, casosNovos)
# #   )
# 
# # ------------------------------------------------------
# # 4️⃣ PADRONIZAR EM BASE SEMANAL
# # ------------------------------------------------------
# # Cada semana começa na segunda-feira (por padrão do lubridate)
# 
# dados_semanais <- dados_brasil %>%
#   mutate(semana = floor_date(data, "week")) %>%
#   group_by(semana) %>%
#   summarise(
#     casosSemanais = sum(casosNovos, na.rm = TRUE),
#     obitosSemanais = sum(obitosNovos, na.rm = TRUE)) %>%
#   arrange(semana)
# 

dados_semanais <- read_csv("dados/dados_covid_brasil_semanal.csv")

# ------------------------------------------------------
# 5️⃣ VISUALIZAÇÃO
# ------------------------------------------------------

# 🔹 Gráfico de casos semanais
ggplot(dados_semanais, aes(x = semana, y = casosSemanais)) +
  geom_line(color = "blue", linewidth = 0.7) +
  labs(
    #title = "Número de Casos de COVID-19 no Brasil (base semanal)",
    #subtitle = "Dados do Painel COVID-19 — Ministério da Saúde",
    x = "Data (início da semana)",
    y = "Casos semanais", caption = "Fonte: Painel COVID-19 — Ministério da Saúde"
  ) +
  theme_minimal(base_size = 13)+
  scale_y_continuous(labels = addUnits)

# 🔹 Gráfico de óbitos semanais (opcional)
ggplot(dados_semanais, aes(x = semana, y = obitosSemanais)) +
  geom_line(color = "red", linewidth = 0.7) +
  labs(
    #title = "Número de Óbitos por COVID-19 no Brasil (base semanal)",
    #subtitle = "Dados do Painel COVID-19 — Ministério da Saúde",
    x = "Data (início da semana)",
    y = "Óbitos semanais", caption = "Fonte: Painel COVID-19 — Ministério da Saúde"
  ) +
  theme_minimal(base_size = 13)+
  scale_y_continuous(labels = addUnits)

# ------------------------------------------------------
# 6️⃣ SALVAR BASE LIMPA
# ------------------------------------------------------
setwd("C:/Users/milly/Downloads/tcc_camilly") # pasta onde salvar
write.csv(dados_semanais, "dados_covid_brasil_semanal.csv", row.names = FALSE)

# ======================================================


# ------------------------------------------------------
# 7 PADRONIZAR EM BASE MENSAL
# ------------------------------------------------------
# 

# dados_mensais <- dados_brasil %>%
#   mutate(mes = floor_date(data, "month")) %>%
#   group_by(mes) %>%
#   summarise(
#     casosMensais = sum(casosNovos, na.rm = TRUE),
#     obitosMensais = sum(obitosNovos, na.rm = TRUE)) %>%
#   arrange(mes)

dados_mensais <- read_csv("dados/dados_covid_brasil_mensal.csv")

# ------------------------------------------------------
# ⃣8  VISUALIZAÇÃO
# ------------------------------------------------------

# 🔹 Gráfico de casos semanais
ggplot(dados_mensais, aes(x = mes, y = casosMensais)) +
  geom_line(color = "sienna4", linewidth = 0.7) +
  labs(
    #title = "Número de Casos de COVID-19 no Brasil (base mensal)",
    #subtitle = "Dados do Painel COVID-19 — Ministério da Saúde",
    x = "Data (início do mês)",
    y = "Casos mensais", caption = "Fonte: Painel COVID-19 — Ministério da Saúde"
  ) + scale_y_continuous(labels = addUnits) +
  theme_minimal(base_size = 13)

# 🔹 Gráfico de óbitos semanais (opcional)
ggplot(dados_mensais, aes(x = mes, y = obitosMensais)) +
  geom_line(color = "orangered", linewidth = 0.7) +
  labs(
    #title = "Número de Óbitos por COVID-19 no Brasil (base mensal)",
    #subtitle = "Dados do Painel COVID-19 — Ministério da Saúde",
    x = "Data (início da mês)",
    y = "Óbitos mensais", caption = "Fonte: Painel COVID-19 — Ministério da Saúde"
  ) + scale_y_continuous(labels = addUnits) +
  theme_minimal(base_size = 13)

# ------------------------------------------------------
# 9 SALVAR BASE LIMPA
# ------------------------------------------------------
setwd("C:/Users/milly/Downloads/tcc_camilly")
write.csv(dados_mensais, "dados_covid_brasil_mensal.csv", row.names = FALSE)

# ======================================================
