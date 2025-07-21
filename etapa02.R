###############################################################################
# PROJETO FINAL – ANÁLISE DOS DADOS CNPq
# ETAPA 2 – ANÁLISE E VISUALIZAÇÃO (já tratando NAs)
###############################################################################

# 1. Pacotes
library(dplyr)
library(readr)
library(sf)
library(tidyr)       # para replace_na()
library(skimr)
library(psych)
library(ggplot2)
library(DT)
library(scales)      # Adicionado para formatação de eixos
library(geobr)       # Adicionado para dados geográficos de municípios

# 2. Leitura das bases
cnpq    <- read_csv("dados/cnpq.csv")
estados <- readRDS("dados/estados.rds")

# =============================================================================
# ATENÇÃO: Adicionar coluna de código de município (se não existir)
# Esta é uma etapa crucial para o mapa de municípios.
# Se sua base 'cnpq.csv' não possui uma coluna 'code_municipio_origem'
# ou similar, você precisará criá-la.
# Uma forma robusta seria cruzar 'instituicao_origem' com uma base de dados
# de instituições e seus municípios, ou usar geocodificação.
# Para este exemplo, vou SIMULAR a adição de uma coluna 'code_municipio_origem'
# para algumas instituições conhecidas do Amazonas, APENAS PARA FINS DE DEMONSTRAÇÃO.
# Você precisará adaptar isso para seus dados reais.
# Exemplo de códigos de município no AM: Manaus (1302603), Itacoatiara (1301902), Parintins (1303403)
# =============================================================================

# Exemplo SIMULADO de adição de code_municipio_origem:
# Isso é um PLACEHOLDER! Você precisará de uma forma real de mapear.
cnpq <- cnpq %>%
  mutate(
    # Exemplo: Mapeia instituições a códigos de município. MUITO SIMPLIFICADO!
    # Na vida real, você precisaria de um dicionário ou geocodificação.
    code_municipio_origem = case_when(
      grepl("UNIVERSIDADE FEDERAL DO AMAZONAS", instituicao_origem, ignore.case = TRUE) ~ 1302603, # Manaus
      grepl("INSTITUTO FEDERAL DE EDUCACAO CIENCIA E TECNOLOGIA DO AMAZONAS - CAMPUS ITACOATIARA", instituicao_origem, ignore.case = TRUE) ~ 1301902, # Itacoatiara
      grepl("UNIVERSIDADE DO ESTADO DO AMAZONAS - CAMPUS PARINTINS", instituicao_origem, ignore.case = TRUE) ~ 1303403, # Parintins
      TRUE ~ NA_real_ # Deixa NA para outros
    )
  )


# 3. Tratamento de valores ausentes
#    • Preenche categorias faltantes
#    • Remove registros sem valor financeiro
cnpq <- cnpq %>%
  mutate(
    linha_fomento     = replace_na(linha_fomento,     "Não informado"),
    area_conhecimento = replace_na(area_conhecimento, "Não informado")
  ) %>%
  filter(!is.na(valor))

# Opcional: relatório rápido de quantos NAs foram tratados
skim_without_charts(cnpq)

# 4. Estatísticas descritivas
skim(cnpq)
describeBy(cnpq$valor, group = cnpq$linha_fomento, mat = TRUE)

# 5. Histogramas e Boxplots
ggplot(cnpq, aes(x = valor)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribuição dos Valores das Bolsas (R$)", x = "Valor (R$)", y = "Frequência") +
  theme_minimal()

ggplot(cnpq, aes(x = linha_fomento, y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Valor das Bolsas por Linha de Fomento", x = "", y = "Valor (R$)") +
  theme_minimal()

# 6. Barras – Top 10 áreas e UFs
cnpq %>%
  count(area_conhecimento, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(reorder(area_conhecimento, n), n)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 Áreas de Conhecimento", x = "", y = "Quantidade") +
  theme_minimal()

cnpq %>%
  count(sigla_uf_origem, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(reorder(sigla_uf_origem, n), n)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 UFs de Origem", x = "", y = "Quantidade") +
  theme_minimal()

# 7. Tabela interativa – maiores bolsas
cnpq %>%
  arrange(desc(valor)) %>%
  select(processo, titulo_projeto, linha_fomento, valor) %>%
  head(10) %>%
  datatable(
    caption = "Top 10 Bolsas por Valor",
    options = list(pageLength = 5)
  )

# 8. Mapa coroplético por UF de origem (já existente)
uf_summary <- cnpq %>%
  filter(!is.na(sigla_uf_origem)) %>%
  group_by(sigla_uf_origem) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE))

map_data <- estados %>%
  left_join(uf_summary, by = c("abbrev_state" = "sigla_uf_origem"))

ggplot(map_data) +
  geom_sf(aes(fill = valor_medio), color = NA) +
  scale_fill_viridis_c(na.value = "grey80") +
  labs(
    title = "Valor Médio das Bolsas por UF de Origem",
    fill  = "Média (R$)"
  ) +
  theme_minimal()

# =============================================================================
# NOVAS SEÇÕES DE ANÁLISE E PLOTS - ADICIONE A PARTIR DAQUI
# =============================================================================

cat("\n✅ NOVAS ANÁLISES ADICIONADAS:\n")

# 9. Análise Aprofundada do Amazonas
# 9.1. Mapa Coroplético por Município no Amazonas (REQUER 'code_municipio_origem')
cat("  - Gerando mapa por município no Amazonas (requer 'code_municipio_origem')...\n")
# Filtrar os dados para o Amazonas
cnpq_am <- cnpq %>%
  filter(sigla_uf_origem == "AM")

# Obter dados geoespaciais dos municípios do Amazonas (código do AM é 13)
# Tenta carregar os dados geoespaciais. Se falhar, retorna NULL.
municipios_am <- tryCatch({
  # Para baixar todos os municípios de um ESTADO, você usa 'code_muni' com o prefixo do estado.
  # 13 é o código IBGE do Amazonas. O '0' adicionado é para completar o prefixo do estado.
  geobr::read_municipality(code_muni = 13, year = 2020)
}, error = function(e) {
  message("ATENÇÃO: Erro ao baixar dados de municípios do Amazonas. Verifique conexão ou versão do geobr. Erro: ", e$message)
  NULL
})


if (!is.null(municipios_am)) {
  # Resumir os dados do CNPq por município de origem no Amazonas
  uf_am_summary_municipio <- cnpq_am %>%
    filter(!is.na(code_municipio_origem)) %>% # Filtrar registros com code_municipio_origem válido
    group_by(code_municipio_origem) %>%
    summarise(valor_medio = mean(valor, na.rm = TRUE),
              num_bolsas = n())
  
  # Verificar se há dados para mapear após o summarise
  if (nrow(uf_am_summary_municipio) == 0) {
    cat("AVISO: Nenhuma bolsa com 'code_municipio_origem' válido encontrada para o Amazonas. O mapa não será gerado.\n")
  } else {
    map_data_am_municipio <- municipios_am %>%
      left_join(uf_am_summary_municipio, by = c("code_muni" = "code_municipio_origem"))
    
    # Mapa do Valor Médio por Município no AM
    print(ggplot(map_data_am_municipio) +
            geom_sf(aes(fill = valor_medio), color = "white", size=0.1) +
            scale_fill_viridis_c(na.value = "grey80", labels = scales::dollar_format(prefix="R$ ")) +
            labs(
              title = "Valor Médio das Bolsas por Município de Origem no Amazonas",
              fill  = "Média (R$)"
            ) +
            theme_void())
    
    # Mapa do Número de Bolsas por Município no AM
    print(ggplot(map_data_am_municipio) +
            geom_sf(aes(fill = num_bolsas), color = "white", size=0.1) +
            scale_fill_viridis_c(na.value = "grey80", labels = scales::comma) +
            labs(
              title = "Número de Bolsas por Município de Origem no Amazonas",
              fill  = "Nº de Bolsas"
            ) +
            theme_void())
  }
} else {
  cat("Não foi possível gerar mapas de município no AM. Verifique as mensagens de erro acima.\n")
}


# 10. Distribuição de Bolsas ao Longo do Tempo (por Ano)
cat("  - Gerando evolução anual do valor total e número de bolsas...\n")
cnpq %>%
  group_by(ano) %>%
  summarise(total_valor = sum(valor, na.rm = TRUE),
            num_bolsas = n()) %>%
  ggplot(aes(x = ano)) +
  geom_line(aes(y = total_valor), color = "blue", size = 1) +
  geom_point(aes(y = total_valor), color = "blue", size = 2) +
  geom_line(aes(y = num_bolsas), color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(y = num_bolsas), color = "red", size = 2) +
  scale_y_continuous(
    name = "Valor Total (R$)",
    labels = scales::dollar_format(prefix = "R$ "),
    sec.axis = sec_axis(~., name = "Número de Bolsas")
  ) +
  labs(
    title = "Evolução Anual do Valor Total e Número de Bolsas",
    x = "Ano"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  annotate("text", x = min(cnpq$ano) + 0.5, y = max(cnpq$valor, na.rm = TRUE) * 0.9,
           label = "Linha azul: Valor Total\nLinha vermelha: Número de Bolsas",
           hjust = 0, vjust = 1, color = "black") # Ajuste a posição do texto se necessário


# 11. Valor Médio por Área de Conhecimento (Top N)
cat("  - Gerando valor médio por área de conhecimento...\n")
top_areas_valor <- cnpq %>%
  filter(area_conhecimento != "Não informado") %>%
  group_by(area_conhecimento) %>%
  summarise(valor_total = sum(valor, na.rm = TRUE)) %>%
  arrange(desc(valor_total)) %>%
  slice_head(n = 15) %>%
  pull(area_conhecimento)

cnpq %>%
  filter(area_conhecimento %in% top_areas_valor) %>%
  group_by(area_conhecimento) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(area_conhecimento, valor_medio), y = valor_medio)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Valor Médio das Bolsas por Área de Conhecimento (Top 15 por Valor Total)",
    x = "Área de Conhecimento",
    y = "Valor Médio (R$)"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ")) +
  theme_minimal()

# 12. Scatter Plot: Valor vs. Duração do Processo
cat("  - Gerando scatter plot de valor vs. duração do processo...\n")
cnpq_com_duracao <- cnpq %>%
  mutate(
    data_inicio_processo = as.Date(data_inicio_processo),
    data_fim_processo = as.Date(data_fim_processo),
    duracao_dias = as.numeric(data_fim_processo - data_inicio_processo)
  ) %>%
  filter(!is.na(duracao_dias), duracao_dias > 0) # Remover NAs e durações inválidas

ggplot(cnpq_com_duracao, aes(x = duracao_dias, y = valor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Adiciona uma linha de regressão
  scale_x_log10(labels = scales::comma) + # Escala log para duração se houver grande variação
  scale_y_log10(labels = scales::dollar_format(prefix = "R$ ")) + # Escala log para valor
  labs(
    title = "Relação entre Valor da Bolsa e Duração do Processo",
    x = "Duração do Processo (dias, escala log)",
    y = "Valor da Bolsa (R$, escala log)"
  ) +
  theme_minimal()

# 13. Gráfico de Barras Empilhadas: Linha de Fomento por Ano
# 13. Gráfico de Barras Empilhadas: Proporção de Linha de Fomento por Ano
cat("  - Gerando gráfico de barras empilhadas de linha de fomento por ano...\n")

# Para evitar muitas categorias, vamos focar nas TOP N linhas de fomento
# e agrupar as demais como "Outras Linhas".
# Definir um limite para o número de linhas a serem exibidas (ex: Top 15)
top_n_linhas_fomento <- 15

# Calcular as principais linhas de fomento por número total de bolsas
principais_linhas <- cnpq %>%
  count(linha_fomento, sort = TRUE) %>%
  slice_head(n = top_n_linhas_fomento) %>%
  pull(linha_fomento)

cnpq %>%
  # Agrupar linhas de fomento que não estão nas principais como "Outras Linhas"
  mutate(linha_fomento_agrupada = ifelse(linha_fomento %in% principais_linhas,
                                         linha_fomento, "Outras Linhas")) %>%
  group_by(ano, linha_fomento_agrupada) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(ano) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = factor(ano), y = proportion, fill = linha_fomento_agrupada)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proporção de Bolsas por Linha de Fomento ao Longo dos Anos (Top 15)",
    x = "Ano",
    y = "Proporção",
    fill = "Linha de Fomento"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\n✅ ETAPA 2 concluída com análises adicionais.\n")
