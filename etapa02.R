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
library(ggrepel)     # Adicionado para evitar sobreposição de texto em plots

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
#     • Preenche categorias faltantes
#     • Remove registros sem valor financeiro
cnpq <- cnpq %>%
  mutate(
    linha_fomento       = replace_na(linha_fomento,       "Não informado"),
    area_conhecimento = replace_na(area_conhecimento, "Não informado")
  ) %>%
  filter(!is.na(valor))

# Opcional: relatório rápido de quantos NAs foram tratados
skim_without_charts(cnpq)

# 4. Estatísticas descritivas
skim(cnpq)
describeBy(cnpq$valor, group = cnpq$linha_fomento, mat = TRUE)

# 5. Histogramas e Boxplots
# Histograma
ggplot(cnpq, aes(x = valor)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  scale_x_log10(
    labels = dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
    breaks = scales::log_breaks(n = 5)
  ) +
  labs(
    x = "Valor (R$) — escala log10",
    y = "Frequência"
  ) +
  theme_minimal()

# Boxplot
# Filtra as 20 principais linhas para uma melhor visualização
top_linhas <- cnpq %>% count(linha_fomento, sort = TRUE) %>% slice_max(n, n = 20) %>% pull(linha_fomento)

cnpq %>%
  filter(linha_fomento %in% top_linhas) %>%
  ggplot(aes(x = reorder(linha_fomento, valor, FUN=median), y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Linha de Fomento", y = "Valor (R$)") +
  theme_minimal()

# 6. Barras – Top 10 áreas e UFs
# Top 10 Áreas de Conhecimento com maior número de bolsas
cnpq %>%
  filter(area_conhecimento != "Não informado") %>%
  count(area_conhecimento, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(area_conhecimento, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Área de Conhecimento", y = "Número de Bolsas") +
  theme_minimal()

# Top 10 Áreas com Menor Número de Bolsas
cnpq %>%
  filter(area_conhecimento != "Não informado") %>%
  count(area_conhecimento, sort = TRUE) %>%
  slice_min(n, n = 10) %>%
  ggplot(aes(x = reorder(area_conhecimento, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    x = "Área de Conhecimento",
    y = "Número de Bolsas",
    title = "Top 10 Áreas com Menor Número de Bolsas"
  ) +
  theme_minimal(base_size = 12)

# Top 10 UFs de Origem com maior número de bolsistas
cnpq %>%
  filter(!is.na(sigla_uf_origem)) %>%
  count(sigla_uf_origem, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(sigla_uf_origem, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(x = "UF de Origem", y = "Número de Bolsas") +
  theme_minimal()

# 7. Tabela interativa – maiores bolsas (adaptado para outliers)
q1  <- quantile(cnpq$valor, 0.25, na.rm = TRUE)
q3  <- quantile(cnpq$valor, 0.75, na.rm = TRUE)
iqr <- q3 - q1

lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

cnpq <- cnpq %>%
  mutate(outlier = case_when(
    valor < lim_inf ~ "Inferior",
    valor > lim_sup ~ "Superior",
    TRUE            ~ "Normal"
  ))

datatable(
  cnpq %>% filter(outlier != "Normal") %>%
    select(ano, sigla_uf_origem, titulo_projeto, valor, outlier),
  options = list(pageLength = 10),
  caption = "Tabela: Outliers Inferiores e Superiores nos Valores das Bolsas."
)


# 8. Mapa coroplético por UF de origem (já existente)
uf_summary <- cnpq %>%
  filter(!is.na(sigla_uf_origem)) %>%
  group_by(sigla_uf_origem) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE))

map_data <- estados %>%
  left_join(uf_summary, by = c("abbrev_state" = "sigla_uf_origem"))

ggplot(map_data) +
  geom_sf(aes(fill = valor_medio), color = "white", size=0.1) +
  scale_fill_viridis_c(na.value = "grey80", labels = scales::dollar_format(prefix="R$ ")) +
  labs(fill = "Média (R$)") +
  theme_void()

# 9. Teste de Hipótese (Exemplo)
# Prepara os dados e realiza o teste
sub_ns <- cnpq %>%
  filter(sigla_uf_origem %in% c("AM","SP","RJ","MG","ES")) %>%
  mutate(
    regiao = ifelse(sigla_uf_origem == "AM", "Norte", "Sudeste")
  )
t_ns <- t.test(valor ~ regiao, data = sub_ns)
# As conclusões do teste t seriam apresentadas no Rmarkdown, não diretamente no script R.

# =============================================================================
# NOVAS SEÇÕES DE ANÁLISE E PLOTS - ADICIONE A PARTIR DAQUI
# =============================================================================

cat("\n✅ NOVAS ANÁLISES ADICIONADAS:\n")

# 10. Análise Aprofundada do Amazonas
# 10.1. Mapa Coroplético por Município no Amazonas (REQUER 'code_municipio_origem')
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

# 10.2. Distribuição de Bolsas ao Longo do Tempo (por Ano)
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


# 10.3. Valor Médio por Área de Conhecimento (Top N)
cat("  - Gerando valor médio por área de conhecimento...\n")
top_areas_valor <- cnppq %>%
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

# 10.4. Scatter Plot: Valor vs. Duração do Processo
cat("  - Gerando scatter plot de valor vs. duração do processo...\n")
cnpq_com_duracao <- cnppq %>%
  mutate(
    data_inicio_processo = as.Date(data_inicio_processo),
    data_fim_processo = as.Date(data_fim_processo),
    duracao_dias = as.numeric(data_fim_processo - data_inicio_processo)
  ) %>%
  filter(!is.na(duracao_dias), duracao_dias > 0) # Remover NAs e durações inválidas

# Cálculo dos limites para detectar outliers
q1_valor <- quantile(cnpq_com_duracao$valor, 0.25, na.rm = TRUE)
q3_valor <- quantile(cnpq_com_duracao$valor, 0.75, na.rm = TRUE)
iqr_valor <- q3_valor - q1_valor
lim_sup_valor <- q3_valor + 1.5 * iqr_valor

# Marca os outliers
cnpq_com_duracao <- cnpq_com_duracao %>%
  mutate(outlier_valor = valor > lim_sup_valor)

# Gráfico melhorado com destaque para outliers
ggplot(cnpq_com_duracao, aes(x = duracao_dias, y = valor)) +
  geom_point(aes(color = outlier_valor), alpha = 0.3, size = 0.7) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), labels = c("Normal", "Outlier")) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linewidth = 1) +
  scale_x_log10(labels = comma_format()) +
  scale_y_log10(labels = dollar_format(prefix = "R$ ")) +
  labs(
    title = "Relação entre Valor da Bolsa e Duração do Processo",
    subtitle = "Pontos vermelhos indicam valores considerados outliers",
    x = "Duração do Processo (dias, escala log)",
    y = "Valor da Bolsa (R$, escala log)",
    color = "Tipo de Valor"
  ) +
  theme_light(base_size = 12) +
  theme(legend.position = "bottom")

# 10.5. Gráfico de Barras Empilhadas: Proporção de Linha de Fomento por Ano
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

# 10.6. Mapa com Instituições do Amazonas
cat("  - Gerando mapa com localização das instituições no Amazonas...\n")

# Carrega os municípios do Amazonas (UF 13)
municipios_am <- geobr::read_municipality(code_muni = 13, year = 2020)

# Coordenadas geográficas das principais instituições (exemplos)
instituicoes_am <- data.frame(
  instituicao = c(
    "UNIVERSIDADE FEDERAL DO AMAZONAS",
    "INSTITUTO FEDERAL DE EDUCACAO CIENCIA E TECNOLOGIA DO AMAZONAS - CAMPUS ITACOATIARA",
    "UNIVERSIDADE DO ESTADO DO AMAZONAS - CAMPUS PARINTINS"
  ),
  lon = c(-60.025, -58.449, -56.735),
  lat = c(-3.101, -2.763, -2.637)
)

# Plotando o mapa
ggplot() +
  geom_sf(data = municipios_am, fill = "gray95", color = "gray70") +
  geom_point(data = instituicoes_am, aes(x = lon, y = lat), color = "blue", size = 3) +
  geom_text_repel(data = instituicoes_am, aes(x = lon, y = lat, label = instituicao), size = 3) +
  labs(
    title = "Localização das Principais Instituições no Amazonas com Bolsas CNPq",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

# 10.7. Comparativo: Instituição que Mais Recebeu Bolsas no Amazonas
cat("  - Gerando comparativo de instituições do Amazonas que mais receberam bolsas...\n")
cnpq %>%
  filter(sigla_uf_origem == "AM") %>%
  count(instituicao_origem, name = "num_bolsas", sort = TRUE) %>%
  slice_max(num_bolsas, n = 10) %>%
  ggplot(aes(x = reorder(instituicao_origem, num_bolsas), y = num_bolsas)) +
  geom_col(fill = "royalblue") +
  coord_flip() +
  labs(
    title = "Top 10 Instituições do Amazonas que Mais Receberam Bolsas do CNPq",
    x = "Instituição de Origem",
    y = "Número de Bolsas"
  ) +
  theme_minimal()

cat("\n✅ ETAPA 2 concluída com análises adicionais.\n")
