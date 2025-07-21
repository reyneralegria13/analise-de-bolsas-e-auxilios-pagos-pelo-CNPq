###############################################################################
# P I P E L I N E   COMPLETO  ·  C N P Q  B O L S A S  E  A U X Í L I O S
# -----------------------------------------------------------------------------
# ETAPA 1 – DOWNLOAD (executar apenas uma vez)
###############################################################################

library(basedosdados)
library(bigrquery)
library(readr)
library(geobr)
library(sf)
library(dplyr)

bq_auth()
basedosdados::set_billing_id("projetofinalgr03")

dir.create("dados", showWarnings = FALSE)

query <- "
WITH 
dicionario_linha_fomento AS (
  SELECT
    chave AS chave_linha_fomento,
    valor AS descricao_linha_fomento
  FROM `basedosdados.br_cnpq_bolsas.dicionario`
  WHERE
    nome_coluna = 'linha_fomento'
    AND id_tabela = 'microdados'
)
SELECT
  dados.ano AS ano,
  dados.processo AS processo,
  dados.data_inicio_processo AS data_inicio_processo,
  dados.data_fim_processo AS data_fim_processo,
  dados.titulo_projeto AS titulo_projeto,
  descricao_linha_fomento AS linha_fomento,
  dados.area_conhecimento AS area_conhecimento,
  dados.sigla_uf_origem AS sigla_uf_origem,
  dados.instituicao_origem AS instituicao_origem,
  dados.sigla_uf_destino AS sigla_uf_destino,
  dados.sigla_instituicao_destino AS sigla_instituicao_destino,
  dados.valor AS valor
FROM `basedosdados.br_cnpq_bolsas.microdados` AS dados
LEFT JOIN dicionario_linha_fomento
  ON dados.linha_fomento = chave_linha_fomento
"

basedosdados::read_sql(query,
                       billing_project_id = basedosdados::get_billing_id()) %>%
  write_csv("dados/cnpq.csv")

estados <- geobr::read_state(year = 2020)
saveRDS(estados, "dados/estados.rds")

cat("✅  ETAPA 1 concluída — bases salvas em 'dados/' (cnpq.csv e estados.rds).\n")
