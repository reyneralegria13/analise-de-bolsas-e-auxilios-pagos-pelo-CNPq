-----

# Análise de Bolsas e Auxílios do CNPq

Este projeto oferece uma análise exploratória e visualização dos dados de bolsas e auxílios concedidos pelo Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq), com foco em tendências temporais, áreas de conhecimento, linhas de fomento e distribuição geográfica, incluindo um detalhamento para o estado do Amazonas.

## Propósito

O objetivo principal deste trabalho é explorar os dados públicos do CNPq para identificar padrões de investimento, entender a distribuição de recursos entre diferentes áreas e regiões, e fornecer insights sobre o cenário de fomento à pesquisa no Brasil.

## Funcionalidades

  * **Extração de Dados:** Conexão direta ao Google BigQuery para obter microdados do CNPq.
  * **Tratamento de Dados:** Limpeza de valores ausentes e pré-processamento para análise.
  * **Estatísticas Descritivas:** Resumos estatísticos dos valores das bolsas e agrupamentos por categorias.
  * **Detecção de Outliers:** Identificação de valores atípicos nos montantes concedidos.
  * **Visualizações Gráficas:**
      * Histogramas e Boxplots para distribuição de valores.
      * Gráficos de barras para Top 10 áreas de conhecimento e UFs de origem.
      * Gráficos de linha para evolução temporal de valores e número de bolsas.
      * Gráficos de barras empilhadas para proporção de linhas de fomento ao longo do tempo.
  * **Mapas Coropléticos:**
      * Valor médio das bolsas por UF de origem.
      * **Análise detalhada do Amazonas:** Valor médio e número de bolsas por município de origem (requer mapeamento de instituições para municípios).
  * **Teste de Hipótese:** Exemplo de análise inferencial para comparar regiões.
  * **Relatório Interativo:** Um relatório completo em HTML gerado via R Markdown, com tabelas interativas e gráficos.

## Como Replicar o Projeto

Para replicar este projeto e executar a análise, siga as instruções abaixo:

### 1\. Pré-requisitos

  * **R e RStudio:** Certifique-se de ter o [R](https://www.r-project.org/) e o [RStudio Desktop](https://posit.co/download/rstudio-desktop/) instalados em seu computador.
  * **Git:** Tenha o [Git](https://git-scm.com/downloads) instalado para clonar o repositório.
  * **Projeto Google Cloud e BigQuery:**
      * Você precisará de uma conta Google e um [Projeto Google Cloud](https://cloud.google.com/resource-manager/docs/creating-managing-projects) configurado.
      * O **faturamento deve estar ativado** no seu Projeto Google Cloud. Embora o uso do `basedosdados` para datasets públicos geralmente se enquadre no nível gratuito do BigQuery (1TB de query por mês), a ativação do faturamento é um requisito do Google para acessar a API.
      * Anote o **ID do seu Projeto Google Cloud**. Você o usará para configurar o `basedosdados`.

### 2\. Configuração do Ambiente

1.  **Clone o Repositório:**
    Abra seu terminal (ou Git Bash) e execute:

    ```bash
    git clone https://github.com/SEU_USUARIO/SEU_REPOSITORIO.git
    # Substitua SEU_USUARIO/SEU_REPOSITORIO pelo caminho real do seu projeto no GitHub
    cd SEU_REPOSITORIO # Navegue até o diretório do projeto
    ```

2.  **Abra o Projeto no RStudio:**
    No RStudio, vá em `File > Open Project...` e selecione o arquivo `.Rproj` dentro da pasta que você acabou de clonar.

3.  **Instale os Pacotes R Necessários:**
    No console do RStudio, execute o seguinte comando para instalar todos os pacotes usados no projeto:

    ```r
    install.packages(c("basedosdados", "bigrquery", "dplyr", "tidyr", "skimr",
                        "psych", "readr", "ggplot2", "sf", "geobr", "DT",
                        "knitr", "scales"))
    ```

### 3\. Download e Preparação dos Dados

Os dados brutos são obtidos diretamente do Google BigQuery via pacote `basedosdados`. Esta etapa precisa ser executada **manualmente uma vez**.

1.  **Abra o arquivo `relatorio_Bolsas_CNPq.Rmd` (ou o nome do seu arquivo R Markdown) no RStudio.**

2.  **Configure a Autenticação e o Projeto Google Cloud:**
    No R Markdown, vá até o chunk de código nomeado `download-dados` (seção "2.1. Obtenção dos Dados").
    Você verá as linhas:

    ```r
    bq_auth() 
    basedosdados::set_billing_id("projetofinalgr03") 
    ```

      * **Execute apenas a linha `bq_auth()`** no console do R. Isso abrirá seu navegador para autenticar sua conta Google. Conceda as permissões necessárias.
      * **Na linha `basedosdados::set_billing_id("projetofinalgr03")`, substitua `"projetofinalgr03"` pelo ID do seu próprio Projeto Google Cloud.**

3.  **Execute o Chunk de Download:**
    Com o ID do projeto configurado e a autenticação feita, **execute TODO o chunk `download-dados` manualmente**. Você pode fazer isso clicando no ícone de "Play" verde no canto superior direito do chunk, ou selecionando todo o código dentro dele e pressionando `Ctrl + Enter` (Windows/Linux) ou `Cmd + Enter` (Mac).

      * Este processo fará o download de `cnpq.csv` e `estados.rds` para a pasta `dados/` dentro do seu projeto. Monitore o console do R para possíveis erros ou avisos.

    **IMPORTANTE:** Se o `download-dados` falhar, o relatório não poderá ser gerado. Verifique sua conexão com a internet, permissões no Google Cloud e se o faturamento está ativo.

### 4\. Gerar o Relatório Final

Após o download bem-sucedido dos dados, você pode gerar o relatório final:

1.  **Abra o arquivo `relatorio_Bolsas_CNPq.Rmd` (ou o nome do seu arquivo R Markdown).**
2.  Clique no botão **"Knit"** (localizado na barra de ferramentas superior do RStudio, acima do script) e selecione a opção `Knit to HTML`.

O RStudio executará todo o código no R Markdown e gerará um arquivo HTML interativo na mesma pasta do seu `.Rmd`.

## Estrutura do Projeto

```
.
├── dados/                       # Pasta para armazenar os dados baixados (cnpq.csv, estados.rds)
│   ├── cnpq.csv
│   └── estados.rds
├── relatorio_Bolsas_CNPq.Rmd    # O arquivo R Markdown principal para a análise e relatório
└── SEU_PROJETO.Rproj            # Arquivo de projeto do RStudio
```

## Limitações (Mapeamento de Municípios)

A análise geográfica detalhada por município no Amazonas (seção 6.1 do relatório) atualmente depende de um mapeamento **simulado e limitado** de instituições para códigos de município (IBGE) via uma lógica `case_when` no script. Isso significa que apenas um pequeno número de municípios pode aparecer nos mapas se seus dados não contiverem bolsas para as instituições especificamente mapeadas.

Para uma análise municipal mais completa, seria necessário:

  * Expandir a lógica `case_when` para incluir mais instituições e seus respectivos municípios.
  * Integrar uma base de dados externa de mapeamento de instituições para municípios.
  * Utilizar técnicas de geocodificação para inferir o município a partir do nome da instituição.

## Autor

Reyner Alegria

-----
