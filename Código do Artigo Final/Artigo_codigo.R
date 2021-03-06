# Primeiro, devem ser realizados os procedimentos b�sicos de carregamento 
# do que ser� utilizado:

setwd("C:\\Users\\Renato\\Desktop\\Mestrado - 2020.1\\SEMESTRE I\\An�lise de Dados\\ARTIGO FINAL\\DADOS\\PRESTA��O DE CONTAS - CANDIDATOS\\")

library(tidyverse)
library(readxl)
library(dplyr)
library(poliscidata)
library(vcd)
library(graphics)
library(scales)
library(dotwhisker)
library(pscl)
library(sjPlot)
library(margins)
library(InformationValue)

# Como o TSE n�o disponibiliza links baix�veis para a importa��o direta
# pelo RStudio, disponibilizaremo os links de acesso ao arquivo para 
# a realiza��o do download dos bancos necess�rios:

link <- "https://raw.githubusercontent.com/lirarenato22/Analise_de_dados_Trabalho_Final/main/consulta_cand_2018_BRASIL.csv"

link_2 <- "https://www.dropbox.com/s/u2ujij8bd3saur3/despesas_contratadas_candidatos_2018_BRASIL.csv?raw=1"

download.file(link, "consulta_cand_2018_BRASIL.csv", mode = "wb")

# Ap�s isso, ser� necess�rio fazer o download do banco de dados 
# "consulta_cand_2018_BRASIL", do qual utilizaremos vari�veis de 
# caracter�sticas dos candidatos.

BANCO_CONSULTA_CANDIDATOS <- read.csv2("consulta_cand_2018_BRASIL.csv")

# A partir do banco inicial "consulta_cand_2018_BRASIL.csv", selecionamos
# as vari�veis CD_CARGO, NR_TURNO, SG_UF, SQ_CANDIDATO, CD_DETALHE_SITUACAO_CAND, 
# DS_COR_RACA, ST_REELEICAO, DS_GENERO, TP_AGREMIACAO, SG_PARTIDO,
# NM_PARTIDO, NM_CANDIDATO e NR_IDADE_DATA_POSSE:

BANCO_CONSULTA_CANDIDATOS_FILTRADO <- BANCO_CONSULTA_CANDIDATOS %>%
  select(CD_CARGO, NR_TURNO, SG_UF, SQ_CANDIDATO, CD_DETALHE_SITUACAO_CAND, 
         DS_COR_RACA, ST_REELEICAO, DS_GENERO, TP_AGREMIACAO, SG_PARTIDO,
         NM_PARTIDO, NM_CANDIDATO, NR_IDADE_DATA_POSSE, DS_SIT_TOT_TURNO) %>%
    filter(NR_TURNO == 1, CD_CARGO %in% c('6', '7'), 
           CD_DETALHE_SITUACAO_CAND %in% c('2', '16'))

# O segundo banco a ser baixado � o 
# "despesas_contratadas_candidatos_2018_BRASIL", do qual utilizaremos as
# vari�veis "SQ_CANDIDATO", "SQ_PRESTADOR_CONTAS", "SG_UF", VR_DESPESA_CONTRATADA,
# e "DS_DESPESA" para podermos 'unificar' os bancos.

download.file(link_2, "despesas_contratadas_candidatos_2018_BRASIL.csv", mode = "wb")

BANCO_DESPESAS_CONTRATADAS <- 
  read.csv2("despesas_contratadas_candidatos_2018_BRASIL.csv")

# Como h� v�rias observa��es por n�mero sequencial, ser� necess�rio unificar
# as observa��es para cada n�mero sequencial.

BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL <- BANCO_DESPESAS_CONTRATADAS %>%
  select(SG_UF, DS_DESPESA, SQ_PRESTADOR_CONTAS, SQ_CANDIDATO, VR_DESPESA_CONTRATADA) %>%
  group_by(SQ_PRESTADOR_CONTAS, SG_UF, SQ_CANDIDATO) %>%
  summarise(sum(VR_DESPESA_CONTRATADA))

# A partir disso, tentaremos conseguir as mesmas informa��es sobre
# impulsionamento de conte�do a partir desse novo banco.

BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO <- BANCO_DESPESAS_CONTRATADAS %>%
  select(SG_UF, DS_ORIGEM_DESPESA, SQ_PRESTADOR_CONTAS, SQ_CANDIDATO, 
         VR_DESPESA_CONTRATADA) %>%
  filter(DS_ORIGEM_DESPESA %in% c('Despesa com Impulsionamento de Conte�dos'))

# Como o banco oferece v�rias informa��es sobre impulsionamento para
# cada n�mero sequencial, precisaremos agrupar os valores por n�mero 
# sequencial.

BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO <- BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO %>%
  group_by(SQ_PRESTADOR_CONTAS, SG_UF, DS_ORIGEM_DESPESA, SQ_CANDIDATO) %>%
  summarise(sum(VR_DESPESA_CONTRATADA))

# Depois disso, unificaremos os bancos 
# "BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL" e
# "BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO" com a fun��o "merge".

BANCO_UNIFICADO <- merge(BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL,
                         BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO,
                         by="SQ_CANDIDATO")

# A partir do primeiro banco unificado, criaremos o "BANCO_UNIFICADO_2",
# agregando o banco "BANCO_CONSULTA_CANDIDATOS_FILTRADO" com a fun��o "merge".

BANCO_UNIFICADO_2 <- merge(BANCO_UNIFICADO,
                           BANCO_CONSULTA_CANDIDATOS_FILTRADO,
                           by="SQ_CANDIDATO")

# Com base no BANCO_UNIFICADO_2, recodificaremos a vari�vel "DS_SIT_TOT_TURNO",
# para podermos obter valores categoriz�veis de forma dummy (ELEITO/N�O ELEITO).


BANCO_UNIFICADO_2_RECODIFICADO <- BANCO_UNIFICADO_2 %>%
  mutate(DS_SIT_TOT_TURNO  = recode(DS_SIT_TOT_TURNO,
                            "SUPLENTE" = "0",
                            "N�O ELEITO" = "0",
                            "ELEITO POR QP" = "1",
                            "ELEITO POR M�DIA" = "1"))

# � necess�rio tamb�m renomear as vari�veis "sum(VR_DESPESA_CONTRATADA).x" e
# "sum(VR_DESPESA_CONTRATADA).y" para "Despesa_Total" e 
# "Despesa_Impulsionamento", respectivamente.

BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO <- BANCO_UNIFICADO_2_RECODIFICADO %>%
  rename(DESPESA_TOTAL= "sum(VR_DESPESA_CONTRATADA).x") %>%
  rename(DESPESA_IMPULSIONAMENTO= "sum(VR_DESPESA_CONTRATADA).y")

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", come�aremos a an�lise 
# explorat�ria das vari�veis.

# "DS_COR_RACA"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_COR_RACA)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(DS_COR_RACA))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_COR_RACA, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "DS_GENERO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_GENERO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(DS_GENERO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_GENERO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "ST_REELEICAO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(ST_REELEICAO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(ST_REELEICAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(ST_REELEICAO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "NR_IDADE_DATA_POSSE"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE)) +
  geom_histogram()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE)) +
  geom_density()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = NR_IDADE_DATA_POSSE)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(y = NR_IDADE_DATA_POSSE)) + 
  geom_boxplot()


# "DESPESA_TOTAL"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL)) +
  geom_histogram(bins = 60)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL)) +
  geom_density(adjust = 15)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = DESPESA_TOTAL)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(y = DESPESA_TOTAL)) + 
  geom_boxplot()


# "DESPESA_IMPULSIONAMENTO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO)) +
  geom_histogram(bins = 60)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO)) +
  geom_density(adjust = 50)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = DESPESA_IMPULSIONAMENTO)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(y = DESPESA_IMPULSIONAMENTO)) + 
  geom_boxplot()


# "TP_AGREMIACAO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(TP_AGREMIACAO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(TP_AGREMIACAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(TP_AGREMIACAO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "DS_SIT_TOT_TURNO"


ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(TP_AGREMIACAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_SIT_TOT_TURNO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)


# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", come�aremos a an�lise 
# explorat�ria entre as vari�veis.

# "DS_COR_RACA" e "DS_SIT_TOT_TURNO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_COR_RACA, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(x = DS_COR_RACA, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = DS_COR_RACA, size = after_stat(prop))) +
  scale_size_area(max_size = 15)

# "DS_GENERO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_GENERO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(x = DS_GENERO, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = DS_GENERO, size = after_stat(prop))) +
  scale_size_area(max_size = 10)

# "ST_REELEICAO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(ST_REELEICAO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

# "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(TP_AGREMIACAO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = TP_AGREMIACAO, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = TP_AGREMIACAO, size = after_stat(prop))) +
  scale_size_area(max_size = 10)

# "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE, 
                                           fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 1)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                           NR_IDADE_DATA_POSSE)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                           NR_IDADE_DATA_POSSE)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL, 
                                           fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 0.5)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_TOTAL)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_TOTAL)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_IMPULSIONAMENTO, 
                                                     fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 0.5)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_IMPULSIONAMENTO)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_IMPULSIONAMENTO)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_TOTAL" e "DESPESA_IMPULSIONAMENTO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO, DESPESA_TOTAL)) +
  geom_point(alpha = 1)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO, DESPESA_TOTAL)) +
  geom_jitter()

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as vari�veis "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO".

t.test(DESPESA_IMPULSIONAMENTO ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as vari�veis "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO".

t.test(DESPESA_TOTAL ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste r de Pearson entre as vari�veis "DESPESA_TOTAL" e 
# "DESPESA_IMPULSIONAMENTO", para observar se h� uma poss�vel correla��o
# entre essas vari�veis.

cor.test(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DESPESA_IMPULSIONAMENTO, 
         BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DESPESA_TOTAL)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# o teste de ANOVA (ou teste de Kruskal) entre as vari�veis
# "DS_COR_RACA" e "DS_SIT_TOT_TURNO".

kruskal.test(DS_SIT_TOT_TURNO ~ DS_COR_RACA, 
             data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as vari�veis "DS_GENERO" e "DS_SIT_TOT_TURNO".

TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_GENERO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as vari�veis "ST_REELEICAO" e "DS_SIT_TOT_TURNO".

TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$ST_REELEICAO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as vari�veis "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO".

TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$TP_AGREMIACAO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as vari�veis "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO".

t.test(NR_IDADE_DATA_POSSE ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG", que
# criaremos agora, come�aremos agora a 
# utilizar regress�es log�sticas entre as vari�veis de interesse.

BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG <- 
  BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO %>%
  mutate(DS_SIT_TOT_TURNO  = recode(DS_SIT_TOT_TURNO,
                                    "0" = 0,
                                    "0" = 0,
                                    "1" = 1,
                                    "1" = 1))

# "DS_COR_RACA" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_COR_RACA, 
                           data = 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO)

# "DS_GENERO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_GENERO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO)
 
# "ST_REELEICAO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ ST_REELEICAO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO)

# "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ TP_AGREMIACAO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO)

# "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ NR_IDADE_DATA_POSSE, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO)

# "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DESPESA_TOTAL, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO)

# "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DESPESA_IMPULSIONAMENTO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO)

# "DS_COR_RACA" + "DS_GENERO" + "ST_REELEICAO" + "TP_AGREMIACAO" + 
# "NR_IDADE_DATA_POSSE" + "DESPESA_TOTAL" + "DESPESA_IMPULSIONAMENTO" e 
# "DS_SIT_TOT_TURNO".

REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_COR_RACA + DS_GENERO + ST_REELEICAO + TP_AGREMIACAO
      + NR_IDADE_DATA_POSSE + DESPESA_TOTAL + DESPESA_IMPULSIONAMENTO + 
        DESPESA_IMPULSIONAMENTO:ST_REELEICAO + DESPESA_IMPULSIONAMENTO:DESPESA_TOTAL, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO)

# Para dar continuidade, transformaremos os coeficientes estimados 
# em probabilidade:

margins(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO)

summary(margins(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO))

# Verificaremos, ent�o, as classifica��es corretas do modelo em rela��o 
# ao que poderia ser considerado por um modelo ing�nuo:



predicted_prob <- predict(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO, 
                          type = "response")


1 - misClassError(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                  predicted_prob, 
                  threshold = 0.5)

# A partir desses comandos, observamos as previs�es corretas. 
# Corrigiremos, ent�o, o threshold para o valor ideal.

opt_cutoff <- optimalCutoff(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                            predicted_prob)


1 - misClassError(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                  predicted_prob, 
                  threshold = 0.260131661480244)

# Assim, otimizamos os valores obtidos.


confusionMatrix(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                predicted_prob, 
                threshold = opt_cutoff)

# Esse �ltimo comando deu a quantidade de acertos e erros do modelo de regress�o
# log�stica por n�s utilizado. A partir de ent�o, transformaremos esse 
# resultado em porcentagem.

prop.table(confusionMatrix(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                           predicted_prob, threshold = opt_cutoff))


# Agora apresentaremos os resultados b�sicos de probabilidade de um modelo
# ing�nuo:

table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO)


prop.table(table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO))

# Nesse sentido, a constru��o do modelo por n�s utilizado representa uma
# previsibilidade de acerto em rela��o ao modelo ing�nuo dada pela equa��o:

0.8508/0.8015889

# O modelo de regress�o log�stica dessa pesquisa se mostra 6,1% mais eficiente
# do que a probabilidade pura dada pelo modelo ing�nuo (85,36% contra 80,16%).
