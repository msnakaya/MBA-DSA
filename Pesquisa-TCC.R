
## Análises de Correspondência Múltipla e Cluster

# Objetivo: verificar possíveis correspondências na pesquisa de TCC

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "psych",
             "Hmisc", # matriz de correlações com p-valor
             "readxl", # importar arquivo Excel
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "fastDummies","splines","correlation","see","ggraph","psych",
             "nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","Rcpp","globals",
             "stargazer","lmtest","caret","pROC","ROCR","nnet","questionr","MASS",
             "pscl","overdisp","magick","nlme","msm","lmeInfo","ggridges",
             "viridis","hrbrthemes", "RColorBrewer",
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando os dados

Dashboards_original <- read_excel("PesquisaTCC.xlsx")


# Categorizar as variáveis quanti
Dashboards_original <- Dashboards_original %>% 
  mutate(B.4b_Tempo_criar_dash = case_when(B.4_Tempo_criar_dash <= 0 ~ "0h",
                                       B.4_Tempo_criar_dash > 0 & B.4_Tempo_criar_dash <= 25 ~ "até25h",
                                       B.4_Tempo_criar_dash > 25 & B.4_Tempo_criar_dash <= 50 ~ "25a50h",
                                       B.4_Tempo_criar_dash > 50 & B.4_Tempo_criar_dash <= 75 ~ "50a75h",
                                       B.4_Tempo_criar_dash > 75 & B.4_Tempo_criar_dash <= 100 ~ "75a100h",
                                       B.4_Tempo_criar_dash > 100 ~ "mais_de100h"))

Dashboards_original <- Dashboards_original %>% 
  mutate(C.3b_Tempo_atualizações_semana = case_when(C.3_Tempo_atualizações_semana <= 0 ~ "0h",
                                       C.3_Tempo_atualizações_semana > 0 & C.3_Tempo_atualizações_semana <= 2.5 ~ "até2.5h",
                                       C.3_Tempo_atualizações_semana > 2.5 & C.3_Tempo_atualizações_semana <= 5 ~ "2.5a5h",
                                       C.3_Tempo_atualizações_semana > 5 & C.3_Tempo_atualizações_semana <= 7.5 ~ "5a7.5h",
                                       C.3_Tempo_atualizações_semana > 7.5 & C.3_Tempo_atualizações_semana <= 10 ~ "7.5a10h",
                                       C.3_Tempo_atualizações_semana > 10 ~ "mais_de10h"))

Dashboards_original <- Dashboards_original %>% 
  mutate(C.4b_Tempo_economizado_semana = case_when(C.4_Tempo_economizado_semana_horas <= 0 ~ "0h",
                                       C.4_Tempo_economizado_semana_horas > 0 & C.4_Tempo_economizado_semana_horas <= 2.5 ~ "até2.5h",
                                       C.4_Tempo_economizado_semana_horas > 2.5 & C.4_Tempo_economizado_semana_horas <= 5 ~ "2.5a5h",
                                       C.4_Tempo_economizado_semana_horas > 5 & C.4_Tempo_economizado_semana_horas <= 7.5 ~ "5a7.5h",
                                       C.4_Tempo_economizado_semana_horas > 7.5 & C.4_Tempo_economizado_semana_horas <= 10 ~ "7.5a10h",
                                       C.4_Tempo_economizado_semana_horas > 10 ~ "mais_de10h"))

# Transformando characters em factors

Dashboards_original$B.4_Tempo_criar_dash <- as.factor(Dashboards_original$B.4_Tempo_criar_dash)
Dashboards_original$Cargo <- as.factor(Dashboards_original$Cargo)
Dashboards_original$B_Criou_Dash <- as.factor(Dashboards_original$B_Criou_Dash)
Dashboards_original$B.1_Criou_quantos_dash <- as.factor(Dashboards_original$B.1_Criou_quantos_dash)
Dashboards_original$B.2_Nivel_dificuldade <- as.factor(Dashboards_original$B.2_Nivel_dificuldade)
Dashboards_original$B.3_Fez_curso_pago <- as.factor(Dashboards_original$B.3_Fez_curso_pago)
Dashboards_original$C_Atualiza_dashboards <- as.factor(Dashboards_original$C_Atualiza_dashboards)
Dashboards_original$C.1_Atualiza_quantos_dash <- as.factor(Dashboards_original$C.1_Atualiza_quantos_dash)
Dashboards_original$C.2_Nivel_dificuldade <- as.factor(Dashboards_original$C.2_Nivel_dificuldade)
Dashboards_original$D_Usuário_quantos_dashboards <- as.factor(Dashboards_original$D_Usuário_quantos_dashboards)
Dashboards_original$D.1_Entendimento_informações <- as.factor(Dashboards_original$D.1_Entendimento_informações)
Dashboards_original$D.2_Rapidez_achar_informações <- as.factor(Dashboards_original$D.2_Rapidez_achar_informações)
Dashboards_original$D.3_Detalhamento_informações <- as.factor(Dashboards_original$D.3_Detalhamento_informações)
Dashboards_original$D.4Visão_macro_negócio <- as.factor(Dashboards_original$D.4Visão_macro_negócio)
Dashboards_original$D.5_Entendimento_metas_depto_empresa <- as.factor(Dashboards_original$D.5_Entendimento_metas_depto_empresa)
Dashboards_original$D.6_Confirma_redireciona_atividades <- as.factor(Dashboards_original$D.6_Confirma_redireciona_atividades)
Dashboards_original$B.4b_Tempo_criar_dash <- as.factor(Dashboards_original$B.4b_Tempo_criar_dash)
Dashboards_original$C.3b_Tempo_atualizações_semana <- as.factor(Dashboards_original$C.3b_Tempo_atualizações_semana)
Dashboards_original$C.4b_Tempo_economizado_semana <- as.factor(Dashboards_original$C.4b_Tempo_economizado_semana)

# Estatísticas descritivas

summary(Dashboards_original)

# retirando colunas desnecessárias

Dashboards <- Dashboards_original[,c(-1 : -4, -10, -14, -15)]

# Tabelas de frequência das variáveis qualitativas
summary(Dashboards)

################################################################################
################### Tabelas de contingência ####################################
###################    linhas 106 a 1600    ####################################
################################################################################
# Cargo x B_Criou_Dash
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$B_Criou_Dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x1 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 6.696 (menor!)

# Cargo x C_Atualiza_dashboards
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x1 -> Qui-quadrado precisa ser maior que 
  # 9,488 para p-valor menor que 0,05: 11.075 (MAIOR)

# Cargo x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x4 -> Qui-quadrado precisa ser maior que  
  # 26,296 para p-valor menor que 0,05: 30.307 (MAIOR)

# Cargo x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 10.329 (menor!)

# Cargo x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 9.583 (menor!)

# Cargo x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x3 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 14.455 (menor!)

# Cargo x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 4.285 (menor!)

# Cargo x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 12.646 (menor!)

# Cargo x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 1.810 (menor!)

# Cargo x B.1_Criou_quantos_dash
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$B.1_Criou_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 13.874 (menor!)

# Cargo x B.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$B.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 28.572 (menor!)

# Cargo x B.3_Fez_curso_pago
Dash1 <- Dashboards[c(1,5)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
summary(Dash1)

sjt.xtab(var.row = Dash1$Cargo,
         var.col = Dash1$B.3_Fez_curso_pago,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x1 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 1.172 (menor!)

# Cargo x B.4b_Tempo_criar_dash
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$B.4b_Tempo_criar_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 27.523 (menor!)

# Cargo x C.1_Atualiza_quantos_dash
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 21.616 (menor!)

# Cargo x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 29.584 (menor!)

# Cargo x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$Cargo,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 38.903 (MAIOR)

# Cargo x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(1,18)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]     #retira observações NA
Dash1 <- droplevels(Dash1)                 #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$Cargo,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3X3 -> Qui-quadrado precisa ser maior que  
  # 16,919 para p-valor menor que 0,05: 9.387 (menor!)

################################################################################

# B_Criou_Dash x C_Atualiza_dashboards
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x1 -> Qui-quadrado precisa ser maior que  
  # 3,841 para p-valor menor que 0,05: 17.368 (MAIOR)

# B_Criou_Dash x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x4 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 2.184 (menor!)

# B_Criou_Dash x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 1.553 (menor!)

# B_Criou_Dash x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 0.562 (menor!)

# B_Criou_Dash x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x3 -> Qui-quadrado precisa ser maior que  
  # 7,815 para p-valor menor que 0,05: 3.652 (menor!)

# B_Criou_Dash x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 13.419 (MAIOR)

# B_Criou_Dash x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 2.154 (menor!)

# B_Criou_Dash x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 6.298 (MAIOR)

# B_Criou_Dash x C.1_Atualiza_quantos_dash
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 21.211 (MAIOR)

# B_Criou_Dash x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 20.749 (MAIOR)

# B_Criou_Dash x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$B_Criou_Dash,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 18.839 (MAIOR)

# B_Criou_Dash x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(2,18)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]     #retira observações NA
Dash1 <- droplevels(Dash1)                 #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B_Criou_Dash,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x3 -> Qui-quadrado precisa ser maior que  
  # 7,815 para p-valor menor que 0,05: 1.746 (menor!)

################################################################################

# B.1_Criou_quantos_dash x B.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$B.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 77.839 (MAIOR)

# B.1_Criou_quantos_dash x B.3_Fez_curso_pago
Dash14 <- Dashboards[c(3,5)]               #separa numa nova tabela apenas 2 colunas
Dash14 <- Dash14[complete.cases(Dash14),]   #retira observações NA
Dash14 <- droplevels(Dash14)                #retira level sem observações
summary(Dash14)

sjt.xtab(var.row = Dash14$B.1_Criou_quantos_dash,
         var.col = Dash14$B.3_Fez_curso_pago,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x1 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 2.494 (menor!)

# B.1_Criou_quantos_dash x B.4b_Tempo_criar_dash
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$B.4b_Tempo_criar_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 85.010 (MAIOR)


# B.1_Criou_quantos_dash x C_Atualiza_dashboards
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x1 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 21.207 (MAIOR)

# B.1_Criou_quantos_dash x C.1_Atualiza_quantos_dash
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 68.859 (MAIOR)

# B.1_Criou_quantos_dash x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 43.732 (MAIOR)

# B.1_Criou_quantos_dash x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 57.513 (MAIOR)

# B.1_Criou_quantos_dash x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(3,18)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]     #retira observações NA
Dash1 <- droplevels(Dash1)                 #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.1_Criou_quantos_dash,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 13.260 (menor!)

# B.1_Criou_quantos_dash x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 17.130 (menor!)

# B.1_Criou_quantos_dash x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 4.231 (menor!)

# B.1_Criou_quantos_dash x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 3.221 (menor!)

# B.1_Criou_quantos_dash x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 7.086 (menor!)

# B.1_Criou_quantos_dash x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 20.156 (MAIOR)

# B.1_Criou_quantos_dash x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 10.689 (menor!)

# B.1_Criou_quantos_dash x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$B.1_Criou_quantos_dash,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 15.976 (menor!)

################################################################################

# B.2_Nivel_dificuldade x B.3_Fez_curso_pago
Dash28 <- Dashboards[c(4,5)]               #separa numa nova tabela apenas 2 colunas
Dash28 <- Dash28[complete.cases(Dash28),]   #retira observações NA
Dash28 <- droplevels(Dash28)                #retira level sem observações
summary(Dash28)

sjt.xtab(var.row = Dash28$B.2_Nivel_dificuldade,
         var.col = Dash28$B.3_Fez_curso_pago,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x1 -> Qui-quadrado precisa ser maior que  
  # 7,815 para p-valor menor que 0,05: 4.566 (menor!)

# B.2_Nivel_dificuldade x B.4b_Tempo_criar_dash
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$B.4b_Tempo_criar_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 82.347 (MAIOR)

# B.2_Nivel_dificuldade x C_Atualiza_dashboards
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x1 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 22.025 (MAIOR)

# B.2_Nivel_dificuldade x C.1_Atualiza_quantos_dash
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 30.565 (menor!)

# B.2_Nivel_dificuldade x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 90.021 (MAIOR)

# B.2_Nivel_dificuldade x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X5 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 86.240 (MAIOR)

# B.2_Nivel_dificuldade x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(4,18)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]     #retira observações NA
Dash1 <- droplevels(Dash1)                 #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.2_Nivel_dificuldade,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x3 -> Qui-quadrado precisa ser maior que  
  # 16,919 para p-valor menor que 0,05: 15.869 menor!)

# B.2_Nivel_dificuldade x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x4 -> Qui-quadrado precisa ser maior que  
  # 26,296 para p-valor menor que 0,05: 35.010 (MAIOR)

# B.2_Nivel_dificuldade x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 6.784 (menor)

# B.2_Nivel_dificuldade x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 4.725 (menor)

# B.2_Nivel_dificuldade x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4X3 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 5.568 (menor)

# B.2_Nivel_dificuldade x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 15.634 (MAIOR)

# B.2_Nivel_dificuldade x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 5.984 (menor)

# B.2_Nivel_dificuldade x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$B.2_Nivel_dificuldade,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 9.459 (menor)

################################################################################

# B.3_Fez_curso_pago x B.4b_Tempo_criar_dash
Dash1 <- Dashboards[c(5,16)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$B.4b_Tempo_criar_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x4 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 2.343 (menor!)

# B.3_Fez_curso_pago x C_Atualiza_dashboards
Dash1 <- Dashboards[c(5,6)]                #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x1 -> Qui-quadrado precisa ser maior que  
  # 3,841 para p-valor menor que 0,05: 0.000 (menor!)

# B.3_Fez_curso_pago x C.1_Atualiza_quantos_dash
Dash1 <- Dashboards[c(5,7)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 3.178 (menor!)

# B.3_Fez_curso_pago x C.2_Nivel_dificuldade
Dash1 <- Dashboards[c(5,8)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 5.024 (menor!)

# B.3_Fez_curso_pago x C.3b_Tempo_atualizações_semana
Dash1 <- Dashboards[c(5,17)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x5 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 5.624 (menor!)

# B.3_Fez_curso_pago x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(5,18)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x3 -> Qui-quadrado precisa ser maior que  
  # 7,815 para p-valor menor que 0,05: 3.290 (menor!)

# B.3_Fez_curso_pago x D_Usuário_quantos_dashboards
Dash1 <- Dashboards[c(5,9)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x4 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 4.368 (menor!)

# B.3_Fez_curso_pago x D.1_Entendimento_informações
Dash1 <- Dashboards[c(5,10)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 3.169 (menor!)

# B.3_Fez_curso_pago x D.2_Rapidez_achar_informações
Dash1 <- Dashboards[c(5,11)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]     #retira observações NA
Dash1 <- droplevels(Dash1)                 #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 2.706  (menor!)

# B.3_Fez_curso_pago x D.3_Detalhamento_informações
Dash1 <- Dashboards[c(5,12)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 0.599  (menor!)

# B.3_Fez_curso_pago x D.4Visão_macro_negócio
Dash1 <- Dashboards[c(5,13)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 1.384  (menor!)

# B.3_Fez_curso_pago x D.5_Entendimento_metas_depto_empresa
Dash1 <- Dashboards[c(5,14)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 2.046  (menor!)

# B.3_Fez_curso_pago x D.6_Confirma_redireciona_atividades
Dash1 <- Dashboards[c(5,15)]               #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]   #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.3_Fez_curso_pago,
         var.col = Dash1$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 2.938  (menor!)

################################################################################

# B.4b_Tempo_criar_dash x C_Atualiza_dashboards
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$C_Atualiza_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x1 -> Qui-quadrado precisa ser maior que  
  # 11,070 para p-valor menor que 0,05: 20.691 (MAIOR)

# B.4b_Tempo_criar_dash x C.1_Atualiza_quantos_dash
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$C.1_Atualiza_quantos_dash,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 54.022 (MAIOR)

# B.4b_Tempo_criar_dash x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 68.136 (menor!)

# B.4b_Tempo_criar_dash x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 99.794 (MAIOR)

# B.4b_Tempo_criar_dash x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(16,18)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$B.4b_Tempo_criar_dash,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 9.359 (menor!)

# B.4b_Tempo_criar_dash x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 25.334 (menor!)

# B.4b_Tempo_criar_dash x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 6.528 (menor!)

# B.4b_Tempo_criar_dash x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 32.724 (MAIOR)

# B.4b_Tempo_criar_dash x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X3 -> Qui-quadrado precisa ser maior que  
  # 24,996  para p-valor menor que 0,05: 8.804 (menor!)

# B.4b_Tempo_criar_dash x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 19.626 (MAIOR)

# B.4b_Tempo_criar_dash x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 7.910 (menor!)

# B.4b_Tempo_criar_dash x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$B.4b_Tempo_criar_dash,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 14.168 (menor!)

################################################################################

# C_Atualiza_dashboards x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x4 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 0.236 (menor!)

# C_Atualiza_dashboards x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 0.626  (menor!)

# C_Atualiza_dashboards x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 0.143  (menor!)

# C_Atualiza_dashboards x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x3 -> Qui-quadrado precisa ser maior que  
  # 7,815 para p-valor menor que 0,05: 3.604 (menor!)

# C_Atualiza_dashboards x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 8.916  (MAIOR)

# C_Atualiza_dashboards x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 1.549  (menor!)

# C_Atualiza_dashboards x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$C_Atualiza_dashboards,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 1x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 1.302  (menor!)

################################################################################

# C.1_Atualiza_quantos_dash x C.2_Nivel_dificuldade
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$C.2_Nivel_dificuldade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 81.953 (MAIOR)

# C.1_Atualiza_quantos_dash x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 90.502 (MAIOR)

# C.1_Atualiza_quantos_dash x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(7,18)]              #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.1_Atualiza_quantos_dash,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x3 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 14.619 (menor!)

# C.1_Atualiza_quantos_dash x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 10.831 (menor!)

# C.1_Atualiza_quantos_dash x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 12.319 (menor!)

# C.1_Atualiza_quantos_dash x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 7.694 (menor!)

# C.1_Atualiza_quantos_dash x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 12.909 (menor!)

# C.1_Atualiza_quantos_dash x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 19.439 (MAIOR)

# C.1_Atualiza_quantos_dash x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 11.144 (menor!)

# C.1_Atualiza_quantos_dash x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$C.1_Atualiza_quantos_dash,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 10.169 (menor!)

################################################################################

# C.2_Nivel_dificuldade x C.3b_Tempo_atualizações_semana
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$C.3b_Tempo_atualizações_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X5 -> Qui-quadrado precisa ser maior que  
  # 37,652 para p-valor menor que 0,05: 128.220 (MAIOR)

# C.2_Nivel_dificuldade x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(8,18)]              #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.2_Nivel_dificuldade,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x3 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 10.863 (menor!)

# C.2_Nivel_dificuldade x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 42.460 (MAIOR)

# C.2_Nivel_dificuldade x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 6.350 (menor!)

# C.2_Nivel_dificuldade x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 19.360 (MAIOR)

# C.2_Nivel_dificuldade x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5X3 -> Qui-quadrado precisa ser maior que  
  # 24,996  para p-valor menor que 0,05: 9.261 (menor!)

# C.2_Nivel_dificuldade x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 19.309 (MAIOR)

# C.2_Nivel_dificuldade x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 12.417 (menor!)

# C.2_Nivel_dificuldade x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$C.2_Nivel_dificuldade,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 8.236 (menor!)

################################################################################

# C.3b_Tempo_atualizações_semana x C.4b_Tempo_economizado_semana
Dash1 <- Dashboards[c(17,18)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.3b_Tempo_atualizações_semana,
         var.col = Dash1$C.4b_Tempo_economizado_semana,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 13.300 (menor!)

# C.3b_Tempo_atualizações_semana x D_Usuário_quantos_dashboards
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x4 -> Qui-quadrado precisa ser maior que  
  # 31,410 para p-valor menor que 0,05: 33.315 (MAIOR)

# C.3b_Tempo_atualizações_semana x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 5.044 (menor!)

# C.3b_Tempo_atualizações_semana x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
# 18,307 para p-valor menor que 0,05: 3.015 (menor!)

# C.3b_Tempo_atualizações_semana x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x3 -> Qui-quadrado precisa ser maior que  
  # 24,996 para p-valor menor que 0,05: 6.456 (menor!)

# C.3b_Tempo_atualizações_semana x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 10.485 (menor!)

# C.3b_Tempo_atualizações_semana x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 5x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 8.802 (menor!)

# C.3b_Tempo_atualizações_semana x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$C.3b_Tempo_atualizações_semana,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 18,307 para p-valor menor que 0,05: 11.672 (menor!)

################################################################################

# C.4b_Tempo_economizado_semana x D_Usuário_quantos_dashboards
Dash1 <- Dashboards[c(18,9)]              #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D_Usuário_quantos_dashboards,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x4 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 12.949 (menor!)

# C.4b_Tempo_economizado_semana x D.1_Entendimento_informações
Dash1 <- Dashboards[c(18,10)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
  # 12,592 para p-valor menor que 0,05: 11.435 (menor)

# C.4b_Tempo_economizado_semana x D.2_Rapidez_achar_informações
Dash1 <- Dashboards[c(18,11)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
  # 12,592 para p-valor menor que 0,05: 5.481 (menor)

# C.4b_Tempo_economizado_semana x D.3_Detalhamento_informações
Dash1 <- Dashboards[c(18,12)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
  # 12,592 para p-valor menor que 0,05: 6.041 (menor)

# C.4b_Tempo_economizado_semana x D.4Visão_macro_negócio
Dash1 <- Dashboards[c(18,13)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
  # 12,592 para p-valor menor que 0,05: 7.521 (menor)

# C.4b_Tempo_economizado_semana x D.5_Entendimento_metas_depto_empresa
Dash1 <- Dashboards[c(18,14)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 13.979 (MAIOR)

# C.4b_Tempo_economizado_semana x D.6_Confirma_redireciona_atividades
Dash1 <- Dashboards[c(18,15)]             #separa numa nova tabela apenas 2 colunas
Dash1 <- Dash1[complete.cases(Dash1),]    #retira observações NA
Dash1 <- droplevels(Dash1)                #retira level sem observações
summary(Dash1)

sjt.xtab(var.row = Dash1$C.4b_Tempo_economizado_semana,
         var.col = Dash1$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 13.887 (MAIOR)

###############################################################################

# D_Usuário_quantos_dashboards x D.1_Entendimento_informações
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.1_Entendimento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
# 15,507 para p-valor menor que 0,05: 16.450 (MAIOR)

# D_Usuário_quantos_dashboards x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 11.472 (menor!)

# D_Usuário_quantos_dashboards x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x3 -> Qui-quadrado precisa ser maior que  
  # 21,026 para p-valor menor que 0,05: 5.117 (menor!)

# D_Usuário_quantos_dashboards x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 14.287 (menor!)

# D_Usuário_quantos_dashboards x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 6.113 (menor!)

# D_Usuário_quantos_dashboards x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D_Usuário_quantos_dashboards,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 3.530 (menor!)

################################################################################

# D.1_Entendimento_informações x D.2_Rapidez_achar_informações
sjt.xtab(var.row = Dashboards$D.1_Entendimento_informações,
         var.col = Dashboards$D.2_Rapidez_achar_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
  # 9,488 para p-valor menor que 0,05: 22.605 (MAIOR)

# D.1_Entendimento_informações x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$D.1_Entendimento_informações,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 2x4 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 16.485 (MAIOR)

# D.1_Entendimento_informações x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$D.1_Entendimento_informações,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 21.759 (MAIOR)

# D.1_Entendimento_informações x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$D.1_Entendimento_informações,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 4x2 -> Qui-quadrado precisa ser maior que  
  # 15,507 para p-valor menor que 0,05: 23.032 (MAIOR)

# D.1_Entendimento_informações x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D.1_Entendimento_informações,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
  # graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
  # 5,991 para p-valor menor que 0,05: 25.242 (MAIOR)

################################################################################

# D.2_Rapidez_achar_informações x D.3_Detalhamento_informações
sjt.xtab(var.row = Dashboards$D.2_Rapidez_achar_informações,
         var.col = Dashboards$D.3_Detalhamento_informações,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 2x3 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 50.424 (MAIOR)

# D.2_Rapidez_achar_informações x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$D.2_Rapidez_achar_informações,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
# 5,991 para p-valor menor que 0,05: 6.599 (MAIOR)

# D.2_Rapidez_achar_informações x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$D.2_Rapidez_achar_informações,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
# 5,991 para p-valor menor que 0,05: 18.268 (MAIOR)

# D.2_Rapidez_achar_informações x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D.2_Rapidez_achar_informações,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
# 5,991 para p-valor menor que 0,05: 15.107 (MAIOR)

################################################################################

# D.3_Detalhamento_informações x D.4Visão_macro_negócio
sjt.xtab(var.row = Dashboards$D.3_Detalhamento_informações,
         var.col = Dashboards$D.4Visão_macro_negócio,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 18.116 (MAIOR)

# D.3_Detalhamento_informações x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$D.3_Detalhamento_informações,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 8.045 (menor!)

# D.3_Detalhamento_informações x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D.3_Detalhamento_informações,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 12,592 para p-valor menor que 0,05: 34.668 (MAIOR)

################################################################################

# D.4Visão_macro_negócio x D.5_Entendimento_metas_depto_empresa
sjt.xtab(var.row = Dashboards$D.4Visão_macro_negócio,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 2x2 -> Qui-quadrado precisa ser maior que  
# 9,488 para p-valor menor que 0,05: 12.565 (MAIOR)

# D.4Visão_macro_negócio x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D.4Visão_macro_negócio,
         var.col = Dashboards$D.6_Confirma_redireciona_atividades,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 9,488 para p-valor menor que 0,05: 16.549 (MAIOR)

################################################################################

# D.5_Entendimento_metas_depto_empresa x D.6_Confirma_redireciona_atividades
sjt.xtab(var.row = Dashboards$D.4Visão_macro_negócio,
         var.col = Dashboards$D.5_Entendimento_metas_depto_empresa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)
# graus de liberdade= 3x2 -> Qui-quadrado precisa ser maior que  
# 9,488 para p-valor menor que 0,05: 12.565 (MAIOR)

################################################################################
##################       FIM DA TABELA DE CONTINGÊNCIA      ####################
################################################################################
############### APLICAÇÃO DA ANÁLISE DE CORRESPONDÊNCIA MÚLTIPLA ###############
################################################################################

# base de dados sem a coluna 5 'B.B_Fez_curso_pago' (sem correspondência) 

Dash_final <- Dashboards[c(-5)]

# Elaboração da análise de correspondência múltipla (ACM)
ACM <- dudi.acm(Dash_final, scannf = FALSE)

# Visualização das coordenadas principais das categorias das variáveis
# Método da matriz de Burt B (componente 'co' do objeto 'ACM')
round(ACM$co, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Visualização das coordenadas-padrão das categorias das variáveis
# Método da matriz binária (componente 'c1' do objeto 'ACM')
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Massas das linhas e colunas (componente 'cw' do objeto 'ACM')
ACM$cw

# Inércias principais (componente 'eig' do objeto 'ACM')
ACM$eig

# Percentual de variância explicada por dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Visualização do percentual de variância explicada por dimensão
data.frame(Dimensão = paste("Dimensão", 1:length(perc_variancia)),
           Variância = perc_variancia) %>%
  ggplot(aes(x = Dimensão,
             y = Variância,
             label = paste0(round(Variância, 2),"%"))) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

# Quantidade de categorias por variável
quant_categorias <- apply(Dash_final,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# mapa com as coordenadas obtidas por meio da matriz de Burt

# Consolidando as coordenadas-padrão obtidas por meio da matriz de Burt
df_ACM <- data.frame(ACM$co, Variável = rep(names(quant_categorias),
                                              quant_categorias))

# Plotando o mapa perceptual total
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 4) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# último mapa de forma interativo
ggplotly(p=ggplot2::last_plot())


################################################################################
#####################  Análise de Cluster das coordenadas  #####################
################################################################################

df_coord <- df_ACM[c(1,2)]

# Aplicando a padronização por ZScore
df_coor <- as.data.frame(scale(df_coord))

round(mean(df_coor$Comp1), 3)  
round(mean(df_coor$Comp2), 3)  

round(sd(df_coor$Comp1), 3)     
round(sd(df_coor$Comp2), 3)    

# Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(df_coor, kmeans, method = "wss", k.max = 15)

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(df_coor,
                         centers = 6)

# Criando variável categórica para indicação do cluster no banco de dados
df_coor$cluster_K <- factor(cluster_kmeans$cluster)
df_ACM$cluster_K <- factor(cluster_kmeans$cluster)

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_K: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_K / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'Comp1'
summary(anova_Comp1 <- aov(formula = Comp1 ~ cluster_K,
                         data = df_coor))

# ANOVA da variável 'Comp2'
summary(anova_Comp2 <- aov(formula = Comp2 ~ cluster_K,
                                 data = df_coor))


## 'Comp1'
group_by(df_ACM, cluster_K) %>%
  summarise(
    mean = mean(Comp1, na.rm = TRUE),
    sd = sd(Comp1, na.rm = TRUE),
    min = min(Comp1, na.rm = TRUE),
    max = max(Comp1, na.rm = TRUE),
    obs = n())

## 'Comp2'
group_by(df_ACM, cluster_K) %>%
  summarise(
    mean = mean(Comp2, na.rm = TRUE),
    sd = sd(Comp2, na.rm = TRUE),
    min = min(Comp2, na.rm = TRUE),
    max = max(Comp2, na.rm = TRUE),
    obs = n())

# Plot do cluster 1

df_ACM_K1 <- df_ACM[df_ACM$cluster_K == "1",c(1:4)]

df_ACM_K1 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw () +
  theme(legend.position = "bottom")

# Plot do cluster 2

df_ACM_K2 <- df_ACM[df_ACM$cluster_K == "2",c(1:4)]

df_ACM_K2 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_x_continuous(limits = c(-1.2, -0.6)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Plot do cluster 3

df_ACM_K3 <- df_ACM[df_ACM$cluster_K == "3",c(1:4)]

df_ACM_K3 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
       scale_x_continuous(limits = c(-1.5, -1.0)) +
       scale_y_continuous(limits = c(6.3, 7)) +
  theme_bw()+
  theme(legend.position = "bottom")

# Plot do cluster 4

df_ACM_K4 <- df_ACM[df_ACM$cluster_K == "4",c(1:4)]

df_ACM_K4 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
       scale_x_continuous(limits = c(-2, 1)) +
       scale_y_continuous(limits = c(1.2, 3.8)) +
  theme_bw()+
  theme(legend.position = "bottom")

# Plot do cluster 5

df_ACM_K5 <- df_ACM[df_ACM$cluster_K == "5",c(1:4)]

df_ACM_K5 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
#     scale_x_continuous(limits = c(-1.2, -0.6)) +
  theme_bw()+
  theme(legend.position = "bottom")

# Plot do cluster 6

df_ACM_K6 <- df_ACM[df_ACM$cluster_K == "6",c(1:4)]

df_ACM_K6 %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point(size = 4) +
  geom_text_repel(max.overlaps = 100, size = 5.5) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
      scale_x_continuous(limits = c(-1.7, -1.1)) +
#     scale_y_continuous(limits = c(6, 7)) +
  theme_bw()+
  theme(legend.position = "bottom")

### FIM







