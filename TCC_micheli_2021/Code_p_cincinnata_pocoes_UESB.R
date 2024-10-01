###############################################################################
### Autores: Antonio Porto, Micheli Carvalho, Antonio Oliveira         ########
### Goal: Objetivo: Analise da estrutura da variancia de subpopulacoes ########
### P. cincinnata                                                      ######## 
###############################################################################

# load packages
library(fBasics)
library(agricolae)
library(lattice)
library(ExpDes)
library(tidyverse)
library(car)
library(readxl)
library(carData)
library(lsmeans)



#### Lendo os arquivos de dados ####
#--------------------------------------------------------------------------- #
rm(list = ls())
dados <- read_excel("dados_cincinnata_pocoes_UESB.xlsx", sheet = 1) # Sheet = layer do arquivo excel que deve ser lido (1-4)
head(dados)
str(dados) # estrutura dos dados


## Unindo caracteres (trais) em um unico vetor
dados <- dados %>% gather(Trait, Value, 4:ncol(dados))
str(dados)


## transformando natureza das variaveis
dados <- transform(dados, ZEE = factor(ZEE), Planta = factor(PLANTA), Rep = factor(Rep.),
                   Trait = factor(Trait))
str(dados)

######## Loop para analise de cada variavel ################ 
# selecionar variavel
# fazer anova
# extrair variancias (E(QM))
# calcular parametros (Ps, Pp/s, Pst, etc...)
# Unir todas informacoes e exporta-las



  for(i in levels(dados$Trait)) {
    print(i)
    dados2 <- dplyr::filter(dados,  Trait == i)
    
    ##  Anova
    model <- aov(Value ~ ZEE + Planta%in%ZEE + Rep%in%Planta, data = dados2)
    anava <- summary(model)
    shapiro.test(model$residuals)
    qqnorm(model$residuals)
    
    ## Configurando tabela da anava
    tab <- anova(model)
    print(tab)
    tab <- round(tab,  digits = 4)
    
    ## Extraindo numero de niveis dentro de cada fator
    z <- nlevels(dados2$ZEE)       #ZEE
    p <- nlevels(dados2$Planta)    # Plantas
    n <- nlevels(dados2$Rep)       # Observacoes dentro de cada planta
    
    # Variance among ZEE
    vz <- abs((tab[1,3] -  tab[2,3])/ (p*n))
    
    # Variance among plants
    vp <- (tab[2,3] -  tab[3,3])/ n
     
    # variance among replicates (leaves, fruits or seeds) 
    vr <- tab[3,3]
    
    
    # Calculando parametros 
    Pz <- (vz  / (vz  + vp + vr) *100) # Pz
    Pp <- (vp / (vz  + vp + vr) *100)  # Pp/z
    Pr <- (vr / (vz  + vp + vr) *100)  # Ps/p
    
    total <-  Pz + Pp + Pr; total # Verificacao
    
    # Cpalculo de Pst
    Pst <- vz /(vz + 2*vp)  #Pst
    
    # Inserindo todos os paramentros estimados em um objeto
    parameters <- data.frame(Pz = Pz, Pp = Pp, Pr = Pr, Pst = Pst)
    
    
    # Teste tukey para niveis de ZEE 
    tZEE <-  data.frame(print(HSD.test(model, "ZEE", group = TRUE)$group))
    
    
######## armazenando e organizando dados para exportacao #########
    
    name <- "Variavel"
    a <- 'Análise de variância'
    b <- "Parametros"
    c <- 'Teste tukey para as ZEEs'
    
    tabl <- cbind(source = rownames(tab),tab)
    tZEE <- cbind(ZEE = rownames(tZEE),tZEE)
    ls1 <- list(name, i, a,tabl,b, parameters, c, tZEE)
    filename <- capture.output(cat(paste0("table_", i, ".csv")))
    lapply(ls1, function(x) write.table( data.frame(x), filename  , append= T , sep=',', row.names = F ))

}
 
# fim



