library(readr)
RAISDF201818a65dfinal <- read.table("RAISDF201818a65d.csv", sep=";", header=T)

######################################################
################Regressões OLS/MQO####################
######################################################

funcpublicos <- RAISDF2018selec %>% filter(trabresPP==2)
funcpsetpriv <- RAISDF2018selec %>% filter(trabresPP==1)
outrosfuncs <- RAISDF2018selec %>% filter(trabresPP==0)


OLSfunpub2 <- lm(funcpublicos$lnremmediaREAL ~ funcpublicos$dhomem+ funcpublicos$Idade+I(funcpublicos$Idade^2)  + funcpublicos$Qtd.Hora.Contr +funcpublicos$fund1comp+funcpublicos$fund2inc+funcpublicos$fund2comp+funcpublicos$medioinc+funcpublicos$mediocomp+funcpublicos$supinc+funcpublicos$supcompmais+funcpublicos$de5a99+funcpublicos$de100a999+funcpublicos$miloumais)
summary(OLSfunpub2)


OLSfunpub <- lm(funcpublicos$lnremmediaREAL ~ funcpublicos$dhomem+ funcpublicos$Idade+I(funcpublicos$Idade^2)  + funcpublicos$Qtd.Hora.Contr +funcpublicos$fund1comp+funcpublicos$fund2inc+funcpublicos$fund2comp+funcpublicos$medioinc+funcpublicos$mediocomp+funcpublicos$supinc+funcpublicos$supcompmais)
summary(OLSfunpub)


OLSfuncpsetpriv<- lm(funcpsetpriv$lnremmediaREAL ~ funcpsetpriv$dhomem+funcpsetpriv$Idade + I(funcpsetpriv$Idade^2)   +funcpsetpriv$Qtd.Hora.Contr + funcpsetpriv$analf++funcpsetpriv$fund1comp+funcpsetpriv$fund2inc+funcpsetpriv$fund2comp+funcpsetpriv$medioinc+funcpsetpriv$mediocomp+funcpsetpriv$supinc+funcpsetpriv$supcompmais+funcpsetpriv$de5a99+funcpsetpriv$de100a999+funcpsetpriv$miloumais)
summary(OLSfuncpsetpriv)


################################################################
################Decomposicoes Oaxaca-Blinder####################
################################################################

table(RAISDF201818a65d$remmediadef)
library(dplyr)
RAISDF2018selec <-  dplyr::select(RAISDF201818a65d, 
                                  Sexo.Trabalhador, 
                                  trabresPP,
                                  Escolaridade.após.2005,
                                  Tamanho.Estabelecimento,
                                  Idade,
                                  Qtd.Hora.Contr,
                                  remmediadef,
                                  salarioREALhora)



#dummy para tamanho do estab
summary(RAISDF2018selec$Tamanho.Estabelecimento)
table(RAISDF2018selec$Tamanho.Estabelecimento)

library(dplyr)

RAISDF2018selec$de5a99 <- ifelse(RAISDF2018selec$Tamanho.Estabelecimento>2 & RAISDF2018selec$Tamanho.Estabelecimento<=6,1,0)
RAISDF2018selec$de100a999 <- ifelse(RAISDF2018selec$Tamanho.Estabelecimento>6 & RAISDF2018selec$Tamanho.Estabelecimento<=9,1,0)
RAISDF2018selec$miloumais <- ifelse(RAISDF2018selec$Tamanho.Estabelecimento==10,1,0)




RAISDF2018selec$dhomem <- ifelse (RAISDF2018selec$Sexo.Trabalhador==1,1,0)
RAISDF2018selec$dmulher <- ifelse (RAISDF2018selec$Sexo.Trabalhador==2,1,0)
table (RAISDF2018selec$trabresPP)

RAISDF2018selec$lnremmediaREAL <-log(RAISDF2018selec$remmediadef)
summary(RAISDF2018selec$lnremmediaREAL)

#atualizar bases

funcpublicos <- RAISDF2018selec %>% filter(trabresPP==2)
funcpsetpriv <- RAISDF2018selec %>% filter(trabresPP==1)
outrosfuncs <- RAISDF2018selec %>% filter(trabresPP==0)

table(funcpublicos$miloumais)
#install.packages("oaxaca")
library(oaxaca)

oaxacafuncpubfin <- oaxaca(lnremmediaREAL ~ Idade+ I(Idade^2) + Qtd.Hora.Contr+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
                           |dmulher|  fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
                           , funcpublicos, reg.fun = lm)
summary(oaxacafuncpubfin)


# oaxacafuncpubfin2 <- oaxaca(lnremmediaREAL ~ Idade+ I(Idade^2) + Qtd.Hora.Contr+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais+de5a99+de100a999+miloumais
#                            |dmulher|  fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
#                            , funcpublicos, reg.fun = lm)
# summary(oaxacafuncpubfin2)

oaxacafuncpsetprivfin <- oaxaca(lnremmediaREAL ~ Idade+ I(Idade^2) + Qtd.Hora.Contr + analf+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais+de5a99+de100a999+miloumais
                                |dmulher|  analf+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
                                , funcpsetpriv, reg.fun = lm)
summary(oaxacafuncpsetprivfin)

# oaxacafuncpsetprivfin2 <- oaxaca(lnremmediaREAL ~ Idade+ I(Idade^2) + Qtd.Hora.Contr + analf+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
#                                 |dmulher|  analf+fund1comp+fund2inc+fund2comp+medioinc+mediocomp+supinc+supcompmais
#                                 , funcpsetpriv, reg.fun = lm)
# summary(oaxacafuncpsetprivfin2)

