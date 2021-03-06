#Opera��es basicas 
1+1
4/2
4^2
4*2
4-5
4.5-2.1
sqrt(4) #raiz quadrada
1+2+2*(2+2)


# Assinalando Objetos
x <-1+4
x+1
y <-10
y
class(y)
y+x
y<x
y==x
y!=x
alturas<- c(169,178,169,175) #Criando var
mean(alturas)
mean(alturas/100)
alturas
z <- c(10,100,10,10)
alturas * z


# Escolhendo o diret�rio
getwd()
setwd("C:/Users/wag_p/Dropbox/Curso SBP Psicometria") # ou Clicando (Session-> Set Working Directory ->Choose)

# Instalar pacotes
install.packages("foreign") # para carregar banco de dados do SPSS
install.packages("psych") # An�lise fatorial explorat�ria, Alfa, Omega, Correla��es e Pressupostos
install.packages("lavaan") # An�lise fatorial Confirmat�ria
install.packages("semTools") # mais ferramentas
install.packages("semPlot") # Para fazer gr�ficos da an�lise fatorial

# Carregando os Pacotes

library(foreign)
library(psych)
library(lavaan)
library(semPlot)


# Carregando banco de dados

library(foreign)
Big5 <- read.spss(file = "BIG FIVE.sav",to.data.frame = TRUE,use.value.labels = TRUE)
ESV<-read.csv(file.choose(),sep=";") #file.choose permite voc� navegar clicando


# Explorando o banco de dados

names(Big5)
summary(Big5)
pairs.panels(ESV[,-c(6,7)])

# Escolhendo casos e vari�veis
ESV[1:10,1:2]
ESV$ESV1
ESV[ , -c(6,7)]


hist(Big5$Pessimista) # variavel categ�rica

Big5 <- read.spss(file = "BIG FIVE.sav",to.data.frame = TRUE,use.value.labels = FALSE) # carregando novamente sem os labels
hist(Big5$Pessimista) # variavel cont�nua
hist(Big5$Pessimista, xlab="Respostas", main="Item Pessimismo", col="lightblue") # hist mais bonitinho

plot(x=Big5$Idade,y=Big5$Pessimista)





cor(Big5$Idade,y=Big5$Pessimista, use = "complete.obs")
cor.test(Big5$Idade,y=Big5$Pessimista, use = "complete.obs", method = "spearman") #Teste correla��o

# Estatisticas descritivas

describe(Big5$Comunicativa)
describeBy(Big5$Comunicativa, group = Big5$Sexo)
boxplot(Big5$Comunicativa ~ Big5$Sexo) 
t.test(Big5$Comunicativa ~ Big5$Sexo) # o sinal "~" indica que algo Sexo est� predizendo a var "comunicativa"


# Subsetting
1:10
bebes[1:10,4:6]
cor(x = bebes[,4:6], use = "pairwise.complete.obs")



### Diagn�stico de itens e pressupostos da An�lise Fatorial Explorat�ria ###

# Pressupostos, correla��o e diagn�stico de itens (inspe��o gr�fica).

# A an�lise fatorial � uma t�cnica que parte da hip�tese da causa comum, 
# isto �, a correla��o entre itens de um teste ou de subtestes
# � explicada por uma ou mais vari�veis latentes, n�o observ�veis. 
# Dito de outra forma, a an�lise fatorial assume um efeito causal a 
# partir de correla��es entre observ�veis.
# Para investigar essas correla��es, vamos observar a distribui��o dos itens:

# use este comando para redefinir o plano de gr�ficos "par(mfrow=c(1,1))"

# construa histogramas das vari�veis
hist(ESV[,1])
shapiro.test(ESV[,1])

# observe as correla��es entre os itens
cor.plot(ESV[,-c(6,7)],numbers = TRUE,cex = 0.8)

# por fim, produza um gr�fico com scatter plot, histogramas e correla��es
pairs.panels(ESV[,-c(6,7)])

#KMO
?KMO
KMO(ESV[,-c(6,7)])
# Bartlett
?cortest.bartlett
cortest.bartlett(ESV[,-c(6,7)])

#- Tecnicas de reten��o de fatores (Kaiser, Scree test, An�lise paralela)

ESV_eig<-eigen(cor(ESV[,-c(6,7)]))
plot(ESV_eig$values,type="b")

# An�lise paralela
fa.parallel(ESV[,-c(6,7)],cor="poly",fa="fa")

# An�lise fatorial 
fa(ESV[,-c(6,7)],cor="poly",fm="minrank")
# Cargas fatoriais, comunalidade, uniqueness 

# Fidedignidade: alpha e Lambda 6 Guttman
alpha(ESV[,-c(6,7)])

# Modelos multidiensionais 
fa.parallel(Big5[,-c(26,27)],cor="poly",fa="fa")
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank")

# rota��o obl�qua
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin")

# Fidedignidade: alpha e Lambda 6 Guttman
# use o argumento "check.keys = TRUE" no caso de itens invertidos
alpha(Big5[,c(1,6,11,16,21)],check.keys = TRUE)
alpha(Big5[,c(2,7,12,17,22)])
alpha(Big5[,c(3,8,13,18,23)])
alpha(Big5[,c(4,9,14,19,24)])
alpha(Big5[,c(5,10,15,20,25)])

# Escores fatoriais
Big5_fa<-fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin",scores = "regression")
View(Big5_fa$scores)
# Permite usar o escore modelado para investigar associa��es com vari�veis relevantes

# teste t por sexo
t.test(Big5_fa$scores[,1]~Big5$Sexo)
t.test(Big5_fa$scores[,2]~Big5$Sexo)
t.test(Big5_fa$scores[,3]~Big5$Sexo)
t.test(Big5_fa$scores[,4]~Big5$Sexo)
t.test(Big5_fa$scores[,5]~Big5$Sexo)
boxplot(Big5_fa$scores[,5]~Big5$Sexo)

# correla��es com vari�veis externas/crit�rio
plot(Big5_fa$scores[,4],Big5$Idade)
abline(lm(Big5_fa$scores[,4]~Big5$Idade), col="red")
cor(Big5_fa$scores[,4],Big5$Idade)


#### An�lise Fatorial Confirmat�ria ####

names(ESV)

cor(ESV[,-c(6,7)], use = "complete.obs")
hist(ESV$ESV2)

KMO(ESV[,-c(6,7)])
cortest.bartlett(ESV[,-c(6,7)])

# Testando unidimensionalidade atrav�s da CFA

# Especificando o modelo

cor(ESV[,-c(6,7)], method = "spearman")

ESV.Mod <- "
SV =~ ESV1 + ESV2 + ESV3+ ESV4 + ESV5
"

Fit.ESV.Mod <- cfa(model = ESV.Mod,data = ESV)

summary(Fit.ESV.Mod,fit.measures = TRUE, standardized=TRUE, rsq=TRUE)

semPaths(Fit.ESV.Mod, what = "std", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)
# Fidedignidade
reliability(Fit.ESV.Mod)

# computando Escores fatoriais

ESV$Sat_Vida <- predict(Fit.ESV.Mod)
describe(ESV$Sat_Vida)


#Dever de casa
# CFA Big5

Big5Mod <- "
Agrad =~ Simpatica + Amavel + Gentil + Bondosa + Compreensiva + Cuidadosa
Extro =~ Comunicativa + Timida + Quieta+ Inibida + Desembaracada
Consc =~  Esforcada + Dedicada + Responsavel + Organizada
Neuro =~ Aborrecida + Deprimida + Ansiosa + Insegura + Pessimista
Abert =~ Criativa + Artistica + Audaciosa + Filosofica + Aventureira

"

fitBig5Mod <- cfa(model = Big5Mod ,data = Big5, estimator="MLM") # fazendo an�lise
summary(fitBig5Mod, fit.measures=TRUE, standardized =TRUE, rsq=TRUE) # Resultados da CFA

indmodificacao <- modificationindices(fitBig5Mod, sort. = TRUE) # Indices de modifica��o - indicam quais mudan�as melhoram o modelo
indmodificacao

Big5Mod2 <- "
Agrad =~ Simpatica + Amavel + Gentil + Bondosa + Compreensiva + Cuidadosa   +Comunicativa
Extro =~ Comunicativa + Timida + Quieta+ Inibida + Desembaracada            +Insegura + Simpatica
Consc =~  Esforcada + Dedicada + Responsavel + Organizada                   +Cuidadosa
Neuro =~ Aborrecida + Deprimida + Ansiosa + Insegura + Pessimista
Abert =~ Criativa + Artistica + Audaciosa + Filosofica + Aventureira        +Desembaracada

Comunicativa ~~ Desembaracada
Compreensiva ~~ Cuidadosa
Audaciosa ~~   Aventureira
"

fitBig5Mod2 <- cfa(model = Big5Mod2 ,data = Big5, estimator="MLM") # fazendo an�lise
summary(fitBig5Mod2, fit.measures=TRUE, standardized =TRUE, rsq=TRUE) # Resultados da CFA
modificationindices(fitBig5Mod2,sort. = T)

# Comparando os modelos

anova(fitBig5Mod, fitBig5Mod2)

semPaths(fitBig5Mod2, what = "std", edge.label.cex = 0.7,
         edge.color = 1, esize = .5, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)


# Finalizando....


dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l", main="OBRIGADO PELA PARTICIPA��O!"))
with(dat, polygon(x,y, col="lightblue")) 
