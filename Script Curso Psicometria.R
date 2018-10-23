#Operações basicas 
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


# Escolhendo o diretório
getwd()
setwd("C:/Users/wag_p/Dropbox/Curso SBP Psicometria") # ou Clicando (Session-> Set Working Directory ->Choose)

# Instalar pacotes
install.packages("foreign") # para carregar banco de dados do SPSS
install.packages("psych") # Análise fatorial exploratória, Alfa, Omega, Correlações e Pressupostos
install.packages("lavaan") # Análise fatorial Confirmatória
install.packages("semTools") # mais ferramentas
install.packages("semPlot") # Para fazer gráficos da análise fatorial

# Carregando os Pacotes

library(foreign)
library(psych)
library(lavaan)
library(semPlot)


# Carregando banco de dados

library(foreign)
Big5 <- read.spss(file = "BIG FIVE.sav",to.data.frame = TRUE,use.value.labels = TRUE)
ESV<-read.csv(file.choose(),sep=";") #file.choose permite você navegar clicando


# Explorando o banco de dados

names(Big5)
summary(Big5)
pairs.panels(ESV[,-c(6,7)])

# Escolhendo casos e variáveis
ESV[1:10,1:2]
ESV$ESV1
ESV[ , -c(6,7)]


hist(Big5$Pessimista) # variavel categórica

Big5 <- read.spss(file = "BIG FIVE.sav",to.data.frame = TRUE,use.value.labels = FALSE) # carregando novamente sem os labels
hist(Big5$Pessimista) # variavel contínua
hist(Big5$Pessimista, xlab="Respostas", main="Item Pessimismo", col="lightblue") # hist mais bonitinho

plot(x=Big5$Idade,y=Big5$Pessimista)





cor(Big5$Idade,y=Big5$Pessimista, use = "complete.obs")
cor.test(Big5$Idade,y=Big5$Pessimista, use = "complete.obs", method = "spearman") #Teste correlação

# Estatisticas descritivas

describe(Big5$Comunicativa)
describeBy(Big5$Comunicativa, group = Big5$Sexo)
boxplot(Big5$Comunicativa ~ Big5$Sexo) 
t.test(Big5$Comunicativa ~ Big5$Sexo) # o sinal "~" indica que algo Sexo está predizendo a var "comunicativa"


# Subsetting
1:10
bebes[1:10,4:6]
cor(x = bebes[,4:6], use = "pairwise.complete.obs")



### Diagnóstico de itens e pressupostos da Análise Fatorial Exploratória ###

# Pressupostos, correlação e diagnóstico de itens (inspeção gráfica).

# A análise fatorial é uma técnica que parte da hipótese da causa comum, 
# isto é, a correlação entre itens de um teste ou de subtestes
# é explicada por uma ou mais variáveis latentes, não observáveis. 
# Dito de outra forma, a análise fatorial assume um efeito causal a 
# partir de correlações entre observáveis.
# Para investigar essas correlações, vamos observar a distribuição dos itens:

# use este comando para redefinir o plano de gráficos "par(mfrow=c(1,1))"

# construa histogramas das variáveis
hist(ESV[,1])
shapiro.test(ESV[,1])

# observe as correlações entre os itens
cor.plot(ESV[,-c(6,7)],numbers = TRUE,cex = 0.8)

# por fim, produza um gráfico com scatter plot, histogramas e correlações
pairs.panels(ESV[,-c(6,7)])

#KMO
?KMO
KMO(ESV[,-c(6,7)])
# Bartlett
?cortest.bartlett
cortest.bartlett(ESV[,-c(6,7)])

#- Tecnicas de retenção de fatores (Kaiser, Scree test, Análise paralela)

ESV_eig<-eigen(cor(ESV[,-c(6,7)]))
plot(ESV_eig$values,type="b")

# Análise paralela
fa.parallel(ESV[,-c(6,7)],cor="poly",fa="fa")

# Análise fatorial 
fa(ESV[,-c(6,7)],cor="poly",fm="minrank")
# Cargas fatoriais, comunalidade, uniqueness 

# Fidedignidade: alpha e Lambda 6 Guttman
alpha(ESV[,-c(6,7)])

# Modelos multidiensionais 
fa.parallel(Big5[,-c(26,27)],cor="poly",fa="fa")
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank")

# rotação oblíqua
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
# Permite usar o escore modelado para investigar associações com variáveis relevantes

# teste t por sexo
t.test(Big5_fa$scores[,1]~Big5$Sexo)
t.test(Big5_fa$scores[,2]~Big5$Sexo)
t.test(Big5_fa$scores[,3]~Big5$Sexo)
t.test(Big5_fa$scores[,4]~Big5$Sexo)
t.test(Big5_fa$scores[,5]~Big5$Sexo)
boxplot(Big5_fa$scores[,5]~Big5$Sexo)

# correlações com variáveis externas/critério
plot(Big5_fa$scores[,4],Big5$Idade)
abline(lm(Big5_fa$scores[,4]~Big5$Idade), col="red")
cor(Big5_fa$scores[,4],Big5$Idade)


#### Análise Fatorial Confirmatória ####

names(ESV)

cor(ESV[,-c(6,7)], use = "complete.obs")
hist(ESV$ESV2)

KMO(ESV[,-c(6,7)])
cortest.bartlett(ESV[,-c(6,7)])

# Testando unidimensionalidade através da CFA

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

fitBig5Mod <- cfa(model = Big5Mod ,data = Big5, estimator="MLM") # fazendo análise
summary(fitBig5Mod, fit.measures=TRUE, standardized =TRUE, rsq=TRUE) # Resultados da CFA

indmodificacao <- modificationindices(fitBig5Mod, sort. = TRUE) # Indices de modificação - indicam quais mudanças melhoram o modelo
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

fitBig5Mod2 <- cfa(model = Big5Mod2 ,data = Big5, estimator="MLM") # fazendo análise
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
with(dat, plot(x,y, type="l", main="OBRIGADO PELA PARTICIPAÇÃO!"))
with(dat, polygon(x,y, col="lightblue")) 
