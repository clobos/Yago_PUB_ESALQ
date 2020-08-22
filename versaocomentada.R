
#                Carregando os Pacotes              #

library(readxl)
library(tibble)
require(forecast)
require(tseries)
require(timeSeries)
require(lmtest)
require(lattice)
require(xtable)
require(timeSeries)
library(forecast)

#mostrando ao R o caminho onde estao os dados


milho <- read_excel("C:/Users/Beatriz/Desktop/IC/milho.xlsx", 
                    col_types = c("numeric", "numeric", "numeric"))

#A funcao TS transforma o banco de dados milho, que é um data.frame, em um formato ts (serie temporal)

Milho <- ts(milho, start= c(2005,1), end = c(2018,12), frequency= 12)

#Pela funcao class, é possivel saber como o R esta lendo o conjunto de dados

class(Milho)

#A funcao plot permite fazer um grafico de serie temporal.

plot(Milho)

#O plot feito acima fez uma grafico para as 3 variaveis do banco de dados,
#mas como eu quero apenas a variavel preço, seleciona-se a coluna 3

pmilho <- Milho [,3]

#Gráfico da série temporal para verificar a estacionaridade           #

plot(pmilho,xlab='Tempo',ylab='Preços', main = "Milho", type= "l", col= "red") 


#Com base no comportamento do grafico os dados não sao estacionarios, então é preciso realizar uma transformação

par(mfrow=c(2,2)) 
plot(pmilho,xlab='Tempo',ylab='')
hist(pmilho,main='',ylab='Frequência',xlab='')
acf(pmilho, main='',ylab='FAC')
pacf(pmilho, main='',ylab='FACP')


##-------------------------------------------------##
#    Transformando a serie em estacionária         #
##-------------------------------------------------##

#A função ndiffs usa um teste de raiz unitária para determinar o número de diferenças necessárias para que séries temporais sejam feitas estacionárias.
#a hipótese nula é que x tem uma raiz unitária contra uma alternativa de série estacionária. Em seguida, o teste retorna o menor número de diferenças necessárias para falhar o teste no nível alpha.

ndiffs(pmilho,alpha=0.05,test="adf") 

d1s <- diff(pmilho, differences = 1)

par(mfrow=c(2,2)) 
plot(d1s,xlab='Tempo',ylab='1ª Diferença')
hist(d1s,main='',ylab='Frequência',xlab='1ª Diferença')
acf(d1s,main='',ylab='FAC') #Média móvel
pacf(d1s,main='',ylab='FACP') #Lag

##-------------------------------------------------##
#  		   Gráfico das difereças                #
# 	   para verificar a estacionaridade           #
##-------------------------------------------------##

par(mfrow=c(2,1))
plot(pmilho,xlab='Tempo',ylab='Série', main='(a)')
abline(h=mean(pmilho),col='red')
plot(d1s,xlab='Tempo',ylab='1ª Diferença', main='(b)')
abline(h=0,col='red')


##-------------------------------------------------##
#  		   Gráficos do modelo ARIMA 1,1,1                #
# 	             e previsão
##-------------------------------------------------##

milho2 <- read_excel("C:/Users/Beatriz/Desktop/milho2.xlsx", 
                     col_types = c("numeric", "numeric", "numeric"))

ano2 <- ts(milho2, start= c(2019,1), end = c(2019,12), frequency= 12)


a <- milho2 [, 3]


fit6<- forecast::Arima(pmilho, order=c(1,1,1))
fcast_cons6 <-forecast(fit6, h= 12)
autoplot(fcast_cons6)

data.frame(a, fcast_cons6)

teste3<- data.frame(x=milho$preco,
                    y=c(fcast_cons6$fitted,fcast_cons6$mean))
matplot(teste3, type="l",col=1:2, lwd=2)


