require(quantmod)
options("getSymbols.warning4.0" = F,
            "getSymbols.auto.assign" = F)

#loading s$p500 data
SPY <- suppressWarnings(getSymbols(c("SPY"),from = "2012-01-01"))
SPY <- as.numeric(SPY$SPY.Close)
#repetição das mesmas series

set.seed(123)
#intervalo de tempo em uma var

t <- 1:(length(SPY)-1)
#configurando balanço inicial de 1000 dolares de uma
#carteira hipotética

vt  <-  c(rep(1000,length(t)))
#retornos S&P500

Rb <- rep(NA,length(t))
for(i in 2:length(t)) 
  {
  Rb[i] <- (SPY[i]/SPY[i-1])-1
  
  }

#calculando retornos com base nos 1000 dolares da carteira
eb <- rep(NA,length(t))
eb[1] <- vt[1]
for (i in 2:length(t)) 
  {
  eb[i]<- eb[i-1]*(1 + Rb[i])
  
  }

#calculando retornos aleatórios de 2 series temporais
#entendendo o desvio padrão no ind. sharpe
#primeiro media de retornos com 0.24 e desv. pdrão de 2.5


Rt <-rep(NA,length(t))
  for (i in 2:length(t)) 
    {
    Rt[i]<- Rb[i] + rnorm(n = 1,
                          mean = 0.24/length(t),
                          sd = 2.5*sd(Rb,na.rm = T))
    
    }

#calculando retornos aleatórios de 2 series temporais
#entendendo o desvio padrão no ind. sharpe
#segundo media de retornos de 0.02 de desv. padrão de 0.75

Rt2 <- rep(NA,length(t))
  for (i in 2:length(t)) 
    {
    Rt2[i]<- Rb[i] + rnorm(n=1,
                           mean = 0.02/length(t),
                           sd=0.75*sd(Rb,na.rm = T))
    
  }

#calculo hipotetico dos 1000 dolares para os dois retornos
#simulados

Et <- rep(NA,length(t))
Et[1]<- vt[1]
for (i in 2:length(t)) {
  Et[i]<- Et[i-1]*(1 + Rt[i])
  
}
#calculo hipotetico dos 1000 dolares para os dois retornos
#simulados 2

Et2 <- rep(NA,length(t))
Et2[1]<- vt[1]
for (i in 2:length(t)) {
  Et2[i]<- Et2[i-1]*(1 + Rt2[i])
  
}

#plotando os resultados

plot(y = Et,x = t,type = "l",col= 1,
     xlab = "time",
     ylab = "equity ($)",
     main = "randon equity curves and S&P500 equity")

grid()
abline(h = 10000)
lines(y = Et2, x = t,col = 2)
lines(y = eb, x = t,col = 8)
legend(x = "topleft",col = c(1,2,8),lwd = 2,
       legend = c("curve 1","curve 2","S&P500"))


#calculo indice de sharpe

SR <- mean(Rt,na.rm = T)/sd(Rt,na.rm = T)
SR2 <- mean(Rt2,na.rm = T)/sd(Rt2,na.rm = T)
Srb <-  mean(Rb,na.rm = T)/sd(Rb,na.rm = T)

#plotando e adicionando indice de sharpe

plot(y = Et,x = t,type = "l",col = 1,
     xlab = "time",
     ylab = "equity($)",
     main = "random equity curves and S&P500 equity")

grid()
abline(h = 10000)
lines(y = Et2,x = t,col = 2)
lines(y = eb,x = t,col = 8)
legend(x = "topleft",col = c(1,2,8),lwd = 2,
       legend = c(paste0("sr = ",round(SR,3)),
                  paste0("SR2 = ",round(SR2,3)),
                  paste0("Srb = ",round(Srb,3))
       )
)




































