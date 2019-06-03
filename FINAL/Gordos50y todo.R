muestra <- 50
replicas <- 30
duracion <- 24
vida <- 100
engordar <- (0.35)*vida
decaido <- (0.15)*vida
insomnio <- (0.30)*vida
flojo <- (0.20)*vida

puntos <- replicas*duracion
pgor <- engordar/puntos
pdep <- decaido/puntos
pin <- decaido/puntos
psen <- flojo/puntos

desde <- 0  #mala alimentacion
hasta <- 0.45

desde1 <- 0.1  #depresion
hasta1 <- 0.6

desde2 <- 0.1  #falta de dormir
hasta2 <- 0.5

desde3 <- 0.4  #sedentarismo
hasta3 <- 0.8

comer <- function(n){
  fat <- runif(1, desde, hasta)
  if (fat <= 0.29){
    return(TRUE)
  }
  if (fat > 0.29){
    return(FALSE)
  }
  return(TRUE)
}

depre <- function(n){
  dep <- runif(1, desde1, hasta1)
  if (dep <= 0.35){
    return(TRUE)
  }
  if (dep > 0.35){
    return(FALSE)
  }
  return(TRUE)
}

no.dormir <- function(n){
  desv <- runif(1, desde2, hasta2)
  if (desv <= 0.3){
    return(TRUE)
  }
  if (desv > 0.3){
    return(FALSE)
  }
  return(TRUE)
}

seden <- function(n){
  sed <- runif(1, desde3, hasta3)
  if (sed <= 0.65){
    return(TRUE)
  }
  if (sed > 0.65){
    return(FALSE)
  }
  return(TRUE)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores(8) - 1))

buffet <-  numeric()
depresion <-  numeric()
dorm <-  numeric()
cdntario <-  numeric()

for (gente in 1:muestra) {
  for (dias in 1:replicas) {
    for (meses in 1:duracion) {
      buffet <- c(buffet, foreach(n = muestra, .combine=c) %dopar% comer(n)) 
      depresion <- c(depresion, foreach(n = muestra, .combine=c) %dopar% depre(n)) 
      dorm <- c(dorm, foreach(n = muestra, .combine=c) %dopar% no.dormir(n))
      cdntario <- c(cdntario, foreach(n = muestra, .combine=c) %dopar% seden(n))
    }
    stopImplicitCluster()
  }
}
total <- data.frame(buffet)
total1 <- data.frame(depresion)
total2 <- data.frame(dorm)
total3 <- data.frame(cdntario)
x1 <- total[1:3000,]
x2 <- total[1:6000,]
x3 <- total[1:9000,]
x4 <- total[1:12000,]
x5 <- total[1:15000,]
x6 <- total[1:18000,]
x7 <- total[1:21000,]
x8 <- total[1:24000,] 
x9 <- total[1:27000,]
x10 <- total[1:30000,]
x11 <- total[1:33000,]
x12 <- total[1:36000,]

b1 <- length(x1[x1==TRUE])
b2 <- length(x2[x2==TRUE])
b3 <- length(x3[x3==TRUE])

b4 <- length(x1[x1==FALSE])
b5 <- length(x2[x2==FALSE])
b6 <- length(x3[x3==FALSE])
##########
b7 <- length(x4[x4==TRUE])
b8 <- length(x5[x5==TRUE])
b9 <- length(x6[x6==TRUE])

b10 <- length(x4[x4==FALSE])
b11 <- length(x5[x5==FALSE])
b12 <- length(x6[x6==FALSE])
###############
b13 <- length(x7[x7==TRUE])
b14 <- length(x8[x8==TRUE])
b15 <- length(x9[x9==TRUE])

b16 <- length(x7[x7==FALSE])
b17 <- length(x8[x8==FALSE])
b18 <- length(x9[x9==FALSE])
##############
b19 <- length(x10[x10==TRUE])
b20 <- length(x11[x11==TRUE])
ban <- length(x12[x12==TRUE])

b21 <- length(x10[x10==FALSE])
b22 <- length(x11[x11==FALSE])
bin <- length(x12[x12==FALSE])
#######################################################
trago3 <- ((b4-b1)*pgor)
trago6 <- ((b5-b2)*pgor)
trago9 <- ((b6-b3)*pgor)
trago12 <- ((b10-b7)*pgor)
trago15 <- ((b11-b8)*pgor)
trago18 <- ((b12-b9)*pgor)
trago21 <- ((b16-b13)*pgor)
trago24 <- ((b17-b14)*pgor)
trago27 <- ((b18-b15)*pgor)
trago30 <- ((b21-b19)*pgor)
trago33 <- ((b22-b20)*pgor)
trago36 <- ((bin-ban)*pgor)
###############################
mes3 <- (trago3+decaido3+no.dormir3+quieto3)/50
mes6 <- (trago6+decaido6+no.dormir6+quieto6)/50
mes9 <- (trago9+decaido9+no.dormir9+quieto9)/50
mes12 <- (trago12+decaido12+no.dormir12+quieto12)/50
mes15 <- (trago15+decaido15+no.dormir15+quieto15)/50
mes18 <- (trago18+decaido18+no.dormir18+quieto18)/50
mes21 <- (trago21+decaido21+no.dormir21+quieto21)/50
mes24 <- (trago24+decaido24+no.dormir24+quieto24)/50
mes27 <- (trago27+decaido27+no.dormir27+quieto27)/50
mes30 <- (trago30+decaido30+no.dormir30+quieto30)/50
mes33 <- (trago33+decaido33+no.dormir33+quieto33)/50
mes36 <- (trago36+decaido36+no.dormir36+quieto36)/50
##################################################################
a <- (vida+mes3)
b <- (vida+mes6) 
c <- (vida+mes9)
d <- (vida+mes12)
e <- (vida+mes15)
f <- (vida+mes18)
g <- (vida+mes21)
h <- (vida+mes24)
i <- (vida+mes27)
j <- (vida+mes30)
k <- (vida+mes33)
l <- (vida+mes36)
gra <- c(a, b, c, d, e, f, g, h, i, j, k, l)

rr <- c(1:12)
eti <- c("2", "4","6", "8","10", "12", "14", "16", "18", "20", "22", "24")

png("calidadvida2.png", units="cm", width=17.5, height=12.5, res=600)
plot(gra, xaxt="n", ann=T, type="o", xaxt="n", ylim = c(0,100),  
     main = "Estatus vida", cex.main=1.5, xlab = "Meses",
     ylab = "Calidad vida población", lwd=2, cex.lab=1.3)
axis(side=1, at=rr, labels=eti)
grid()
graphics.off()