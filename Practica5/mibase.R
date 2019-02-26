f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra
desde <- 3
hasta <- 7
cuantos <- 500
Wolfram <- 0.04883505
lista <- data.frame()
parte <- function() {
  valores <- generador(Tamaño)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() -1 ))


for (Tamaño in c(100,1000,10000,100000)) {
  print(paste("Tamaño", Tamaño))
  for (repeticiones in 1:30) {
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
     integral <- sum(montecarlo) / (cuantos * Tamaño)
    resultados <- (pi / 2) * integral
    diferencia <- (Wolfram-resultados)
    lista <- rbind(lista, c(repeticiones, Tamaño, Wolfram, resultados, diferencia))
     }
}
stopImplicitCluster()

deco <- c("gray", "yellow", "green", "pink")
names(lista)=c("Repeticion","Tamaño_población","Wolfram","Resultado","Diferencias")

png("rangodeerror.png") 
boxplot(data=lista,Diferencias~Tamaño_población,xlab="Tamaño de muestra",ylab="Valores", boxwex=0.35, col=deco)
abline(h=0, col="red")
graphics.off()


png("finalcomparacion.png") 
boxplot(data=lista,Resultado~Tamaño_población,xlab="Tamaño de muestra",ylab="Aproximacion al valor Wolfram Alpha", boxwex=0.35, col=deco)
abline(h=0.04883505, col="brown")
graphics.off()

