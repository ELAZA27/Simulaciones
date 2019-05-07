library(ggplot2)
library(reshape2)
library(microbenchmark)
library(parallel)

binario <- function(d, l) {
  b <- rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995 # negros
modelos[modelos=='g'] <- 0.92 # grises
modelos[modelos=='b'] <- 0.002 # blancos

r <- 5
c <- 3
dim <- r * c

for (j in 1:n) {
  d <- sample(0:9, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  imagen <- matrix(pixeles, nrow=r, ncol=c, byrow=TRUE)
  print(imagen)
  plot.sociomatrix(imagen, drawlab=FALSE, diaglab=FALSE,
                   main=paste(d, ""), cex.main=5)
}
graphics.off()

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

repetitions <- 30

trainNP <- microbenchmark(
  for (t in 1:5000) { # entrenamiento
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,]
    correcto <- binario(d, n)
    for (i in 1:n) {
      w <- neuronas[i,]
      deseada <- correcto[i]
      resultado <- sum(w * pixeles) >= 0
      if (deseada != resultado) {
        ajuste <- tasa * (deseada - resultado)
        tasa <- tranqui * tasa
        neuronas[i,] <- w + ajuste * pixeles
      }
    }
  }
  , times = repetitions, unit = "s")

testNP <- microbenchmark({
  contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
  rownames(contadores) <- 0:tope
  colnames(contadores) <- c(0:tope, NA)
  
  for (t in 1:300) { # prueba
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    correcto <- binario(d, n)
    salida <- rep(FALSE, n)
    for (i in 1:n) {
      w <- neuronas[i,]
      deseada <- correcto[i]
      resultado <- sum(w * pixeles) >= 0
      salida[i] <- resultado
    }
    r <- min(decimal(salida, n), k) # todos los no-existentes van al final
    contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
  }
}
, times = repetitions, unit = "s")

newrons <- function(i, neuronas, pixeles){
  w <- neuronas[i,]
  return(sum(w * pixeles) >= 0)
} 

newr <- function(){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  
  salida <- sapply(1:n, newrons, neuronas, pixeles)
  
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  return(c(d+1, r+1))
}

suppressMessages(library(doParallel))
Mc <- makeCluster(detectCores() - 1)
registerDoParallel(Mc)

testP <- microbenchmark({
  contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
  rownames(contadores) <- 0:tope
  colnames(contadores) <- c(0:tope, NA)
  conta <- foreach(t = 1:300, .combine = "rbind", .export = c("tope", "dim",
                                                              "modelos", "neuronas",
                                                              "pixeles")) %dopar% newr()
  for (i in 1:300){
    contadores[conta[i, 1], conta[i, 2]] <- contadores[conta[i, 1], conta[i, 2]] + 1
  }
}
, times = repetitions, unit = "s")
stopCluster(Mc)

trains <- cbind("Paralela" = trainNP$time)
tests <- cbind("Paralela" = testNP$time, "Secuencial" =testP$time)

tests <- melt(tests)
ggplot(tests, aes(x=as.factor(Var2), y=value/10000000)) + geom_violin(aes(fill=as.factor(Var2))) + 
  geom_boxplot(fill= "#d6d4bc", width=0.1, lwd =1.5) + scale_fill_manual( values=c("orange", "brown")) +
  labs(x = "Ejecución", y = "Segundos") + theme_light(base_size = 14) + guides(fill=FALSE) 
ggsave("Tejec.png")

library(microbenchmark)
library(ggplot2)
library(grid)
library(gridExtra)
library(xtable)

binario <- function(d, l) {
  b <- rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

newrons <- function(i, neuronas, pixeles){
  w <- neuronas[i,]
  return(sum(w * pixeles) >= 0)
} 

newr <- function(){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- sapply(1:n, newrons, neuronas, pixeles)
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  return(c(d+1, r+1))
}

Neg <- c(0.8, 0.9, 0.995)
Gri <- c(0.85, 0.95, 0.992)
Bla <- c(0.002, 0.1, 0.2)
data <- data.frame("Porcentaje"=numeric(), "N" = numeric(), "G" = numeric(), "B"= numeric())
for (negro in Neg){
  for (gris in Gri) {
    for (blanco in Bla){
      
      modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
      modelos[modelos=='n'] <- negro
      modelos[modelos=='g'] <- gris
      modelos[modelos=='b'] <- blanco
      
      r <- 5
      c <- 3
      dim <- r * c
      
      tasa <- 0.15
      tranqui <- 0.99
      
      tope <- 9
      digitos <- 0:tope
      k <- length(digitos)
      n <- floor(log(k-1, 2)) + 1
      neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
      
      for (t in 1:5000) { # entrenamiento
        d <- sample(0:tope, 1)
        pixeles <- runif(dim) < modelos[d + 1,]
        correcto <- binario(d, n)
        for (i in 1:n) {
          w <- neuronas[i,]
          deseada <- correcto[i]
          resultado <- sum(w * pixeles) >= 0
          if (deseada != resultado) {
            ajuste <- tasa * (deseada - resultado)
            tasa <- tranqui * tasa
            neuronas[i,] <- w + ajuste * pixeles
          }
        }
      }
      
      suppressMessages(library(doParallel))
      Mc1 <- makeCluster(detectCores() - 1)
      registerDoParallel(Mc1)
      
      contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
      rownames(contadores) <- 0:tope
      colnames(contadores) <- c(0:tope, NA)
      conta <- foreach(t = 1:300, .combine = "rbind", .export = c("tope", "dim", "modelos", "neuronas", "pixeles")) %dopar% newr()
      for (i in 1:300){
        contadores[conta[i, 1], conta[i, 2]] <- contadores[conta[i, 1], conta[i, 2]] + 1
      }
      stopCluster(Mc1)
     
      atino <- numeric()
      noatino <- numeric()
      for (i in 1:(dim(contadores)[1])){
        noatino[i] <- 0
        for (j in 1:(dim(contadores)[2])){
          if(i == j){
            atino[i] <- contadores[i,j]
          } else {
            noatino[i] <- sum(noatino[i], contadores[i,j])
          }
        }
      }
      porcen <- numeric()
      for (i in 1:length(atino)){
        porcen[i] <- noatino[i]/(atino[i]+noatino[i])
        por <- c(porcen[i], negro, gris, blanco)
        data <- rbind(data, por)
      }
    }
  }
}
colnames(data) <- c("Error", "Negro", "Gris", "Blanco")
data$Neg <- as.factor(data$Neg)
data$Gri <- as.factor(data$Gri)
data$Bla <- as.factor(data$Bla)
lm_model <- lm(Error ~ Neg * Gri * Bla, data = data)
av <- aov(lm_model)
data$Ajustados <- fitted(av)
data$Residuales <- residuals(av)
summary(av)
plot(av)

print(xtable(summary(av), type='latex'), file="tabla.tex")
ggQQ <- function(lm) {
  # https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2/19990107#19990107
  d <- data.frame(std.resid = rstandard(lm))
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape=1, size=3) +           # open circles
    labs(title=NULL,             # plot title
         x="Quantiles te\u{f3}ricos",      # x-axis label
         y="Residuales estandarizados") +   # y-axis label
    geom_abline(slope = slope, intercept = int, col = "red", lwd = 1)  # dashed reference line
  p <- p + theme_light(base_size = 20)
  return(p)
}
ggQQ(lm_model)
ggsave("Evalnorm.png")