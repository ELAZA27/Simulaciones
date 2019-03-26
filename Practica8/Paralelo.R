library(testit) # para pruebas, recuerda instalar antes de usar
library(parallel)
paralelo <- data.frame()
k <- 15000
n <- 1500000
cluster <- makeCluster(detectCores() - 1)

romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}

r <- function(i){
  return(as.vector(romperse(as.numeric(freq[i, 1]), as.numeric(freq[i, 2]))))
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

union <- function(x) {
  return (exp(-x / c))
}

u <- function(i){
  return(as.vector(unirse(as.numeric(freq[i, 1]), as.numeric(freq[i, 2]))))
}

clusterExport(cluster, "romperse")
clusterExport(cluster, "rotura")
clusterExport(cluster, "unirse")
clusterExport(cluster, "assert")
clusterExport(cluster, "union")
clusterExport(cluster, "u")

    for (replicas in 1:50) {
      inicial1<- Sys.time()
      originales <- rnorm(k)
      cumulos <- originales - min(originales) + 1
      cumulos <- round(n * cumulos / sum(cumulos))
      assert(min(cumulos) > 0)
      diferencia <- n - sum(cumulos)
      if (diferencia > 0) {
        for (i in 1:diferencia) {
          p <- sample(1:k, 1)
          cumulos[p] <- cumulos[p] + 1
        }
      } else if (diferencia < 0) {
        for (i in 1:-diferencia) {
          p <- sample(1:k, 1)
          if (cumulos[p] > 1) {
            cumulos[p] <- cumulos[p] - 1
          }
        }
      }
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      assert(sum(cumulos) == n)
      c <- median(cumulos) # tamanio critico de cumulos
      d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
      clusterExport(cluster, "c")
      clusterExport(cluster, "d")
      
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      duracion <- 5
      digitos <- floor(log(duracion, 10)) + 1
      for (paso in 1:duracion) {
        assert(sum(cumulos) == n)
        cumulos <- integer()
        clusterExport(cluster, "freq")
        cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), r))
       
        acomoda <- c()
        for(i in 1:length(cumulos)){
          acomoda <-c(acomoda, cumulos[[i]])
        }
        cumulos <- acomoda
        assert(sum(cumulos) == n)
        assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
        freq <- as.data.frame(table(cumulos)) # actualizar urnas
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)
        cumulos <- integer()
        clusterExport(cluster, "freq")
        cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), u))
        
        acomoda <- c()
        for(i in 1:length(cumulos)){
          acomoda <-c(acomoda, cumulos[[i]])
        }
        cumulos <- acomoda
        assert(sum(abs(cumulos)) == n)
        assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
        juntarse <- -cumulos[cumulos < 0]
        cumulos <- cumulos[cumulos > 0]
        assert(sum(cumulos) + sum(juntarse) == n)
        nt <- length(juntarse)
        if (nt > 0) {
          if (nt > 1) {
            juntarse <- sample(juntarse)
            for (i in 1:floor(nt / 2) ) {
              cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
            }
          }
          if (nt %% 2 == 1) {
            cumulos <- c(cumulos, juntarse[nt])
          }
        }
        assert(sum(cumulos) == n)
        freq <- as.data.frame(table(cumulos))
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)
        tl <- paste(paso, "", sep="")
        while (nchar(tl) < digitos) {
          tl <- paste("0", tl, sep="")
        }
        png(paste("p8_ct", tl, ".png", sep=""), width=300, height=300)
        tope <- 50 * ceiling(max(cumulos) / 50)
        hist(cumulos, breaks=seq(0, tope, 50), 
             main=paste("Paso", paso, "con ambos fenómenos"), freq=FALSE,
             ylim=c(0, 0.05), xlab="Tamaño", ylab="Frecuencia relativa")
        graphics.off()
      }
      termino1 <- Sys.time()
      paralelo <- rbind(paralelo, c(termino1-inicial1, k, n, replicas))
    }


names(paralelo) <- c("Tiempo", "Moleculas", "Cumulos", "Replicas")
paralelo$Nivel <- "P"
totaltiempos <- rbind(secuencial, paralelo)
png("Comparación de métodos.png")
boxplot(totaltiempos$Tiempo~totaltiempos$Nivel, main = "Comparación de métodos", ylab = "Tiempo en minutos", col = c("brown", "grey87"), names = c("Secuencial", "Paralela"))
graphics.off()