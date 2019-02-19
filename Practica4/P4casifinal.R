tamaño <- c(50, 60, 70)
semillitas <- c(20,25,30)

celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}

propaga <- function(replica) {
  # probabilidad de propagacion interna
  prob <- 1
  dificil <- 0.99
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  while (TRUE) { # hasta que la propagacion termine
    grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
    largo <-  largo + 1
    frontera <- numeric()
    interior <- numeric()
    for (v in 1:vc) {
      vecino <- vp[v,]
      xs <- xg + vecino$dx # columna del vecino potencial
      ys <- yg + vecino$dy # fila del vecino potencial
      if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
        if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
          if (voronoi[yg, xg] == voronoi[ys, xs]) {
            interior <- c(interior, v)
          } else { # frontera
            frontera <- c(frontera, v)
          }
        }
      }
    }
    elegido <- 0
    if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
      if (length(frontera) > 1) {
        elegido <- sample(frontera, 1)
      } else {
        elegido <- frontera # sample sirve con un solo elemento
      }
      prob <- 1 # estamos nuevamente en la frontera
    } else if (length(interior) > 0) { # no hubo frontera para propagar
      if (runif(1) < prob) { # intentamos en el interior
        if (length(interior) > 1) {
          elegido <- sample(interior, 1)
        } else {
          elegido <- interior
        }
        prob <- dificil * prob # mas dificil a la siguiente
      }
    }
    if (elegido > 0) { # si se va a propagar
      vecino <- vp[elegido,]
      xg <- xg + vecino$dx
      yg <- yg + vecino$dy
    } else {
      break # ya no se propaga
    }
  }
  if (largo >= limite) {
    png(paste("p4g_", replica, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
  }
  return(largo)
}
#for (r in 1:10) { # para pruebas sin paralelismo
#    propaga(r)
#}

limite <- n # grietas de que largo minimo queremos graficar

inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}

vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]


#n tamaño de la matriz
for (n in tamaño){
  print(paste("matriz ", n))
  for(k in semillitas){
    print(paste("total semillas ", k))
    limite <- n
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k) # almacenar las coordenadas "x" de las semillas
    y <- rep(0, k) # coordenadas "y" de las semillas
    
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1) 
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores() - 1))
    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    stopImplicitCluster()
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
    rotate <- function(x) t(apply(x, 2, rev))
    png("p4s.png")
    par(mar = c(0,0,0,0))
    image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
    png("p4c.png")
    par(mar = c(0,0,0,0))
    image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores() - 1))
    largos <- foreach(r = 1:200, .combine=c) %dopar% propaga(r)
    stopImplicitCluster()
    summary(largos)
    
  
  }
}

tablita<-matrix(resultados,nrow =3200,ncol=2,byrow = TRUE)
tablita<-as.data.frame(tablita)
names(tablita)<-c("grieta","manhattan")
replicas<-(rep(1:200,16))
matricula<-c(rep(50,1066),rep(60,1067),rep(70,1067))
p<-c(rep(20,213),rep(25,213),rep(30,214))
semillas<-rep(p,5)
datos<-data.frame()
datos<-cbind(matricula,semillas,replicas,tablita)
dm<-dist(datos,method="manhattan")
cluster1<-hclust(dm)
boxplot(dm, col="blue", main="caliz", outline=F, boxwex=0.35)
manhattan<-dm
resultados <- c(manhattan, largos)

png("zonas contra manhattan.png")
boxplot(datos$manhattan ~datos$matricula, xlab="zonas", ylab="Distancia manhattan", ylim = c(0, 100), col=c("55"), main="Manhattan", range=0.5, outline=FALSE,boxwex=0.25)
png("zonas contra grieta.png")
boxplot(datos$grieta ~datos$matricula, xlab="zonas", ylab="Largo de la grieta", ylim = c(0, 100) ,col=("2"),main="GRIETA", range=0.5, outline=FALSE,boxwex=0.35)
graphics.off()


for (i in tamaño) {
  par(mfrow = c(1, 2))
  png("queso.png")
  boxplot(if (range(datos$cluster1[which(datos$matricula == i)])[2] < range(datos$grieta[which(datos$matricula == i)])[2]) {
    lmts <- range(datos$grieta[which(datos$matricula == i)])
  } else {
    lmts <- range(datos$cluster1[which(datos$Zona == i)])
  })
  graphics.off()
 par(mfrow = c(1, 1))
  boxplot(datos$grieta[which(datos$matricula == i)] ~datos$semillas[which(datos$matricula == i)],
          ylab = "Grieta", xlab = "Semillas", col=FALSE, ylim = c(0, 100), boxwex=0.25, outline=FALSE, range=1.5)
  boxplot(datos$cluster1[which(datos$matricula == i)] ~datos$semillas[which(datos$matricula == i)],
          ylab = "Grieta", xlab = "Semillas", col=FALSE, ylim = c(0, 100), boxwex=0.25, outline=FALSE, range=1.5)
  
    graphics.off()
}

anova<-data.frame(datos)
stacked_groups<-stack(anova)
stacked_groups
resultadosa<-aov(values ~ ind, data = stacked_groups)
summary(resultadosa) 
summary.aov()