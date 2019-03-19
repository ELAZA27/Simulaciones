f <- function(x) { # modificamos para que sea interesante
  return(5 * cos(14*x - 3) * sin(2*x^2 - 4 * x) + 2 * x^2 - 4 * x)
}
g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- 3
step <- 0.15
replicas <- 100
r <- seq(1000, 10000, 50)

replica <- function(t) {
  curr <- runif(2, low, high)
  best <- curr
  for (tiempo in 1:t) {
    deltax <- runif(1,0,step)
    deltay <- runif(1,0,step)
    
    up <- best + deltay
    down <- best - deltay
    right <- best + deltax
    left <- best - deltax
    
    upright <- best + c(deltax,deltay)
    upleft <- best + c(-deltax,deltay)
    
    downright <- best + c(deltax,-deltay)
    downleft <- best +c(-deltax,-deltay)
   
      if (g(up[1],up[2])>g(best[1],best[2])) {
        best <- up
      }
      if (g(upright[1],upright[2])>g(best[1],best[2])) {
        best <- upright
      }
      if (g(right[1],right[2])>g(best[1],best[2])) {
        best <- right
      }
      if (g(downright[1],downright[2])>g(best[1],best[2])) {
        best <- downright
      }
      if (g(down[1],down[2])>g(best[1],best[2])) {
        best <- down
      }
      if (g(downleft[1],downleft[2])>g(best[1],best[2])) {
        best <- downleft
      }
      if (g(left[1],left[2])>g(best[1],best[2])) {
        best <- left
      }
      if (g(upleft[1],upleft[2])>g(best[1],best[2])) {
        best <- upleft
      }
      else {
        best <- best
      }
    }  
 return(best)
}


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (pot in 2:4) {
  tmax <- 10^pot
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  png(paste("grafica_3D", tmax, ".png", sep=""), width=1500, height=1500)
  x <- seq(low, high, step)
  y <- x
  z <- outer(x, y, g)
  persp(x,y,z, col='orange',expand = 0.5, shade=0.2)
  valores <- f(resultados)
  points(resultados, valores, pch=1600, col="red")
  mejor <- which.max(valores)
  abline(v =resultados[mejor], col="green", lwd=3)
  graphics.off()
   }
stopImplicitCluster()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for(graf in 2:4) {
  tmax <- graf
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  valores <- f(resultados)
  mejor <- which.max(valores)
  x <- seq(low, high, step)
  y <- x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  points(resultados, valores, pch=16, col="red")
  png("das.png", width=900, height=900)
  levelplot(z ~ x * y, data = d, main = "")
  graphics.off()
}

stopImplicitCluster()
