pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
} 
poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}
eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}
domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
funciones <- c("pick.one", "poli", "eval", "domin.by")
variables <- c("vc", "md", "tc")
replicas <- 30

library(parallel)
mc <- makeCluster(detectCores() - 1)
clusterExport(mc, funciones)
clusterExport(mc, variables)
resultados <- data.frame(fun = integer(), rep = integer(), SolNoDom = integer(), Porcentaje = numeric())
n <- 150
for (k in 2:10) {
  clusterExport(mc, "k")
  for (r in 1:replicas) {
    
    obj <- parSapply(mc, 1:k, function(i) {
      return(list(poli(md, vc, tc)))
    })
    
    minim <- (runif(k) > 0.5)
    sign <- (-1)^minim
    
    clusterExport(mc, c("obj", "sign", "n"))
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(parRapply(mc, sol, function(i) {
      evaluacion <- double()
      for (j in 1:k) { # para todos los objetivos
        evaluacion <- c(evaluacion, eval(obj[[j]], i, tc))
      }
      return(evaluacion)
    }), nrow=n, ncol=k, byrow = TRUE)
    
    clusterExport(mc, "val")
    #mejores <- parSapply(cl, 1:k, function(i) {
    #    return(which.max(sign[i] * val[,i]))
    #    })
    
    datos <- t(parSapply(mc, 1:n, function(i) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
      }
      dominadores <- sum(d)
      no.dom <- dominadores == 0 # nadie le domina
      return(c(dominadores, no.dom))
    }))
    
    datos <- data.frame(Dominadores = datos[,1], NoDominados = as.logical(datos[,2]))
    
    frente <- subset(val, datos$NoDominados) # solamente las no dominadas
    
    resultados <- rbind(resultados, data.frame(fun = k, rep = r,
                                               SolNoDom = dim(frente)[1], Porcentaje = dim(frente)[1]/n))
  }
}
stopCluster(mc)

resultados$fun <- as.factor(resultados$fun)
library(ggplot2)
png("grafp11violin.png")
ggplot(resultados, aes(x = as.factor(fun), y = 100*Porcentaje, color = fun)) + 
  geom_violin(scale = "width") + geom_boxplot(width = 0.2) + ylab("Porcentaje") + 
  xlab("Funciones Objetivo")
  axis.text.x = element_text(size=20)
  axis.text.y = element_text(size=20)
graphics.off()