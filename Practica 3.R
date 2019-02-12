compuesto <- function(n) {
  if (n < 4) {
    return(FALSE)
  }
  for (i in 2:(n-1)) {
    if (n %% i == 0) { 
      return(TRUE)
    }
  }
  return(FALSE)
}

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 50000
hasta <-  250000
original <- desde:hasta
invertido <- hasta:desde
replicas <- 10

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores(7) - 1))
otp <-  numeric()
itp <-  numeric()
atp <-  numeric()
for (r in 1:replicas) {
  otp <- c(otp, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  itp <- c(itp, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  atp <- c(atp, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()

summary(otp) # primos desde:hasta
summary(itp) # primos hasta:desde
summary(atp) # primos aleatorio

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores(7) - 1))
otc <-  numeric() 
itc <-  numeric() 
atc <-  numeric() 
for (r in 1:replicas) {
  otc <- c(otc, system.time(foreach(n = original, .combine=c) %dopar% compuesto(n))[3]) # de menor a mayor
  itc <- c(itc, system.time(foreach(n = invertido, .combine=c) %dopar% compuesto(n))[3]) # de mayor a menor
  atc <- c(atc, system.time(foreach(n = sample(original), .combine=c) %dopar% compuesto(n))[3]) # orden aleatorio
}
stopImplicitCluster()

summary(otc) # compuestos desde:hasta
summary(itc) # compuestos hasta:desde
summary(atc) # compuestos aleatorio
# graficas boxplot
par (mfrow=c(1,1))
nombres <- c("Original", "Invertido", "Aleatorio")
boxplot(summary(otp), summary(itp), summary(atp), main="Números primos 6 núcleos", xlab="Vectores" , ylab="Tiempo", boxwex=0.15, names = nombres, range = 0)
boxplot(summary(otc), summary(itc), summary(atc), main="Números compuestos 6 núcleos", xlab="Vectores" , ylab="Tiempo", boxwex=0.15, names = nombres, range = 0)