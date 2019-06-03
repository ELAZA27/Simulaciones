w1 <- total2[1:3000,]
w2 <- total2[1:6000,]
w3 <- total2[1:9000,]
w4 <- total2[1:12000,]
w5 <- total2[1:15000,]
w6 <- total2[1:18000,]
w7 <- total2[1:21000,]
w8 <- total2[1:24000,] 
w9 <- total2[1:27000,]
w10 <- total2[1:30000,]
w11 <- total2[1:33000,]
w12 <- total2[1:36000,]

desve1 <- length(w1[w1==TRUE])
desve2 <- length(w2[w2==TRUE])
desve3 <- length(w3[w3==TRUE])

desve4 <- length(w1[w1==FALSE])
desve5 <- length(w2[w2==FALSE])
desve6 <- length(w3[w3==FALSE])
##########
desve7 <- length(w4[w4==TRUE])
desve8 <- length(w5[w5==TRUE])
desve9 <- length(w6[w6==TRUE])

desve10 <- length(w4[w4==FALSE])
desve11 <- length(w5[w5==FALSE])
desve12 <- length(w6[w6==FALSE])
###############
desve13 <- length(w7[w7==TRUE])
desve14 <- length(w8[w8==TRUE])
desve15 <- length(w9[w9==TRUE])

desve16 <- length(w7[w7==FALSE])
desve17 <- length(w8[w8==FALSE])
desve18 <- length(w9[w9==FALSE])
##############
desve19 <- length(w10[w10==TRUE])
desve20 <- length(w11[w11==TRUE])
desve30 <- length(w12[w12==TRUE])

desve21 <- length(w10[w10==FALSE])
desve22 <- length(w11[w11==FALSE])
desve31 <- length(w12[w12==FALSE])
###########################

no.dormir3 <- ((desve4-desve1)*pin)
no.dormir6 <- ((desve5-desve2)*pin)
no.dormir9 <- ((desve6-desve3)*pin)
no.dormir12 <- ((desve10-desve7)*pin)
no.dormir15 <- ((desve11-desve8)*pin)
no.dormir18 <- ((desve12-desve9)*pin)
no.dormir21 <- ((desve16-desve13)*pin)
no.dormir24 <- ((desve17-desve14)*pin)
no.dormir27 <- ((desve18-desve15)*pin)
no.dormir30 <- ((desve21-desve19)*pin)
no.dormir33 <- ((desve22-desve20)*pin)
no.dormir36 <- ((desve31-desve30)*pin)
