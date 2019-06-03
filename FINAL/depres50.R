y1 <- total1[1:3000,]
y2 <- total1[1:6000,]
y3 <- total1[1:9000,]
y4 <- total1[1:12000,]
y5 <- total1[1:15000,]
y6 <- total1[1:18000,]
y7 <- total1[1:21000,]
y8 <- total1[1:24000,] 
y9 <- total1[1:27000,]
y10 <- total1[1:30000,]
y11 <- total1[1:33000,]
y12 <- total1[1:36000,]

d1 <- length(y1[y1==TRUE])
d2 <- length(y2[y2==TRUE])
d3 <- length(y3[y3==TRUE])

d4 <- length(y1[y1==FALSE])
d5 <- length(y2[y2==FALSE])
d6 <- length(y3[y3==FALSE])
##########
d7 <- length(y4[y4==TRUE])
d8 <- length(y5[y5==TRUE])
d9 <- length(y6[y6==TRUE])

d10 <- length(y4[y4==FALSE])
d11 <- length(y5[y5==FALSE])
d12 <- length(y6[y6==FALSE])
###############
d13 <- length(y7[y7==TRUE])
d14 <- length(y8[y8==TRUE])
d15 <- length(y9[y9==TRUE])

d16 <- length(y7[y7==FALSE])
d17 <- length(y8[y8==FALSE])
d18 <- length(y9[y9==FALSE])
##############
d19 <- length(y10[y10==TRUE])
d20 <- length(y11[y11==TRUE])
dan <- length(y12[y12==TRUE])

d21 <- length(y10[y10==FALSE])
d22 <- length(y11[y11==FALSE])
din <- length(y12[y12==FALSE])
###########################

decaido3 <- ((d4-d1)*pdep)
decaido6 <- ((d5-d2)*pdep)
decaido9 <- ((d6-d3)*pdep)
decaido12 <- ((d10-d7)*pdep)
decaido15 <- ((d11-d8)*pdep)
decaido18 <- ((d12-d9)*pdep)
decaido21 <- ((d16-d13)*pdep)
decaido24 <- ((d17-d14)*pdep)
decaido27 <- ((d18-d15)*pdep)
decaido30 <- ((d21-d19)*pdep)
decaido33 <- ((d22-d20)*pdep)
decaido36 <- ((din-dan)*pdep)
