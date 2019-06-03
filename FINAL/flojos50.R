z1 <- total3[1:3000,]
z2 <- total3[1:6000,]
z3 <- total3[1:9000,]
z4 <- total3[1:12000,]
z5 <- total3[1:15000,]
z6 <- total3[1:18000,]
z7 <- total3[1:21000,]
z8 <- total3[1:24000,] 
z9 <- total3[1:27000,]
z10 <- total3[1:30000,]
z11 <- total3[1:33000,]
z12 <- total3[1:36000,]

flo1 <- length(z1[z1==TRUE])
flo2 <- length(z2[z2==TRUE])
flo3 <- length(z3[z3==TRUE])

flo4 <- length(z1[z1==FALSE])
flo5 <- length(z2[z2==FALSE])
flo6 <- length(z3[z3==FALSE])
##########
flo7 <- length(z4[z4==TRUE])
flo8 <- length(z5[z5==TRUE])
flo9 <- length(z6[z6==TRUE])

flo10 <- length(z4[z4==FALSE])
flo11 <- length(z5[z5==FALSE])
flo12 <- length(z6[z6==FALSE])
###############
flo13 <- length(z7[z7==TRUE])
flo14 <- length(z8[z8==TRUE])
flo15 <- length(z9[z9==TRUE])

flo16 <- length(z7[z7==FALSE])
flo17 <- length(z8[z8==FALSE])
flo18 <- length(z9[z9==FALSE])
##############
flo19 <- length(z10[z10==TRUE])
flo20 <- length(z11[z11==TRUE])
floja <- length(z12[z12==TRUE])

flo21 <- length(z10[z10==FALSE])
flo22 <- length(z11[z11==FALSE])
floji <- length(z12[z12==FALSE])
###########################

quieto3 <- ((flo4-flo1)*psen)
quieto6 <- ((flo5-flo2)*psen)
quieto9 <- ((flo6-flo3)*psen)
quieto12 <- ((flo10-flo7)*psen)
quieto15 <- ((flo11-flo8)*psen)
quieto18 <- ((flo12-flo9)*psen)
quieto21 <- ((flo16-flo13)*psen)
quieto24 <- ((flo17-flo14)*psen)
quieto27 <- ((flo18-flo15)*psen)
quieto30 <- ((flo21-flo19)*psen)
quieto33 <- ((flo22-flo20)*psen)
quieto36 <- ((floji-floja)*psen)