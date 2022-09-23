# Importando bibliotecas
library('survival')
library('KMsurv')

# Entrada dos dados 
tempos<-c(7,8,8,8,8,12,12,17,18,22,30,30,30,30,30,30,8,8,9,10,10,14,15,15,18,19,21,22,22,23,25,8,8,8,8,8,8,9,10,10,10,11,17,19)
eventos<-c(rep(1,10), rep(0,6), rep(1,15), rep(1,13))
grupo<-c(rep(1,16), rep(2,15), rep(3,13))

# Estimador Kaplan-Meier
ekm<-survfit(Surv(tempos,eventos)~grupo)
summary(ekm)

# Gráfico Kaplan-Meier
plot(ekm, lty=c(1,4,2), main="Curva de Sobrevivência Kaplan-Meier dos Camundogos Infectados com Malária", xlab="Tempo (em dias)", ylab="S(t) estimada", mark.time=T)
abline(h=0.5, col="red")
legend(1, 0.3, lty=c(1,4,2), c("Grupo 1","Grupo 2","Grupo 3"), lwd=1, bty="n", cex=0.8)

# Log-rank estimador Kaplan-Meier
survdiff(Surv(tempos,eventos)~grupo, rho=0)
survdiff(Surv(tempos[1:31],eventos[1:31])~grupo[1:31], rho=0)
survdiff(Surv(tempos[17:44],eventos[17:44])~grupo[17:44], rho=0)
survdiff(Surv(c(tempos[1:16],tempos[32:44]),c(eventos[1:16],eventos[32:44]))~c(grupo[1:16],grupo[32:44]), rho=0)

# Estimador Nelson-Aalen
ena.g1<-survfit(coxph(Surv(tempos[1:15],eventos[1:15])~grupo[1:15]), type="breslow")
summary(ena.g1)
ena.g2<-survfit(coxph(Surv(tempos[16:31],eventos[16:31])~grupo[16:31]), type="breslow")
summary(ena.g2)
ena.g3<-survfit(coxph(Surv(tempos[32:44],eventos[32:44])~grupo[32:44]), type="breslow")
summary(ena.g3)

# Gráfico Nelson-Aalen
plot(ena.g1, lty=1, main="Curva de Sobrevivência Nelson-Aalen dos Camundogos Infectados com Malária", xlab="Tempo (em Dias)", ylab="S(t) estimada")
abline(h=0.5, col="red")
par(new=TRUE)
plot(ena.g2, lty=4, xlab="Tempo (em dias)", ylab="S(t) estimada", conf.int=FALSE)
par(new=TRUE)
plot(ena.g3, lty=2, xlab="Tempo (em dias)", ylab="S(t) estimada", conf.int=FALSE)
legend(1, 0.3, lty=c(1,4,2), c("Grupo 1","Grupo 2","Grupo 3"), lwd=1, bty="n", cex=0.8)

# Entrada dos dados
intervalos<-c(7,8,12,17,18,22,30,8,9,10,14,15,18,19,21,22,23,25,8,9,10,11,17,19)
mortes<-c(1,4,2,1,1,1,0,2,1,2,1,2,1,1,1,2,1,1,6,1,3,1,1,1)
cens<-c(rep(0,6),6,rep(0,17))

# Estimador Atuarial
eac.g1 = lifetab(tis=intervalos[1:7], ninit=16, nlost=cens[1:7], nevent=mortes[1:7])
round(eac.g1, 4)
eac.g2 = lifetab(tis=intervalos[8:18], ninit=15, nlost=cens[8:18], nevent=mortes[8:18])
round(eac.g2, 4)
eac.g3 = lifetab(tis=intervalos[19:24], ninit=13, nlost=cens[19:24], nevent=mortes[19:24])
round(eac.g3, 4)

# Gráfico Atuarial
x1<-rep(intervalos[1:7], rep(2,7))[1:13]
x1<-append(x1, 0, 0)
y1<-rep(eac.g1$surv, rep(2,7))
plot(x1, y1, type="l", lty=1, xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,30), ylim=c(0,1), main="Curva de Sobrevivência Atuarial dos Pacientes no Tratamento de Hepatite")
abline(h=0.5, col="red")
x2<-rep(intervalos[8:18], rep(2,11))[1:21]
x2<-append(x2, 0, 0)
y2<-rep(eac.g2$surv, rep(2,11))
par(new=TRUE)
plot(x2, y2, type="l", lty=4,  xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,30), ylim=c(0,1))
x3<-rep(intervalos[19:24], rep(2,6))[1:11]
x3<-append(x3, 0, 0)
y3<-rep(eac.g3$surv, rep(2,6))
par(new=TRUE)
plot(x3, y3, type="l", lty=2,  xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,30), ylim=c(0,1))
legend(1, 0.3, lty=c(1,4,2), c("Grupo 1","Grupo 2","Grupo 3"), lwd=1, bty="n", cex=0.8)
