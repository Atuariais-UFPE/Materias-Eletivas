# Instalando bibliotecas
#install('KMsurv') # Apenas na primeira vez que rodar o código

# Importando bibliotecas
library('survival')
library('KMsurv')

# Entrada dos dados
tempos<-c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
eventos<-c(rep(0,2), rep(1,2), rep(0,11), rep(1,3), rep(0,2), rep(1,4), rep(0,5))
grupo<-c(rep(1,15), rep(2,14))

# Estimador Kaplan-Meier
ekm<-survfit(Surv(tempos,eventos)~grupo)
summary(ekm)

# Gráfico Kaplan-Meier
plot(ekm, lty=c(1,4), main="Curva de Sobrevivência Kaplan-Meier dos Pacientes no Tratamento de Hepatite", xlab="Tempo (em semanas)", ylab="S(t) estimada")
abline(h=0.5, col="red")
legend(1, 0.3, lty=c(1,4), c("Controle","Esteróide"), lwd=1, bty="n", cex=0.8)

# Teste Log-rank para Estimador Kaplan-Meier
survdiff(Surv(tempos,eventos)~grupo, rho=0)

# Estimador Nelson-Aalen
ena.g1<-survfit(coxph(Surv(tempos[1:15],eventos[1:15])~grupo[1:15]), method="breslow")
summary(ena.g1)
ena.g2<-survfit(coxph(Surv(tempos[16:29],eventos[16:29])~grupo[16:29]), method="breslow")
summary(ena.g2)

# Gráfico Nelson-Aalen
plot(ena.g1, lty=1, main="Curva de Sobrevivência Nelson-Aalen dos Pacientes no Tratamento de Hepatite", xlab="Tempo (em semanas)", ylab="S(t) estimada", conf.int=FALSE)
abline(h=0.5, col="red")
par(new=TRUE)
plot(ena.g2, lty=4, conf.int=FALSE)
legend(1, 0.3, lty=c(1,4), c("Controle","Esteróide"), lwd=1, bty="n", cex=0.8)

# Entrada dos dados
intervalos<-c(1,2,3,5,16,1,4,5,7,8,10,12,16)
mortes<-c(0,0,2,0,0,3,0,1,1,1,1,0,0)
cens<-c(1,1,1,2,8,1,1,0,0,0,1,1,3)

# Estimador Atuarial
eac.g1 = lifetab(tis=intervalos[1:5], ninit=15, nlost=cens[1:5], nevent=mortes[1:5])
round(eac.g1, 4)
eac.g2 = lifetab(tis=intervalos[6:13], ninit=14, nlost=cens[6:13], nevent=mortes[6:13])
round(eac.g2, 4)

# Gráfico Atuarial
x1<-rep(intervalos[1:5], rep(2,5))[1:9]
x1<-append(x1, 0, 0)
y1<-rep(eac.g1$surv, rep(2,5))
plot(x1, y1, type="l", lty=1, xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,16), ylim=c(0,1), main="Curva de Sobrevivência Atuarial dos Pacientes no Tratamento de Hepatite")
abline(h=0.5, col="red")
x2<-rep(intervalos[6:13], rep(2,8))[1:15]
x2<-append(x2, 0, 0)
y2<-rep(eac.g2$surv, rep(2,8))
par(new=TRUE)
plot(x2, y2, type="l", lty=4,  xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,16), ylim=c(0,1))
legend(1, 0.3, lty=c(1,4), c("Controle","Esteróide"), lwd=1, bty="n", cex=0.8)
