library('survival')
library('KMsurv')

# Entrada dos dados 
tempos<- c(3,4,5.7,6.5,6.5,8.4,10,10,12,15)
eventos<- c(1,0,0,1,1,0,1,0,1,1)

# Estimador Kaplan-Meier
ekm<-survfit(Surv(tempos,eventos)~1)
summary(ekm)

# Gráfico Kaplan-Meier
plot(ekm, lty=1, xlab="Tempo (em Semanas)", ylab="S(t) estimada", main="Curva de Sobrevivência Kaplan-Meier de Reincidência do Tumor Sólido", conf.int=F)
abline(h=0.5, col="red")
legend(1, 0.3, lty=1, c("Tumor Sólido"), lwd=1, bty="n", cex=0.8)

# Estimador Nelson-Aalen
ena<-survfit(coxph(Surv(tempos,eventos)~1), method="breslow")
summary(ena)

# Gráfico Nelson-Aalen
plot(ena, lty=1, main="Curva de Sobrevivência Nelson-Aalen de Reincidência do Tumor Sólido", xlab="Tempo (em semanas)", ylab="S(t) estimada", conf.int=FALSE)
abline(h=0.5, col="red")
legend(1, 0.3, lty=1, c("Tumor Sólido"), lwd=1, bty="n", cex=0.8)

# Entrada dos dados
intervalos<-c(3,4,5.7,6.5,8.4,10,12,15)
reincidencia<-c(1,0,0,2,0,0,1,1)
cens<-c(0,1,1,0,1,1,0,0)

# Estimador Atuarial
eac = lifetab(tis=intervalos, ninit=10, nlost=cens, nevent=reincidencia)
round(eac, 4)

# Gráfico Atuarial
x<-rep(intervalos, rep(2,8))[1:15]
x<-append(x, 0, 0)
y<-rep(eac$surv, rep(2,8))
plot(x, y, type="l", lty=1, xlab="Tempo (em semanas)", ylab="S(t) estimada", xlim=c(0,16), ylim=c(0,1), main="Curva de Sobrevivência Atuarial de Reincidência de Tumor Sólido")
abline(h=0.5, col="red")
legend(1, 0.3, lty=1, c("Tumor Sólido"), lwd=1, bty="n", cex=0.8)