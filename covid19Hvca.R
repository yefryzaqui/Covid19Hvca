data.covid=read.delim("clipboard", header = TRUE, sep = ",")
summary(data.covid)
str(data.covid)
head(data.covid)
data.covid$DEPARTAMENTO
table(data.covid$DEPARTAMENTO)
length(table(data.covid$DEPARTAMENTO))
data.Hvca=subset(data.covid,DEPARTAMENTO=="HUANCAVELICA")
str(data.Hvca)
####
data.frame(table(data.Hvca$FECHA_RESULTADO))
length(table(data.Hvca$FECHA_RESULTADO))
hvca.fecha=as.Date(data.Hvca$FECHA_RESULTADO,format="%d/%m/%Y")
table(hvca.fecha)
hvca.Casos=(table(hvca.fecha))
length(hvca.Casos)
hvca.dias<- seq(1,80,by=1)
hvca.data <-data.frame(hvca.dias,hvca.Casos)
head(hvca.data)
tail(hvca.data)
plot(hvca.data$hvca.dias,hvca.data$Freq,xlab = "Días",ylab="Número de Casos",col="blue",type="l",
     main="Figura N°1: Casos confirmados de COVID-19 en el Hvca")

# Modelo
modelo <- lm(log(hvca.data$Freq)~hvca.data$hvca.dias) #Regresión exponencial
summary(modelo)
#Grafico de dispersion 
plot(hvca.data$hvca.dias,hvca.data$Freq,pch=21,bg="red",xlab="Día",
     ylab="Número de casos",main="Figura N°2: Casos confirmado de COVID-19 en el Hvca")
lines(hvca.data$hvca.dias,hvca.data$Freq,col="blue",type = "b")
grid(10,10,lwd = 2)

a1 <- exp(modelo$coefficients[1])
a2 <- modelo$coefficients[2]
text(6,60,bquote(paste('y = ',.((format(a1,digits = 6))),'*e^',
                       .(format(a2,digits = 6)),'x')))

curve(a1*exp(a2*x),add=T,col="red",type="l",lwd=1)

#Proyecciones
options(scipen=999)
Fecha <- seq(as.Date('2020-03-31'), by='day', length=80)
dia   <- seq(1,80,by=1)
y1    <- c(hvca.data$Freq)
Casos_estimados <- a1*exp(a2*hvca.data$hvca.dias)
COVID_19_estim <- data.frame(Fecha,y1,trunc(Casos_estimados))
names(COVID_19_estim)<-c("Fecha","Casos reales","Casos estimados")
COVID_19_estim
