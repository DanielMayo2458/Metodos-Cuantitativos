test=read.csv("D:/yuri/maestria mexico/Descargas Profesores/2do semestre/econometria/Tarea 1/Metodos-Cuantitativos/test.csv")
test

x = test$Precio
y= test$Superficie

plot(x,y)

y=mx+n


#recta de regresion lineal 
m=cov(y,x)/var(x)

n=mean(y)-m*mean(x)

m;n
plot(x,y)
abline(n,m,col="blue")

resy=m*30+n
resy

reg1=lm(y~x)
summary(reg1)

