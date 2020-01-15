#Dataset Description
#COCINA ELECTRICA	Kwh promedio mensual de uso para cocina
#LAVANDERIA	Kwh promedio mensual de uso para Lavadora/Secadora
#AIREACONDICIONADO	Kwh promedio mensual de uso para Aire Acondicionado
#PCT_USODIA	Porcentaje usado en el dia
#PCT_USONOCHE	Porcentaje usado en la noche
#ENMORA	Flag indicando si existe al menos 1 factura en mora en los ultimos 3 meses
#LLAMADASACALLCENTER	Cantidad de llamadas que el cliente ha realizado al call center en los ultimos 3 meses
#REGION	Region del pais (1 Occidente, 2 Central, 3 Oriente)
#ANTIGUEDAD_CLIENTE	Meses desde que se creo el contrato
#USUARIO_CANAL_DIGITAL	Flag que indica si usa la app de la empresa
#PUNTO_PAGO	Punto de pago mas frecuente del cliente {1,2,3,4}{BANCO,CAJAEXPRESS,AGENCIAS,ONLINE}


setwd("C:/workshopDS/datasets")
ds<-read.table("EnergyDataSet.csv",dec=".",sep=",",header=TRUE,colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
dim(ds)
str(ds)
summary(ds)

boxplot(ds, col='gold')
boxplot(ds$cocina, col='gold')
boxplot(ds$total, col='gold')
boxplot(ds$total,ds$cocina,ds$antiguedad,names = c("Total", "Cocina", "Antiguedad") ,col='gold')

boxplot(ds$total,
main = "Consumo Total en KW",
xlab = "KW por Cliente",
col = "orange",
border = "brown",
horizontal = TRUE,
notch = TRUE
)


plot(ds$antiguedad,ds$total)
plot(ds$total,ds$pct_usodia)
plot(ds$mora,ds$pct_usonoche)
hist(ds$aireacondicionado)
hist(ds$antiguedad)
mean(ds$cocina)
median(ds$cocina)
range(ds$cocina)
sd(ds$cocina)
plot(ds$cocina)
hist(ds$cocina)
curve(dnorm(x, mean(ds$cocina, na.rm = T), sd(ds$cocina, na.rm = T)),
      xlim = c(-0, 100))


library("rgl")
library("magick")	  


plot3d( ds[,4], ds[,8], ds[,6],  type = "s", radius = .9 )



	  

library(ggplot2)
library(ggcorrplot)

#Matriz Correlaciones

corr <- round(cor(ds), 1)
corr

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correl. Energy Case", 
           ggtheme=theme_bw)
		   
		   

theme_set(theme_bw())

# Pie Chart

pie <- ggplot(ds, aes(x = "", fill = factor(usaapp))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="usaapp", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of usaapp", 
       caption="Source: DS")
  
pie + coord_polar(theta = "y", start=0)

table (ds$usaapp)

##Elbow Methodol.

library('cluster')

#Select the number of clusters based on:	a) The sum of squared error (SSE)
	   kmax <- 10
	   res <- rep(0,kmax)
	   for(i in 1:kmax){
	      res[i] <- kmeans(ds,i,nstart=30)$tot.withinss
	   }
plot(1:kmax,res,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")	


library("factoextra")
fviz_nbclust(ds, kmeans, method = "wss")
fviz_nbclust(ds, kmeans, method = "silhouette")
	
set.seed(123)

#Escalando
ds.sd <- scale(ds)
class(ds.sd)
ds.sd <- data.frame(ds.sd)
class(ds.sd)


#Construyendo el modelo. Se le indica a R que pruebe con 30 puntos iniciales y seleccione aquel que presente menos error
kmm <- kmeans(ds.sd,3,iter.max=100,nstart=30)
print(kmm)


#Agregar el resultado de nuestro Kmeans al Dataset original.
dsnew <- cbind(ds,kmm$cluster)
aggregate(dsnew, by=list(cluster=kmm$cluster), mean)

write.table(dsnew,file="clusters_kmeans.csv",row.names=FALSE)


