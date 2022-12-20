library(dplyr)
library(tidyr)
library(ggplot2)


#############################################################################################################################
#####################################editar la tabla de datos################################################################
#############################################################################################################################

datos<-data.frame(Práctica_Mujeres_Trabajadoras)
datos$ANIOP<-factor(datos$ANIOP,levels = levels(factor(datos$ANIOP)),labels = c("no_labora","si_labora"))
datos$ANIOA<-factor(datos$ANIOA,levels = levels(factor(datos$ANIOA)),labels = c("no_labora","si_labora"))
datos$RAZAN<-factor(datos$RAZAN,levels = levels(factor(datos$RAZAN)),labels = c("blanca","negra"))
datos$NIN1<-factor(datos$NIN1,levels = levels(factor(datos$NIN1)),labels = c("no_hay","si_hay"))
datos$NIN2<-factor(datos$NIN2,levels = levels(factor(datos$NIN2)),labels = c("no_hay","si_hay"))
datos

##########################################################################################################
###########################################respuesta pregunta 1 25%###########################################
##########################################################################################################

tabla_0<-table(select(datos,c(ANIOA,ANIOP)))   # Función "select" proviene de la paquetería dplyr
tabla_0                                        # Función "table" genera la tabla cruzada de frecuencias absolutas
datos_0<-as.data.frame(tabla_0)                # # convierte el objeto tabla a un objeto data frame, es necesario ya que la función ggplot lo exige
datos_0

ggplot(data=datos_0, aes(x=ANIOP, y=Freq,fill=ANIOA)) + 
  geom_bar(stat="identity",position=position_dodge()) + geom_text(aes(label = Freq), vjust=0,size = 3.5)+
  scale_fill_manual(values=c("blue", "purple","black"))

tabla_0_prop<-prop.table(tabla_0)
tabla_0_prop
datos_0_prop<-as.data.frame(tabla_0_prop)   # convierte el objeto tabla a un objeto data frame, es necesario ya que la función ggplot lo exige

ggplot(data=datos_0_prop, aes(x=ANIOA, y=Freq, fill=ANIOP)) + 
  geom_bar(stat="identity",position = position_dodge2())+geom_text(aes(label=Freq),vjust=0, size=3.5) +ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("blue", "purple","black"))


#############################################################################################
####################respuesta pregunta 2#####################################################
#############################################################################################

# Tabla de estadística descriptiva del salario del esposo por grupo de mujeres
aggregate(INGESP~ANIOA,FUN=summary,data=datos)

# Gráfica Box plot del salario del esposo por grupo de mujeres
ggplot(data =datos , mapping = aes(x = ANIOA, y = INGESP)) +
  geom_boxplot()


##################################################################################################
######################################respuesta pregunta 3#######################################
##################################################################################################

# Tabla de estadística descriptiva
summary(select(datos,EDAD))

# Gráfica Box plot
ggplot(data = datos, mapping = aes(x ="",y=EDAD)) +xlab("")+
  geom_boxplot()

normalidad<-ggplot(data = datos, aes(EDAD))+xlab("")+ylab("Conteos")
normalidad+geom_histogram(color="darkblue",fill="lightblue",bins =5)

# Tabla de estadística descriptiva de la edad por grupo (si laboran o no)
aggregate(EDAD~ANIOA,FUN=summary,data=datos)
# Gráfica de la edad por cada grupo
ggplot(data = datos, mapping = aes(ANIOA,y=EDAD))+geom_boxplot()


# Distribución de la variable Edad por grupo (si labora o no)
ggplot(datos)+ 
  geom_histogram(bins = 10, aes( EDAD)) + 
  facet_grid(ANIOA~., scales = 'free') +
  xlab("Edad") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de la variable edad") +
  theme_minimal()

####################################################################################################
##########################Pregunta 4################################################################
####################################################################################################

aggregate(EDUC~ANIOA,FUN=summary,data=datos)  # no mostrar código porque la variable EDUC se considera más bien categórica
############################################
#####convertir a factor la variable EDUC###
###########################################

table(datos$EDUC)
datos$EDUC<-factor(datos$EDUC)

##################################
#######tabla de proporciones #####
##################################

######varible ANIOA###############
ANIOA_d <- prop.table(table(select(datos,ANIOA)))
ANIOA_d
dato_ANIOA <-as.data.frame(ANIOA_d)
dato_ANIOA
dato_ANIOA<- rename(dato_ANIOA,ANIOA=Var1)
dato_ANIOA

ggplot(data = dato_ANIOA, aes(x=ANIOA, y=Freq))+
  geom_bar(stat = "identity") + geom_text(aes(label= Freq), vjust = -0.3, size=3.5) + ylab("Frecuencia relativa")+
  scale_fill_manual(values = c("darkgreen","purple"))


ggplot(data = dato_ANIOA, aes(x="", y=Freq, fill=ANIOA))+
  geom_bar(stat = "identity", color="white") + 
  geom_text(aes(label= Freq), position= position_stack(vjust = 0.5), color="white", size=5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("blue", "steelblue", "orange", "gray", "green"))+
  theme_void()+
  labs(title = "Gráfico de pastel")

###########variable EDUC#############
educ_d <- prop.table(table(select(datos,EDUC)))
educ_d

dato_educ<- as.data.frame(educ_d) 

dato_educ<- rename(dato_educ,EDUC=Var1)

ggplot(data = dato_educ, aes(x=EDUC, y=Freq))+
  geom_bar(stat = "identity") + geom_text(aes(label= Freq), vjust = -0.3, size=3.5) + ylab("Frecuencia relativa")+
  scale_fill_manual(values = c("darkgreen","purple"))


ggplot(data = dato_educ, aes(x="", y=Freq, fill=EDUC))+
  geom_bar(stat = "identity", color="white") + 
  geom_text(aes(label= Freq), position= position_stack(vjust = 0.5), color="white", size=5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("blue", "steelblue", "orange", "gray", "green"))+
  theme_void()+
  labs(title = "Gráfica de pastel")

#############tabla de frecuencia cruzada EDUC y ANIOA###
tabla_1<-table(select(datos,EDUC,ANIOA))
tabla_1
tabla_1_prop<-prop.table(tabla_1)
tabla_1_prop


###################conversión de tabla a data frame##############
#################################################################

datos_1<-as.data.frame(tabla_1)
datos_1
datos_1_prop<-as.data.frame(tabla_1_prop)

# Gráfico de frecuencias relativas de la variable EDUC  (ESTE ES EL IMPORTANTE)
ggplot(data = datos_1_prop, aes(x=EDUC, y=Freq, fill=ANIOA))+
  geom_bar(stat = "identity", position= position_dodge())+ 
  geom_text(aes(label= Freq), vjust = 0, size=3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values = c("green", "purple", "black"))  # Hasta aquí, min 37.48

########################Grafico de la frecuencias de##################
###############################la variabel ANIOA######################
ggplot(data=datos_1, aes(x=ANIOA, y=Freq)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkgreen", "purple","black"))
###########################Gráfica de la frecuencia relativa#############
################################de la variable ANIOA####################

ggplot(data=datos_1_prop, aes(x=ANIOA, y=Freq)) + 
  geom_bar(stat="identity") +ylab("Frecuencia relativa")
  scale_fill_manual(values=c("darkgreen", "purple","black"))

###########################Gr?fica de la frecuencia relativa#############
################################de la variable EDUC####################

ggplot(data=datos_1_prop, aes(x=EDUC, y=Freq)) + 
  geom_bar(stat="identity") +ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

ggplot(data=datos_1, aes(x=EDUC, y=Freq,fill=ANIOA)) + 
  geom_bar(stat="identity",position = position_stack()) +
  scale_fill_manual(values=c("darkgreen", "purple","black"))

ggplot(data=datos_1_prop, aes(x=EDUC, y=Freq,fill=ANIOA)) + 
  geom_bar(stat="identity",position = position_stack()) +ylab("frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

###################################################################################
#######################pregunta 5############################################################
#############################################################################################

#################tabla cruazada ANIOA y NIN1####################
################################################################

tabla_02<-table(select(datos,ANIOA,NIN1))  
datos_02<-as.data.frame(tabla_02)
table_02_prop<-prop.table(tabla_02)
table_02_prop
datos_02_prop<-as.data.frame(table_02_prop)


##########grafica de la tabla cruazada ANIOA y NIN1############
###############################################################

ggplot(data=datos_02_prop, aes(x=ANIOA, y=Freq,fill=NIN1)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  ylab("frecuencia relativa") + geom_text(aes(label= Freq), vjust = -0.4, size=3.5)+
  scale_fill_manual(values=c("green", "purple","black"))

########################tabla cruzada ANIOA y NIN2################
##################################################################

tabla_002<-table(select(datos,ANIOA,NIN2))  
tabla_002_prop<-prop.table(tabla_002)
tabla_002_prop

datos_002_prop<-as.data.frame(tabla_002_prop)

###################grafica de latabla cruzada ANIOA y NIN2################################
##########################################################################################
ggplot(data=datos_002_prop, aes(x=ANIOA, y=Freq,fill=NIN2)) +
  geom_bar(stat="identity",position = position_dodge()) + geom_text(aes(label= Freq), vjust = -0.4, size=3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values=c("green", "purple","black"))


##############tabla cruzada ANIOA, NIN1 y NIN2###########################################
#########################################################################################

tabla_0002<-table(select(datos,ANIOA,NIN1,NIN2))  
datos_0002<-as.data.frame(tabla_0002)
table_0002_prop<-prop.table(tabla_0002)
table_0002_prop
datos_0002_prop<-as.data.frame(table_0002_prop)

##############grafica de la tabla cruazada ANIOA, NIN1 y NIN2##############################
###########################################################################################


ggplot(data=datos_0002_prop, aes(x=ANIOA, y=Freq,fill=NIN2)) +facet_grid(NIN1~.)+
  geom_bar(stat="identity",position = position_dodge()) + geom_text(aes(label= Freq), vjust = -0.8, size=3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values=c("blue", "purple","black"))


###################################################################################
#######################pregunta 6############################################################
#############################################################################################

tabla_3<-table(select(datos,c(ANIOP,ANIOA,RAZAN)))
prop.table(tabla_3)
datos_3<-as.data.frame(tabla_3)
datos_3_prop<-as.data.frame(prop.table(tabla_3))

ggplot(data=datos_3_prop, aes(x=ANIOP, y=Freq,fill=ANIOA)) +
  facet_grid(RAZAN~.) +
  geom_bar(stat="identity",position = position_dodge()) + geom_text(aes(label= Freq), vjust = -0.25, size=3.5)+
  ylab("frecuencia relativa")+scale_fill_manual(values=c("green", "purple","black"))

# Lo mismo que arriba pero separado
ggplot(data=filter(datos_3_prop,RAZAN=="negra"), aes(x=ANIOP, y=Freq,fill=ANIOA)) +
  geom_bar(stat="identity",position = position_dodge()) + geom_text(aes(label= Freq), vjust = -0.3, size=3.5)+
  ylab("frecuencia relativa")+scale_fill_manual(values=c("green", "purple","black"))

ggplot(data=filter(datos_3_prop,RAZAN=="blanca"), aes(x=ANIOP, y=Freq,fill=ANIOA)) +
  geom_bar(stat="identity",position = position_dodge()) + geom_text(aes(label= Freq), vjust = -0.3, size=3.5)+
  ylab("frecuencia relativa")+scale_fill_manual(values=c("green", "purple","black"))

