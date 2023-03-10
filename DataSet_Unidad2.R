summary(adult)


#Sustitución de NA´s para columnas con datos no categoricos
#----------------------------------------------------------
promedio.age <- mean(adult$age, na.rm = TRUE)
adult$age[is.na(adult$age)] <- promedio.age

promedio.fnlwgt <- mean(adult$fnlwgt, na.rm = TRUE)
adult$fnlwgt[is.na(adult$fnlwgt)] <- promedio.fnlwgt

promedio.education <- mean(adult$education.num, na.rm = TRUE)
adult$education.num[is.na(adult$education.num)] <- promedio.education

promedio.capitalgain <- mean(adult$capital.gain, na.rm = TRUE)
adult$capital.gain[is.na(adult$capital.gain)] <- promedio.capitalgain

promedio.capitalloss <- mean(adult$capital.loss, na.rm = TRUE)
adult$capital.loss[is.na(adult$capital.loss)] <- promedio.capitalloss

promedio.hoursperweek <- mean(adult$hours.per.week, na.rm = TRUE)
adult$hours.per.week[is.na(adult$hours.per.week)] <- promedio.hoursperweek


#Conversión de variables categoricas a valores numericos para eliminacion de NA's
#--------------------------------------------------------------------------------
levels.wk <- c('Private', 'Federal-gov', 'State-gov', 'Local-gov', 'Self-emp-not-inc', 'Self-emp-inc')
adult$workclass <- match(adult$workclass, levels.wk)

levels.occupation <- c('Exec-managerial', 'Machine-op-inspct', 'Prof-specialty', 'Other-service', 'Adm-clerical', 'Craft-repair', 
                       'Transport-moving', 'Handlers-cleaners', 'Sales', 'Farming-fishing', 'Tech-support', 'Protective-serv',
                       'Machine-op-inspct', 'Priv-house-serv')
adult$occupation <- match(adult$occupation, levels.occupation)

levels.nc <- c('Yugoslavia', 'Vietnam', 'United-States', 'Trinadad&Tobago', 'Thailand', 'Taiwan', 'South', 'Scotland',
               'Puerto-Rico', 'Portugal', 'Poland', 'Philippines', 'Peru', 'Outlying-US(Guam-USVI-etc)', 
               'Nicaragua', 'Mexico', 'Laos', 'Japan', 'Jamaica', 'Italy', 'Ireland', 'Iran', 'India', 'Hungary', 'Hong',
               'Honduras', 'Holand-Netherlands', 'Haiti', 'Guatemala', 'Greece', 'Germany', 'France', 'England', 'El-Salvador', 
               'Ecuador', 'Dominican-Republic', 'Cuba', 'Columbia', 'China', 'Canada', 'Cambodia')
adult$native.country <- match(adult$native.country, levels.nc)
#Sutitucion de mediana en NA para variables categoricas
#--------------------------------------------------------
(median.wk <- median(adult$workclass, na.rm = TRUE))
adult$workclass[is.na(adult$workclass)] <- median.wk

(median.occ <- median(adult$occupation, na.rm = TRUE))
adult$occupation[is.na(adult$occupation)] <- median.occ

(median.nc <- median(adult$native.country, na.rm = TRUE))
adult$native.country[is.na(adult$native.country)] <- median.nc
#Conversión a factores de variables categoricas
#----------------------------------------------
adult$workclass = factor(adult$workclass, levels = c(1:6), labels = c('Private', 'Federal-gov', 'State-gov', 'Local-gov', 'Self-emp-not-inc', 'Self-emp-inc'))
adult$occupation = factor(adult$occupation, levels = c(1:14), labels = c('Exec-managerial', 'Machine-op-inspct', 'Prof-specialty', 'Other-service', 'Adm-clerical', 'Craft-repair', 
                                                                         'Transport-moving', 'Handlers-cleaners', 'Sales', 'Farming-fishing', 'Tech-support', 'Protective-serv',
                                                                         'Machine-op-inspct', 'Priv-house-serv'))
adult$native.country = factor(adult$native.country, levels = c(1:41), labels = c('Yugoslavia', 'Vietnam', 'United-States', 'Trinadad&Tobago', 'Thailand', 'Taiwan', 'South', 'Scotland',
                                                                                 'Puerto-Rico', 'Portugal', 'Poland', 'Philippines', 'Peru', 'Outlying-US(Guam-USVI-etc)', 
                                                                                 'Nicaragua', 'Mexico', 'Laos', 'Japan', 'Jamaica', 'Italy', 'Ireland', 'Iran', 'India', 'Hungary', 'Hong',
                                                                                 'Honduras', 'Holand-Netherlands', 'Haiti', 'Guatemala', 'Greece', 'Germany', 'France', 'England', 'El-Salvador', 
                                                                                 'Ecuador', 'Dominican-Republic', 'Cuba', 'Columbia', 'China', 'Canada', 'Cambodia'))


#Visualización de los Datos
plot(adult$fnlwgt ~ adult$age,
     xlab="Edad", ylab="Peso Asignado",
     main="Relacion entre edad y peso asignado")

plot(adult$fnlwgt ~ adult$age,
     xlab="Edad", ylab="Peso Asignado",
     main="Relacion entre edad y peso asignado")

ggplot()+geom_histogram(data=adult,
                        aes(x=age), fill="gold", color="black",
                        binwidth = 10)+
  labs(x="Edad", y="Número de personas", title="Edad de personas censadas")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Estos tienen mejor presentación
ggplot(adult, aes(x=adult$age, y=adult$fnlwgt, color=adult$race)) + 
  geom_point(size=0.7) +
  theme_ipsum()

#
adult$sex = with(adult, reorder(sex, hours.per.week, median))

p <- adult %>%
  ggplot( aes(x=sex, y=hours.per.week, fill=sex)) + 
  geom_violin() +
  xlab("Sexo") +
  theme_ipsum() +
  xlab("")
p

#
levels.wk <- c('Private', 'Federal-gov', 'State-gov', 
               'Local-gov', 'Self-emp-not-inc', 
               'Self-emp-inc')
adult$workclass <- match(adult$workclass, levels.wk)

p <- adult %>%
  ggplot( aes(x=workclass, fill=workclass)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Clase trabajadora") +
  xlab("Numero de clase") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p

adult$workclass = factor(adult$workclass, levels = c(1:6), labels = c('Private', 'Federal-gov', 'State-gov', 'Local-gov', 'Self-emp-not-inc', 'Self-emp-inc'))

#------------------------------------

hist(adult$age)
library(ggplot2)

a<-ggplot(adult,aes(x=income))+geom_bar(fill="purple")+facet_wrap(~income,nrow=2)+
  theme_dark()+ggtitle("Ricos vs pobres")+
  theme(plot.title = element_text(family="Comic Sans MS",
                                  size=rel(1.5),
                                  vjust=1.5,hjust=0.5,
                                  face="bold",
                                  color="red",
                                  lineheight=1.5))
a

b<-ggplot(adult,aes(x=age))+geom_boxplot(aes(fill=income))+facet_wrap(~income,nrow=2)+
  scale_fill_manual(values=c("brown","yellow"))+theme_dark()+ggtitle("Ricos vs pobres")+
  theme(plot.title = element_text(family="Comic Sans MS",
                                  size=rel(1.5),
                                  vjust=1.5,hjust=0.5,
                                  face="bold",
                                  color="purple",
                                  lineheight=1.5))
b

c<-ggplot(data=adult,mapping=aes(x=age))+geom_bar(aes(fill=income))+facet_wrap(~income,nrow=2)+
  scale_fill_manual(values=c("brown","yellow"))+ theme_dark()+ggtitle("Ricos vs pobres")+
  theme(plot.title = element_text(family="Comic Sans MS",
                                  size=rel(1.5),
                                  vjust=1.5,hjust=0.5,
                                  face="bold",
                                  color="purple",
                                  lineheight=1.5))
c

ggplot(data=adult,mapping=aes(x=age,fill=factor(income)))+
  facet_wrap(~income,nrow=2)+
  geom_histogram(bins=30,
                 position="identity",
                 alpha=0.8)+
  labs(title="Grafica entre Ricos y Pobres",
       fill="Diferencia mensual",
       x="Edades",
       y="Personas",
       subtitle ="Grafica",
       caption="Data")

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
ggplot(data=adult,mapping=aes(x =age, y = income)) +
  facet_wrap(~income,nrow=2)+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Edad", option = "age") +
  labs(title = 'Ricos vs Pobres',
       fill="vs",
       x="Edades",
       y="Personas",
       subtitle ="Grafica",
       caption="Data") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Library
library(ggplot2)
library(hrbrthemes)
# linear trend + confidence interval
p3 <- ggplot(adult, aes(x=capital.gain, y=age)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()+
  labs(title="Prediccion Grafica",
       x="Ganancia capital",
       y="Edades",
       subtitle ="Regresión lineal",
       caption="Prediccion de las edades con mayor capital")
p3
