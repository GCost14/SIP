library(tidyverse)
#install.packages("rio")
library(rio) #Para que se usa??
library(car)
library(haven)
library(ggmosaic)

#Leo los datos
SIP <- read_dta("SIP_2018-2020_old.dta")

names(SIP)
head(SIP)
str(SIP)

names(entrevistada) #Que es entrevistada?


#selecciono las variables con las que quiero trabajar y asigno nombres
datos <- SIP %>% 
  select(edadm, alturam, pesom, imc, imc_cat, 
         semgest, rn_peso, rn_sexo, soltera, casada, unionlibre, otroeecivil, 
         anosedu, educ, primaria, secundaria, universidad, 
         lugar_parto, localidad, depto, planeado, parto_inicio, parto_terminacion, 
         parto_episio, parto_sin_desgarr, parto_desgarr_grado, 
         parto_ocit_preal, parto_posicion, parto_acomp_TDP, parto_analgesia, parto_atendio)



#Peso al nacer (en gramos)

ggplot(data = datos, aes(x = rn_peso)) +
  geom_histogram(binwidth = 2 * IQR(datos$rn_peso) / (length(datos$rn_peso)^(1/3)), alpha=0.6,  fill = "aquamarine4") +
  labs(x = "Peso al nacer (en gramos)", y = "Frecuencia")

ggplot(data = datos, aes(x = rn_peso)) +
  geom_boxplot( fill = "aquamarine4") +
  labs(x = "Peso al nacer (en gramos)", y = "Frecuencia")


#peso al nacer según sexo biológico del recién nacido
#Genero tabla resumen con el total de niños, el peso promedio y el desvío estandar según el sexo biológico
tabla_pesos_sexo <- datos %>%
  group_by(rn_sexo) %>%
  summarise("cantidad" = n(), "Promedio"=  mean(rn_peso), "Desvío estándar" = sd(rn_peso))

tabla_pesos_sexo #Que sexo es ".", lo reasignamos? lo eliminamos?

#boxplot peso al nacer según sexo biológico
ggplot(data = datos, aes(x = rn_peso, fill = rn_sexo)) +
  geom_boxplot() +
  labs(x = "Peso al nacer (en gramos)")

#boxplot peso al nacer según sexo biológico sin "indefinido" y "."
datos %>% 
  filter(rn_sexo %in% c("Femenino", "Masculino")) %>% 
  ggplot(aes(x = rn_peso, fill = rn_sexo)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2", name = "Sexo biológico") +
  labs(x = "Peso al nacer (en gramos)")


#Modalidad de inicio del parto
addmargins(table(datos$parto_inicio))

#etiquetar los nombres de las variables
datos$parto_inicio_tipo <- factor(datos$parto_inicio, 
                       labels = c( "SD","cesarea_prog", "espontáneo","inducido"))

barplot(prop.table(table(datos$parto_inicio_tipo)))

barplot(prop.table(table(datos$parto_inicio_tipo)),col=c("green","yellow", "orange", "red" ),
        legend.text=c("sin_dato","cesárea_prog", "espontáneo", "inducido"),main="Tipos de inicio de parto",ylim=c(0,0.8),
        ylab ="Frecuencias Relativas",las=1,font.axis=4)

datos %>%
  group_by(parto_inicio) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n, na.rm=TRUE)*100) %>%
  ggplot(aes(x = fct_reorder(parto_inicio, -percent), y = percent, fill = parto_inicio)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Modalidad de inicio del parto", y = "Porcentaje de partos") +
  geom_text(aes(x = parto_inicio, label = round(percent), y = percent, vjust = -0.5)) +
  ylim(0,60) +
  theme(legend.position = "none")


#Tabla que relaciona modalidad de inicio con finalización del parto. 

addmargins(table(datos$parto_terminacion, useNA = "ifany"))

tabla2 <- addmargins(table(datos$parto_inicio, datos$parto_terminacion, useNA = "ifany"))
tabla2 #Que son algunas categorias? Forceps? Vacuum?

tabla3 <- prop.table(x=tabla2, margin=1)
tabla3

datos$parto_inicio <- factor(datos$parto_inicio, labels = c("Sin dato", "Electiva", "Espontaneo", "Inducido"))
datos$parto_terminacion <- factor(datos$parto_terminacion, labels = c("Sin dato", "Cesarea", "Espontanea", "Forceps", "Otra", "Vacuum"))

ggplot(data = datos) +
  geom_mosaic(aes(x = product(parto_inicio), fill = parto_terminacion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", y = "") +
  theme(aspect.ratio = 1, legend.position = "none")

ggplot(data = datos) +
  geom_mosaic(aes(x = product(parto_terminacion), fill = parto_inicio)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", y = "") +
  theme(aspect.ratio = 1, legend.position = "none")



# Genero tabla resumen de los datos de sexo por perfil
tabla_perfiles_por_sexo <- datos %>%
  group_by(Perfil) %>%
  summarize(Mujer = paste(round(100*length(Genero[Genero == 1])/n(), 2), "%"),
            Hombre = paste(round(100*length(Genero[Genero == 2])/n(), 2), "%"))
#Que variable es perfil? y genero?



##################################################

boxplot(rn_peso ~ parto_terminacion, data = datos)

datos %>% 
  filter(parto_terminacion %in% c("Cesarea", "Espontanea", "Forceps", "Otra")) %>% 
  ggplot(aes(parto_terminacion,rn_peso, fill=parto_terminacion)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Peso al nacer (en gramos)", x = "Modalidad de terminación del parto") +
  theme(legend.position = "none")

boxplot(rn_peso ~ semgest, data = datos)


#obtengo el diagrama de dispersión entre el peso de los niños al nacer y las semanas de gestación
ggplot(datos, aes(x=semgest, y=rn_peso)) + 
  geom_point() + theme_light()

#obtengo el diagrama de dispersión entre el peso de los niños al nacer y las semanas de gestación
ggplot(datos, aes(x=semgest, y=rn_peso)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()


library(usethis)
usethis::edit_git_config()
# Modificar en el fichero ".gitconfig" los apartados: "name" y "email" 
# y guardar el fichero

usethis::use_git()
# Elegir siempre la opción: 1
# Y ante la ventana, seleccionar: "Save"

