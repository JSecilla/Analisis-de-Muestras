
#### C?digo Trabajo An?lisis de Muestras - 2? Parte ####

##############################################################################################################

### Generaci?n de datos ###

##############################################################################################################

if (!require("sae")) install.packages("sae")
library("sae")

NIU=100452943
set.seed(NIU)
cual = sample(1:17,1)

data(incomedata)
datosECV = incomedata
datos16 = subset(datosECV, (datosECV$labor>0))
datos16$age = datos16$age - 1
nrows = dim(datos16)[[1]]
datos16$horas = round(rnorm(nrows,34,3), 1)
datos16$horas[(datos16$labor==2) | (datos16$labor == 3)] = 0
datos16$income = round(jitter(datos16$income),1)
datos16$income[datos16$labor==2] = datos16$income[datos16$labor==2]*0.7
datos16$income[datos16$labor==3] = 0
datosFinal = data.frame(ca=datos16$ac, prov=datos16$prov, 
                        provlab=datos16$provlab, gen=datos16$gen, 
                        edad=datos16$age, nac=datos16$nat, 
                        neduc=datos16$educ, sitemp=datos16$labor, 
                        ingnorm=datos16$income, horas=datos16$horas,
                        factorel=round(datos16$weight,1))

datos_CastillaLaMancha = datosFinal[datosFinal[,1]==8,]
write.table(datos_CastillaLaMancha,"datos_CastillaLaMancha.txt",row.names=FALSE)

datos=datos_CastillaLaMancha


##############################################################################################################

### Estimaci?n de ingresos mediante dise?o muestral ###

##############################################################################################################

datos_M = datos[which(datos$sitemp == 1 & datos$gen == 1),]  # Datos de empleados masculinos
datos_F = datos[which(datos$sitemp == 1 & datos$gen == 2),]  # Datos de empleadas femeninas

ing_M = datos_M[,9]      # Ingresos de empleados masculinos
ing_F = datos_F[,9]      # Ingresos de empleadas femeninas

w_M = datos_M[,11]       # Pesos de muestreo para empleados masculinos
w_F = datos_F[,11]       # Pesos de muestreo para empleadas femeninas

pinc_M = 1/w_M           # Probabilidades de inclusi?n de empleados masculinos
pinc_F = 1/w_F           # Probabilidades de inclusi?n de empleadas femeninas

N_M = sum(datos_M[,11])  # Estimaci?n de tama?o poblacional de empleados masculinos
N_F = sum(datos_F[,11])  # Estimaci?n de tama?o poblacional de empleadas femeninas


#Cargo los datos de otra forma para poder copiar y pegar el código.

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
if (!require("survey")) install.packages("survey")
library("survey")
if (!require("mase")) install.packages("mase")
library("mase")

datos_CastillaLaMancha <- dplyr::filter(datos_CastillaLaMancha, sitemp == 1) #Filtramos por personas empleadas
datosMujeres <- dplyr::filter(datos_CastillaLaMancha, gen == 2) #Datos de las mujeres
datosHombres <- dplyr::filter(datos_CastillaLaMancha, gen == 1) #Datos de los hombres

## Apartado 1 ##   

(media_M = sum(ing_M/(N_M*pinc_M)))  # Estimaci?n del ingreso medio de empleados masculinos
(MediaSalarioHombres <- weighted.mean(x = datosHombres$ingnorm,w = datosHombres$factorel))
horvitzThompson(y = datosHombres$ingnorm,pi = datosHombres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2]

(media_F = sum(ing_F/(N_F*pinc_F)))  # Estimaci?n del ingreso medio de empleadas femeninas
(MediaSalarioMujeres <- weighted.mean(x = datosMujeres$ingnorm,w = datosMujeres$factorel))
horvitzThompson(y = datosMujeres$ingnorm,pi = datosMujeres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2]

#Vemos que de las tres formas nos devuelve el mismo resultado.

## Apartado 2 ##

Matriz_pinc_M = pinc_M%*%t(pinc_M)  # Matriz de probabilidades de inclusi?n de segundo orden de empleados masculinos
diag(Matriz_pinc_M) = pinc_M
Matriz_pinc_F = pinc_F%*%t(pinc_F)  # Matriz de probabilidades de inclusi?n de segundo orden de empleadas femeninas
diag(Matriz_pinc_F) = pinc_F        

## Apartado 3 ##

# Usamos el estimador propio de la varianza del estimador de Horvitz-Thompson pues el de Yates-Grunding devuelve una varianza negativa.

(var_media_M = sum((1-pinc_M)/(pinc_M^2)*(ing_M/N_M)^2))  # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 1 para empleados masculinos

(Varest_hombres <- as.numeric(horvitzThompson(
  y = datosHombres$ingnorm,
  pi = datosHombres$factorel^(-1),
  var_est = TRUE,
  var_method = "LinHT",
  B = 1000,
))[4])
(EM_Hombres <- sqrt(Varest_hombres))


(var_media_F = sum((1-pinc_F)/(pinc_F^2)*(ing_F/N_F)^2))  # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 1 para empleadas femeninas

(Varest_mujeres <- as.numeric(horvitzThompson(
  y = datosMujeres$ingnorm,
  pi = datosMujeres$factorel^(-1),
  var_est = TRUE,
  var_method = "LinHTSRS",
  B = 1000
)[4]))
(EM_Mujeres <- sqrt(Varest_mujeres))

#En este apartado nos salen resultados distintos, luego le voy a preguntar sobre cual es el comando bueno para 
#estimar bien la varianza ya que usando uno que comentó el no me sale.

#También he probado a asignarle un diseño muestral y calcularlo de esa forma, la media si me la estima bien, pero las varianzas respecto a mi estimación se sobreestiman
#Nose si se puede hacer de esta forma, tengo pensado preguntarle luego

dsrsR_Hombres = svydesign(id = ~1, weights= datosHombres$factorel, data = datosHombres) #Diseño de los hombres
summary (dsrsR_Hombres)

dsrsR_Mujeres = svydesign(id = ~1, weights= datosMujeres$factorel, data = datosMujeres) #Diseño de las mujeres
summary (dsrsR_Mujeres)

svymean(~ingnorm , dsrsR_Hombres, na.rm = TRUE )
svymean(~ingnorm , dsrsR_Mujeres, na.rm = TRUE )

## Apartado 4 ##

# Estimamos el coeficiente de variaci?n mediante el cociente de la ra?z de la varianza muestral pesada entre la media muestral pesada

var_pesada_M = sum(w_M*(ing_M-media_M)^2)/sum(w_M)      # Estimaci?n (sesgada) de la varianza poblacional con pesos de los ingresos de empleados masculinos
(cv_M = sqrt(var_pesada_M)/media_M)                     # Estimaci?n (sesgada pero consistente) del coeficiente de variaci?n poblacional de los ingresos de empleados masculinos
(CVH <- EM_Hombres/MediaSalarioHombres)



var_pesada_F = sum(w_F*(ing_F-media_F)^2)/sum(w_F)      # Estimaci?n (sesgada) de la varianza poblacional con pesos de los ingresos de empleadas femeninas
(cv_F = sqrt(var_pesada_F)/media_F )                      # Estimaci?n (sesgada pero consistente) del coeficiente de variaci?n poblacional de los ingresos de empleadas femeninas
(CVM <- EM_Mujeres/MediaSalarioMujeres)

## Apartado 5 ##

# Obtenemos el intervalo de confianza al 95% de Chebyshev aproximado con la estimaci?n de varianza del estimador del apartado 3
# En este y el resto de apartados de intervalos de confianza aplicamos una correcci?n de Bonferroni, de modo que cada intervalo es al 97,5% de confianza de manera aislada, pero simult?neamente al 95% de confianza en conjunto

(IC_Chebyshev_media_M = c(media_M-sqrt(var_media_M/0.025),media_M+sqrt(var_media_M/0.025))) # Intervalo de confianza al 97,5% del ingreso medio de empleados masculinos
(IC_Chebyshev_media_F = c(media_F-sqrt(var_media_F/0.025),media_F+sqrt(var_media_F/0.025)))  # Intervalo de confianza al 97,5% del ingreso medio de empleadas femeninos


## Apartado 6 ##

(IC_TCL_media_M = c(media_M-qnorm(0.9875)*sqrt(var_media_M),media_M+qnorm(0.9875)*sqrt(var_media_M))) # Intervalo de confianza al 97,5% del ingreso medio de empleados masculinos
confint(svymean(~ingnorm , dsrsR_Hombres , na.rm = TRUE))
(IC_TCL_media_F = c(media_F-qnorm(0.9875)*sqrt(var_media_F),media_F+qnorm(0.9875)*sqrt(var_media_F)))  # Intervalo de confianza al 97,5% del ingreso medio de empleadas femeninos
confint(svymean(~ingnorm , dsrsR_Mujeres , na.rm = TRUE ))

## Apartado 7 ##

# Los gr?ficos deben hacerse teniendo en cuenta los pesos. De momento los he dejado sin hacer.


#Te adjunto mi propuesta de graficos

#Histograma para los hombres con ggplot

ggplot(data = datosHombres, aes(x = ingnorm, weight = factorel)) + 
  geom_histogram(fill = "blue", color = "black",bins = 10 )+ #bins es el numero de barras que usa, si se quiere especificar una anchura usar binwidth()) 
  theme_bw() +
  labs(title = "Histograma de ingresos de los hombres") 

#Histograma con el diseño muestral

svyhist(~ingnorm , dsrsR_Hombres , main =" Histograma de los ingresos de los hombres " ,col ="blue" )


#Histograma para las mujeres con ggplot
ggplot(data = datosMujeres, aes(x = ingnorm, weight = factorel)) +
  geom_histogram(color = "black", fill = "pink", show.legend = TRUE, bins = 10) + 
  theme_bw() +
  labs(title = "Histograma de ingresos de las mujeres")

#Histograma con el diseño muestral

svyhist(~ingnorm , dsrsR_Mujeres , main =" Muestra ponderada " ,col ="pink" )

#Boxplot para los hombres con ggplot

ggplot(data = datosHombres, aes(x =ingnorm, weight = factorel)) + 
  geom_boxplot(fill = "Blue") + coord_flip() + theme_light()

#Boxplot con el diseño muestral para hombres
svyboxplot(ingnorm~1, design = dsrsR_Hombres,col ="blue" )


#Boxplot para las mujeres con ggplot

ggplot(data = datosMujeres, aes(x = ingnorm, weight = factorel)) + 
  geom_boxplot(fill = "pink") + theme_bw() + coord_flip()

svyboxplot(ingnorm~1, design = dsrsR_Mujeres,col ="pink" )

##############################################################################################################

### Estimaci?n de la tasa de individuos en riesgo de pobreza ###

##############################################################################################################

z = 6700

dicotomizar <- function (datos, z) {  # Funci?n para dicotomizar datos en funci?n de si son menores a z. V?lido para z>1.
  datos[which(datos<z)] = 1
  datos[which(datos>=z)] = 0
  return(datos)
}

riesgo_M = dicotomizar(ing_M, z)      # Variable dicot?mica acerca del si un empleado masculino se encuentra en riesgo de pobreza
riesgo_F = dicotomizar(ing_F, z)      # Variable dicot?mica acerca del si una empleada femenina se encuentra en riesgo de pobreza

datosHombres <- mutate(datosHombres, pobreza = ifelse(ingnorm < z,1,0))
datosMujeres <- mutate(datosMujeres, pobreza = ifelse(ingnorm < z,1,0))


## Apartado 1 ##

(prop_M = sum(riesgo_M/(N_M*pinc_M))) # Estimaci?n de la proporci?n de empleados masculinos en riesgo de pobreza
(Prop_Pobreza_H <- weighted.mean(x = datosHombres$pobreza,w = datosHombres$factorel))
horvitzThompson(y = datosHombres$pobreza,pi = datosHombres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2]

(prop_F = sum(riesgo_F/(N_F*pinc_F)))  # Estimaci?n de la proporci?n de empleadas femeninas en riesgo de pobreza
(Prop_Pobreza_M <- weighted.mean(x = datosMujeres$pobreza,w = datosMujeres$factorel))
horvitzThompson(y = datosMujeres$pobreza,pi = datosMujeres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2]


## Apartado 2 ##

# Ya hecho en la secci?n de estimaci?n de ingresos medios

## Apartado 3 ##

# Usamos el estimador propio de la varianza del estimador de Horvitz-Thompson pues el de Yates-Grunding devuelve una varianza negativa.

(var_prop_M = sum((1-pinc_M)/(pinc_M^2)*(riesgo_M/N_M)^2))  # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 1 para empleados masculinos
(Varest_hombres_Pobreza <- as.numeric(horvitzThompson(
  y = datosHombres$pobreza,
  pi = datosHombres$factorel^(-1),
  var_est = TRUE,
  var_method = "LinHTSRS",
  B = 1000
)[4]))

(EM_Hombres_Pobreza <- sqrt(Varest_hombres_Pobreza))

(var_prop_F = sum((1-pinc_F)/(pinc_F^2)*(riesgo_F/N_F)^2))  # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 1 para empleadas femeninas
(Varest_mujeres_Pobreza <- as.numeric(horvitzThompson(
  y = datosMujeres$pobreza,
  pi = datosMujeres$factorel^(-1),
  var_est = TRUE,
  var_method = "LinHTSRS",
  B = 1000
)[4]))
(EM_Mujeres_Pobreza <- sqrt(Varest_mujeres_Pobreza))


## Apartado 4 ##

# Estimamos la varianza poblacional con var(riesgo) = prop(1-prop). En realidad esto devuelve el mismo resultado que el estimador anterior de la varianza pesada.
# Al divir su ra?z por la proporci?n estimada, queda cv(riesgo) = sqrt((1-prop)/prop).

(cv_riesgo_M = sqrt((1-prop_M)/prop_M))      # Estimaci?n del coeficiente de variaci?n poblacional con pesos del riesgo de pobreza de empleados masculinos
(CVH_Pobreza <- EM_Hombres_Pobreza/Prop_Pobreza_H)

(cv_riesgo_F = sqrt((1-prop_F)/prop_F))      # Estimaci?n del coeficiente de variaci?n poblacional con pesos del riesgo de pobreza de empleadas femeninas               
(CVM_Pobreza <- EM_Mujeres_Pobreza/Prop_Pobreza_M)

## Apartado 5 ##

# Obtenemos el intervalo de confianza al 95% de Chebyshev aproximado con la estimaci?n de varianza del estimador del apartado 3
# Acotamos el rango del intervalo por debajo por 0 y por encima por 1 ya que el par?metro poblacional de inter?s se encuentra entre 0 y 1

IC_Chebyshev_prop_M = c(max(0,prop_M-sqrt(var_prop_M/0.025)),min(1,prop_M+sqrt(var_prop_M/0.025)))             # Intervalo de confianza al 97,5% para la proporci?n de empleados masculinos en riesgo de pobreza
IC_Chebyshev_prop_F = c(max(0,prop_F-sqrt(var_prop_F/0.025)),min(1,prop_F+sqrt(var_prop_F/0.025)))             # Intervalo de confianza al 97,5% para la proporci?n de empleadas femeninas en riesgo de pobreza


## Apartado 6 ##

IC_TCL_prop_M = c(max(0,prop_M-qnorm(0.9875)*sqrt(var_prop_M)),min(1,prop_M+qnorm(0.9875)*sqrt(var_prop_M)))   # Intervalo de confianza al 97,5% para la proporci?n de empleados masculinos en riesgo de pobreza
IC_TCL_prop_F = c(max(0,prop_F-qnorm(0.9875)*sqrt(var_prop_F)),min(1,prop_F+qnorm(0.9875)*sqrt(var_prop_F)))   # Intervalo de confianza al 97,5% para la proporci?n de empleadas femeninas en riesgo de pobreza


## Apartado 7 ##

# De nuevo, dejamos los gr?ficos sin hacer por el momento, aunque estimamos las proporciones de cada provincia.
# Comenzamos obteniendo las variables de cada provincia.

riesgo_Albacete_M = dicotomizar(datos_M[which(datos_M[,3]=="Albacete"),9], z)
riesgo_Albacete_F = dicotomizar(datos_F[which(datos_F[,3]=="Albacete"),9], z)

riesgo_CiudadReal_M = dicotomizar(datos_M[which(datos_M[,3]=="CiudadReal"),9], z)
riesgo_CiudadReal_F = dicotomizar(datos_F[which(datos_F[,3]=="CiudadReal"),9], z)

riesgo_Cuenca_M = dicotomizar(datos_M[which(datos_M[,3]=="Cuenca"),9], z)
riesgo_Cuenca_F = dicotomizar(datos_F[which(datos_F[,3]=="Cuenca"),9], z)

riesgo_Guadalajara_M = dicotomizar(datos_M[which(datos_M[,3]=="Guadalajara"),9], z)
riesgo_Guadalajara_F = dicotomizar(datos_F[which(datos_F[,3]=="Guadalajara"),9], z)

riesgo_Toledo_M = dicotomizar(datos_M[which(datos_M[,3]=="Toledo"),9], z)
riesgo_Toledo_F = dicotomizar(datos_F[which(datos_F[,3]=="Toledo"),9], z)

# Obtenemos las probabilidades de inclusi?n y estimaciones de tama?os poblacionales

pinc_Albacete_M = 1/datos_M[which(datos_M[,3]=="Albacete"),11]
pinc_Albacete_F = 1/datos_F[which(datos_F[,3]=="Albacete"),11]
N_Albacete_M = sum(datos_M[which(datos_M[,3]=="Albacete"),11])
N_Albacete_F = sum(datos_F[which(datos_F[,3]=="Albacete"),11])

pinc_CiudadReal_M = 1/datos_M[which(datos_M[,3]=="CiudadReal"),11]
pinc_CiudadReal_F = 1/datos_F[which(datos_F[,3]=="CiudadReal"),11]
N_CiudadReal_M = sum(datos_M[which(datos_M[,3]=="CiudadReal"),11])
N_CiudadReal_F = sum(datos_F[which(datos_F[,3]=="CiudadReal"),11])

pinc_Cuenca_M = 1/datos_M[which(datos_M[,3]=="Cuenca"),11]
pinc_Cuenca_F = 1/datos_F[which(datos_F[,3]=="Cuenca"),11]
N_Cuenca_M = sum(datos_M[which(datos_M[,3]=="Cuenca"),11])
N_Cuenca_F = sum(datos_F[which(datos_F[,3]=="Cuenca"),11])

pinc_Guadalajara_M = 1/datos_M[which(datos_M[,3]=="Guadalajara"),11]
pinc_Guadalajara_F = 1/datos_F[which(datos_F[,3]=="Guadalajara"),11]
N_Guadalajara_M = sum(datos_M[which(datos_M[,3]=="Guadalajara"),11])
N_Guadalajara_F = sum(datos_F[which(datos_F[,3]=="Guadalajara"),11])

pinc_Toledo_M = 1/datos_M[which(datos_M[,3]=="Toledo"),11]
pinc_Toledo_F = 1/datos_F[which(datos_F[,3]=="Toledo"),11]
N_Toledo_M = sum(datos_M[which(datos_M[,3]=="Toledo"),11])
N_Toledo_F = sum(datos_F[which(datos_F[,3]=="Toledo"),11])

# Finalmente calculamos los estimadores de proporciones de empleados en riesgo de pobreza de cada provincia

prop_Albacete_M = sum(riesgo_Albacete_M/(N_Albacete_M*pinc_Albacete_M))
prop_Albacete_F = sum(riesgo_Albacete_F/(N_Albacete_F*pinc_Albacete_F))

prop_CiudadReal_M = sum(riesgo_CiudadReal_M/(N_CiudadReal_M*pinc_CiudadReal_M))
prop_CiudadReal_F = sum(riesgo_CiudadReal_F/(N_CiudadReal_F*pinc_CiudadReal_F))

prop_Cuenca_M = sum(riesgo_Cuenca_M/(N_Cuenca_M*pinc_Cuenca_M))
prop_Cuenca_F = sum(riesgo_Cuenca_F/(N_Cuenca_F*pinc_Cuenca_F))

prop_Guadalajara_M = sum(riesgo_Guadalajara_M/(N_Guadalajara_M*pinc_Guadalajara_M))
prop_Guadalajara_F = sum(riesgo_Guadalajara_F/(N_Guadalajara_F*pinc_Guadalajara_F))

prop_Toledo_M = sum(riesgo_Toledo_M/(N_Toledo_M*pinc_Toledo_M))
prop_Toledo_F = sum(riesgo_Toledo_F/(N_Toledo_F*pinc_Toledo_F))


#Te dejo por aquí lo que yo he utilizado

datos_CastillaLaMancha<- mutate(datos_CastillaLaMancha, pobreza = ifelse(ingnorm < 6700,1,0))
Albacete <- filter(datos_CastillaLaMancha,prov == 2)
Ciudad_Real <- filter(datos_CastillaLaMancha,prov == 13)
Toledo <- filter(datos_CastillaLaMancha,prov == 45)
Cuenca <- filter(datos_CastillaLaMancha,prov == 16)
Guadalajara <- filter(datos_CastillaLaMancha,prov == 19)


PropAlbacete <- as.numeric(horvitzThompson(y = Albacete$pobreza,pi = Albacete$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2])

PropCiudad_Real <- as.numeric(horvitzThompson(y = Ciudad_Real$pobreza,pi = Ciudad_Real$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2])

PropToledo <- as.numeric(horvitzThompson(y = Toledo$pobreza,pi = Toledo$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2])

PropCuenca <- as.numeric(horvitzThompson(y = Cuenca$pobreza,pi = Cuenca$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2])

PropGuadalajara <- as.numeric(horvitzThompson(y = Guadalajara$pobreza,pi = Guadalajara$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[2])


if (!require("sp")) install.packages("sp")
library(sp)
elmapa = readRDS("C:/Users/jsm01/Downloads/gadm36_ESP_2_sp.rds")

if (!require("colorspace")) install.packages("colorspace")
library(colorspace)
CM = elmapa[elmapa$NAME_1=="Castilla-La Mancha",]
CM$cantidad = c(PropAlbacete,PropCiudad_Real,PropCuenca,PropGuadalajara,PropToledo)
textos = c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo")
coordenadas = coordinates(CM)
lista = list("sp.text", coordenadas, textos)
spplot(CM, "cantidad", sp.layout=lista, col.regions= sequential_hcl(1000))





## Apartado 8 ##

# En este apartado y en los dos siguientes aprovechamos la identidad de que el estimador del total es igual al estimador de la proporci?n multiplicado por el tama?o poblacional N
# Los c?lculos a trav?s de las correspondientes f?rmulas del estimador de Horvitz-Thompson para totales deber?an ofrecer los mismos resultados



(tot_riesgo_M = N_M*prop_M)    # Estimaci?n del n?mero de empleados masculinos en riesgo de pobreza en Castilla-La Mancha
(Total_Pobreza_Hombres <- as.numeric(horvitzThompson(y = datosHombres$pobreza,pi = datosHombres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[1]))

(tot_riesgo_F = N_F*prop_F)    # Estimaci?n del n?mero de empleadas femeninas en riesgo de pobreza en Castilla-La Mancha
(Total_Pobreza_Mujeres <- as.numeric(horvitzThompson(y = datosMujeres$pobreza,pi = datosMujeres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[1]))

## Apartado 9 ##

(var_tot_riesgo_M = N_M^2*var_prop_M) # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 8 para empleados masculinos
(Var_Total_Pobreza_Hombres <- as.numeric(horvitzThompson(y = datosHombres$pobreza,pi = datosHombres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[3]))


(var_tot_riesgo_F = N_F^2*var_prop_F)  # Estimaci?n de la varianza del estimador de Horvitz-Thompson usado en el apartado 8 para empleadas femeninas
(Var_Total_Pobreza_Mujeres <- as.numeric(horvitzThompson(y = datosMujeres$pobreza,pi = datosMujeres$factorel^(-1),var_est = TRUE,var_method = "LinHTSRS")[3]))


## Apartado 10 ##

(cv_tot_M = sqrt(var_tot_riesgo_M)/tot_riesgo_M)    # Estimaci?n (sesgada) del coeficiente de variaci?n del estimador del total de empleados masculinos en riesgo de pobreza
(CVTH <- sqrt(Var_Total_Pobreza_Hombres)/Total_Pobreza_Hombres)

(cv_tot_F = sqrt(var_tot_riesgo_F)/tot_riesgo_F)    # Estimaci?n (sesgada) del coeficiente de variaci?n del estimador del total de empleadas femeninas en riesgo de pobreza       
(CVTM <- sqrt(Var_Total_Pobreza_Mujeres)/Total_Pobreza_Mujeres)

##############################################################################################################


