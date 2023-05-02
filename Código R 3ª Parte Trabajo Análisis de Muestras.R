#### Código Trabajo Análisis de Muestras - 3ª Parte ####

##############################################################################################################

### Generación de datos ###

if (!require(sae)) install.packages("sae")
library("sae")
data(incomedata)

set.seed(292)
NIU = 100452943
plus = round(runif(1,0,500))
set.seed(NIU+plus)
cual = sample(1:17,1)       # Devuelve 1: Andalucía

datosECV = incomedata
datosECVmas16 = subset(datosECV, (datosECV$labor>0))
datosECVmas16$age = datosECVmas16$age - 1
nrows = dim(datosECVmas16)[[1]]
datosECVmas16$horas = round(rnorm(nrows,34,3), 1)
datosECVmas16$horas[(datosECVmas16$labor==2) | (datosECVmas16$labor == 3)] = 0
datosECVmas16$income = round(jitter(datosECVmas16$income),1)
datosECVmas16$income[datosECVmas16$educ==1] = rnorm(length(datosECVmas16$income[datosECVmas16$educ==1]),datosECVmas16$income[datosECVmas16$educ==1]*0.8, 200)
datosECVmas16$income[datosECVmas16$educ==3] = rnorm(length(datosECVmas16$income[datosECVmas16$educ==3]),datosECVmas16$income[datosECVmas16$educ==3]*1.2, 100)
datosECVmas16$income[datosECVmas16$labor==2] = datosECVmas16$income[datosECVmas16$labor==2]*0.7
datosECVmas16$income[datosECVmas16$labor==3] = 0
datosFinal = 
  data.frame(ca=datosECVmas16$ac, prov=datosECVmas16$prov, 
             provlab=datosECVmas16$provlab, gen=datosECVmas16$gen, 
             edad=datosECVmas16$age, nac=datosECVmas16$nat, 
             neduc=datosECVmas16$educ, sitemp=datosECVmas16$labor, 
             ingnorm=datosECVmas16$income, horas=datosECVmas16$horas)

datos_Andalucia = datosFinal[datosFinal[,1]==1,]
write.table(datos_Andalucia,"datos_Andalucia.txt",row.names=FALSE)
datos = datos_Andalucia
datos = datos[which(datos$sitemp!=3),]
datos[which(datos$sitemp==1),8] = 0
datos[which(datos$sitemp==2),8] = 1


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de muestreo simple aleatorio ###

##############################################################################################################


### Generación de la muestra ###


N_pob_activa = nrow(datos)                         
n_pob_activa = ceiling(N_pob_activa/10)            

mas_pob_activa = datos[sample(1:N_pob_activa, n_pob_activa, replace = FALSE),]   
mas_empleados = mas_pob_activa[which(mas_pob_activa$sitemp==0),]                 

N_empleados = length(which(datos$sitemp == 0))    
n_empleados = nrow(mas_empleados)                  


### Estimación de los ingresos medios ###

if (!require(sae)) install.packages("survey")
library("survey")

mas_empleados$tamaño = N_empleados                
diseño_empleados_mas = svydesign(id = ~1, fpc = ~tamaño, data = mas_empleados)

svymean(~ingnorm, diseño_empleados_mas, na.rm = TRUE)
confint(svymean(~ingnorm, diseño_empleados_mas, na.rm = TRUE))


### Estimación de la tasa de paro ###

mas_pob_activa$tamaño = N_pob_activa
diseño_pob_activa_mas = svydesign(id = ~1, fpc = ~tamaño, data = mas_pob_activa)

svymean(~sitemp, diseño_pob_activa_mas, na.rm = TRUE)
confint(svymean(~sitemp, diseño_pob_activa_mas, na.rm = TRUE))


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de razón y regresión ###

##############################################################################################################


### Estimación de los ingresos medios a través del estimador de razón ###

media_horas_trabajo_empleados = mean(datos[which(datos$sitemp == 0),10])

svyratio(~ingnorm, ~horas, diseño_empleados_mas)
predict(svyratio(~ingnorm, ~horas, design = diseño_empleados_mas), total = media_horas_trabajo_empleados)

R_ingresos = mean(mas_empleados$ingnorm)/mean(mas_empleados$horas)
ing_medios_razon = R_ingresos*media_horas_trabajo_empleados
var_ing_medios_razon = (1-n_empleados/N_empleados)/n_empleados*(var(mas_empleados$ingnorm)+R_ingresos^2*var(mas_empleados$horas)-2*R_ingresos*cov(mas_empleados$ingnorm,mas_empleados$horas))

sqrt(var_ing_medios_razon)

centro = ing_medios_razon
radio = qnorm(0.025, lower.tail=FALSE)*sqrt(var_ing_medios_razon)
IC_ing_medios_razon = c(centro - radio, centro + radio)

IC_ing_medios_razon


### Estimación de la tasa de paro a través del estimador de razón ###

media_horas_trabajo_pob_activa = mean(datos$horas)

svyratio(~sitemp, ~horas, diseño_pob_activa_mas)
predict(svyratio(~sitemp, ~horas, design = diseño_pob_activa_mas), total = media_horas_trabajo_pob_activa)

R_tasa_paro = mean(mas_pob_activa$sitemp)/mean(mas_pob_activa$horas)
tasa_paro_razon = R_tasa_paro*media_horas_trabajo_pob_activa
var_tasa_paro_razon = (1-n_pob_activa/N_pob_activa)/n_pob_activa*(var(mas_pob_activa$sitemp)+R_tasa_paro^2*var(mas_pob_activa$horas)-2*R_tasa_paro*cov(mas_pob_activa$sitemp,mas_pob_activa$horas))

sqrt(var_tasa_paro_razon)

centro = tasa_paro_razon
radio = qnorm(0.025, lower.tail=FALSE)*sqrt(var_tasa_paro_razon)
IC_tasa_paro_razon = c(max(0,centro - radio), min(1,centro + radio))

IC_tasa_paro_razon


### Estimación de los ingresos medios a través del estimador de regresión ###

diseño_empleados_regresion = calibrate(diseño_empleados_mas, formula = ~horas, population = c("(Intercept)" = N_empleados, horas = media_horas_trabajo_empleados*N_empleados))
svymean(~ingnorm, diseño_empleados_regresion)

b_ingresos = cov(mas_empleados$ingnorm, mas_empleados$horas)/var(mas_empleados$horas)
ing_medios_regresion = mean(mas_empleados$ingnorm) + b_ingresos*(media_horas_trabajo_empleados-mean(mas_empleados$horas))
var_ing_medios_regresion = (1-n_empleados/N_empleados)/n_empleados*(sum((mas_empleados$ingnorm-mean(mas_empleados$ingnorm))^2)/(n_empleados-2)-cov(mas_empleados$ingnorm,mas_empleados$horas)^2/var(mas_empleados$horas))

sqrt(var_ing_medios_regresion)

centro = ing_medios_regresion
radio = qnorm(0.025, lower.tail=FALSE)*sqrt(var_ing_medios_regresion)
IC_ing_medios_regresion = c(centro - radio, centro + radio)

IC_ing_medios_regresion


### Estimación de la tasa de paro a través del estimador de regresión ###

diseño_pob_activa_regresion = calibrate(diseño_pob_activa_mas, formula = ~horas, population = c("(Intercept)" = N_pob_activa, horas = media_horas_trabajo_pob_activa*N_pob_activa))
svymean(~sitemp, diseño_pob_activa_regresion)

b_tasa_paro = cov(mas_pob_activa$sitemp, mas_pob_activa$horas)/var(mas_pob_activa$horas)
tasa_paro_regresion = mean(mas_pob_activa$sitemp) + b_tasa_paro*(media_horas_trabajo_pob_activa-mean(mas_pob_activa$horas))
var_tasa_paro_regresion = (1-n_pob_activa/N_pob_activa)/n_pob_activa*(sum((mas_pob_activa$sitemp-mean(mas_pob_activa$sitemp))^2)/(n_pob_activa-2)-cov(mas_pob_activa$sitemp,mas_pob_activa$horas)^2/var(mas_pob_activa$horas))

sqrt(var_tasa_paro_regresion)

centro = tasa_paro_regresion
radio = qnorm(0.025, lower.tail=FALSE)*sqrt(var_tasa_paro_regresion)
IC_tasa_paro_regresion = c(centro - radio, centro + radio)

IC_tasa_paro_regresion


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de muestreo estratificado ###

##############################################################################################################


### Cálculo de afijación de Neyman para estimación de los ingresos medios ###

mas_empleados_1 = mas_empleados[which(mas_empleados$neduc == 1),]
mas_empleados_2 = mas_empleados[which(mas_empleados$neduc == 2),]
mas_empleados_3 = mas_empleados[which(mas_empleados$neduc == 3),]

N1_empleados = length(which(datos$neduc == 1 & datos$sitemp == 0))
N2_empleados = length(which(datos$neduc == 2 & datos$sitemp == 0))
N3_empleados = length(which(datos$neduc == 3 & datos$sitemp == 0))

W1_empleados = N1_empleados/N_empleados
W2_empleados = N2_empleados/N_empleados
W3_empleados = N3_empleados/N_empleados

suma_vars_empleados = W1_empleados*sqrt(var(mas_empleados_1$ingnorm)) + W2_empleados*sqrt(var(mas_empleados_2$ingnorm)) + W3_empleados*sqrt(var(mas_empleados_3$ingnorm))
n1_ingresos_neyman = round(n_empleados*W1_empleados*sqrt(var(mas_empleados_1$ingnorm))/suma_vars_empleados)
n2_ingresos_neyman = round(n_empleados*W2_empleados*sqrt(var(mas_empleados_2$ingnorm))/suma_vars_empleados)
n3_ingresos_neyman = round(n_empleados*W3_empleados*sqrt(var(mas_empleados_3$ingnorm))/suma_vars_empleados)

c(n1_ingresos_neyman,n2_ingresos_neyman,n3_ingresos_neyman)


### Cálculo de afijación de Neyman para estimación de la tasa de paro ###

mas_pob_activa_1 = mas_pob_activa[which(mas_pob_activa$neduc == 1),]
mas_pob_activa_2 = mas_pob_activa[which(mas_pob_activa$neduc == 2),]
mas_pob_activa_3 = mas_pob_activa[which(mas_pob_activa$neduc == 3),]

N1_pob_activa = length(which(datos$neduc == 1))
N2_pob_activa = length(which(datos$neduc == 2))
N3_pob_activa = length(which(datos$neduc == 3))

W1_pob_activa = N1_pob_activa/N_pob_activa
W2_pob_activa = N2_pob_activa/N_pob_activa
W3_pob_activa = N3_pob_activa/N_pob_activa

suma_vars_pob_activa = W1_pob_activa*sqrt(var(mas_pob_activa_1$sitemp)) + W2_pob_activa*sqrt(var(mas_pob_activa_2$sitemp)) + W3_pob_activa*sqrt(var(mas_pob_activa_3$sitemp))
n1_tasa_paro_neyman = round(n_pob_activa*W1_pob_activa*sqrt(var(mas_pob_activa_1$sitemp))/suma_vars_pob_activa)
n2_tasa_paro_neyman = round(n_pob_activa*W2_pob_activa*sqrt(var(mas_pob_activa_2$sitemp))/suma_vars_pob_activa)
n3_tasa_paro_neyman = round(n_pob_activa*W3_pob_activa*sqrt(var(mas_pob_activa_3$sitemp))/suma_vars_pob_activa)

c(n1_tasa_paro_neyman,n2_tasa_paro_neyman,n3_tasa_paro_neyman)


### Estimación de los ingresos medios a través del estimador de muestreo estratificado ###

n1_empleados = length(which(mas_empleados$neduc == 1))
n2_empleados = length(which(mas_empleados$neduc == 2))
n3_empleados = length(which(mas_empleados$neduc == 3))

if (!require(sae)) install.packages("sampling")
library("sampling")

datos = datos[order(datos$neduc),]
mae_empleados_estratos = strata(datos[which(datos$sitemp == 0),], "neduc", size = c(n1_empleados,n2_empleados,n3_empleados), method = "srswor")
datos$neduc = as.factor(datos$neduc)
muestra_empleados_estratos = getdata(datos, mae_empleados_estratos)
muestra_empleados_estratos$tamaño = with(muestra_empleados_estratos, ifelse(neduc == "1", N1_empleados, ifelse(neduc == "2", N2_empleados, N3_empleados)))
muestra_empleados_estratos$pesos = 1/muestra_empleados_estratos$Prob

mas_empleados = mas_empleados[order(mas_empleados$neduc),]
muestra_empleados_estratos[,1:10] = mas_empleados[,c(1:6,8:10,7)]

diseño_empleados_estratos = svydesign(id = ~1, weights = ~pesos, fpc = ~tamaño, strat = ~neduc, data = muestra_empleados_estratos)
svymean(~ingnorm, diseño_empleados_estratos, na.rm = TRUE)
confint(svymean(~ingnorm, diseño_empleados_estratos, na.rm = TRUE))

ing_medios_estratos = svyby(~ingnorm, ~neduc, diseño_empleados_estratos, svymean)
ing_medios_estratos

IC_ing_medios_estratos = matrix(c(1:3,ing_medios_estratos[,2]-qnorm(0.025,lower.tail=FALSE)*ing_medios_estratos[,3],ing_medios_estratos[,2]+qnorm(0.025,lower.tail=FALSE)*ing_medios_estratos[,3]),ncol=3)
colnames(IC_ing_medios_estratos) = c("Nivel educativo", "IC_infimo", "IC_supremo")
IC_ing_medios_estratos 


### Estimación de la tasa de paro a través del estimador de muestreo estratificado ###

n1_pob_activa = length(which(mas_pob_activa$neduc == 1))
n2_pob_activa = length(which(mas_pob_activa$neduc == 2))
n3_pob_activa = length(which(mas_pob_activa$neduc == 3))

mae_pob_activa_estratos = strata(datos[which(datos$sitemp == 0),], "neduc", size = c(n1_pob_activa,n2_pob_activa,n3_pob_activa), method = "srswor")
datos$neduc = as.factor(datos$neduc)
muestra_pob_activa_estratos = getdata(datos, mae_pob_activa_estratos)
muestra_pob_activa_estratos$tamaño = with(muestra_pob_activa_estratos, ifelse(neduc == "1", N1_pob_activa, ifelse(neduc == "2", N2_pob_activa, N3_pob_activa)))
muestra_pob_activa_estratos$pesos = 1/muestra_pob_activa_estratos$Prob

mas_pob_activa = mas_pob_activa[order(mas_pob_activa$neduc),]
muestra_pob_activa_estratos[,1:10] = mas_pob_activa[,c(1:6,8:10,7)]

diseño_pob_activa_estratos = svydesign(id = ~1, weights = ~pesos, fpc = ~tamaño, strat = ~neduc, data = muestra_pob_activa_estratos)
svymean(~sitemp, diseño_pob_activa_estratos, na.rm = TRUE)
confint(svymean(~sitemp, diseño_pob_activa_estratos, na.rm = TRUE))

tasa_paro_estratos = svyby(~sitemp, ~neduc, diseño_pob_activa_estratos, svymean)
tasa_paro_estratos

IC_tasa_paro_estratos = matrix(c(1:3,tasa_paro_estratos[,2]-qnorm(0.025,lower.tail=FALSE)*tasa_paro_estratos[,3],tasa_paro_estratos[,2]+qnorm(0.025,lower.tail=FALSE)*tasa_paro_estratos[,3]),ncol=3)
colnames(IC_tasa_paro_estratos) = c("Nivel educativo", "IC_infimo", "IC_supremo")
IC_tasa_paro_estratos 


### Histogramas y gráficos de caja de ingresos y situación laboral ###

# Gráficas de ingresos de empleados 

svyhist(~ingnorm, diseño_empleados_estratos, main = "Histograma de ingresos de empleados", col = "pink")

library(ggplot2)

ggplot(data = muestra_empleados_estratos, aes(y = ingnorm, group = neduc, weight = pesos, fill = factor(neduc))) +
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() + labs(x = "Niveles educativos") + scale_fill_manual(values = c("red", "yellow", "violet")) + theme_bw()


# Gráficas de situación laboral de población activa (no adecuados para variables binarias)

svyhist(~sitemp, diseño_pob_activa_estratos, main = "Histograma de tasa de paro de la población activa", col = "pink")

ggplot(data = muestra_pob_activa_estratos, aes(y = sitemp, group = neduc, weight = pesos, fill = factor(neduc))) +
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() + labs(x = "Niveles educativos") + scale_fill_manual(values = c("red", "yellow", "violet")) + theme_bw()

