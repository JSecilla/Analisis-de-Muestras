datos_CastillaLaMancha <- read.csv("C:/Users/jsm01/OneDrive/Escritorio/Tercero/Análisis de muestras/Trabajo/datos_CastillaLaMancha.txt", sep="")

tablafrecuencias <- function( x , names = c ( ) ) {
   M = matrix(NA, nrow = length(unique(x)), ncol = 5 )
   colnames (M) = c( "xi" ,"ni" ,"fi" ,"Ni" ,"Fi" )
   rownames(M) = names
   M[ , 1 ] = sort(unique(x))
   M[ , 2 ] = table(x)
   M[ , 3 ] = M[ , 2 ] /length( x )
 M[ , 4 ] = cumsum(M[ , 2 ] )
 M[ , 5 ] = cumsum(M[ , 3 ] )
   return(M)
}

tablafrecuencias(datos_CastillaLaMancha$prov, names = c("Albacete","Ciudad Real", "Cuenca", "Guadalajara","Toledo"))
table(datos_CastillaLaMancha$prov)


diagrama_barras <- function(x , names = c(), xlab = c(), ylab = c(), main = c()){
  
  A <- table(x)
  names(A) <- names
  barplot(A, xlab = xlab, ylab = ylab, main = main)
}

diagrama_barras(datos_CastillaLaMancha$prov, names = c("Albacete","Ciudad Real", "Cuenca", "Guadalajara","Toledo"), xlab = c("Provincia"), ylab = c("Frecuencias absolutas"), main = c("Número de muestras por provincia") )

grafico_mosaico <- function(x,y, names_x = c(), names_y = c(), xlab = c(), ylab = c(), main = c(), col = c()){
  A <- table(x,y)
  rownames(A) <- names_x
  colnames(A) <- names_y
  mosaicplot(A, xlab = xlab, ylab = ylab, main = main, col = col)
}

grafico_mosaico(datos_CastillaLaMancha$edad, datos_CastillaLaMancha$neduc, names_x = c("16-24","25-49","50-64",">65"),
                                                                           names_y = c("Primaria","Secundaria","Postsecundaria"),
                                                                           main = c("Relación entre nivel educativo y edad"),
                xlab = c("Rango de edades"),ylab = c("Nivel educativo"), col = c("darkgreen", "white", "darkgreen"))

tablas_contingencia <- function(x,y, names_x = c(), names_y = c()){
  T1 <- table(x,y)
  T1 <- cbind(T1, rowSums(T1))
  T1 <- rbind(T1, colSums(T1))
  rownames(T1) <- c(names_x, "Total")
  colnames(T1) <- c(names_y, "Total")
  T2 <- T1/T1[,4]
  T2[,4] <- T1[,4]/max(T1[,4])
  print(T1)
  return(T2)
}
tablas_contingencia(x = datos_CastillaLaMancha$edad, y = datos_CastillaLaMancha$neduc, names_x = c("16-24","25-49","50-64",">65"),
                    names_y = c("Primaria","Secundaria","Postsecundaria"))

ingresos <- datos_CastillaLaMancha[which(datos_CastillaLaMancha$sitemp ==1), 9]
ing_1 <- datos_CastillaLaMancha[which(datos_CastillaLaMancha$sitemp ==1 & datos_CastillaLaMancha$neduc == 1), 9]
ing_2 <- datos_CastillaLaMancha[which(datos_CastillaLaMancha$sitemp ==1 & datos_CastillaLaMancha$neduc == 2), 9]
ing_3 <- datos_CastillaLaMancha[which(datos_CastillaLaMancha$sitemp ==1 & datos_CastillaLaMancha$neduc == 3), 9]

intervalos_sturges <- function(x, cantidad = NULL){
  if(is.null(cantidad)==TRUE){
    cantidad <- ceiling(1 + log(length(x))/log(2))
  }
  longitud <- ceiling((max(x)-min(x))/cantidad)
  intervalos <- seq(from = floor(min(x)),
                    to = floor(min(x)) + longitud*cantidad ,
                    by = longitud)
  return(intervalos)
}
intervalos_sturges(ingresos)

qqnorm(ingresos, main = "Grafico Q-Q Normal - Ingresos", xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestrales")
qqline(ingresos)


#####
#####
#PARTE 2

"
Revisar libreria mase y usar ggplot
"