library(stringr)
library(data.table)
library(tidyr)
#library(RODBC)

rm(list = ls())

ruta.raw <- '../../data/raw/'
ruta.clean <- '../../data/clean/'
nombre.fichero.huella <- 'huella_remedies.txt'
zip.actual <- 'BITSTREAM_OSP.zip'
#txt.actual <- 'MM-FTTH-CO.txt'
zip.previo <- 'PREVIO_BITSTREAM_OSP.zip'
#txt.previo <- 'MM-FTTH-CO.txt'
zip.bloqueados <- 'BITSTREAM_OSP.zip'
#txt.bloqueados <- 'MM-FTTH-BL.txt'

## Fichero exportar reporting cobertura global
fichero.report.huella <- str_c(ruta.clean, nombre.fichero.huella)


# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

### listamos los ficheros del zip y seleccionamos los que coincidan con las máscaras

lista.into.zip <- unzip(str_c(ruta.raw, zip.actual), list = T)
txt.actual <- lista.into.zip[str_sub(lista.into.zip$Name,1,10) == 'MM-FTTH-CO', c("Name")]
txt.bloqueados <- lista.into.zip[str_sub(lista.into.zip$Name,1,10) == 'MM-FTTH-BL', c("Name")]

lista.into.zip <- unzip(str_c(ruta.raw, zip.previo), list = T)
txt.previo <- lista.into.zip[str_sub(lista.into.zip$Name,1,10) == 'MM-FTTH-CO', c("Name")]



### Cargamos el fichero previo de Remedies y el nuevo y el de bloqueos nuevo


actual <- data.table(read.table(unz(str_c(ruta.raw, zip.actual), txt.actual),
                                header = F,sep = ";", comment.char = "", 
                                quote = "", colClasses = "character", strip.white = T, fileEncoding = "UTF-8", encoding = "UTF-8"))
previo <- data.table(read.table(unz(str_c(ruta.raw, zip.previo), txt.previo),
                                header = F,sep = ";", comment.char = "",
                                quote = "", colClasses = "character", strip.white = T, fileEncoding = "UTF-8", encoding = "UTF-8"))
bloqueados <- data.table(read.table(unz(str_c(ruta.raw, zip.actual), txt.bloqueados),
                                header = F,sep = ";", comment.char = "", 
                                quote = "", colClasses = "character", strip.white = T, fileEncoding = "UTF-8", encoding = "UTF-8"))

encabezados <- c("GESCAL37",
                 "Codigo.Postal",
                 "Provincia",
                 "Poblacion",
                 "Tipo.via",
                 "Nombre.Via",
                 "ID_TECNICO_DE_LA_VIA",
                 "Numero",
                 "BIS",
                 "Bloque_finca",
                 "Portal_puerta",
                 "Letra",
                 "Escalera",
                 "Planta",
                 "Mano1",
                 "Mano2",
                 "Obsservaciones.comentario",
                 "Flag.dummy",
                 "Cod.INE.Via",
                 "Codigo.Censal",
                 "Codigo.Pai",
                 "Codigo.OLT",
                 "Codigo.CTO",
                 "Tipo.de.CTO",
                 "Direccion.Ubicacion.CTO",
                 "Tipo.de.permiso",
                 "Tipo.caja.de.derivacion",
                 "N.unidades.inmobiliarias",
                 "N.Viviendas",
                 "Fecha.de.alta")

names(actual) <- encabezados
names(bloqueados) <- encabezados
names(previo) <- encabezados

## write.table(btsregulada[Flag.dummy == 0, .N,  by=.(Provincia, Poblacion)], "clipboard", row.names = F, sep = ';')

write.table(data.frame(actual[, .N, by = .(Provincia, Poblacion, Tipo.via, Nombre.Via, Numero, BIS)]),
            '../../data/raw/direcciones.txt',
            row.names = F,
            quote = F,
            sep = ';')

write.table(data.frame(actual),
          '../../data/raw/MM-FTTH-CO.txt',
          row.names = F,
          quote = F,
          sep = ';',
          fileEncoding = 'UTF-8')


direcciones.remedy <- dcast(actual, ID_TECNICO_DE_LA_VIA+Provincia+Poblacion+Codigo.Postal+Tipo.via+Nombre.Via+Numero+BIS+Codigo.Pai+Codigo.OLT~Flag.dummy)
direcciones.remedy[, G18 := str_c(direcciones.remedy$ID_TECNICO_DE_LA_VIA, direcciones.remedy$Numero, direcciones.remedy$BIS)]
direcciones.remedy$Codigo.Pai <- str_sub(direcciones.remedy$Codigo.Pai,2, nchar(direcciones.remedy$Codigo.Pai))
setnames(direcciones.remedy, c("0", "1", "Codigo.Pai"), c("UUII", "accesos", "MIGA"))
setcolorder(direcciones.remedy, c("G18", "ID_TECNICO_DE_LA_VIA", "Provincia", "Poblacion","Codigo.Postal", "Tipo.via", "Nombre.Via", "Numero", "BIS","MIGA","Codigo.OLT", "UUII", "accesos"))
direcciones.remedy[, 'tipo.huella':= 'Remedies']
#AAMMDD <- file.info(str_c(ruta.raw, zip.actual))$mtime
#AAMMDD <- gsub('-', '', AAMMDD)
AAMMDD <- substr(txt.actual, 14, 19)
direcciones.remedy[, 'fecha.dato':= AAMMDD]

write.table(data.frame(direcciones.remedy),
            fichero.report.huella,
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')


rm(direcciones.remedy)


## Renombramos la columna ID_DOMICILIO.TO a GESCAL37

#names(actual)[names(actual)=="ID_DOMICILIO.TO"] <- "GESCAL37"
#names(previo)[names(previo)=="ID_DOMICILIO.TO"] <- "GESCAL37"
#names(bloqueados)[names(bloqueados)=="ID_DOMICILIO.TO"] <- "GESCAL37"

## Exportamos a un txt (luego importar a Excel) para enviar a Residencial, solo columnas de interes
write.csv(data.frame(actual[, .(G37 = str_sub(actual$GESCAL37, 2),Codigo.Postal,Provincia,Poblacion,Tipo.via,Nombre.Via,Numero,BIS,Bloque_finca,Portal_puerta,Letra,Escalera,Planta,Mano1,Mano2,Obsservaciones.comentario,Flag.dummy,Codigo.Censal, Fecha.de.alta)]),
          '../../data/clean/remedies.csv',
          row.names = F,
          quote = F)

names(actual)[names(actual)=="Flag.dummy"] <- "Acceso"
names(previo)[names(previo)=="Flag.dummy"] <- "Acceso"
names(bloqueados)[names(bloqueados)=="Flag.dummy"] <- "Acceso"

## Comprobamos las longitudes de los G37 de ambas tablas antes de los cruces

tmp <- actual[, list(cantidad=length(GESCAL37)), by = str_length(GESCAL37)]
tmp2 <- previo[, list(cantidad=length(GESCAL37)), by = str_length(GESCAL37)]

##Seleccionamos las columnas a conservar
##cols.interes <- list(GESCAL37,Codigo.Postal,Provincia,Poblacion,Acceso, Codigo.Pai)

actual <- actual[, .(GESCAL37,Provincia,Poblacion,Acceso,Codigo.Pai)]
previo <- previo[, .(GESCAL37,Provincia,Poblacion,Acceso,Codigo.Pai)]
names(bloqueados)[names(bloqueados)=="Prov"] <- "Provincia"
names(bloqueados)[names(bloqueados)=="codigo.pai"] <- "Codigo.Pai"
bloqueados <- bloqueados[, .(GESCAL37,Provincia,Poblacion,Acceso,Codigo.Pai)]

actual$origen <- "C"
previo$origen <- "P"

#Quitamos la J del codigo Pai
actual$Codigo.Pai <- str_sub(actual$Codigo.Pai, 2)
previo$Codigo.Pai <- str_sub(previo$Codigo.Pai, 2)
bloqueados$Codigo.Pai <- str_sub(bloqueados$Codigo.Pai, 2)

totales <- data.table(merge(actual, previo, all.x = T, all.y = T, by= "GESCAL37", suffixes = c(".actuales", ".previos")))

#sustituimos los NA que se hayan generado en el full join
totales$origen.previos[is.na(totales$origen.previos)] <- "noprevio"
totales$origen.actuales[is.na(totales$origen.actuales)] <- "noactual"

totales[, list(cuenta=length(GESCAL37)), by = c("origen.previos")]
totales[, list(cuenta=length(GESCAL37)), by = c("origen.actuales")]

totales <- unite_(totales, "origen.total", c("origen.actuales", "origen.previos"),remove = F)

table(totales$origen.total)

## Analizamos los que estaban en el listado anterior y no están en el actual

no.actual <- totales[origen.total == "noactual_P",]

## Eliminar columnas irrelevantes

no.actual[,c("Provincia.actuales", "Poblacion.actuales", "Acceso.actuales", "Codigo.Pai.actuales", "origen.actuales", "origen.previos") := NULL]
## Renombramos las columnas para eliminar sufijos
colnames(no.actual) <- c("GESCAL37", "origen.total", "Provincia", "Poblacion", "Acceso", "Codigo.Pai")

no.actual <- merge(no.actual, bloqueados[,.(GESCAL37,Codigo.Pai)], by.x = "GESCAL37", by.y = "GESCAL37", all.x = T)
no.actual[, c("Codigo.Pai.y") := ifelse(is.na(no.actual$Codigo.Pai.y), "baja_cambiodir", "bloqueado")]
colnames(no.actual) <- c("GESCAL37", "origen.total", "Provincia", "Poblacion", "Acceso", "Codigo.Pai", "Movimiento")

table(no.actual$Movimiento)


## Comparamos el previo con el actual a nivel de UUII para regularizar facturacion

resumen.actual <- totales[origen.actuales != "noactual", list(cuenta=length(GESCAL37)), by = c("Codigo.Pai.actuales", "Provincia.actuales", "Poblacion.actuales", "Acceso.actuales")]
resumen.actual <- dcast(resumen.actual, Codigo.Pai.actuales+Provincia.actuales+Poblacion.actuales ~ Acceso.actuales, fun.aggregate = sum, value.var = "cuenta")

resumen.anterior <- totales[origen.previos != "noprevio", list(cuenta=length(GESCAL37)), by = c("Codigo.Pai.previos","Provincia.previos", "Poblacion.previos", "Acceso.previos")]
resumen.anterior <- dcast(resumen.anterior, Codigo.Pai.previos+Provincia.previos+Poblacion.previos ~ Acceso.previos, fun.aggregate = sum, value.var = "cuenta")

# Insertamos id de busqueda únicos
#resumen.actual <- unite_(resumen.actual, "id.busqueda", c("Codigo.Pai.actuales", "Poblacion.actuales"), remove = F)
#resumen.anterior <- unite_(resumen.anterior, "id.busqueda", c("Codigo.Pai.previos", "Poblacion.previos"), remove = F)

# Renombrar columnas de los resumenes

# obtenemos la cadena de nombres de columna para copiar y pegar desde la consola y modificar

colnames(resumen.actual) <- c("MIGA","Provincia","Poblacion","UUII","Accesos")
colnames(resumen.anterior) <- c("MIGA","Provincia","Poblacion","UUII","Accesos")

resumen.total <- merge(resumen.actual, resumen.anterior, all.x = "T", 
                       by.x = c("MIGA", "Poblacion", "Provincia"), 
                       by.y = c("MIGA", "Poblacion", "Provincia"),
                       suffixes = c(".actual", ".previo"))
resumen.total$UUII.incremental <- resumen.total$UUII.actual - ifelse(is.na(resumen.total$UUII.previo), 0, resumen.total$UUII.previo)
resumen.total$Accesos.incremental <- resumen.total$Accesos.actual - ifelse(is.na(resumen.total$Accesos.previo), 0, resumen.total$Accesos.previo)

# Agregamos el nombre de la cabecera
cabeceras <- data.table(MIGA =c("0810001","0810034","0810043","2810003","2810037","2810041","2812001","2910002","2910004","4110005","4110019","4110021","4610012"),
                        Cabecera=c("BARCELONA/ARENES","BADALONA/VENTOS","HOSPITALET DE LLOBREGAT","DELICIAS","PILAR","MANOTERAS","ALCORCON/S.J.VALDERAS","MALAGA/MALDONADO","MALAGA/SOL","SEVILLA/TRIANA","SEVILLA/PINO MONTANO","SEVILLA/AEROPUERTO","VALENCIA/SAN VICENTE"))

resumen.total <- merge(resumen.total, cabeceras, all.x = T, by.x = "MIGA", by.y = "MIGA")
resumen.total <- resumen.total[,.(MIGA,Cabecera,Poblacion,Provincia,UUII.actual,Accesos.actual,UUII.previo,Accesos.previo,UUII.incremental,Accesos.incremental)]

resumen.cabecera <- resumen.total[,list(
  UUII.actual = sum(UUII.actual, na.rm = T),
  Accesos.actual = sum(Accesos.actual, na.rm = T),
  UUII.previo = sum(UUII.previo, na.rm = T),
  Accesos.previo = sum(Accesos.previo, na.rm = T),
  UUII.incremental = sum(UUII.incremental, na.rm = T),
  Accesos.incremental = sum(Accesos.incremental, na.rm = T)
  ), by = c("Provincia","MIGA", "Cabecera")]

resumen.municipio <- resumen.total[,list(
  UUII.actual = sum(UUII.actual, na.rm = T),
  Accesos.actual = sum(Accesos.actual, na.rm = T),
  UUII.previo = sum(UUII.previo, na.rm = T),
  Accesos.previo = sum(Accesos.previo, na.rm = T),
  UUII.incremental = sum(UUII.incremental, na.rm = T),
  Accesos.incremental = sum(Accesos.incremental, na.rm = T)
), by = c("Provincia", "Poblacion")]

#Agreamos fila de totales
totales  <- resumen.total[,list(
  UUII.actual = sum(UUII.actual, na.rm = T),
  Accesos.actual = sum(Accesos.actual, na.rm = T),
  UUII.previo = sum(UUII.previo, na.rm = T),
  Accesos.previo = sum(Accesos.previo, na.rm = T),
  UUII.incremental = sum(UUII.incremental, na.rm = T),
  Accesos.incremental = sum(Accesos.incremental, na.rm = T)
),]

# Al resumen x cabecera
totales.cabecera <- totales
totales.cabecera$Provincia <- "Total"
totales.cabecera$MIGA <- ""
totales.cabecera$Cabecera <- ""
resumen.cabecera <- rbind(resumen.cabecera, totales.cabecera)

# Al resumen x municipio
totales.municipio <- totales
totales.municipio$Provincia <- "Total"
totales.municipio$Poblacion <- ""
resumen.municipio <- rbind(resumen.municipio, totales.municipio)

# Al resumen total
totales.total <- totales
totales.total$MIGA <- "Total"
totales.total$Cabecera <- ""
totales.total$Poblacion <- ""
totales.total$Provincia <- ""

resumen.total <- rbind(resumen.total, totales.total)


write.table(resumen.total, file = '../../data/clean/resumen_total.csv',
          sep=",",
          na = "",
          dec=",",
          row.names = F)
write.table(resumen.cabecera, file = '../../data/clean/resumen_cabecera.csv',
          sep=",",
          na = "",
          dec=",",
          row.names = F)
write.table(resumen.municipio, file = '../../data/clean/resumen_municipio.csv',
            sep=",",
            na = "",
            dec=",",
            row.names = F)
write.table(resumen.actual, file = '../../data/clean/para_historico.csv',
            sep=",",
            na = "",
            dec=",",
            row.names = F)

## Resumen UUII y accesos
resumen.actual[,.(total.UUII = sum(UUII), total.Accesos = sum(Accesos))]


