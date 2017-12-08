library(stringr)
library(data.table)
library(tidyr)


rm(list = ls())


raw.zip <- 'C:/00_datos_usuario/01_projects/01_remedy/data/raw/BITSTREAM_OSP.zip'


## Fichero exportar reporting cobertura global
fichero.report.AF <- './data/clean/huella_acceso_fibra_VDF.txt'


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

## Identificamos el fichero de acceso fibra en el comprimido

lista.into.zip <- unzip(raw.zip, list = T)

accesofibra.file <- lista.into.zip[str_sub(lista.into.zip$Name,1,10) == 'BT_082_904', c("Name")]


### Cargamos el fichero 
acceso.fibra <- data.table(read.table(unz(raw.zip, accesofibra.file),
                                      header = F,sep = ";", comment.char = "",
                                      quote = "", colClasses = "character", strip.white = T,
                                      fileEncoding = "UTF-8", encoding = "UTF-8"))

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

names(acceso.fibra) <- encabezados

direcciones.AF <- dcast(acceso.fibra, ID_TECNICO_DE_LA_VIA+Provincia+Poblacion+Tipo.via+Nombre.Via+Numero+BIS+Codigo.Pai+Codigo.Postal+Codigo.OLT~Flag.dummy)
direcciones.AF <- direcciones.AF[(ID_TECNICO_DE_LA_VIA)!='',]

direcciones.AF[, G18 := str_c(direcciones.AF$ID_TECNICO_DE_LA_VIA, direcciones.AF$Numero, direcciones.AF$BIS)]
direcciones.AF$Codigo.Pai <- str_sub(direcciones.AF$Codigo.Pai,2, nchar(direcciones.AF$Codigo.Pai))


if(is.na(match("1", names(direcciones.AF)))){direcciones.AF[, '1':='']}

setnames(direcciones.AF, c("0", "1", "Codigo.Pai"), c("UUII", "accesos", "MIGA"))
setcolorder(direcciones.AF, c("G18", "ID_TECNICO_DE_LA_VIA", "Provincia", "Poblacion","Codigo.Postal", "Tipo.via", "Nombre.Via", "Numero", "BIS", "MIGA","Codigo.OLT", "UUII", "accesos"))
direcciones.AF[, 'tipo.huella':= 'AccesoFibra_VDF']
#AAMMDD <- file.info(accesofibra.file)$mtime
#AAMMDD <- gsub('-', '', AAMMDD)
AAMMDD <- substr(accesofibra.file, 12, 17)
direcciones.AF[, 'fecha.dato':= AAMMDD]
direcciones.AF <- data.table(direcciones.AF)

write.table(data.frame(direcciones.AF),
            fichero.report.AF,
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')

## Resumen general

direcciones.AF[,.(suma.UUII = sum(as.integer(UUII)), suma.accesos = sum(as.integer(accesos)))]

# ### analisis temporal
# 
# clientes <- data.table(paste.table())
# clientes$G37_cliente <- as.character(clientes$G37_cliente)
# summary(clientes)
# class(clientes$G37_cliente)
# clientes$G37_cliente <- str_pad(clientes$G37_cliente, 38, side = c("right"), pad = " ")
# acceso.fibra$GESCAL37 <- str_pad(acceso.fibra$GESCAL37, 38, side = c("right"), pad = " ")
# 
# View(acceso.fibra[1:20,])
# copy.table(merge(clientes, acceso.fibra, all.x = T, by.x="G37_cliente", by.y = "GESCAL37"))
# acceso.fibra[, .N, nchar(GESCAL37)]
# 
# acceso.fibra[GESCAL37 == "J28000010479500003 0022",]
