## Integra los nuevos ficheros que se encuentren en los directorios de entrada
## Compara con el log de ficheros ya integrados anteriormente para procesar sólo los adicionales
## Mueve los ficheros integrados OK y KO a los directorios de histórico KO y OK, a un subdirectorio con la fecha de proceso
## Crea los ficheros de salida con los datos integrados de AI y CTO con la fecha actual, y numero de fichero, para su posterior validación
## Crea/Modifica el nuevo log de ficheros integrados para ejecuciones posteriores

## Library load

library(readxl)
library(stringr)
library(data.table)

## Environment cleanning

rm(list = ls())

## Path and static variables definition

## Definicion de rutas y variables estáticas
in.files <- '../../data/raw/adsl/'
ruta.clean <- '../../data/clean/'
aggregated.file <- str_c(in.files, 'total_adsl.txt', sep = '', collapse = T)

fecha <- format(Sys.time(), "%Y%m%d")

ficheros <-  list.files(in.files, full.names = T)
ficheros <- ficheros[str_to_upper(str_sub(ficheros, start = str_length(ficheros)-3)) == "XLSX"]

## Fichero exportar reporting cobertura global
fichero.report.ADSL <- str_c(ruta.clean,'huella_ADSL.txt')


for (in.process.file in ficheros){
  
  print(in.process.file)
  if (.Platform$OS.type == "windows") flush.console()
  
  adsl.data <- data.table(read_excel(in.process.file, sheet = 1, col_names = F, skip = 1))
  
  if (exists("agregado.ADSL")){
    
    agregado.ADSL <- (rbind(agregado.ADSL, adsl.data))
    
  } else {
    
    agregado.ADSL <- adsl.data
    
  }

  rm(adsl.data)
  
  }

names(agregado.ADSL) <- c('Provincia', 'Poblacion', 'Tipo.via', 'Nombre.Via', 'Numero')

## Cargamos el callejero de Telefonica creado desde los iroXX para poblar el gescal de calle y G18

gescales.adsl <- data.table(read.table('../../data/raw/adsl/gescales_ADSL_R.txt',
                            header = T,
                            quote = "",
                            sep = ';', 
                            encoding = 'UTF-8', 
                            comment.char = "",
                            colClasses = 'character'))


agregado.ADSL <- merge(agregado.ADSL, 
      gescales.adsl, 
      all.x = T,
      by.x = c("Provincia","Poblacion", "Tipo.via", "Nombre.Via"),
      by.y = c("PROVINCIA_ADSL", "POBLACION_ADSL","tipo_calle_adsl", "nombre_calle_adsl"))



## Cargamos migas y cajas terminales creadas desde irolista para recuperar el MIGA

gescal.miga <- data.table(read.table('../../data/clean/irolista/direcciones_TESA_XY_desde_cajas.txt',
                                       header = T,
                                       sep = ',',
                                      na = "",
                                       encoding = 'UTF-8',
                                       comment.char = "",
                                       colClasses = 'character'))

gescal.miga$G18.CAJA <- str_trim(gescal.miga$G18.CAJA)
gescal.miga$CENTRAL <- str_trim(gescal.miga$CENTRAL)
gescal.miga <- gescal.miga[, .N, .(G18.CAJA, CENTRAL)]
gescal.miga[, ranking := rank(CENTRAL, ties = "random"), by = c("G18.CAJA")]
gescal.miga <- gescal.miga[ranking == 1,]
gescal.miga$ranking <- NULL
gescal.miga$N <- NULL


#gescal.miga <- (unique(gescal.miga))


agregado.ADSL$G18 <- str_trim(str_c(agregado.ADSL$GESCAL12, agregado.ADSL$Numero))
agregado.ADSL <- agregado.ADSL[, .N, .(G18, GESCAL12, CP, Provincia, Poblacion, Tipo.via, Nombre.Via, Numero)]
agregado.ADSL$N <- NULL
setcolorder(agregado.ADSL, c("G18", "GESCAL12", "CP", "Provincia", "Poblacion", "Tipo.via", "Nombre.Via", "Numero"))

#merge(agregado.ADSL, gescal.miga, all.x = T, by.x = "G18", by.y = "G18.CAJA")


## Agregamos las columnas que faltan para tener la misma estructura que Remedies y AF

agregado.ADSL[, ':=' (BIS = '', MIGA = '', UUII = '', accesos = '')]
setnames(agregado.ADSL, c("GESCAL12"), c("ID_TECNICO_DE_LA_VIA"))
agregado.ADSL$CP <- NULL
setcolorder(agregado.ADSL, c("G18", "ID_TECNICO_DE_LA_VIA", "Provincia", "Poblacion", "Tipo.via", "Nombre.Via", "Numero", "BIS", "MIGA","UUII", "accesos"))
agregado.ADSL[, 'tipo.huella':= 'ADSL']
AAMMDD <- '170130'
agregado.ADSL[, 'fecha.dato':= AAMMDD]

## Dejamos un único dato por G18, ya que hay duplicados a distintas centrales
#matriz_lev[, ranking := rank(distancia, ties = "random"), by = c("M1", "T1", "V1cruce")]

agregado.ADSL[, ranking := rank(MIGA, ties = "random"), by = c("Provincia", "Poblacion", "Tipo.via", "Nombre.Via", "Numero")]
agregado.ADSL <- agregado.ADSL[ranking ==1, ]
agregado.ADSL <- merge(agregado.ADSL, gescal.miga, all.x = T, by.x = "G18", by.y = "G18.CAJA")
agregado.ADSL$ranking <- NULL
agregado.ADSL$MIGA <- NULL
setnames(agregado.ADSL, c("CENTRAL"), c("MIGA"))
setcolorder(agregado.ADSL, c("G18", "ID_TECNICO_DE_LA_VIA", "Provincia", "Poblacion", "Tipo.via", "Nombre.Via", "Numero", "BIS", "MIGA","UUII", "accesos", "tipo.huella", "fecha.dato"))

View(agregado.ADSL[1:15,])
View(gescal.miga[1:15,])

write.table(data.frame(agregado.ADSL),
            fichero.report.ADSL,
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')


