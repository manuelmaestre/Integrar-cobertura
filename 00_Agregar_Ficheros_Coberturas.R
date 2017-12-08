## Une todos los ficheros .txt del directorio de trabajo (ADSL, Remedies, AF)
## Genera ficheros de salida:
## 1.- Agregado directo de los ficheros de entrada
## 2.- Resumen por dirección y tecnología de acceso
## 3.- Resumen por municipio y tecnología de acceso


## Library load

library(readxl)
library(stringr)
library(data.table)

## Environment cleanning

rm(list = ls())

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


## Path and static variables definition

## Definicion de rutas y variables estáticas
in.files <- './00_in_data/'
out.files <- './01_out_data/'
aggregated.file <- str_c(out.files, '01_total_direcciones_cobertura.txt', sep = '', collapse = T)
FTTH.file <- str_c(out.files, '01_total_direcciones_FTTH.txt', sep = '', collapse = T)
address.tec.file <- str_c(out.files, '02_direccion_tecnologia.txt', sep = '', collapse = T)
city.tec.file <- str_c(out.files, '03_municipio_tecnologia.txt', sep = '', collapse = T)
fecha <- format(Sys.time(), "%Y%m%d")

#ficheros <-  list.files(in.files, full.names = T)

#He sacado la huella ADSL pq no incluye la OLT, hay que revisar el código que genera los datos ADSL para incluir la columna OLT (vacia)
#"C:/00_datos_usuario/01_projects/01_remedy/data/clean/huella_adsl.txt",

ficheros <- c("C:/00_datos_usuario/01_projects/01_remedy/data/clean/huella_remedies.txt",
              "C:/00_datos_usuario/01_projects/15_AF_Bitstream_OSP/data/clean/huella_acceso_fibra_OSP.txt",
              "C:/00_datos_usuario/01_projects/15_AF_Bitstream_OSP/data/clean/huella_acceso_fibra_VDF.txt",
              "C:/00_datos_usuario/01_projects/15_AF_Bitstream_OSP/data/clean/huella_acceso_fibra_JAZZ.txt",
              "C:/00_datos_usuario/01_projects/14_Mut_MM/00_Plan_contingencia_hogares_CTOs/01_Validacion_AIs/04_Inventory_DB/DWH/huella_mutualizada_MMB.txt",
              "C:/00_datos_usuario/01_projects/14_Mut_MM/00_Plan_contingencia_hogares_CTOs/01_Validacion_AIs/04_Inventory_DB/DWH/huella_mutualizada_OSP.txt",
              "C:/00_datos_usuario/01_projects/14_Mut_MM/00_Plan_contingencia_hogares_CTOs/01_Validacion_AIs/04_Inventory_DB/DWH/huella_StandAlone.txt")


ficheros <- ficheros[str_to_upper(str_sub(ficheros, start = str_length(ficheros)-2)) == "TXT"]


for (in.process.file in ficheros){
  
  print(in.process.file)
  if (.Platform$OS.type == "windows") flush.console()
  
  current.data <- data.table(read.csv(file = in.process.file,
                                      header = T,
                                      sep = ";",
                                      quote = "",
                                      dec = ",",
                                      colClasses = 'character',
                                      comment.char = "",
                                      encoding = 'UTF-8'))
  
  ## Renombrar columnas del data.table
  colnames(current.data) <- c("G18", "ID_TECNICO_DE_LA_VIA", "Provincia", "Poblacion", "Codigo.Postal", "Tipo.via", "Nombre.Via", "Numero", "BIS", "MIGA", "OLT", "UUII", "accesos", "tipo.huella", "fecha.dato")
  
  
  if (exists("total.data")){
    
    total.data <- (rbind(total.data, current.data))
    
  } else {
    
    total.data <- current.data
    
  }

  rm(current.data)
  
  }

total.data <- unique(total.data)

## Generamos fichero 1.- Agregado directo de los ficheros de entrada

write.table(data.frame(total.data),
            aggregated.file,
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')

## Generamos fichero 2.- Igual que el anterior pero sin ADSL. Para cruces con clientes y tráfico de fibra


write.table(data.frame(total.data[tipo.huella != 'ADSL',]),
            FTTH.file,
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')


## Fichero para compartir en OneDrive

write.table(data.frame(total.data[tipo.huella != 'ADSL',]),
            'C:/Users/manuel.maestre/OneDrive - Grupo Mas Movil/DWH/coberturaFTTH/01_total_direcciones_FTTH.txt',
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')







## Resumen general
total.data$UUII <- as.integer(total.data$UUII)
total.data$accesos <- as.integer(total.data$accesos)
total.data[,.(suma.UUII = sum(UUII), suma.accesos = sum(accesos)), by=.(tipo.huella)]
copy.table(total.data[,.(suma.UUII = sum(UUII), suma.accesos = sum(accesos)), by=.(tipo.huella)])

