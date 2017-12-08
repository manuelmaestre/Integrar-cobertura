ECHO Actualizar Remedies
rem @ECHO OFF
CD C:\00_datos_usuario\01_projects\01_remedy\code\processed
rscript --vanilla comparativa_anterior_actual.R

ECHO Actualizar Acceso Fibra
rem @ECHO OFF
CD C:\00_datos_usuario\01_projects\15_AF_Bitstream_OSP
rscript --vanilla 01_Cargar_huella_AFJAZZ_Exportar_agrupado_plantilla.R
rscript --vanilla 02_Cargar_huella_AFOSP_Exportar_agrupado_plantilla.R
rscript --vanilla 03_Cargar_huella_AFVDF_Exportar_agrupado_plantilla.R

ECHO Actualizar DWH
rem @ECHO OFF
CD C:\00_datos_usuario\01_projects\000_DWH_txt_files\00_Coberturas
rscript --vanilla 00_Agregar_Ficheros_Coberturas.R

ECHO Actualizar QlikView Coberturas
"c:\Program Files\QlikView\QV.exe" /l C:\00_datos_usuario\01_projects\000_DWH_txt_files\00_Coberturas\Cobertura.qvw

@echo off
echo .................................
echo .................................
echo Pulse una tecla para cerrar la ventana de comandos
pause>nul