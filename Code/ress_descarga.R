# Descarga de base de datos del Registro de Empleo en la Seguridad Social
# El Quantificador 2023

# Preliminares --------------------------------------------------------------------------------------------

# Cargar librerias

if(!require(googledrive)) install.packages("googledrive")


# Descarga de datos ------------------------------------------------------------------------------------------

# Creamos un directorio temporal

td <- tempdir()

# Creamos un archivo temporal

tf <- tempfile(tmpdir=td, 
               fileext = ".zip")

# Link y descarga de la base de datos provisionales

id<-"1PhkwxWjUSp4DzC4wj58BJxwMlhmUBqf8"

drive_deauth() # Hacer login con una cuenta de google. Si no sirve, correr drive_auth()

drive_user() 

public_file <- drive_get(as_id(id))

drive_download(public_file,
               path = tf,
               overwrite = TRUE)

# Descompresion del archivo zip en el directorio temporal

unzip(tf, exdir = td, overwrite = T)

# Link y descarga de la base de datos semiprovisionales

id2<-"1gCyrh2vKKOdSOmktPgXnSQ5fvFJlU4Wb"

public_file2 <- drive_get(as_id(id2))

drive_download(public_file2,
               path = tf,
               overwrite = TRUE)

# Descompresion del archivo zip en el directorio temporal

unzip(tf, exdir = td, overwrite = T)

# Lectura de datos ----------------------------------------------------------------------------------------

# Lista de todos los archivos del 2022 al 23

list_csv_files <-
  list.files(path = td,
             full.names = T,
             pattern = '*.csv')[13:27]

ress_raw <-
  readr::read_csv(list_csv_files,
                  id = 'file_name')
