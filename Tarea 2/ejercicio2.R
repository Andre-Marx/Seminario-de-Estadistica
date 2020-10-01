### SEMINARIO DE ESTADISTICA
## PROFESOR: JIMMY HERNANDEZ MORALES
## AYUNDATE: MIGUEL HINOJOSA MEDRANO

## INTEGRANTES DEL EQUIPO:
## cANALES OCAMPO LUIS GERARDO
## PUENTE AREVALO ANDRE MARX
## NAJERA NAJERA ALFREDO
## ORTIZ REYES ISMAEL

##### EJERCICIO 2

## PRUEBAS PARA LA NORMAL MULTIVARIADA

    ## LIBRERIAS

       library("MVN")

    ## BASES A UTILIZAR
       
       iris
       quiero<-c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width")
       
       ##VERSICOLOR
       a<-iris[iris$Species=="versicolor",]
       a<-a[,names(a) %in% quiero]
       
       ##VIRGINICA
       b<-iris[iris$Species=="virginica",]
       b<-b[,names(b) %in% quiero]
       
       ##CETOSA
       c<-iris[iris$Species=="setosa",]
       c<-c[,names(c) %in% quiero]
       
       ##PRUEBAS PARA PROBAR NORMAL MULTIVARIADA
       
       ##ROYSTON
       a.royston <- mvn(data =a, mvnTest = "royston", multivariatePlot = "qq")
       b.royston <- mvn(data =b, mvnTest = "royston", multivariatePlot = "qq")
       c.royston <- mvn(data =c, mvnTest = "royston", multivariatePlot = "qq")
       
       a.royston
       ##CON ESTA PRUEBA PODEMOS CONCLUIR QUE LOS DATOS DE LA VERSICOLOR SI SE AJUSTAN A UNA NORMAL MULTIVARIADA
       b.royston
       ##CON ESTA PRUEBA PODEMOS CONCLUIR QUE LOS DATOS DE LA VIRGINICA SI SE AJUSTAN A UNA NORMAL MULTIVARIADA
       c.royston
       ##POR PURA DUDA CHECAMOS SI LOS DATOS DE LA SETOSA SE AJUSTAN A UNA NORMAL MULTIVARIADA Y PODEMOS VER
       ##QUE SI SE AJUSTAN
       
       ##MARDIA
       a.mardia<- mvn(data =a, mvnTest = "mardia", multivariatePlot = "qq")
       b.mardia<- mvn(data =b, mvnTest = "mardia", multivariatePlot = "qq")
       c.mardia<- mvn(data =c, mvnTest = "mardia", multivariatePlot = "qq")
       
       a.mardia
       ##CON ESTA PRUEBA AL IGUAL QUE LA DE ROYSTON SE CONCLUYE QUE LOS DATOS DE LA VERSICOLOR SE AJUSTAN A
       ##UNA NORMAL MULTIVARIADA
       b.mardia
       ##CON ESTA PRUEBA IGUAL SE CONCLUYE QUE LOS DATOS DE LA VIRGINICA SI SE AJUSTAN A UNA NORMAL MULTIVARIADA
       c.mardia
       ##CON ESTA PRUEBA CONCLUIMOS QUE TAMBIEN LOS DATOS DE LA SETOSA SE AJUSTAN A UNA NORMAL MULTIVARIADA
       
       ##HZ
       a.hz<- mvn(data =a, mvnTest = "hz", multivariatePlot = "qq")
       b.hz<- mvn(data =b, mvnTest = "hz", multivariatePlot = "qq")
       c.hz<- mvn(data =c, mvnTest = "hz", multivariatePlot = "qq")

       a.hz       
       ##SE CONCLUYE CON LA PRUEBA HZ QUE LOS DATOS DE LA VERSICOLOR SI SE AJUSTAN A UNA NORMAL MULTIVARIADA
       b.hz
       ##SE CONCLUYE QUE LOS DATOS DE LA VIRGINICA SI SE AJUSTAN A UNA NORMAL MULTIVARIADA CON LA PRUEBA HZ  
       c.hz
       ##PODEMOS VER CON ESTA PRUEBA QUE LOS DATOS DE LA SETOSA SI SE AJUSTAN A UNA NORMAL MULTIVARIADA
       
       ##PRUEBAS DE HOMOCEDASTICIDAD
       
       ##VERSICOLOR
       bartlett.test(a)
       ##AQUI PODEMOS VER QUE NO HAY DIFERENCIAS SIGNIFICATIVAS EN LA VARIANZA DE ESTA ESPECIA RESPECTO A SUS
       ##COVARIABLES
       
       ##VIRGINICA
       bartlett.test(b)
       #PODEMOS VER QUE NO EXISTE DIFERENCIA SIGNIFICATIVA DE VARIANZAS CON ESTA ESPECIA
       
       ##SETOSA
       bartlett.test(c)
       ##CONCLUIMOS QUE NO HAY DIFERENCIA SIGNIFICATIVA EN LAS VARIANZAS CON ESTA ESPECIE
       
       ##VERSICOLOR VS VIRGINICA
       bartlett.test(a,b)
       ##PODEMOS CONCLUIR QUE NO HAY DIFERENCIAS SIGNIFICATIVAS ENTRE LAS VARIANZAS DE ESTOS 2 GRUPOS
       
       ##VERSICOLOR VS CETOSA
       bartlett.test(a,c)
       ##CON ESTO PODEMOS VER QUE TAMPOCO ENTRE ESTOS DOS GRUPOS NO HAY DIFERENCIA SIGNIFICATIVA DE
       ##SUS VARIANZAS
       
       ##VIRGINICA VS SETOSA
       bartlett.test(b,c)
       ##FINALMENTE CON ESTA PRUEBA TAMBIEN PODEMOS VER QUE ENTRE ESTOS DOS GRUPOS NO HAY DIFERENCIA
       ##SIGNIFICATIVA DE SUS VARIANZAS
       
       ##CASO ESPECIAL
       ##VERSICOLOR VS VIRGINICA VS SETOSA
       bartlett.test(a,b,c)
       ##CONCLUIMOS CON LOS 3 GRUPOS TOMANDO EN CUENTA TODAS LAS VARIABLES QUE NO HAY DIFERENCIA
       ##SIGNIFICATIVA ENTRE SUS VARIANZAS
       