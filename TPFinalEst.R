
library(ISLR)
library(plotmo)
library(glmnet)

TrainTest <- read.table("TrainTest.txt")

body <- read.csv("body.csv",header = FALSE)

#Le asignamos el nombre correspondiente a las columnas del data set
colnames(body)<- (c("BIAC","BIIL","BITRO","CHEST1","CHEST2","ELBOW","WRIST",
                    "KNEE","ANKLE","SHOUL","CHESTG","WAISTG","NAVEL","HIP","GLUTE","BICEP",
                    "FLOREA","KNEEG","CALF","ANKLEG","WRISTG","AGE","WEIG","HEIG","GEN"))

#Lectura de Datos -----------------------------------------------------------------------------------------------------------
 
#Nos fijamos si hay datos NULL
any(is.na(body))

#Nos Fijamos el tipo de dato de cada columna
sapply(body, class)

# Conclusion: No hay datos nulos y los tipos de dato corresponden a lo pedido

#Etapa Exploratoria -----------------------------------------------------------------------------------------------------------



#B.)

#Primero separamos los datos segun su genero

#Weight Men y su medianda
WEIG_M <-  body[body$GEN == 1, ]$WEIG

T_M <- median(WEIG_M)


#Weight Women y su mediana 
WEIG_W <-  body[body$GEN == 0, ]$WEIG

T_W <- median(WEIG_W)

#BootStrap para pesos de hombres
B = 1000

# Tomamos 1000 muestras de tamano |WEIGH_M| de los datos que tenemos 
n <- length(WEIG_M)
estMedMen<- 0

for(i in 1:B){
  #Para cada muestra estimamos la mediana y la almacenamos 
  muestra <- sample(WEIG_M, n, replace = TRUE )
  estMedMen[i] <- median(muestra)
}

seBootMen <- sd(estMedMen)

#Definimos el intervalo de confianza aproximado para la mediana del peso masculino
alfa <- 0.05

#Z_alfa/2
z <- qnorm(1-alfa/2)

IDC_Men <- c(T_M - z*seBootMen,T_M + z*seBootMen)

#BootStrap para pesos de Mujeres

# Tomamos 1000 muestras de tamano |WEIGH_M| de los datos que tenemos 
n <- length(WEIG_W)
estMedWom<- 0

for(i in 1:B){
  #Para cada muestra estimamos la mediana y la almacenamos 
  muestra <- sample(WEIG_W, n, replace = TRUE )
  estMedWom[i] <- median(muestra)
}

seBootWom <- sd(estMedWom)

#Definimos el intervalo de confianza aproximado para la mediana del peso masculino
alfa <- 0.05

#Z_alfa/2
z <- qnorm(1-alfa/2)

IDC_Wom <- c(T_M - z*seBootWom,T_M + z*seBootWom)

#Falta la interpretacion
#Se podria decir que el peso no es una medida muy significativa a la hora de diferencia a un hombre de una mujer, 
#ya que ambos tienen medias muy parecidas. Aunque los hombres tienen mas dispersion, por lo que podemos decir que
#cuanto mas alejado se encuentre un peso de la media mas probabilidad hay de que sea un hombre. Ademas a la hora de diferenciar
#grupos de hombres de grupos de mujeres, podemos usar el desvio estandar como diferenciador

#C)

HEIG_M <-  body[body$GEN == 1, ]$HEIG

HEIG_W <-  body[body$GEN == 0, ]$HEIG

plot(WEIG_M,HEIG_M,main = "Peso-Altura Hombres", xlab = "Peso" , ylab = "Altura")

plot(WEIG_W,HEIG_W,main = "Peso-Altura Mujeres", xlab = "Peso" , ylab = "Altura")

# Los graficos parecen indicar que a mayor peso se tendra mayor altura y vice-versa.

#Graficamos ambos en simultaneo
plot(WEIG_M,HEIG_M,main = "Peso-Altura Mixto", xlab = "Peso" , ylab = "Altura",col = "blue")
points(WEIG_W,HEIG_W,col= "pink")

# Ver si como diferenciar a los hombres de las mujeres

#D.)

# Grafico de los hombres
plot(WEIG_M,HEIG_M,main = "Peso-Altura Hombres", xlab = "Peso" , ylab = "Altura")
ksm <- ksmooth(WEIG_M,HEIG_M, "normal", bandwidth = 10)
lines(ksm$x, ksm$y, col = "red", lwd = 2)

# Grafico de las mujeres
plot(WEIG_W,HEIG_W,main = "Peso-Altura Mujeres", xlab = "Peso" , ylab = "Altura")
ksm <- ksmooth(WEIG_W,HEIG_W, "normal", bandwidth = 10)
lines(ksm$x, ksm$y, col = "red", lwd = 2)

# En el caso de los hombres, podemos ver que salvo el final la regresion no parametrica se parece mucho a una regresion lineal.
# Similarmente en las mujeres,la regresion no parametrica es parecida a una regresion lineal, sobre todo en la zona donde se concentran
# los datos. Por lo que sospechamos una regresion lineal para los hombres y las mujeres.

# E.)
# calculo la estimacion en Xi sin usar el i-esimo dato
mi_h = function(X,Y,i,h){
  n=length(X)
  Xi=X[i] #punto a evaluar la estimacion
  sum=0
  temp=0 
  for(j in 1:n){
    if(j!=i){
      sum=sum+(Y[j]*dnorm((X[j]-Xi)/h))
      temp=temp+dnorm((X[j]-Xi)/h)
    }
  }
  return(sum/temp)
}
# calculo el error cuadratico de la convalidacion cruzada
CV = function(X,Y,h){
  n=length(X)
  sum=0
  for(i in 1:n){
    sum=sum+(Y[i]-mi_h(X,Y,i,h))^2
  }
  return(sum/n)
}
# busco el h en hs que minimize el CV
h_opt= function(X,Y,hs){
  n=length(hs)
  h=hs[1]
  cv_h=CV(X,Y,h)
  for(i in 2:n){
    temp=CV(X,Y,hs[i])
    if(temp<cv_h){
      h=hs[i]
      cv_h=temp
    }
  }
  return(h)
}

hs= seq(5,20,by=0.5)

# No entiendo como usar la nota, y como quiere que grafique

# Para cada g´enero grafique la funci´on objetivo y represente all´ı la ventana ´optima hallada
# de acuerdo al criterio que est´a utilizando.
# Nota: Para verificar la respuesta, explorar la funci´on train de la librer´ıa caret.

# F.)

# Grafico de los hombres
h_M=h_opt(WEIG_M,HEIG_M,hs)
plot(WEIG_M,HEIG_M,main = "Peso-Altura Hombres, regresion no parametrica con ventana 5 y regresion lineal", xlab = "Peso" , ylab = "Altura")
# Reg. no parametrica con h optimo
ksm <- ksmooth(WEIG_M,HEIG_M, "normal", bandwidth = h_M)
lines(ksm$x, ksm$y, col = "red", lwd = 2)
# Regresion lineal de cuadrados minimos
regL_M <- lm(HEIG_M ~ WEIG_M)
abline(regL_M, col = "blue",lwd =2)

# Grafico de las mujeres
h_W=h_opt(WEIG_W,HEIG_W,hs)
plot(WEIG_W,HEIG_W,main = "Peso-Altura Mujeres, regresion no parametrica con ventana 5 y regresion lineal", xlab = "Peso" , ylab = "Altura")
ksm <- ksmooth(WEIG_W,HEIG_W, "normal", bandwidth = h_W)
lines(ksm$x, ksm$y, col = "red", lwd = 2)
# Regresion lineal de cuadrados minimos
regL_W <- lm(HEIG_W ~ WEIG_W)
abline(regL_W, col = "blue",lwd =2)

# Con esto podemos notar que una regresion lineal funciona notablemente, sobre todo en zonas
# con gran concentracion de datos.



# Regresion lineal -----------------------------------------------------------------------------------------------------------

# G.)

TrainTest= TrainTest$V1

#Datos de Training del modelo
bodyTrain = body[TrainTest,]
colnames(bodyTrain)<- (c("BIAC","BIIL","BITRO","CHEST1","CHEST2","ELBOW","WRIST",
                    "KNEE","ANKLE","SHOUL","CHESTG","WAISTG","NAVEL","HIP","GLUTE","BICEP",
                    "FLOREA","KNEEG","CALF","ANKLEG","WRISTG","AGE","WEIG","HEIG","GEN"))


#Datos de validacion del modelo
bodyTest = body[!TrainTest,]
colnames(bodyTest)<- (c("BIAC","BIIL","BITRO","CHEST1","CHEST2","ELBOW","WRIST",
                         "KNEE","ANKLE","SHOUL","CHESTG","WAISTG","NAVEL","HIP","GLUTE","BICEP",
                         "FLOREA","KNEEG","CALF","ANKLEG","WRISTG","AGE","WEIG","HEIG","GEN"))


# ajusto el modelo para predecir WIEG en funcion del resto

modelo=lm(WEIG ~ BIAC+BIIL+BITRO+CHEST1+CHEST2+ELBOW+WRIST+KNEE+ANKLE+SHOUL+CHESTG+WAISTG+NAVEL+HIP + GLUTE+BICEP+FLOREA+KNEEG+CALF+ANKLEG+WRISTG+AGE+HEIG+GEN, data = bodyTrain)

resumen = summary(modelo)

# Significacion de c/ coeficiente

# Como citerio para ver si una variable es significativa o no usaremos el test
# Beta_i = 0 vs.  Beta_i != 0 con alfa 0.05
# Es decir, si el p - valor del test de hipotesis anterior es mayor a 0.05, consideramos que la variable
# es significativa, pues podemos decir con confianza del 95% que la variable no se va a 0

# En que momento se usa esto?
significativo = c()

pValor = summary(modelo)$coefficients[, "Pr(>|t|)"]

#Vector booleano que nos dice si una variable es o no significativa
esSignificativa = c(pValor <= 0.05 )

#Nombres de las variables que usamos 
variables = names(body)

#Nos fijamos cuales son las variables que consideramos significativas
variablesSig = variables[esSignificativa]

print("Las variables significativas segun nuestro citerio son")

for(i in 1:length( variablesSig)){
  print(variablesSig[i])
  
}

# Analisis del F-Statistic

resumen

# EL F-statistic de nuestro modelo es un valor muy pequeno, esto esta en linea con el item anterior, pues lo que
# mide el F-statistic es el p-valor del test :
# H0: B_1 = B_2 =...=B_N = 0 vs. H1: existe B_i !=0

# Como tenemos muchos parametros, 11 de 25, para los cuales el p-valor del test H0: B_i= 0 vs. H1: B_i !=0 es muy bajo 
# (por ejemplo HEIG) tiene sentido que al hacer un test que toma como H0 que todos nuestros parametros son nulos, este tenga
# un p-vlaor muy bajo, cosa que efectivamente sucede.

# ¿Sospecha el efecto de alg´un fen´omeno? no entiendo a que se refiere

# Correlaciones lineales entre las variables explicativas

matriz_correlaciones = cor(bodyTrain)

heatmap(matriz_correlaciones)

# Como podemos ver hay una gran multicolinealidad entre las variables, esto es un problema ya que 
# haria que los errores estandar de las estimaicones sea mayor.\

# Calculamos el error del modelo
predicciones = predict(modelo, bodyTest)

# Dado que separamos datos de entrenamiento y datos de prueba, lo logico es calcular 
# el error a partir de las predicciones de bodyTest para evitar sobreestimaciones

error = sum((bodyTest$WEIG - predicciones)^2)/length(bodyTest)

# H.)

# Para comezar a eliminar variables, podriamos empezar por ver cuales tienen una correlacion muy alta.
# Visualmente esto lo podemos apreciar via heatmap, sin embargo resulta util definir un criterio numerico 
# para agrupar variables segun corelacion( i.e. por distacia a 1 o -1). De esta manera obtendremos una
# particiones del grupo de covariables y tomaremos una variable de cada grupo de esta particion, para seleccionar
# la variable de cada particion, tomaremos la de mayor signifciacion. 



# I.) 

# ni idea sobre LASSO

# J.)

# K.)