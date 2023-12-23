
library(ISLR)
library(plotmo)
library(glmnet)
library(caret)

TrainTest <- read.table("TrainTest.txt")

#We will be using the following data set provied to us by Dr. Ana Bianco
body <- read.csv("body.csv",header = FALSE)

#We assign the coresponding names to the columns
colnames(body)<- (c("BIAC","BIIL","BITRO","CHEST1","CHEST2","ELBOW","WRIST",
                    "KNEE","ANKLE","SHOUL","CHESTG","WAISTG","NAVEL","HIP","GLUTE","BICEP",
                    "FLOREA","KNEEG","CALF","ANKLEG","WRISTG","AGE","WEIG","HEIG","GEN"))

#We separate our initial training data
bodyTest = body[!TrainTest,]
bodyTrain = body[TrainTest,]

#-----------------------------------------------------------------------------------------------

#The function formula recivies a epsilon and returns the corresponding formula according to the established criteria 

formula <- function(e){
  
  edge <- function(matriz, cluster,i){
    res = FALSE
    for (j in cluster){
      if(abs(matriz[i,j])>=e){
        return(TRUE)
      }
    }
    return(res)
  } 
  clusters = function(data, coef){ # le paso la informacion y el coeficiente de correlacion soportado,
    # o sea si la correlacion es mayor a coef pueden llegar a estar en el mismo cluster
    
    matriz = cor(data) # matriz de covarianzas
    n = length(colnames(data))
    res = list() # lista de clusters
    usados = c() # voy guardando que varibles ya tienen un cluster asignado
    
    for (i in 1:n){
      if(!(i %in% usados)){ # si la i-esima variable no tiene un cluster creo uno con solo esta
        cluster = c(i)
        usados = append(usados,i)
        for(j in 1:n){ # voy agregando variables al cluster, que no use y son adecuadas para este
          if(!(j %in% usados) && edge(matriz,cluster,j)){
            cluster = append(cluster,j)
            usados = append(usados,j)
          }
        }
        res = append(res, list(cluster))
      }
    }
    return(res)
    
    
  }
  
  # Armamos los grupos con los datos de entrenamiento y un coeficiente de covarianza mayor a 0.7 de dos a dos entre los grupos
  # Recordamos sacar WEIG de bodyTrain
  grupos= clusters(subset(bodyTrain,select=-WEIG),0.8)
  
  # We will select the most significant covariable in each connected component of the resulting graph
  variables_filtradas=c()
  
  for(i in 1:length(grupos)){
    cluster=grupos[[i]]
    temp=cluster[1]
    for(j in cluster){
      if(pValor[j+1]>pValor[temp+1]){ # sumamos uno ya que estan corridos los valores al tener el intercept
        temp=j
      }
    }
    
    variables_filtradas=append(variables_filtradas,temp)
  }
  
  variables_filtradas=colnames(subset(bodyTrain,select=-WEIG))[variables_filtradas]
  print(variables_filtradas)
  
  # We modify the formula of our linear regresion, using only the covariables that have "survived"
  formula = as.formula(paste("WEIG ~", paste(variables_filtradas, collapse = "+")))
  
  return(formula)
  
}

#-----------------------------------------------------------------------------------------------
va_aca = function(matriz, coef, cluster,i){
  res = FALSE
  for (j in cluster){
    if(abs(matriz[i,j])>=coef){
      return(TRUE)
    }
  }
  return(res)
}

clusters = function(data, coef){ # le paso la informacion y el coeficiente de correlacion soportado,
  # o sea si la correlacion es mayor a coef pueden llegar a estar en el mismo cluster
  
  matriz = cor(data) # matriz de covarianzas
  n = length(colnames(data))
  res = list() # lista de clusters
  usados = c() # voy guardando que varibles ya tienen un cluster asignado
  
  for (i in 1:n){
    if(!(i %in% usados)){ # si la i-esima variable no tiene un cluster creo uno con solo esta
      cluster = c(i)
      usados = append(usados,i)
      for(j in 1:n){ # voy agregando variables al cluster, que no use y son adecuadas para este
        if(!(j %in% usados) && va_aca(matriz,coef,cluster,j)){
          cluster = append(cluster,j)
          usados = append(usados,j)
        }
      }
      res = append(res, list(cluster))
    }
  }
  return(res)
}

# Armamos los grupos con los datos de entrenamiento y un coeficiente de covarianza mayor a 0.7 de dos a dos entre los grupos
# Recordamos sacar WEIG de bodyTrain
grupos= clusters(subset(bodyTrain,select=-WEIG),0.8)

# We will select the most significant covariable in each connected component of the resulting graph
variables_filtradas=c()

for(i in 1:length(grupos)){
  cluster=grupos[[i]]
  temp=cluster[1]
  for(j in cluster){
    if(pValor[j+1]>pValor[temp+1]){ # sumamos uno ya que estan corridos los valores al tener el intercept
      temp=j
    }
  }
  
  variables_filtradas=append(variables_filtradas,temp)
}

variables_filtradas=colnames(subset(bodyTrain,select=-WEIG))[variables_filtradas]
print(variables_filtradas)

# We modify the formula of our linear regresion, using only the covariables that have "survived"
formula = as.formula(paste("WEIG ~", paste(variables_filtradas, collapse = "+")))

#We now use our training and testing data to aproxiamate an error 
bodyTrain_filtrado = subset(bodyTrain,select=append(variables_filtradas,"WEIG"))

bodyTest_filtrado = subset(bodyTest,select=append(variables_filtradas,"WEIG"))

modelo_filtrado = lm(formula,data=bodyTrain_filtrado)

resumen_filtrado = summary(modelo_filtrado)

predicion_filtrado <- predict(modelo_filtrado, newdata = bodyTest_filtrado)

#We calculate the error of our model
error_filtrado = mean((predicion_filtrado - bodyTest_filtrado$WEIG)^2)


MSE <- function(pred, testdata){
  return(mean((predicion_filtrado - bodyTest_filtrado$WEIG)^2))
}

#As we can see, the resulting linear regresion has proven to be quite adequate, however, the training data and our 
# epsilon have both been chosen at random, we will now us a randomized k-fold cross-validation in an attempt to make
# an even better system 

# We will use the standard k = 5

set.seed(5644)

# Create a 5-fold cross-validation index
k <- 5

cv_index <- createDataPartition(body$WEIG, p = 1/k, list = FALSE)

# Display the indices of each fold
cv_index

# We save the n-th fold in the n-th position of the following lists
trainSetFold <- c()
testSetFold <- c()

for(i in 1:k){
  # Add training and testing sets for the i-th fold
  fold_indices <- cv_index[[k]]
  trainSetFold <- c(trainSetFold,body[-fold_indices, ])
  testSetFold <- c(testSetFold, body[fold1_indices, ])
}

# Now we define our epsilon grid 
epGrid <- seq(0.5, 0.99, by = 0.01)


#Now, for search for the model with the least MSE

bestEp <- 0.5

for epsilon in epGrid








