
#####################################################################

#Calcula los valores de pj para j=0,1...200 y los guarda en un vector
psubj = rep(0, 201)
for( j in 1:201 ){
  #psubj[1] = p_0
  psubj[j] = dpois(x=(j-1), lambda = 10) 
}

#Calcula los valores de qj para j = 1...200 y los guarda en un vector
qsubj = rep(0,201)
for( j in 1:201 ){
  qsubj[j] = 1 - ppois(q = (j-2), lambda = 10, lower.tail = TRUE)
}

#####################################################################



recompensaJacobi <- function(s,a){
  rta = 0
  if(s+a-1 >= 0){
    #Con el ciclo se calcula suma 10j p_j para j=0...s+a-1
    for(j in 0:(s+a-1)){
      rta = rta + (10*j*psubj[j+1])
    }
    
    rta = rta + (10*(s+a)*qsubj[s+a+1] - 3*a - s)             #Se suma el resto.
    #Si a>0 se debe hacer el descuento de 2 que es el valor de realizar el pedido.
    if(a > 0){ rta = rta - 2  }
    
    #Hasta aquí rta = r(s,a), falta multiplicar por 1/1-lambda p(s|s,a)
    rta = rta * (1/(1- (lambda*psubj[a+1])) )
    
    }
  return(rta)
}

sumaJacobi <- function(s,a,v0){
  rta = 0
  for (j in 0:M){
    #Sumamos sobre los estados diferentes de s.
    if(j != s){
      pjsa = 0 #p(j|s,a)
      #Vemos los casos posibles para p(j|s,a)
      if( j > 0 && j <= (s+a) && (s+a) <= 100){
        pjsa = psubj[s+a-j+1]
      }
      else if( (s+a) <= M && j == 0 ){
        pjsa = qsubj[s+a+1]
      }
      rta = rta +  ( (pjsa*v0[j+1])/ (1-lambda * psubj[a+1]))
    }
  }
  return(rta)
}

#M, maximo stock
M = 100

#lambda
lambda = 0.9

#Definimos v0(s) = 0 para todo s.
#Esta variable la utilizaremos para las iteraciones de forma recursiva. Es vn.
v0 = rep(0,M+1)

#Este va a representar vn+1 cuando calculemos iterativamente.
vn = rep(0,M+1)

#epsilon lo tomamos como 0.1
epsilon = 0.1

#Inicializamos n como 0.
n = 0

#Condición de parada del ciclo.
parar = FALSE

while(parar == FALSE){
  print(n)
  #Itero sobre los s
  for( s in 0:M ) {
    #Los valores a encontrar el máximo (r(s,a) + sum...)
    valores <- rep(0, (M-s))
    
    #Comenzamos la iteración sobre A(i)
    for( a in 0:(M-s) ){
      valores[a+1] = recompensaJacobi(s,a) + (lambda * sumaJacobi(s,a,v0))
    }
    #Tomamos el maximo entre los valores calculados
    vn[s] = max(valores)
  }
  
  #Condición de parada
  norma = max(abs(vn-v0))
  
  if(norma < ((epsilon*(1-lambda))/(2*lambda))){
    parar = TRUE
  }
  v0 <- vn #Actualizo mi v0 como vn para poder utilizarlo en el siguiente cálculo de vn+1
  n = n+1  #Actualizo el n
}


vfinal = vn

#Terminó el paso 3 del algoritmo
print("Terminó el tercer paso.")

#Comienza el paso 4.
d = rep(0,M+1)

#Itero sobre los s
for( s in 0:M )
{
  #Los valores a encontrar el máximo (r(s,a) + sum...)
  valores = rep(0, (M-s))
  
  #Comenzamos la iteración sobre A(i)
  for( a in 0:(M-s) ){
    valores[a+1] = recompensa(s,a) + (0.9 * suma(s,a,vn))
  }
  
  d[s+1] = (which.max(valores)-1)
  
}

#################################################################################
#################################################################################
