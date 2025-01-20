# Calcular con el binomial con ayuda de las probabilidades
##Aplicar funciones a mis probabilidades

# Calcula la distancia de Hamming entre tú y quién quieras. ¿Cuántas diferencias hay en sus respuestas?
yo <- c(1, 1,	1,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0)
isa <- c(1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1)

# Distancia de Hamming
sum (yo != isa) #Hay 3 diferencias 

#Distancia de Hamming  entre todas las personas de la base de datos
##2da formna
x <- 1
while (x<23) {
  if (1>0){
    sum((gustos[8,2:14]) != gustos[x, 2:14]) -> up
    print(up)
    #x+1=x 
    x <- x+1
  }
}
#¿Cuáles son las dos personas más similares? ¿Cuáles son las más diferentes?
#Persona más similar = Andrés, más diferente = Daniela


#Supongamos que la probabilidad de que una persona disfrute de Kpop en la base de datos es p. Si seleccionamos  al azar 5 personas, ¿cuál es la probabilidad de que 3 sean fans de Kpop?
combin <- function (n,k) {
  factorial (n) / (factorial(k)*factorial (n-k))
}

combin(5,3)

bino <- function (n,k,p) {
  combin (n,k)*(p^k)*((1-p)^(n-k))
}

bino (5,3,0.27)

#En nuestra base de datos, calculamos la probabilidad de consumir Chocolate (p). Si seleccionamos 10 personas, ¿cuál es la probabilidad de que  6 de ellas consuman Chocolate?
combin (10,6)
bino (10,6,0.91)

#Si sabemos que la probabilidad de que una persona haga Jogging  y Nadar  está dada por la base de datos
#¿cuál es la probabilidad de que exactamente  de ellas realicen ambas actividades?
#probabilidad 2/22 = 0.09
#que de 6, personas, 4 tengan ambas
combin(6,4)
bino(6,4,0.09)

#Seleccionamos  al azar 8 personas. ¿Cuál es la probabilidad de que   5 sean fanáticos de Rom-Coms ?
combin (8,5)
bino (8,5,0.45)

#Si seleccionamos  al azar, 7 personas ¿cuál es la probabilidad de que  al menos una de ellas conduzca un auto?
combin(7,1)
bino(7,1,0.45)

#Si seleccionamos  al azar 5 personas , ¿cuál es la probabilidad de que  exactamente 3 consuman Alcohol?
combin(5,3)
bino(5,3,0.86)

#Si seleccionamos  al azar 6 personas, ¿cuál es la probabilidad de que al menso 3 sean fans de los cómics ?
combin(6,3)
bino(6,3,0.45)

#Si seleccionamos  4 personas al azar, ¿cuál es la probabilidad de que  al menos 3. de ellas disfruten de bailar?
combin(4,3)
bino(4,3,0.68)
