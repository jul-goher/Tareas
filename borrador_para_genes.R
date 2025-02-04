#####################################################
######       Gráfica de apariciones           #######
#####################################################
library(ggplot2)

str(registro_lectura)
as.character(registro_lectura$subsec)
as.numeric(registro_lectura$apariciones, registro_lectura$tamaño)

point_apar  

par(mar=c(6,4,4,2))
barplot(registro_lectura$apariciones, col = registro_lectura$tamaño, 
        xlab = "Subsecuencias", ylab = "# de matches", 
        names.arg = registro_lectura$subsec, main = "Lecturas", space = 1)

############################################################
######       Gráfica de secuencias por gen           #######
############################################################

sec_gen <- c(sum(registro_lectura$gen1),sum(registro_lectura$gen2),sum(registro_lectura$gen3),
  sum(registro_lectura$gen4),sum(registro_lectura$gen5),sum(registro_lectura$gen6),
  sum(registro_lectura$gen7),sum(registro_lectura$gen8),sum(registro_lectura$gen9),
  sum(registro_lectura$gen10))

genes <- c("Gen 1","Gen 2", "Gen 3","Gen 4","Gen 5","Gen 6","Gen 7","Gen 8","Gen 9", "Gen 10")

names(sec_gen) <- genes
sec_gen

barplot(sec_gen, col = sec_gen, xlab = "Genes", ylab = "# de matches",
        main = "Lecturas")

############################################################
######                    Análisis                   #######
############################################################

analisis_1 <- lm(apariciones~tamaño, registro_lectura) #  Reg. lineal
summary(analisis_1)
plot(analisis_1)# Supuestos

analisis_2 <- lm (apariciones ~ poly(tamaño,2) , data = registro_lectura) # Reg. cuadrada
summary(analisis_2)
plot(analisis_2) # Supuestos

# Podemos cambiar o quit<r el titulo de la graf
point_apar <- ggplot(registro_lectura, aes(x=tamaño , y=apariciones))+
  geom_point(colour = registro_lectura$subsec) + theme_light() +
  xlab("Longitud") + ylab("# de matches") +
  ggtitle("Mach de cada secuencia con respecto su longitud") + geom_smooth(method = lm, se=TRUE) 

# stat_smooth(aes(), method = "lm", formula = y~poly(x,2), se=TRUE, size=1) 
# Esta es de la reg.cuadra pero no sé cual vean mejor
# Yo creo que en la lineal se cumplen más los supuestos. 

point_apar

