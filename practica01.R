
library(profvis)
library(ggplot2)

simulacion <- function(objetivo=10, max_iter=1e5, guardar_historia=FALSE){
  
  colores <- c("amarillo", "cafe", "negro")
  caramelos <- rep(colores, each=15)
  chupetines <- 0
  iter <- 0
  
  historia <- data.frame(iteracion=0, chupetines=0)
  
  while(chupetines < objetivo & iter < max_iter){
    iter <- iter + 1
    
    if(all(c("amarillo","cafe","negro") %in% caramelos)){
      for(c in c("amarillo","cafe","negro")){
        caramelos <- caramelos[-match(c, caramelos)]
      }
      chupetines <- chupetines + 1
      
    } else if(chupetines > 0 & length(caramelos) > 0){
      chupetines <- chupetines - 1
      caramelos <- caramelos[-1]  
      nuevos <- sample(colores, 4, replace=TRUE)
      caramelos <- c(caramelos, nuevos)
    } else {
      caramelos <- c(caramelos, sample(colores, 3, replace=TRUE))
    }
    
    if(guardar_historia){
      historia <- rbind(historia, data.frame(iteracion=iter, chupetines=chupetines))
    }
  }
  
  if(guardar_historia){
    return(historia)
  } else {
    return(iter)
  }
}
profvis({
  resultado <- simulacion(objetivo=10)
  cat("Número de iteraciones para conseguir 10 chupetines:", resultado, "\n")
  
  replicas <- replicate(30, simulacion(10))
  cat("Promedio de iteraciones:", mean(replicas), "\n")
})
historia <- simulacion(objetivo=10, guardar_historia=TRUE)

ggplot(historia, aes(x=iteracion, y=chupetines)) +
  geom_line(color="blue", size=1) +
  geom_point(color="red") +
  theme_minimal() +
  labs(
    title="Evolución de los chupetines en la simulación",
    x="Iteraciones",
    y="Cantidad de chupetines"
  )

