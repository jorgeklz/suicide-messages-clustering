
library("tidyverse")
library("caret")
library("gtools")
library("aricode")
library("viridis")          
set.seed(1987)
idioma <- "es" 
clase <- "multi"


if(clase=="binary"){
      #LEER GROUND TRUTH (BINARY)
      label_binary <- read.table(paste0("Datos/label_",idioma,"_", clase,".txt"))
      label_binary$grupo <- ifelse(label_binary$V1=="No.Risk", 1, 2)
      
      tabla <- list()
      z=1
      
      for (distancia in c("coseno","WMD","LSI")){
      
        for(a in 1:7){
          
          soluciones <- read.csv(paste0("Results/", idioma,"_", clase, "_", distancia,"_algoritmo_", a,".csv"), stringsAsFactors = F, sep=",")
        
          for(y in 1:ncol(soluciones) ){
              result <- confusionMatrix(as.factor(soluciones[,y]), as.factor(label_binary$grupo))
              
              accuracy <- result$byClass[11]
              precision <- result$byClass[5]
              recall <- result$byClass[6]
              fscore <- result$byClass[7]
              tabla[[z]] <- c(idioma=idioma, distan=distancia, alg=a, sol=y, accuracy,precision, recall,fscore )
              z=z+1
          }
      
        }
      }
      
      resul_tab <- as_tibble(do.call("rbind", tabla))
      
      colnames(resul_tab)[5] <- "Accuracy"
            
            
            

      
      setEPS()
      postscript(paste0("paper_",idioma, "_",clase,"_boxplot.eps"), width =8 , height = 4)
      resul_tab %>% ggplot(aes(alg, as.numeric(Accuracy))) + 
        geom_boxplot(aes(fill = alg)) +   
        stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=4) + 
       # geom_text(data = means, aes(label = weight, y = weight + 0.08)) +
        scale_color_viridis(discrete = TRUE )+
        scale_fill_viridis(discrete = TRUE) +
        theme_bw() +   ggtitle(paste0(idioma, clase)) +
        theme(legend.position = "bottom")  +  facet_wrap(~distan) #+ coord_cartesian(ylim = c(0.2, 0.6))
      dev.off()
            
            
    #Tablas de estadisticas por distancia
            
    for(filtro in c("coseno", "LSI", "WMD")){
      
      write.csv(resul_tab %>% filter(distan==filtro) %>% group_by(idioma, alg)  %>%
        dplyr::summarize(Accu = round(mean(as.numeric(Accuracy), na.rm=TRUE),4),
                         Prec = round(mean(as.numeric(Precision), na.rm=TRUE),4),
                         Reca = round(mean(as.numeric(Recall), na.rm=TRUE),4),
                         Fscor = round(mean(as.numeric(F1), na.rm=TRUE),4)
                         ), file=paste0("tabla_superv_", idioma,"_", filtro,".csv") )
        
    }
            
            #gustavo <- resul_tab %>% select(-c(sol))
            
            #write.csv(gustavo, "datos2.csv", row.names = FALSE)
            
            
            
}else{
    
    
  if(idioma=="es"){
    label_multiple <- read.table("Datos/label_es_multi.txt")
  }else{
    label_multiple <- read.table("Datos/label_en_multi.txt")
  }
  
  grupos_reales <- 1:length(levels(label_multiple$V1))
  grupos_reales_permutacion <- permutations(n=length(grupos_reales), r=max(grupos_reales), v=grupos_reales)
  tabla <- list()
  z=1
      #no aplico finalmente lo de permutacion por que la fcunion clustComp ya controla eso.
      for(fila in 1:1){
    
      #for(fila in 1:nrow(grupos_reales_permutacion)){
      
            if(idioma=="es"){
              label_cod<- label_multiple %>%
                mutate(grupo = case_when(V1=="No.risk" ~  as.numeric(grupos_reales_permutacion[fila, 1]),
                                         V1=="Possible" ~  as.numeric(grupos_reales_permutacion[fila, 2]),
                                         V1=="Urgent" ~  as.numeric(grupos_reales_permutacion[fila, 3])))
            }else{
              label_cod <- label_multiple %>%
                mutate(grupo = case_when(V1=="No.risk" ~  as.numeric(grupos_reales_permutacion[fila, 1]),
                                         V1=="Possible" ~  as.numeric(grupos_reales_permutacion[fila, 2]),
                                         V1=="Urgent" ~  as.numeric(grupos_reales_permutacion[fila, 3]),
                                         V1=="Immediate" ~  as.numeric(grupos_reales_permutacion[fila, 4])))
            }

          clase_real <- as.factor(label_cod$grupo)
      
      
    
      for (distancia in c("coseno","WMD","LSI")){
        
        for(a in 1:7){
          
          soluciones <- read.csv(paste0("Results/", idioma,"_", clase, "_", distancia,"_algoritmo_", a,".csv"), stringsAsFactors = F, sep=",")
          
          
          for(y in 1:ncol(soluciones) ){
            clase_obtenida <- soluciones[,y]
            result <- clustComp(clase_obtenida, clase_real)$ARI
            tabla[[z]] <- c(idioma=idioma, distan=distancia, alg=a, sol=y, ARI=result, version_perm=fila)
            z=z+1
          }
          
        }
      }
  
      
      
    }

  
  resul_tab <- as_tibble(do.call("rbind", tabla))
  
  
  #boxplot
  
  setEPS()
  postscript(paste0("paper_",idioma, "_",clase,"_boxplot.eps"), width =8 , height = 4)
  resul_tab %>% ggplot(aes(alg, as.numeric(ARI))) + 
    geom_boxplot(aes(fill = alg)) +   
    stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=4) + 
    # geom_text(data = means, aes(label = weight, y = weight + 0.08)) +
    scale_color_viridis(discrete = TRUE )+
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +   ggtitle(paste0(idioma, clase)) +
    theme(legend.position = "bottom")  +  facet_wrap(~distan) #+ coord_cartesian(ylim = c(0.2, 0.6))
  dev.off()
  
  #Tablas de estadisticas por distancia
  
  for(filtro in c("coseno", "LSI", "WMD")){
    
    write.csv(resul_tab %>% filter(distan==filtro) %>% group_by(idioma, alg)  %>%
                dplyr::summarize(ARI = round(mean(as.numeric(ARI), na.rm=TRUE),4)
                ), file=paste0("tabla_ARI_", idioma,"_", filtro,".csv") )
    
  }
  # 
  # gustavo <- resul_tab %>% select(-c(sol, version_perm))
  # 
  # write.csv(gustavo, "ari_datos1.csv", row.names = FALSE)

}












#Negative=No.Risk


# ARI



# library("aricode")
# data(iris)
# cl_obtenido <- cutree(hclust(dist(iris[,-5])), 4)
# cl_real <- iris$Species
# 
# clustComp(cl_obtenido, cl_real)
