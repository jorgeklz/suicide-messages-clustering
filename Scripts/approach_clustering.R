#@jorgeklz -> jorge.parraga@usach.cl

# Load libraries ----------------------------------------------------------
if(!require("amap"))     { install.packages("amap", repos="http://cran.rstudio.com/");    library("amap") }   # compara si dos dataframes son iguales
if(!require("tidyverse"))     { install.packages("tidyverse", repos="http://cran.rstudio.com/")   ; library("tidyverse")    }    
if(!require("tibble"))     { install.packages("tibble", repos="http://cran.rstudio.com/")   ; library("tibble")    } 
if(!require("purrr"))     { install.packages("purrr", repos="http://cran.rstudio.com/")   ; library("purrr")    } 
if(!require("RCurl"))     { install.packages("RCurl", repos="http://cran.rstudio.com/")   ; library("RCurl")    } 
if(!require("data.table"))     { install.packages("data.table", repos="http://cran.rstudio.com/")   ; library("data.table")    }  
if(!require("magrittr"))     { install.packages("magrittr", repos="http://cran.rstudio.com/")   ; library("magrittr")    } 
if(!require("Rmpi"))     { install.packages("Rmpi", repos="http://cran.rstudio.com/")   ; library("Rmpi")    } 
if(!require("moc.gapbk"))     { install.packages("moc.gapbk", repos="http://cran.rstudio.com/")   ; library("moc.gapbk")    } 
if(!require("foreach"))     { install.packages("foreach", repos="http://cran.rstudio.com/")   ; library("foreach")    }   # implementa foreach para usar con paralelismo
if(!require("doParallel"))  { install.packages("doParallel", repos="http://cran.rstudio.com/"); library("doParallel") }   # implementa paralelismo
if(!require("igraph"))      { install.packages("igraph", repos="http://cran.rstudio.com/");     library("igraph") }   # implementa libreria de grafos
if(!require("nsga2R"))      { install.packages("nsga2R", repos="http://cran.rstudio.com/");     library("nsga2R") }   # implementa funciones NSGA-2
if(!require("functional"))  { install.packages("functional", repos="http://cran.rstudio.com/"); library("functional") }   # #quitar filas con NA
if(!require("utils"))       { install.packages("utils", repos="http://cran.rstudio.com/");      library("utils") }   # Gives us the progress bar object.
if(!require("compare"))     { install.packages("compare", repos="http://cran.rstudio.com/");    library("compare") }   # compara si dos dataframes son iguales


# Parameters --------------------------------------------------------------

idioma <- "en"
clase <-  "multi" #"multi"
iteraciones <- 20


# Load similarity matrices ------------------------------------------------

  md_cos <- read.csv(file = paste0("Scripts/distancia_", idioma,"_", clase, "_cosine.csv"), header = FALSE)
  md_wmd <- read.csv(file = paste0("Scripts/distancia_", idioma,"_", clase, "_WMD.csv"), header = FALSE)
  md_lsi <- read.csv(file = paste0("Scripts/distancia_", idioma,"_", clase, "_LSI.csv"), header = FALSE)
  


# Preprocessing  ----------------------------------------------------------

# hacer diagonal 1.  Luego convertir a distancia. (1-x)
 for(x in 1:nrow(md_cos)){  for(y in 1:nrow(md_cos)){ if(x==y){ md_cos[x,y]=1}}}; md_cos <- (1 - (md_cos))
 for(x in 1:nrow(md_lsi)){  for(y in 1:nrow(md_lsi)){ if(x==y){ md_lsi[x,y]=1}}}; md_lsi <- (1 - (md_lsi))
#   
#   

 
# Run clustering ----------------------------------------------------------

set.seed(1987)

if(clase=="binary"){
  k <- 2
}else if(idioma=="es"){
  k <- 3
}else{
  k <- 4
}

  

  for(d in 1:3){
    
    if(d==1){ distancia <- "coseno"; md <- md_cos}
    if(d==2){ distancia <- "WMD"; md <- md_wmd}
    if(d==3){ distancia <- "LSI"; md <- md_lsi    } 
    
    res_1 <- lapply(1:iteraciones, function(x) amap::Kmeans(md,k)$cluster)
    res_2 <- lapply(1:iteraciones, function(x) cluster::pam(md, k, diss=TRUE)$clustering)
    res_3 <- lapply(1:iteraciones, function(x)  cutree(hclust(as.dist(md), method="single"), k))
    res_4 <- lapply(1:iteraciones, function(x)  cutree(hclust(as.dist(md), method="complete"), k))
    res_5 <- lapply(1:iteraciones, function(x)  cutree(hclust(as.dist(md), method="average"), k))
    res_6 <- lapply(1:iteraciones, function(x)  cutree(hclust(as.dist(md), method="ward.D"), k))
    res_7 <- lapply(1:iteraciones, function(x)  cutree(hclust(as.dist(md), method="ward.D2"), k))
    
    
      sapply(1:7, function(a) {
          if(a==1){
            nombre <- res_1
          }else if(a==2){
            nombre <- res_2
          }else if(a==3){
            nombre <- res_3
          }else if(a==4){
            nombre <- res_4
          }else if(a==5){
            nombre <- res_5
          }else if(a==6){
            nombre <- res_6
          }else{
            nombre <- res_7
          }
        particion <- as.data.frame(do.call("cbind", nombre))
        write.csv(particion, file=paste0("Results/", idioma,"_", clase, "_", distancia,"_algoritmo_", a,".csv"), row.names = FALSE) 
        }
      )
    
  }  

 

  
  
  
  
  
  
  
 
# resultados <- lapply(1:iteraciones, function(x) moc.gabk(md_wmd, md_cos, num_k = k, generation = 50, pop_size = 30, local_search = TRUE, cores = 2))
# 
# # Save results ------------------------------------------------------------
# 
# sapply(1:iteraciones, 
#        function(x) {
#                 write.csv(resultados[[x]]$population, file=paste0("Results/", idioma,"_", clase, "_poblacion_iter_",x,".csv"), row.names = FALSE) 
#                 write.csv(resultados[[x]]$matrix.solutions, file=paste0("Results/",idioma,"_", clase, "_matriz_particion_iter_",x,".csv"), row.names=FALSE)
#         }
#       )
# 






















