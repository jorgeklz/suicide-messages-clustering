#@jorgeklz -> jorge.parraga@usach.cl

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("tibble")
library("XML")
library("methods")
library("RCurl")
library("httr")
library("text2vec")
library("data.table")
library("magrittr")
library("tm")
library("Rmpi")
library("wordcloud")

# Function to generate number of document in corpus -----------------------
genera_numero_archivo <- function(x){
  if(x<10){
    numero=paste0("000", x)
  }else if (x<100){
    numero=paste0("00", x)
  }else{
    numero=paste0("0", x)
  }
  return(numero)
}


# Load corpus: only text and Alert Level ----------------------------------
numero_documentos <- 102
documentos <- lapply(1:numero_documentos, function(x){
  
    num_archivo=genera_numero_archivo(x)
    #arch <- paste0("https://raw.githubusercontent.com/PlataformaLifeUA/corpus/master/GATE_Document_",archivo,".xml")
    #url <- getURL(arch)
    archivo <- paste0("Datos/corpus/GATE_Document_",num_archivo,".xml")
    
    #Load xml file
    url <- read_file(archivo)
    datos <- xmlTreeParse(url, ignoreBlanks=FALSE, useInternalNodes = TRUE, trim=FALSE)
    #nodos <- getNodeSet(datos, "//TextWithNodes")

    #Extract the root node form the xml file.
    rootnode <- xmlRoot(datos)
    
    # Node 4 -> <TextWithNodes>
    # Node 10 -> <!-- The default annotation set -->
    # Node 16.2 -> <Annotation Id="2" Type="SuicidalClassification" StartNode="0" EndNode="1030">
    # Node 16.2.2.4 -> Language
    # Node 16.2.4.4 -> AlertLevel

    #texto
    #texto <- xmlValue(rootnode[[4]])#, ignoreComments=TRUE, recursive=TRUE, trim = FALSE)
    texto <- unlist(xpathApply(datos, "//TextWithNodes", xmlValue))
    #clase
    clase <- xmlValue(rootnode[[16]][[2]][[4]][[4]])
    #language
    lang <- xmlValue(rootnode[[16]][[2]][[2]][[4]])
    #lang <- unlist(xpathApply(datos, "//TextWithNodes", xmlValue))
    
    Encoding(texto) <- "UTF-8"
    documento <- c(text=texto, alert_level=clase, lang=lang)
    
})



datos.procesados <- do.call(rbind, documentos)
#Quitar espacios al principio y fin del texto
datos.procesados <- apply(datos.procesados, 2, str_squish)

corpus_all <- datos.procesados %>% as_tibble(datos.procesados)
 
corpus_all <- corpus_all %>% mutate(alert_level=parse_factor(alert_level, 
                                             levels = c("No risk", "Urgent", "Possible", "Immediate")),
                            lang=parse_factor(lang, levels = c("EN", "ES")))
# 



# Preprocessing ---------------------------------------------------------------

corpus_all <- corpus_all %>% mutate(id=as.character(1:nrow(.)), 
                                    alert_level=if_else(alert_level=="No risk", "No.risk", as.character(alert_level)),
                                    alert_category=if_else(alert_level!="No.risk", "Risk", "No.Risk"))

corpus_en <- corpus_all %>% filter(lang=="EN")  

corpus_es <- corpus_all %>% filter(lang=="ES") 


write.table(corpus_all, file=paste0("Datos/corpus_total.csv"), sep=" ", col.names = FALSE, row.names = FALSE, quote=FALSE)



# Pre-processing each subset: all, english, spanish --------------------------------------------------

caso <- list(corpus_en, corpus_es, corpus_all)

for (crp in caso){
 
    # get info from files xml
    textos <- crp$text
    ids <- crp$id
    classes <- crp$alert_level
    category <- crp$alert_category
    
    
    df <- data.frame(doc_id = ids, text = textos, stringsAsFactors = FALSE)
    # convert to corpus
    docs <- Corpus(DataframeSource(df))

    # Elimina signos de puntuacion
    docs <- tm_map(docs,removePunctuation)
    # Elimina n?meros
    docs <- tm_map(docs, removeNumbers)
    # Lleva a min?sculas
    docs <- tm_map(docs, tolower)
    
    # Elimina espacio blanco adicional
    docs <- tm_map(docs, stripWhitespace)
    # Eliminamos direcciones web o URL
    docs <- tm_map(docs, content_transformer( function(x, pattern) gsub(pattern," ",x) ), "http[[:alnum:][:punct:]]*")
    
    # Convierte a formato de texto plano
    # docs <- tm_map(docs, PlainTextDocument)
    
    
    # Remueve palabras vac?as genericas en este caso del idioma ingl?s
    if(nrow(crp)>100){
      docs <- tm_map(docs, removeWords, stopwords("spanish")) 
      docs <- tm_map(docs, removeWords, stopwords("english"))
      archivo <- "corpus_all"
    }else if(as.character(crp$lang[1])=="ES"){
      docs <- tm_map(docs, removeWords, stopwords("spanish"))
      archivo <- "corpus_es"
    }else if(as.character(crp$lang[1])=="EN"){
      docs <- tm_map(docs, removeWords, stopwords("english"))
      archivo <- "corpus_en"
    } 
      
    
    # get texts of each documents
    proc_text <- lapply(1:length(docs), function(x) docs[[x]]$content)
    proc_class <- sapply(1:length(docs), function(x) classes[x])
    proc_category <- sapply(1:length(docs), function(x) category[x])
    
    textos_procesados <- data.frame(textos = do.call("rbind", proc_text), clases = proc_class, categoria=proc_category, stringsAsFactors = FALSE)
    
    # generate texts where each documents is a row and the last word is the class
    
    # case binary
    write.table(textos_procesados %>% select(-c(clases)), file=paste0("Datos/",archivo,"_binary.csv"), sep=" ", col.names = FALSE, row.names = FALSE, quote=FALSE)
    
    # case multi-class
    write.table(textos_procesados %>% select(-c(categoria)), file=paste0("Datos/",archivo,"_multi.csv"), sep=" ", col.names = FALSE, row.names = FALSE, quote=FALSE)
    
    
}
    

