#based on https://www.kernix.com/blog/similarity-measure-of-textual-documents_p12
import re, string, unicodedata
import os
import nltk
import inflect
import numpy as np
from nltk import word_tokenize, sent_tokenize
from nltk import download
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics.pairwise import pairwise_distances
from gensim import corpora
import gensim
from gensim.models import TfidfModel
from gensim.models import LsiModel
from gensim.similarities import MatrixSimilarity
from gensim.models import Word2Vec

#os.getcwd()
download('punkt')
download('stopwords')
   
stop_words = stopwords.words('english')
   
def preprocess(text):
   text = text.lower()
   doc = word_tokenize(text)
   doc = [word for word in doc if word not in stop_words]
   doc = [word for word in doc if word.isalpha()]
   doc = re.sub(r"http\S+", "", doc)
   return doc
   

def filter_docs(corpus, texts, labels, condition_on_doc):
    """
    Filter corpus, texts and labels given the function condition_on_doc which takesa doc.
    The document doc is kept if condition_on_doc(doc) is true.
    """
    number_of_docs = len(corpus)
       
    if texts is not None:
       texts = [text for (text, doc) in zip(texts, corpus)
             if condition_on_doc(doc)]
       
    labels = [i for (i, doc) in zip(labels, corpus) if condition_on_doc(doc)]
    corpus = [doc for doc in corpus if condition_on_doc(doc)]
       
    print("{} docs removed".format(number_of_docs - len(corpus)))
                   
    return (corpus, texts, labels)
               
#corpus, texts, y = filter_docs(corpus, texts, y, lambda doc: (len(doc) != 0))

textos = []
textos_labels = []
textos_file = "../Datos/corpus_en_binary.csv"



with open(textos_file, 'r') as f:
   for line in f:
       # each line is a snippet: a bag of words separated by spaces and the category
       line = line.split()
       category = line[-1]
       doc = line[:-1]
       textos.append(doc)
       textos_labels.append(category)
   
textos, _, textos_labels = filter_docs(textos, None, textos_labels, lambda doc: (len(doc) != 0))





"""
similarity
"""



"""
Metodo 2 and 3. Cosine and WMD
"""   
filename = '../Datos/GoogleNews-vectors-negative300.bin.gz'
#word2vec_model = Word2Vec.load_word2vec_format(filename, binary=True)   
word2vec_model = gensim.models.KeyedVectors.load_word2vec_format(filename, binary=True)     
word2vec_model.init_sims(replace=True)

def document_vector(word2vec_model, doc):
   # remove out-of-vocabulary words
   doc = [word for word in doc if word in word2vec_model.vocab]
   return np.mean(word2vec_model[doc], axis=0)           
           

def has_vector_representation(word2vec_model, doc):
   return not all(word not in word2vec_model.vocab for word in doc)
               

textos, _, textos_labels = filter_docs(textos, None, textos_labels, 
                                           lambda doc: has_vector_representation(word2vec_model, doc))
   

with open('../Datos/your_file.txt', 'w') as f:
    for item in textos_labels:
        f.write("%s\n" % item)




"""
Metodo 1. LSI
"""   
dictionary_textos = corpora.Dictionary(textos)
corpus_gensim_textos = [dictionary_textos.doc2bow(doc) for doc in textos]
tfidf_textos = TfidfModel(corpus_gensim_textos)
corpus_tfidf_textos = tfidf_textos[corpus_gensim_textos]
lsi_textos = LsiModel(corpus_tfidf_textos, id2word=dictionary_textos, num_topics=200)
lsi_index_textos= MatrixSimilarity(lsi_textos[corpus_tfidf_textos])
#sims['snippets']['LSI'] = np.array([lsi_index_snippets[lsi_snippets[corpus_tfidf_snippets[i]]]
                                                   #for i in range(len(snippets))])
documentos_procesados= np.array([lsi_index_textos[lsi_textos[corpus_tfidf_textos[i]]]
                                                   for i in range(len(textos))])      
np.savetxt("distancia_LSI.csv", documentos_procesados, delimiter=",")
    





"""
COsine
"""           
documentos_procesados=np.array([document_vector(word2vec_model, doc) 
                    for doc in textos])         
np.savetxt("distancia_cosine.csv",cosine_similarity(documentos_procesados), delimiter=",")
   

"""
WMD
""" 
A = np.array([[i] for i in range(len(textos))])        
def f(x, y):
    return word2vec_model.wmdistance(textos[int(x)], textos[int(y)])

#X_wmd_distance_snippets = pairwise_distances(A, metric=f, n_jobs=-1)
 
np.savetxt("distancia_WMD.csv", pairwise_distances(A, metric=f, n_jobs=-1), delimiter=",")



