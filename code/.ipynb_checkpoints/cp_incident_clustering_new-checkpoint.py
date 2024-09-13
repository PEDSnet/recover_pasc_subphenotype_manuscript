import os

#os.environ["OMP_NUM_THREADS"] = "1" # export OMP_NUM_THREADS=1
#os.environ["OPENBLAS_NUM_THREADS"] = "1" # export OPENBLAS_NUM_THREADS=1
#os.environ["MKL_NUM_THREADS"] = "1" # export MKL_NUM_THREADS=1
#os.environ["VECLIB_MAXIMUM_THREADS"] = "1" # export VECLIB_MAXIMUM_THREADS=1
#os.environ["NUMEXPR_NUM_THREADS"] = "1" # export NUMEXPR_NUM_THREADS=1


import csv
import sys
import time
import pandas as pd
import numpy as np
import sklearn
import matplotlib.pyplot as plt
import gensim
from sklearn import cluster
from sklearn import metrics
import random
import psycopg2
from numpy import dot
from numpy.linalg import norm
from gensim.models import Word2Vec
import multiprocessing
import logging  # Setting up the loggings to monitor gensim
logging.basicConfig(format="%(levelname)s - %(asctime)s: %(message)s", datefmt= '%H:%M:%S', level=logging.INFO)
from gensim.models.doc2vec import Doc2Vec
import math
import umap
from sklearn.decomposition import PCA
from sklearn.metrics.pairwise import cosine_similarity # <<< this isn't being used!
from typing import List
import seaborn as sns
# import libraries and load english language model and word vector model
#from __future__ import print_function
import time
import umap.umap_ as umap
import hdbscan
import sqlalchemy

def plot_distortions(df, N=20):
    distortions = []
    K = range(1, N)
    for k in K:
        print(k)
        kmeanModel = cluster.KMeans(n_clusters=k)
        kmeanModel.fit(df)
        distortions.append(kmeanModel.inertia_)

    plt.figure(figsize=(16, 8))
    plt.plot(K, distortions, 'bx-')
    plt.xlabel('k')
    plt.ylabel('Distortion')
    plt.title('The Elbow Method')
    plt.show()
    plt.savefig("../results/rules_based_distortions.png")


def kmeans_clust(df, NUM_CLUSTERS):
    kmeans = cluster.KMeans(n_clusters=NUM_CLUSTERS)
    kmeans.fit(df)

    labels = kmeans.labels_
    centroids = kmeans.cluster_centers_

    print("Cluster id labels for inputted data")
    print(labels)
    print("Centroids data")
    print(centroids)

    print(
        "Score (Opposite of the value of X on the K-means objective which is Sum of distances of samples to their closest cluster center):")
    print(kmeans.score(df))

    silhouette_score = metrics.silhouette_score(df, labels, metric='euclidean')

    print("Silhouette_score: ")
    print(silhouette_score)
    return (kmeans.labels_)


def tsne_embed(df, vocab):
    labels = []
    tokens = []

    for word in vocab:
        tokens.append(df.drop(columns=['cluster']).loc[word])
        labels.append(word)

    tsne_model = TSNE(perplexity=30, n_components=2, init='pca', n_iter=1000, random_state=23, n_jobs=16)
    new_values = tsne_model.fit_transform(tokens)

    x = []
    y = []
    for value in new_values:
        print(value)
        x.append(value[0])
        y.append(value[1])
    return (x, y)


class Word:
    def __init__(self, text, vector):
        self.text = text
        self.vector = vector


class Sentence:
    def __init__(self, word_list):
        self.word_list = word_list

    # return the length of a sentence
    def len(self):
        return (len(self.word_list))

    def __str__(self):
        word_str_list = [word.text for word in self.word_list]
        return ' '.join(word_str_list)

    def __repr__(self):
        return self.__str__()


def preloading_sentences(sentence_list, model):
    """
    Converts a list of sentences into a list of Sentence (and Word) objects

    input: a list of sentences, embedding_size
    output: a list of Sentence objects, containing Word objects, which contain 'text' and word vector attributes
    """
    vocab = list(model.wv.key_to_index.keys())
    embedding_size = 200
    all_sent_info = []
    for sentence in sentence_list:
        sent_info = []
        words = sentence.split(" ")
        for word in words:
            if word in vocab:
                sent_info.append(Word(word, model.wv.get_vector(word)))
        # todo: if sent_info > 0, append, else don't
        all_sent_info.append(Sentence(sent_info))
    return (all_sent_info)


def get_word_frequency(word_text, model):
    wf = model.wv.get_vecattr(word_text, "count")
    return (wf)


def sentence_to_vec(sentence_list, embedding_size, model, a=1e-3):
    """
    A SIMPLE BUT TOUGH TO BEAT BASELINE FOR SENTENCE EMBEDDINGS

    Sanjeev Arora, Yingyu Liang, Tengyu Ma
    Princeton University
    """
    sentence_set = []  # intermediary list of sentence vectors before PCA
    sent_list = []  # return list of input sentences in the output
    a_vals = []
    for sentence in sentence_list:
        this_sent = []
        vs = np.zeros(embedding_size)  # add all w2v values into one vector for the sentence
        sentence_length = sentence.len()
        for word in sentence.word_list:
            this_sent.append(word.text)
            word_freq = get_word_frequency(word.text, model)
            a_value = a / (a + word_freq)  # smooth inverse frequency, SIF
            a_vals.append(a_value)
            vs = np.add(vs, np.multiply(a_value, word.vector))  # vs += sif * word_vector
        vs = np.divide(vs, sentence_length)  # weighted average, normalized by sentence length
        sentence_set.append(vs)  # add to our existing re-caculated set of sentences
        sent_list.append(' '.join(this_sent))
    # calculate PCA of this sentence set
    pca = PCA(n_components=embedding_size)
    pca.fit(np.array(sentence_set))
    u = pca.components_[0]  # the PCA vector
    u = np.multiply(u, np.transpose(u))  # u x uT
    # pad the vector? (occurs if we have less sentences than embeddings_size)
    if len(u) < embedding_size:
        for i in range(embedding_size - len(u)):
            u = np.append(u, 0)
    # resulting sentence vectors, vs = vs -u * uT * vs
    sentence_vecs = []
    for vs in sentence_set:
        sub = np.multiply(u, vs)
        sentence_vecs.append(np.subtract(vs, sub))
    return (sentence_vecs, sent_list, a_vals)


def sentence_to_vec_alt(sentence_list, embedding_size, model, a=1e-3):
    """
    A SIMPLE BUT TOUGH TO BEAT BASELINE FOR SENTENCE EMBEDDINGS

    Sanjeev Arora, Yingyu Liang, Tengyu Ma
    Princeton University
    """
    sentence_set = []  # intermediary list of sentence vectors before PCA
    sent_list = []  # return list of input sentences in the output
    for sentence in sentence_list:
        this_sent = []
        vs = np.zeros(embedding_size)  # add all w2v values into one vector for the sentence
        sentence_length = sentence.len()
        for word in sentence.word_list:
            this_sent.append(word.text)
            word_freq = get_word_frequency(word.text, model)
            # a_value = a / (a + word_freq) # smooth inverse frequency, SIF
            a_value = 1
            vs = np.add(vs, np.multiply(a_value, word.vector))  # vs += sif * word_vector
        vs = np.divide(vs, sentence_length)  # weighted average, normalized by sentence length
        sentence_set.append(vs)  # add to our existing re-caculated set of sentences
        sent_list.append(' '.join(this_sent))
    # calculate PCA of this sentence set
    pca = PCA(n_components=embedding_size)
    pca.fit(np.array(sentence_set))
    u = pca.components_[0]  # the PCA vector
    u = np.multiply(u, np.transpose(u))  # u x uT
    # pad the vector? (occurs if we have less sentences than embeddings_size)
    if len(u) < embedding_size:
        for i in range(embedding_size - len(u)):
            u = np.append(u, 0)
    # resulting sentence vectors, vs = vs -u * uT * vs
    sentence_vecs = []
    for vs in sentence_set:
        sub = np.multiply(u, vs)
        sentence_vecs.append(np.subtract(vs, sub))
    return (sentence_vecs, sent_list)


def get_cos_distance(sentence_list):
    """
    Create Sentence and Word objects from a list and pass them to sentence_to_vec()
    Return a matrix of the _cosine distance_ of elements in the matrix
    This is used for sentence similarity functions

    input: A list of plaintext sentences
    output: A list of sentence distances
    """
    sentence_vectors, sent_list = get_sen_embeddings(sentence_list)
    cos_list = cosine_similarity(sentence_vectors, Y=None, dense_output=True)
    return (cos_list, sent_list)


def get_most_similar(utterance, sentence_list):
    """
    Takes an input utterance and a corpus sentence_list to compare it to,
    and returns a dict of the utterance, closest question, and relevant answer
    """
    sentence_list.append(utterance)
    cos_list, sent_text = get_cos_distance(sentence_list)
    # check out the similarity matrix for the utterance
    tmp_list = list(cos_list[len(cos_list) - 1])
    # get the index of the question with the highest simliarity
    tmp_indx = tmp_list.index(max(tmp_list[0:len(tmp_list) - 2]))
    return (tmp_indx)


def sentence_to_vec_centroid(sentence_list, embedding_size, model, cluster_centroid_df, a=1e-3):
    """
    A SIMPLE BUT TOUGH TO BEAT BASELINE FOR SENTENCE EMBEDDINGS

    Sanjeev Arora, Yingyu Liang, Tengyu Ma
    Princeton University
    """
    i = 0
    sentence_set = []  # intermediary list of sentence vectors before PCA
    sent_list = []  # return list of input sentences in the output
    for sentence in sentence_list:
        this_sent = []
        vs = np.zeros(embedding_size)  # add all w2v values into one vector for the sentence
        sentence_length = sentence.len()
        for word in sentence.word_list:
            this_sent.append(word.text)
            word_freq = get_word_frequency(word.text, model)
            # a_value = a / (a + word_freq) # smooth inverse frequency, SIF
            a_value = 1
            print(i)
            i = i + 1
            word_vector = cluster_centroid_df.loc[cluster_centroid_df.index == int(word.text)].values
            vs = np.add(vs, np.multiply(a_value, word.vector))  # vs += sif * word_vector
        vs = np.divide(vs, sentence_length)  # weighted average, normalized by sentence length
        sentence_set.append(vs)  # add to our existing re-caculated set of sentences
        sent_list.append(' '.join(this_sent))
    # calculate PCA of this sentence set
    pca = PCA(n_components=embedding_size)
    pca.fit(np.array(sentence_set))
    u = pca.components_[0]  # the PCA vector
    u = np.multiply(u, np.transpose(u))  # u x uT
    # pad the vector? (occurs if we have less sentences than embeddings_size)
    if len(u) < embedding_size:
        for i in range(embedding_size - len(u)):
            u = np.append(u, 0)
    # resulting sentence vectors, vs = vs -u * uT * vs
    sentence_vecs = []
    for vs in sentence_set:
        sub = np.multiply(u, vs)
        sentence_vecs.append(np.subtract(vs, sub))
    return (sentence_vecs, sent_list)

engine = sqlalchemy.create_engine('postgresql://lormanv:Epdnnbmkssbsidinnonsp1!@reslnpedsndb06.research.chop.edu:5432/dcc_covid_wk141_221215')

model=Word2Vec.load('./model_all_sites_full')
vocab=list(model.wv.key_to_index.keys())
normed_vectors=model.wv.get_normed_vectors()
normed_vector_df=pd.DataFrame(normed_vectors, index=vocab)

rules_based_cohort_sentences = pd.read_sql_query("SELECT * FROM concept_embeddings.cp_cohort_incident_non_misc_sentences_pasc_78;", engine)
rules_based_sentence_list=rules_based_cohort_sentences.codes.values.tolist()
rules_based_sentences=preloading_sentences(rules_based_sentence_list, model)
rules_based_sentence_vecs=sentence_to_vec(sentence_list=rules_based_sentences, embedding_size=200, model=model, a=1000000)
#rules_based_sentence_vecs_alt=sentence_to_vec_alt(sentence_list=rules_based_sentences, embedding_size=200, model=model)

rules_based_normalized_sentence_vecs=[ v / np.linalg.norm(v) for v in rules_based_sentence_vecs[0]]
#rules_based_normalized_sentence_vecs_alt=[ v / np.linalg.norm(v) for v in rules_based_sentence_vecs_alt[0]]

rules_based_sentence_vec_df=pd.DataFrame(rules_based_normalized_sentence_vecs)
#rules_based_sentence_vec_df_alt=pd.DataFrame(rules_based_normalized_sentence_vecs_alt)

rules_based_sentence_vec_df['person_id']=rules_based_cohort_sentences.person_id

clusterable_embedding = umap.UMAP(
    n_neighbors=50,
    min_dist=0,
    n_components=2,
    random_state=42,
).fit_transform(rules_based_sentence_vec_df.drop(columns=['person_id']))


labels = hdbscan.HDBSCAN(
    min_samples=5,
    min_cluster_size=(np.floor(rules_based_sentence_vec_df.shape[0]*0.04).astype('int')
)).fit_predict(clusterable_embedding)
set(labels)
sum(labels==-1)

clustered = (labels >= -1)
plt.scatter(clusterable_embedding[~clustered, 0],
            clusterable_embedding[~clustered, 1],
            color=(0.5, 0.5, 0.5),
            s=0.1,
            alpha=0.5)
plt.scatter(clusterable_embedding[clustered, 0],
            clusterable_embedding[clustered, 1],
            c=labels[clustered],
            s=0.1,
            cmap='Spectral');



embedded_df=pd.DataFrame(clusterable_embedding)

embedded_df['person_id']=rules_based_cohort_sentences.person_id
embedded_df['cluster']=labels

embedded_df.to_sql(name="".join( ["cp_incident_non_misc_embedded_df_new", "_pasc_88"]), con=engine, if_exists='replace', schema='concept_embeddings')

clusterable_embedding_b=clusterable_embedding[(labels==0) | (labels==5) | (labels==4)]


labels_b = hdbscan.HDBSCAN(
    min_samples=1,
    min_cluster_size=(np.floor(clusterable_embedding_b.shape[0]*0.05).astype('int')
)).fit_predict(clusterable_embedding_b)
set(labels_b)
sum(labels==-1)

clustered_b = (labels_b >= -1)
plt.scatter(clusterable_embedding_b[~clustered_b, 0],
            clusterable_embedding_b[~clustered_b, 1],
            color=(0.5, 0.5, 0.5),
            s=0.1,
            alpha=0.5)
plt.scatter(clusterable_embedding_b[clustered_b, 0],
            clusterable_embedding_b[clustered_b, 1],
            c=labels_b[clustered_b],
            s=0.1,
            cmap='Spectral');



embedded_df_b=pd.DataFrame(clusterable_embedding_b)

embedded_df_b['person_id']=embedded_df[(labels==0) | (labels==4) | (labels==5)]['person_id'].values
embedded_df_b['cluster']=labels_b

embedded_df_b.to_sql(name="".join( ["cp_incident_non_misc_embedded_df_complex", "_pasc_88"]), con=engine, if_exists='replace', schema='concept_embeddings')

