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
#import umap
from sklearn.decomposition import PCA
from sklearn.metrics.pairwise import cosine_similarity # <<< this isn't being used!
from typing import List
import seaborn as sns
# import libraries and load english language model and word vector model
#from __future__ import print_function
import time
#import umap.umap_ as umap
#import hdbscan
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






print("hello")