library(lda)
library(LDAvis)
library(topicmodels)
library(servr)

doc.matrix = read.table("text_data.txt",header = T)

topic_true = read.table("topic_doc_true.txt")

vocab = colnames(doc.matrix)

doc.length = apply(doc.matrix,1,function(x) sum(x))

term.frequency = colSums(doc.matrix)

documents = lapply(as.data.frame(t(doc.matrix)),function(x) as.matrix(rbind(0:99,x)))


K = 3
G = 3000
alpha = 0.05
eta = 0.02

set.seed(111)

lda.fit = lda.collapsed.gibbs.sampler(documents = documents,
                                  K = K, 
                                  vocab = vocab, 
                                  num.iterations = G, 
                                  alpha = alpha, 
                                  eta = eta, 
                                  initial = NULL, 
                                  burnin = 0,
                                  compute.log.likelihood = TRUE)

theta = t(apply(lda.fit$document_sums + alpha, 2, function(x) x/sum(x)))
# t(theta)

phi = t(apply(t(lda.fit$topics) + eta, 2, function(x) x/sum(x)))
phi

topic_true
t(theta)

true_main_topic = apply(topic_true,2,which.max)
estimate_main_topic = apply(t(theta),2,which.max)
table(true_main_topic,estimate_main_topic)
