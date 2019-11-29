### R scripts for inter-annotator agreement
### and Sentiment Analysis of "Ciàula scopre la luna"

library(XML)
library(irr)
library(tidyverse)
library(reshape2)
library(syuzhet)
library(udpipe)
library(ggpubr)

# Access annotations file

data <- xmlTreeParse("pirandello_novella_full_comments.xml")
paragraphs <- xmlElementsByTagName(data$doc$children[[1]], "paragrafo", recursive = T)

# Save sentiment/comments in separated lists

paragraph_txt <- character()
sentiments_full <- list()
comments_full <- list()
for(i in 1:length(paragraphs)){
  paragraph_txt[i] <- xmlValue(xmlElementsByTagName(paragraphs[[i]], "frase", recursive = T)[[1]])
  sentiments <- xmlElementsByTagName(paragraphs[[i]], "sentiment", recursive = T)
  sentiments_full[[i]] <- as.numeric(unlist(lapply(sentiments, xmlValue)))
  studenti_baroni <- which((!is.na(sentiments_full[[i]]) & sentiments_full[[i]] < -5) | (!is.na(sentiments_full[[i]]) & sentiments_full[[i]] > 5))
  for(studente_barone in studenti_baroni){
    nome_studente_barone <- xmlAttrs(xmlElementsByTagName(paragraphs[[i]], "sentiment", recursive = T)[studente_barone][[1]])
    cat(nome_studente_barone, "ha barato! Valore di sentiment:", sentiments_full[[i]][studente_barone], "\n[valori fuori scala trasformati in NAs]\n")
    sentiments_full[[i]][studente_barone] <- NA
  }
  comments <- xmlElementsByTagName(paragraphs[[i]], "commento", recursive = T)
  comments_full[[i]] <- character()
  for(n in 1:length(comments)){
    if(length(xmlValue(comments[[n]]))>0)
      comments_full[[i]][n] <- xmlValue(comments[[n]])
    if(length(xmlValue(comments[[n]]))==0)
      comments_full[[i]][n] <- ""
  }
  print(i)
}

# Calculate mean sentiment per paragraph

full_sentiment_per_paragraph <- unlist(lapply(sentiments_full, function(x) mean(x, na.rm = T)))

# Statistical analysis (inter-annotator agreement)

# Prepare matrix for calculations
full_annotation_matrix <- matrix(NA, length(sentiments_full[[1]]), length(sentiments_full))
for(i in 1:length(sentiments_full)){
  for(n in 1:length(sentiments_full[[i]])){
    full_annotation_matrix[n,i] <- sentiments_full[[i]][n]
  }
}

kripp.alpha(full_annotation_matrix)

# Simplify matrix: positive and negative 
for(i in 1:length(sentiments_full)){
  for(n in 1:length(sentiments_full[[i]])){
    if(is.na(sentiments_full[[i]][n]))
      next
    if(sentiments_full[[i]][n] < 0)
      full_annotation_matrix[n,i] <- -1
    if(sentiments_full[[i]][n] > 0)
      full_annotation_matrix[n,i] <- 1
    if(sentiments_full[[i]][n] == 0)
      full_annotation_matrix[n,i] <- NA
  }
}

kripp.alpha(full_annotation_matrix)

# Calculate alpha (and p-value) for each group of paragraphs

# Re-calculate matrix
full_annotation_matrix <- matrix(NA, length(sentiments_full[[1]]), length(sentiments_full))
for(i in 1:length(sentiments_full)){
  for(n in 1:length(sentiments_full[[i]])){
    full_annotation_matrix[n,i] <- sentiments_full[[i]][n]
  }
}

p_values <- numeric()
alpha_values <- numeric()

window_dim <- 5
for(i in 1:dim(full_annotation_matrix)[2]){
  # t-test
  test <- t.test(full_annotation_matrix[,i])
  p_values[i] <- test$p.value
  if(i %in% ((1+window_dim):(dim(full_annotation_matrix)[2]-window_dim))){
    tmp_annotation_matrix <- full_annotation_matrix[,(i-window_dim):(i+window_dim)]
    tmp_annotation_matrix[which(tmp_annotation_matrix<0)] <- -1
    tmp_annotation_matrix[which(tmp_annotation_matrix>0)] <- 1
    tmp_annotation_matrix[which(tmp_annotation_matrix==0)] <- NA
    alpha <- kripp.alpha(tmp_annotation_matrix)
    alpha_values[i] <- alpha$value
    print(i)
  }
}

plot(p_values, type = "l")
plot(alpha_values, type = "l")

# Good plots

plot_proportion <- 0.4

# plot 1 (sentiment annotations)
data <- as.data.frame(full_annotation_matrix)
colnames(data) <- 1:dim(full_annotation_matrix)[2]
data <- melt(data)
colnames(data) <- c("paragraph","sentiment")

p <- ggplot(data, aes(x=paragraph,y=sentiment)) +
  geom_count(aes(alpha = ..n.., size = ..n..)) +
  guides(color = 'legend') +
  scale_x_discrete(breaks = seq(0,111,10)) +
  scale_y_continuous(breaks = seq(-5,5,1), minor_breaks = seq(-5,5,0.5))
p
ggsave(plot = p, filename = "Sentiment_annotations.png", width = 16*plot_proportion, height = 9*plot_proportion, dpi = 300)

# plot 2 (alphas with moving window)
data <- data.frame(paragraph = 1:dim(full_annotation_matrix)[2], alpha = c(alpha_values,rep(NA,window_dim)))

p <- ggplot(data, aes(x=paragraph,y=alpha)) +
  geom_line() +
  scale_x_continuous(minor_breaks = seq(0,111,5), breaks = seq(0,111,10))
p

ggsave(plot = p, filename = "Sentiment_alphas.png", width = 16*plot_proportion, height = 9*plot_proportion, dpi = 300)

#########
### Part 2. Sentiment analysis
#########

udmodel_italian <- udpipe_load_model(file = "resources/italian-isdt-ud-2.3-181115.udpipe")

# Prepare Sentix dictionary

custom_dictionary <- read.csv("resources/sentix", sep = "\t", header = F, stringsAsFactors = F)

colnames(custom_dictionary) <- c("lemma", "POS", "Wordnet_synset_ID", "positive_score", "negative_score", "polarity", "intensity")

exclude <- which(grepl("_", custom_dictionary$lemma))
custom_dictionary <- custom_dictionary[-exclude,]
exclude <- which(custom_dictionary$lemma == "")
custom_dictionary <- custom_dictionary[-exclude,]

txt <- sapply(custom_dictionary$lemma, FUN=function(x) paste(x, collapse = "\n"))
x <- udpipe_annotate(udmodel_italian, x = txt, tokenizer = "vertical")
x <- as.data.frame(x)
for(i in 1:length(x$doc_id)){
  if(x$token[i] != custom_dictionary$lemma[i]){
    print(i)
    break
  }
}

custom <- data.frame(word = tolower(x$lemma), value=custom_dictionary$polarity*custom_dictionary$intensity, stringsAsFactors = F)

Sentix <- with(custom, aggregate(list(value = value), list(word = word), mean))

Sentix <- Sentix[-(1:20),]

# Prepare OpeNER dictionary

data <- xmlTreeParse("resources/it-sentiment_lexicon.lmf.xml")
entries <- xmlElementsByTagName(data$doc$children[[1]], "LexicalEntry", recursive = T)

full_lemmas <- character()
full_polarities <- character()
full_confidences <- numeric()
for(entry in entries){
  lemma <- xmlChildren(entry)[[1]]
  sense <- xmlChildren(entry)[[2]]
  polarity <- xmlChildren(sense)[[2]]
  confidence <- xmlChildren(sense)[[1]]
  if(length(xmlAttrs(lemma))==0){
    full_lemmas <- c(full_lemmas, NA)
    print("no lemma")
  }
  full_lemmas <- c(full_lemmas, xmlAttrs(lemma))
  if(length(xmlAttrs(polarity))==0){
    full_polarities <- c(full_polarities, NA)
    print("no polarity")
  }
  full_polarities <- c(full_polarities, xmlAttrs(polarity))
  if(length(xmlAttrs(confidence))==0){
    full_confidences <- c(full_confidences, NA)
    print("no confidence")
  }
  full_confidences <- c(full_confidences, as.numeric(xmlAttrs(confidence)[[1]]))
}

OpeNER <- data.frame(word = full_lemmas, value = full_polarities, confidence = full_confidences, stringsAsFactors = F)

OpeNER <- OpeNER[-which(is.na(OpeNER$value)),]
OpeNER <- OpeNER[-which(OpeNER$value == "neutral"),]
OpeNER$value[which(OpeNER$value == "positive")] <- 1
OpeNER$value[which(OpeNER$value == "negative")] <- -1
OpeNER$value <- as.numeric(OpeNER$value)*OpeNER$confidence
OpeNER[which(is.na(OpeNER$value)),]
OpeNER <- OpeNER[-which(is.na(OpeNER$value)),]
OpeNER$confidence <- NULL

# function to prepare text with udPipe
prepare_text <- function(text_string, chosen_language = "Italian"){
  x <- udpipe_annotate(udmodel_italian, x = text_string)
  x <- as.data.frame(x)
  my_text <- paste(x$lemma, collapse = " ")
  return(my_text)
}

# Sentiment analysis with Sentix

comments_sa_matrix <- matrix(NA, length(comments_full[[1]]), length(comments_full))
for(i in 1:length(comments_full)){
  for(n in 1:length(comments_full[[i]])){
    my_text <- prepare_text(comments_full[[i]][n])
    comments_sa_matrix[n,i] <- get_sentiment(my_text, method = "custom", lexicon = Sentix)
  }
  print(i)
}

# Calculate mean sentiment per comments
full_sentiment_per_comments_sentix <- colMeans(comments_sa_matrix, na.rm = T)
plot(full_sentiment_per_comments_sentix, type = "l")

# Repeat analysis with OpeNER

comments_sa_matrix <- matrix(NA, length(comments_full[[1]]), length(comments_full))
for(i in 1:length(comments_full)){
  for(n in 1:length(comments_full[[i]])){
    my_text <- prepare_text(comments_full[[i]][n])
    comments_sa_matrix[n,i] <- get_sentiment(my_text, method = "custom", lexicon = OpeNER)
  }
  print(i)
}

# Calculate mean sentiment per comments
full_sentiment_per_comments_opener <- colMeans(comments_sa_matrix, na.rm = T)

plot(full_sentiment_per_comments_opener, type = "l")

# Calculate sentiment of text and compare

full_sa_per_paragraph_sentix <- numeric()
full_sa_per_paragraph_opener <- numeric()
for(paragraph in paragraph_txt){
  print(paragraph)
  my_text <- prepare_text(paragraph)
  full_sa_per_paragraph_sentix <- c(full_sa_per_paragraph_sentix, get_sentiment(my_text, method = "custom", lexicon = Sentix))
  full_sa_per_paragraph_opener <- c(full_sa_per_paragraph_opener,get_sentiment(my_text, method = "custom", lexicon = OpeNER))
}

plot(full_sa_per_paragraph_sentix, type = "l")
plot(full_sa_per_paragraph_opener, type = "l")

cor.test(full_sentiment_per_paragraph, full_sa_per_paragraph_sentix)
cor.test(full_sentiment_per_paragraph, full_sa_per_paragraph_opener)
cor.test(full_sentiment_per_paragraph, full_sentiment_per_comments_opener)
cor.test(full_sentiment_per_paragraph, full_sentiment_per_comments_sentix)

# plots with rolling mean
###function for rolling plot (taken from https://github.com/mjockers/syuzhet/blob/master/R/syuzhet.R)
rolling_plot <- function (raw_values, window = 0.1){
  wdw <- round(length(raw_values) * window)
  rolled <- rescale(zoo::rollmean(raw_values, k = wdw, fill = 0))
  half <- round(wdw/2)
  rolled[1:half] <- NA
  end <- length(rolled) - half
  rolled[end:length(rolled)] <- NA
  return(rolled)
}

full_sentiment_per_paragraph_rp <- rolling_plot(full_sentiment_per_paragraph)
full_sa_per_paragraph_sentix_rp <- rolling_plot(full_sa_per_paragraph_sentix)
full_sa_per_paragraph_opener_rp <- rolling_plot(full_sa_per_paragraph_opener)
full_sentiment_per_comments_sentix_rp <- rolling_plot(full_sentiment_per_comments_sentix)
full_sentiment_per_comments_opener_rp <- rolling_plot(full_sentiment_per_comments_opener)

plot(full_sentiment_per_paragraph_rp, type = "l")
plot(full_sa_per_paragraph_sentix_rp, type = "l")
plot(full_sa_per_paragraph_opener_rp, type = "l")
plot(full_sentiment_per_comments_sentix_rp, type = "l")
plot(full_sentiment_per_comments_opener_rp, type = "l")

cor.test(full_sa_per_paragraph_opener, full_sa_per_paragraph_sentix)
cor.test(full_sentiment_per_comments_opener, full_sentiment_per_comments_sentix)

# Good plots
plot_proportion <- 0.7

# plot 1 (paragraphs)
data <- data.frame(
  manual_annotation = full_sentiment_per_paragraph/sum(abs(full_sentiment_per_paragraph)), 
  paragraph_Sentix = full_sa_per_paragraph_sentix/sum(abs(full_sa_per_paragraph_sentix)),
  paragraph_OpeNER = full_sa_per_paragraph_opener/sum(abs(full_sa_per_paragraph_opener)))
data <- melt(data)
colnames(data) <- c("method", "sentiment")
data$paragraph <- rep(1:length(full_sentiment_per_paragraph), length(unique(data$method)))

p1 <- ggplot(data, aes(x=paragraph, y=sentiment, group=method, color=method, linetype=method)) +
  geom_line()+
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_linetype_manual(values=c("dashed","solid","solid")) +
  scale_x_continuous(minor_breaks = seq(0,111,5), breaks = seq(0,111,10))
p1

# plot 2 (comments)
data <- data.frame(
  manual_annotation = full_sentiment_per_paragraph/sum(abs(full_sentiment_per_paragraph)), 
  comments_Sentix = full_sentiment_per_comments_sentix/sum(abs(full_sentiment_per_comments_sentix)), 
  comments_OpeNER = full_sentiment_per_comments_opener/sum(abs(full_sentiment_per_comments_opener)))
data <- melt(data)
colnames(data) <- c("method", "sentiment")
data$paragraph <- rep(1:length(full_sentiment_per_paragraph), length(unique(data$method)))

p2 <- ggplot(data, aes(x=paragraph, y=sentiment, group=method, color=method, linetype=method)) +
  geom_line()+
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_linetype_manual(values=c("dashed","solid","solid")) +
  scale_x_continuous(minor_breaks = seq(0,111,5), breaks = seq(0,111,10))
p2

# both plots in a single figure
figure <- ggarrange(p1, p2 + font("x.text", size = 10),
                    ncol = 2, nrow = 1, align = "v")

ggsave(figure, filename = "Sentiment_analysis_paragraph_comments.png", width = 18*plot_proportion, height = 6*plot_proportion, dpi = 300)

# ...with rolling mean

# plot 1 (paragraphs)
data <- data.frame(
  manual_annotation = full_sentiment_per_paragraph_rp/sum(abs(full_sentiment_per_paragraph_rp), na.rm = T), 
  paragraph_Sentix = full_sa_per_paragraph_sentix_rp/sum(abs(full_sa_per_paragraph_sentix_rp), na.rm = T),
  paragraph_OpeNER = full_sa_per_paragraph_opener_rp/sum(abs(full_sa_per_paragraph_opener_rp), na.rm = T))
data <- melt(data)
colnames(data) <- c("method", "sentiment")
data$paragraph <- rep(1:length(full_sentiment_per_paragraph_rp), length(unique(data$method)))

p3 <- ggplot(data, aes(x=paragraph, y=sentiment, group=method, color=method, linetype=method)) +
  geom_line()+
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_linetype_manual(values=c("dashed","solid","solid")) +
  scale_x_continuous(minor_breaks = seq(0,111,5), breaks = seq(0,111,10))
p3


# plot 2 (comments)
data <- data.frame(
  manual_annotation = full_sentiment_per_paragraph_rp/sum(abs(full_sentiment_per_paragraph_rp), na.rm = T), 
  comments_Sentix = full_sentiment_per_comments_sentix_rp/sum(abs(full_sentiment_per_comments_sentix_rp), na.rm = T), 
  comments_OpeNER = full_sentiment_per_comments_opener_rp/sum(abs(full_sentiment_per_comments_opener_rp), na.rm = T))
data <- melt(data)
colnames(data) <- c("method", "sentiment")
data$paragraph <- rep(1:length(full_sentiment_per_paragraph_rp), length(unique(data$method)))

p4 <- ggplot(data, aes(x=paragraph, y=sentiment, group=method, color=method, linetype=method)) +
  geom_line()+
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_linetype_manual(values=c("dashed","solid","solid")) +
  scale_x_continuous(minor_breaks = seq(0,111,5), breaks = seq(0,111,10))
p4

# both plots in a single figure
figure <- ggarrange(p3, p4 + font("x.text", size = 10),
                    ncol = 2, nrow = 1, align = "v")

ggsave(figure, filename = "Sentiment_analysis_paragraph_comments_rm.png", width = 18*plot_proportion, height = 6*plot_proportion, dpi = 300)