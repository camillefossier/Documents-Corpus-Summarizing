get_iterator <- function(documents) {
  iterator <- itoken(documents,
                     preprocessor=tolower, # replace capital letters
                     tokenizer=word_tokenizer, # split the text into unigrams
                     progressbar=F)
  iterator
}

get_vocabulary <- function(iterator) {
  stop <- stopwords("english")
  stop <- c(stop, "mr", "said")
  vocabulary <- create_vocabulary(iterator, stopwords = stop)
  vocabulary = vocabulary %>% prune_vocabulary(term_count_min = 10)
  vocabulary
}

get_dtm <- function(vocabulary) {
  vectorizer <- vocab_vectorizer(vocabulary)
  dtm = create_dtm(iterator, vectorizer)
  dtm
}

get_dtm_tfidf <- function(dtm) {
  tfidf <- TfIdf$new(smooth_idf = TRUE, sublinear_tf = TRUE)
  dtm.tfidf <- tfidf$fit_transform(dtm)
  dtm.tfidf
}

###############
### BROWSER ###
###############

make.request <- function(phrase) {
  unlist(strsplit(tolower(phrase), " "))
}

vectorize <- function(words, vocab) {
  v <- rep(0, nrow(vocab))
  for (w in words) {
    idx = which(vocab$term == w)
    v[idx] = v[idx] + 1
  }
  v
}

cosinus <- function(u, v) {
  if (all(u == 0) || all(v == 0)) return(0)
  u <- u / sqrt(sum(u ^ 2))
  v <- v / sqrt(sum(v ^ 2))
  c(u %*% v)
}

closest <- function(doc, dtm, k) {
  distance = apply(dtm, 1, function(x) cosinus(x, doc))
  if (k == 0) k <- length(distance)
  ord = order(distance, decreasing = T)
  ord[1:k]
}

search <- function(phrase, corpus, vocab, dtm, k=10) {
  r = vectorize(make.request(phrase), vocab)
  ord <- closest(r, dtm, k)
  ord
}





get_doc_matrices <- function(entity_dataset){
  ent_dataset = entity_dataset[, -2]
  person_matrix= get_doc_matrix(ent_dataset, "PERSON")
  event_matrix = get_doc_matrix(ent_dataset, "EVENT")
  place_matrix = get_doc_matrix(ent_dataset, "GPE")
  
  list(PERSON=person_matrix,EVENT=event_matrix, PLACE=place_matrix)
  
} 

get_doc_matrix <- function(ent_dataset, type){
  require(Matrix)
  
  num_docs = max(unique(ent_dataset$doc_id))
  person_data = ent_dataset[ent_dataset$entity_type == type, ]
  person_count = group(person_data)
  entities = unique(person_count$entity)
  mat = matrix(data = 0, nrow= length(entities), ncol= num_docs )
  rownames(mat) = entities
  for(i in 1:nrow(person_count)){
    line = person_count[i,]
    name = str_split(line[[2]], "\n")[[1]]
    mat[name, line[[1]]] = as.numeric(line[[3]])
  }
  
  res = Matrix(mat,sparse=T)
  res
  
}

group <- function(dataset) {
  dataset %>%
    group_by(doc_id, entity) %>%
    summarise(num = length(entity))
  
}

get_nearest <- function(index, category, number_of_suggestions, list_of_matrix ) {
  mat = list_of_matrix[[category]]
  vec = mat[index,]
  if(sum(vec) == 0)
    return (NULL)
  else {
    res = list()
    for(cat in names(list_of_matrix)){
      domain_matrix = list_of_matrix[[cat]]
      suggestions = domain_matrix %*% vec
      suggestions = suggestions[suggestions != 0]
      ls = length(suggestions)
      if (ls ==0){
        res[[cat]] = c()
      }else {
        all_names  = rownames(domain_matrix)
        names = all_names[order(suggestions, decreasing = T)][1:min(number_of_suggestions,ls) ]
        res[[cat]] = names
      }
    }
    return(res)
  }
}

###############################################################

to_trimestrial_dates <- function(dates) {
  # Setting all days to 1
  dates$mday = rep(1, length(dates))
  
  # Months are stored as a value between 0 and 11
  # We bring them back to a trimestrial value :
  # [0,1,2,3,4,5,6,7,8,9,10,11] ==> [0,0,0,3,3,3,6,6,6,9,9,9]
  dates$mon = unlist(lapply(dates$mon, function(m) floor(m/3 + 0.1) * 3))
  
  dates
}

library(dplyr)
group_by_dates <- function(dataframe, ...) {
  amounts = dataframe %>% 
    group_by(dates) %>%
    summarise(...)
  amounts
}

