
corpus_path = "../data/nyt.csv"
dtm_path = "../data/dtm.mtx"
dtm_colnames_path = "../data/colnames.csv"
entities_path = "../data/full_entities.csv"
topics_path = "../data/topics_probabilities.csv"
entity_matrices_root_path = "../data/entity_matrices/"




load_dtm <- function(){
  mat <- read_sparse(dtm_path, dtm_colnames_path)
  mat
}

read_sparse <- function(mat_path, names_path, row = F) {
  require(Matrix)
  mat = readMM(mat_path)
  names_data = read.csv2(names_path)
  names = names_data[,2]
  if(row)
    rownames(mat) = names
  else 
    colnames(mat) = names
  mat
}


load_entity_matrices <- function(){
  res = list()
  files = list.files(entity_matrices_root_path)
  names = unique( unlist(lapply(files, function(x) str_split(x, "\\.")[[1]][1])))
  for (name in names) {
    mat_path = paste(entity_matrices_root_path,name,".mtx", sep="")
    mat_name_path = paste(entity_matrices_root_path,name,".csv", sep="")
    mat = read_sparse(mat_path, mat_name_path, row=T)
    res[[name]] = mat
  }
  res
}

get_iterator <- function(documents) {
  iterator <- itoken(documents,
                     preprocessor=tolower, # replace capital letters
                     tokenizer=word_tokenizer, # split the text into unigrams
                     progressbar=F)
  iterator
}

get_vocabulary <- function(iterator) {
  stop <- stopwords("english")
  stop <- c(stop, "mr", "said", "lead")
  vocabulary <- create_vocabulary(iterator, stopwords = stop)
 # vocabulary = vocabulary %>% prune_vocabulary(term_count_min = 10)
  vocabulary
}

get_dtm <- function(vocabulary,iterator,tfidf = T) {
  require(text2vec)
  vectorizer <- vocab_vectorizer(vocabulary)
  dtm = create_dtm(iterator, vectorizer)
  if(tfidf){
    tfidf <- TfIdf$new(smooth_idf = TRUE, sublinear_tf = TRUE)
    res = tfidf$fit_transform(dtm)
  }else{
    res = dtm
  }
  res
}

get_dtm_tfidf <- function(dtm) {
  tfidf <- TfIdf$new(smooth_idf = TRUE, sublinear_tf = TRUE)
  dtm.tfidf <- tfidf$fit_transform(dtm)
  dtm.tfidf
}

# Finding most relevant articles for each topic
relevant_topic_documents <- function(topics, documents, n=10) {
  ordered_articles = apply(topics, 2, function(t) order(t, decreasing = T))
  titles = apply(ordered_articles[1:n,], 2, function(a) documents[a])
  titles
}

###############
### BROWSER ###
###############

# Splits a string request on whitespaces to make a vector
make.request <- function(phrase) {
  unlist(strsplit(tolower(phrase), " "))
}

# Vectorizes request according to existing vocabulary
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





get_doc_matrices <- function(entity_dataset, tfidf=F){
  ent_dataset = entity_dataset[, -2]
  person_matrix= get_doc_matrix(ent_dataset, "PERSON",tfidf)
  event_matrix = get_doc_matrix(ent_dataset, "EVENT",tfidf)
  place_matrix = get_doc_matrix(ent_dataset, "GPE",tfidf)
  
  list(PERSON=person_matrix,EVENT=event_matrix, PLACE=place_matrix)
  
} 

get_doc_matrix <- function(ent_dataset, type,tfidf){
  require(Matrix)
  require(stringr)
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
  if(tfidf){
    count_docs = rowSums(mat >=1)
    idf = log(ncol(mat) / count_docs)
    mat = mat * idf
    
  }
  res = Matrix(mat,sparse=T)
  res
  
}

group <- function(dataset) {
  dataset %>%
    group_by(doc_id, entity) %>%
    summarise(num = length(entity))
  
}

search <- function(search_expression, category, date_min = NULL, date_max=NULL, number_of_suggestions= 15, list_of_matrices, list_of_dates){
 dates = filter_dates(list_of_dates, date_min, date_max)
 if(length(dates) == 0){
   stop("no articles on those dates")
 }
 cat_mat =list_of_matrices[[category]]
 if(is.null(cat_mat))
   stop("Not a valid category")
 
 words = rownames(cat_mat)
 index = search_key_word(search_expression, words)
 if(index == 0)
    stop("no match found")
 
 new_matrices_list = list()
 get_nearest(index, category, number_of_suggestions, list_of_matrices, dates)
 
 
}

search_key_word <- function(search_expression, words){
  #return an index
  #0 if doesn't match
  
}
get_nearest <- function(index, category, number_of_suggestions, list_of_matrix, colindexes ) {
  mat = list_of_matrix[[category]][, colindexes]
  vec = mat[index,]
  if(sum(vec) == 0)
    return (NULL)
  else {
    res = list()
    for(cat in names(list_of_matrix)){
      domain_matrix = list_of_matrix[[cat]][, colindexes]
      suggestions = domain_matrix %*% vec
      mat_rownorms = apply(domain_matrix, 1, function(v) sqrt(sum(v ^ 2)))
      vec_norm = sqrt(sum(vec^2))
      suggestions = suggestions / (mat_rownorms * vec_norm)
      suggestions = suggestions[suggestions != 0 & is.finite(suggestions)]
      ls = length(suggestions)
      if (ls ==0){
        res[[cat]] = c()
      }else {
        all_names  = rownames(domain_matrix)
        names = all_names[order(suggestions, decreasing = T)][1:min(number_of_suggestions,ls) ]
        res[[cat]] = names
      }
    }
    
    #TODO : add articles
    return(res)

  }
}



count_words <- function(x){
  require(text2vec)
  length(unlist(word_tokenizer(x)))
}

# Return indices of dates which are in the range : date_min <= d < date_max
filter_dates <- function(dates, date_min = NULL, date_max = NULL) {
  dates = as.Date(dates)
  if (is.null(date_min)) date_min = min(dates) else date_min = as.Date(date_min)
  if (is.null(date_max)) date_max = max(dates) else date_max = as.Date(date_max)
  which(date_min <= dates & dates < date_max)
}

filter_entity <- function(entities, types=NULL, tokens=NULL, indices=NULL, entities_only=F) {
  if (!is.null(indices)) {
    entities = entities[entities$doc_id %in% indices,]
  }
  if (!is.null(types)) {
    entities = entities[entities$entity_type %in% types,]
  }
  if (!is.null(tokens)) {
    entities = entities[entities$entity %in% tokens,]
  }
  
  if (entities_only) {
    entities
  }
  else {
    unique(entities$doc_id)
  }
}

###############################################################

to_monthly_dates <- function(dates) {
  
  # Setting all days to 1
  dates$mday = rep(1, length(dates))
  dates
}

# Groups every dates in a vector of POSIX dates, that are in the same trimester
to_trimestrial_dates <- function(dates) {
  
  dates = to_monthly_dates(dates)
  
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

find_word <- function(word, list_words) {
  grep(word, list_words, fixed=T)
}


