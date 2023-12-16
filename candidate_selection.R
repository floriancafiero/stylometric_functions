# candidate_selection Function
candidate_selection <- function(corpus_dir, external_text_path, n = 1000, percent_closest = NULL, percent_farthest = NULL) {
  library(tm)
  library(slam)
  library(data.table)

  # Helper Functions
  slice_text <- function(text, n) {
    words <- unlist(strsplit(text, "\\s+"))
    lapply(seq(1, length(words), by = n), function(i) {
      paste(words[i:min(i+n-1, length(words))], collapse = " ")
    })
  }

  cosine_similarity <- function(vector1, vector2) {
    as.numeric(crossprod_simple_triplet_matrix(vector1, vector2) / 
               (sqrt(col_sums(vector1^2)) * sqrt(col_sums(vector2^2))))
  }

  # Process the corpus
  corpus <- VCorpus(DirSource(corpus_dir, encoding = "UTF-8"), 
                    readerControl = list(language = "en"))
  docs <- lapply(corpus, content)
  slices_corpus <- lapply(docs, slice_text, n = n)

  # Process the external text
  external_text <- readLines(external_text_path)
  slices_external <- slice_text(external_text, n = n)

  # Compute cosine distances
  results <- data.table(doc_id = integer(), slice_id = integer(), external_slice_id = integer(), distance = numeric())

  for (i in seq_along(slices_corpus)) {
    for (j in seq_along(slices_corpus[[i]])) {
      corpus_slice <- DocumentTermMatrix(Corpus(VectorSource(slices_corpus[[i]][j])))
      for (k in seq_along(slices_external)) {
        external_slice <- DocumentTermMatrix(Corpus(VectorSource(slices_external[k])))
        distance <- cosine_similarity(corpus_slice, external_slice)
        results <- rbindlist(list(results, data.table(doc_id = i, slice_id = j, external_slice_id = k, distance = distance)), fill = TRUE)
      }
    }
  }

  # Sort and filter results based on user choice
  setorder(results, -distance)
  if (!is.null(percent_closest)) {
    results <- results[1:(.N * percent_closest / 100)]
  }
  if (!is.null(percent_farthest)) {
    results <- results[.N - (.N * percent_farthest / 100) + 1:.N]
  }

  return(results)
}
