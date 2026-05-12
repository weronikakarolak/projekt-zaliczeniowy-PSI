#' ---
#' title: "Klastrowanie i asocjacje w zbiorze prac naukowych"
#' author: "Autor: Jan Jaworski, Weronika Karolak, Mikołaj Dąbrowski "
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: journal      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    # Kod domyślnie zwinięty (estetyczniej)
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#'     css: "custom.css"     # Możliwość stworzenia własnego stylowania (opcjonalne)
#' ---

# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury słów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele


# Dane tekstowe ----


#Ładowanie danych z pliku csv

data <- read.csv("ai_detection_dataset.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Usuwamy wiersze, które nie mają tekstu lub mają duplikaty tytułów
data <- data[data$abstract != "" & !is.na(data$abstract), ]
data <- data[!duplicated(data$title), ]
 df_for_corpus <- data %>%
  select(doc_id = title, text = abstract)
 
 # Utwórz korpus dokumentów tekstowych
 corpus <- VCorpus(DataframeSource(df_for_corpus)) 


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


#1.1 Normalizacja i usunięcie zbędnych znaków ----


# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))



# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")

 #zamiana slowa models na model
  toModel <- content_transformer(function(x) {
   gsub("\\bmodels\\b", "model", x)
  })
 
 corpus <- tm_map(corpus, toModel)


# Sprawdzenie
# corpus[[1]][[1]][7:9]



corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
# corpus[[1]][[1]][7:9]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, c("know","can","well","just","will","get","come","yes","no","now"))
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
# corpus[[1]][[1]][7:9]

corpus_completed <- corpus



#1.2 Tokenizacja ----


#1.3 Macierze częstości TDM i DTM ----


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus_completed)
tdm
# inspect(tdm)


tdm_m <- as.matrix(tdm)



# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
# inspect(dtm)

dtm_m <- as.matrix(dtm)


#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)

# Można zliczyć same częstości słów w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)


#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, max.words = 20, 
          colors = brewer.pal(8, "Dark2"), scale = c(3, 0.8), random.order = FALSE, rot.per = 0.1)


# Wyświetl top 10
print(head(tdm_df, 10))



#' #  4. Klastrowanie i model Bag of Words:
# 4. Klastrowanie i model Bag of Words: ----
#4.1 Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



# Użyj utworzonej wcześniej macierzy DTM
dtm

# inspect(dtm)

dtm_m[1:5, 1:5]




#4.2 Klastrowanie k-średnich (k-means) ----


# Dobór liczby klastrów
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")



# Wykonaj klastrowanie kmeans
# (sprawdź wyniki dla k = 3,4,5)
set.seed(99) # ziarno losowe dla replikacji wyników



# i) Liczba klastrów k = 2 ----
k <- 2 # ustaw liczbę klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 słów
# Dla każdego klastra: liczba dokumentów oraz top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Połącz wszystko w ramkę danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury słów dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadające dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
    # generuj chmurę słów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"),scale = c(3, 0.8), random.order = FALSE, rot.per = 0.1)
  title(paste("Chmura słów - Klaster", i))
}




# a) Przypisanie dokumentów do klastrów
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
# print(documents_clusters)


# b) Wizualizacja przypisania dokumentów do klastrów
ggplot(documents_clusters, aes(x = Klaster, fill = Klaster)) +
  geom_bar(width = 0.7) +
  labs(
    title = "Liczba dokumentów w poszczególnych klastrach",
    subtitle = "Wynik klastrowania dla k = 2",
    x = "Klaster",
    y = "Liczba dokumentów",
    fill = "Klaster"
  ) +
  theme_minimal(base_size = 13)

# ii) Liczba klastrów k = 3 ----
k <- 3 # ustaw liczbę klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 słów
# Dla każdego klastra: liczba dokumentów oraz top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Połącz wszystko w ramkę danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury słów dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadające dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurę słów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"),scale = c(3, 0.8), random.order = FALSE, rot.per = 0.1)
  title(paste("Chmura słów - Klaster", i))
}




# a) Przypisanie dokumentów do klastrów
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
# print(documents_clusters)


# b) Wizualizacja przypisania dokumentów do klastrów
ggplot(documents_clusters, aes(x = Klaster, fill = Klaster)) +
  geom_bar(width = 0.7) +
  labs(
    title = "Liczba dokumentów w poszczególnych klastrach",
    subtitle = "Wynik klastrowania dla k = 3",
    x = "Klaster",
    y = "Liczba dokumentów",
    fill = "Klaster"
  ) +
  theme_minimal(base_size = 13)





# iii) Liczba klastrów k = 4 ----
k <- 4 # ustaw liczbę klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 słów
# Dla każdego klastra: liczba dokumentów oraz top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Połącz wszystko w ramkę danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury słów dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadające dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurę słów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"),scale = c(3, 0.8), random.order = FALSE, rot.per = 0.1)
  title(paste("Chmura słów - Klaster", i))
}




# a) Przypisanie dokumentów do klastrów
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
# print(documents_clusters)


# b) Wizualizacja przypisania dokumentów do klastrów
ggplot(documents_clusters, aes(x = Klaster, fill = Klaster)) +
  geom_bar(width = 0.7) +
  labs(
    title = "Liczba dokumentów w poszczególnych klastrach",
    subtitle = "Wynik klastrowania dla k = 4",
    x = "Klaster",
    y = "Liczba dokumentów",
    fill = "Klaster"
  ) +
  theme_minimal(base_size = 13)
#' # 5. Szukanie asocjacji
# 5. Szukanie asocjacji ----

findAssocs(tdm,"health",0.5)
findAssocs(tdm,"vital",0.5)
findAssocs(tdm,"methods",0.5)
findAssocs(tdm,"app",0.5)
# Wizualizacja asocjacji
# 5.1 Wizualizacja asocjacji ----


# Wytypowane słowo i próg asocjacji
target_word <- "health"
cor_limit <- 0.5


# Oblicz asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)



# Wykres lizakowy z natężeniem
# na podstawie wartości korelacji score:
ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_color_gradient(low = "#a6bddb", high = "#08306b") +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )

