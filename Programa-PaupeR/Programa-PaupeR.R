### Libraries
library(tokenizers) 
library(tidyverse)
library(readxl)
library(janitor)


### Extra

partial_match <- function (pattern, string) {
  
  exact <- which(string %in% pattern)
  
  if (length(exact) != 0){
    names(exact) <- string[exact]
  }
  
  partial <- sapply(pattern, function (p) grep(p, string, ignore.case = TRUE), simplify = TRUE)
  partial <- unlist(partial)
  
  tab <- table(partial)
  
  if (any(as.numeric(tab) > 1)){
    index.duplicated <- which(duplicated(partial) | duplicated(partial, fromLast = TRUE))
    word.length <- str_count(names(partial[index.duplicated]))
    index.keep <- which.max(word.length)
    index.diff <- setdiff(index.duplicated, index.keep)
    partial <- partial[-index.diff]
  } 
  
  if (length(exact) != 0){
    add <- which(!(partial %in% exact))
  } else {
    add <- integer(0)
  }
  
  if (length(add) != 0){
    out <- c(exact, partial[add])
  } else if (length(exact) != 0){
    out <- exact
  } else {
    out <- partial
  }
  
  return(out)
}

### Data
dt <- read_xlsx(path = " ", sheet = "Data") %>%
  clean_names() 
glimpse(dt)

lexicon <- read_xlsx(path = " ", sheet = "Lexicon") %>%
  clean_names()



### Step by step

# Initialize objects to be used in the step by step
citations <- dt$citacao
words_list <- lexicon$termo
data <- dt
lexicon_table <- lexicon

# Breaking citations into words
# Read more: https://smltar.com/tokenization.html#what-is-a-token
#list_citations <- tokenize_words(citations, lowercase = FALSE)

#Manual tokenizacao
list_citations <- str_split(citations, pattern = " ")

#initialize vectors
duplicate <- rep(NA, length(citations))
repeated<- rep(NA, length(citations))
anterior <- rep(NA, length(citations))
posterior <- rep(NA, length(citations))
termo_original <- rep(NA, length(citations))
termo <- rep(NA, length(citations))
index <- NULL

j <- 1
for (i in 1:length(citations)){
  
  #Manual tokenization
  list_citations[[i]] <- str_remove_all(list_citations[[i]], pattern = "[[:punct:]]")
  
  # Uppercase to lowercase
  list_citations[[i]] <- str_to_lower(list_citations[[i]])
  
  #index of the word
  #exact match which(list_citations[[i]] %in% words_list)
  index <- partial_match(words_list, list_citations[[i]])
  index <- unlist(index)
  
  
  # Case 1: There is one word from the lexicon
  if (length(index) == 1){
    termo[j] <- str_remove(names(index), "[[:digit:]]")
    termo_original[j] <- list_citations[[i]][index]
    duplicate[j] <- "Nao"
    repeated[j] <- 1
    
    # We will take the prior word only if termo is not the first word
    if ((index-1) > 0){
      anterior[j] <- list_citations[[i]][(index - 1)]
    } else {
      anterior[j] <- NA
    }
    
    # We will take the posterior if termo is not the last word
    if ((index+1) <= length(list_citations[[i]])){
      posterior[j] <- list_citations[[i]][(index + 1)]
    } else {
      posterior[j] <- NA
    }
    
    # Case 2: There is more than one word from the lexicon
  } else if (length(index) > 1) {
    termo[j:(j+length(index)-1)] <- str_remove(names(index), "[[:digit:]]")
    termo_original[j:(j+length(index)-1)] <- list_citations[[i]][index] 
    duplicate[j:(j+length(index)-1)] <- "Sim"
    repeated[j:(j+length(index)-1)] <- 1:length(index)
    
    # We will duplicate a line in the dataset to accomodate more than
    # one word from the lexicon
    data <- data %>%
      add_row(data[rep(j,length(index)-1), ], .after = j)
    
    anterior[j:(j+length(index)-1)] <- 
      ifelse (index - 1 > 0, 
              list_citations[[i]][(index - 1)], NA)
    
    posterior[j:(j+length(index)-1)] <- 
      ifelse ((index+1) <= length(list_citations[[i]]),
              list_citations[[i]][(index + 1)], NA)
    
    
    j <- j + length(index)-1
    
    # Case 3: There is zero words from the lexicon. Probably, there is
    # an issue with the text from the citation
  } else {
    
    termo[j] <- NA
    termo_original[j] <- NA
    duplicate[j] <- NA
    repeated[j] <- NA
    anterior[j] <- NA
    posterior[j] <- NA
    
  }
  
  j <- j + 1
  index <- NULL
  
}

updated_data <- bind_cols(data, 
                          termo_original = termo_original,
                          termo = termo,
                          palavra_anterior = anterior, 
                          palavra_posterior = posterior,
                          duplicate = duplicate) %>%
  left_join(lexicon_table, by = "termo")

# Writing an Excel file
# Change to write.csv2 if Excel is in Portuguese
write_excel_csv2(updated_data, file = "updated_dataset2.csv2")


### Making a function, then you do not have to repeat the code above all the time.


extract_text <- function(data, 
                         lexicon_table,
                         citations,
                         words_list,
                         save = TRUE,
                         file = "updated_dataset"){

  # Read more: https://smltar.com/tokenization.html#what-is-a-token
  #list_citations <- tokenize_words(citations, lowercase = FALSE)
  
  #Manual tokenization
  list_citations <- str_split(citations, pattern = " ")
  
  #initialize vectors
  duplicate <- rep(NA, length(citations))
  repeated<- rep(NA, length(citations))
  anterior <- rep(NA, length(citations))
  posterior <- rep(NA, length(citations))
  termo_original <- rep(NA, length(citations))
  termo <- rep(NA, length(citations))
  index <- NULL
  
  j <- 1
  for (i in 1:length(citations)){
    
    #Manual tokenizacao
    list_citations[[i]] <- str_remove_all(list_citations[[i]], pattern = "[[:punct:]]")
    
    # Uppercase to lowercase
    list_citations[[i]] <- str_to_lower(list_citations[[i]])
    
    #index of the word
    #exact match which(list_citations[[i]] %in% words_list)
    index <- partial_match(words_list, list_citations[[i]])
    index <- unlist(index)
    
    
    # Case 1: There is one word from the lexicon
    if (length(index) == 1){
      termo[j] <- str_remove(names(index), "[[:digit:]]")
      termo_original[j] <- list_citations[[i]][index]
      duplicate[j] <- "Não"
      repeated[j] <- 1
      
      # We will take the prior word only if termo is not the first word
      if ((index-1) > 0){
        anterior[j] <- list_citations[[i]][(index - 1)]
      } else {
        anterior[j] <- NA
      }
      
      # We will take the posterior if termo is not the last word
      if ((index+1) <= length(list_citations[[i]])){
        posterior[j] <- list_citations[[i]][(index + 1)]
      } else {
        posterior[j] <- NA
      }
      
      # Case 2: There is more than one word from the lexicon
    } else if (length(index) > 1) {
      termo[j:(j+length(index)-1)] <- str_remove(names(index), "[[:digit:]]")
      termo_original[j:(j+length(index)-1)] <- list_citations[[i]][index] 
      duplicate[j:(j+length(index)-1)] <- "Sim"
      repeated[j:(j+length(index)-1)] <- 1:length(index)
      
      # We will duplicate a line in the dataset to accomodate more than
      # one word from the lexicon
      data <- data %>%
        add_row(data[rep(j,length(index)-1), ], .after = j)
      
      anterior[j:(j+length(index)-1)] <- list_citations[[i]][(index - 1)]
      posterior[j:(j+length(index)-1)] <- list_citations[[i]][(index + 1)]
      j <- j + length(index)-1
      
      # Case 3: There is zero words from the lexicon. Probably, there is
      # an issue with the text from the citation
    } else {
      
      termo[j] <- NA
      termo_original[j] <- NA
      duplicate[j] <- NA
      repeated[j] <- NA
      anterior[j] <- NA
      posterior[j] <- NA
      
    }
    
    j <- j + 1
    index <- NULL
    
  }
  
  out <- bind_cols(data, 
                   termo_original = termo_original,
                   termo = termo,
                   palavra_anterior = anterior, 
                   palavra_posterior = posterior,
                   duplicate = duplicate) %>%
    left_join(lexicon_table, by = "termo")
  
  if (save)
    # Writing an Excel file
    # Change to write.csv2 if Excel is in Portuguese
    write.csv(out, file = paste0(file, ".csv"))
  
  return(out)
}

### Testing function

test <- extract_text(data = dt_unique,
                     lexicon_table = lexicon,
                     citations = dt_unique$citacao,
                     words_list = lexicon$termo,
                     save = TRUE,
                     file = "updated_dataset")


## Checking inconsistencies
temp <- test %>% select(id, repeated, termo, palavra_anterior,
                        palavra_posterior, duplicate)

out <- left_join(dt, temp, by = c("id", "repeticao" = "repeated")) %>%
  mutate(termo_inconsistencia =
           ifelse(termo_manual != termo, 
                  "Sim", "Nao"),
         palavra_anterior_inconsistencia =
           ifelse(palavra_anterior_manual != palavra_anterior, 
                  "Sim", "Nao"),
         palavra_posteriorerior_inconsistencia =
           ifelse(palavra_posterior_manual != palavra_posterior, 
                  "Sim", "Nao")) 


# Writing an Excel file to check inconsistencies
# Change to write.csv2 if Excel is in Portuguese
write.csv(out, file = "test.csv")

