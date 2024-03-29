


##---- Similarity function ----
stock.dict <- data.frame(code = c(0:9, LETTERS), 
                         num = 0:35)

StrSim <- function(ssc1, ssc2, weight = NULL) {
  
  ssc1.split <- as.character(str_split(ssc1, '', simplify = TRUE))
  ssc2.split <- as.character(str_split(ssc2, '', simplify = TRUE))
  
  len1 <- length(ssc1.split)
  len2 <- length(ssc2.split)
  
  if (is.null(weight)) {
    weight <- 1 / len1
    
  } else if (length(weight) < len1) {
    warning('Append zeros to weight vector.')
    weight.append <- rep(0, len1 - length(weight))
    weight <- append(weight, weight.append)
    
  } else if (length(weight) > len1) {
    warning('Cut the weight vector to ssc1 length.')
    weight <- weight[1:len1]
  }
  
  id <- c()
  
  if (len1 == 0 || len2 == 0) {
    similarity <- 0
    
  } else if (len1 == 1 || len2 == 1) {
    similarity <- as.integer(all(ssc1.split == ssc2.split))
    
  } else {
    for (i in 1:(len1 - 1)) {
      if (ssc1.split[i] == ssc2.split[i]) {
        id <- append(id, 1)
      } else {
        id <- append(id, 0)
      }
    }
    
    id.num <- 1 - abs(stock.dict$num[stock.dict$code == ssc1.split[len1]] - 
                        stock.dict$num[stock.dict$code == ssc2.split[len1]]) / 
      max(stock.dict$num[stock.dict$code == ssc1.split[len1]], 
          stock.dict$num[stock.dict$code == ssc2.split[len1]])
    id <- append(id, id.num)
    
    similarity <- sum(id * weight)
  }
  
  return(similarity)
}


##---- Next value ----
NextArray <- function(needle, threshold) {
  
  next.array <- c(0)
  
  i <- 1
  j <- 0
  while (i < length(needle)) {
    if (j == 0 || StrSim(needle[i], needle[j]) > threshold) {
      i = i + 1
      j = j + 1
      
      if (i <= length(needle) && StrSim(needle[i], needle[j]) > threshold) {
        next.array <- append(next.array, j)
      } else {
        next.array <- append(next.array, 0)
      }
      
    } else {
      j <- next.array[j]
    }
  }
  
  return(next.array)
}


##---- KMP index ----
KMPIndex <- function(needle, haystack, threshold = 0.8) {
  
  next.array <- NextArray(needle, threshold)
  
  start.id <- c()
  i <- 1
  while (i < length(haystack)) {
    j <- 1
    while(
      i <= length(haystack) && j <= length(needle)
    ) {
      if (
        j == 0 || StrSim(haystack[i], needle[j]) > threshold
      ) {
        i <- i + 1
        j <- j + 1
      } else {
        i <- ifelse(StrSim(haystack[i - 1], needle[j - 1]) > threshold, i - 1, i)
        j <- next.array[j]
      }
    }
    
    if (j == length(needle) + 1) {
      start.id <- append(start.id, i - length(needle))
    } else {
      start.id <- ifelse(is.null(start.id), NaN, start.id)
    }
  }
  
  return(start.id)
}

