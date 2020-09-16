
##  GtoMG function - G改为MG
GtoMG <- function(x){
  
  
  ## 提取运算部分
  x1 <- stri_split_regex(x, ":|,|\\(|/|\\)|;|%| |\\+", simplify = TRUE)
  
  if (length(x1) == 1) {
    x3 <- ifelse(grepl('[[:digit:]]G',x),
                 paste(as.numeric(gsub("G", "", x)) * 1000, "MG", sep = ""),
                 x)
  } else {
    
    
    ## 提取分隔符
    punct <- stri_extract_all_regex(x, "[[:punct:]]| |\\+", simplify = TRUE) %>%
      as.vector() %>%
      .[. != "."]
    punct <- c(gsub("\"", "", punct), "")
    
    
    ## 转换
    x2 <- sapply(x1,
                 
                 function(x) {
                   
                   ifelse(grepl('[[:digit:]]G',x),
                          paste(as.numeric(gsub("G", "", x)) * 1000, "MG", sep = ""),
                          x)
                 })
    
    
    ## 重组
    x3 <- paste(x2, punct, sep = "", collapse = "")
  }
  
  
  
  return(x3)
}



## CPA final function

# clean_spec_cpa ----
clean_spec_cpa <- function(x){
  # x <- c("0.48G", "10ML∶对乙酰氨基酚0.125G,咖啡因7.5MG,马来酸氯苯那敏1.5MG,人工牛黄5MG")
  # x <- "10ML∶对乙酰氨基酚0.125G,咖啡因7.5MG,马来酸氯苯那敏1.5MG,人工牛黄5MG"
  
  x <- gsub(' ', '', x)
  x <- toupper(x)#改成大写
  x <- gsub('([\u4E00-\u9FA5]+)','',x)#去掉中文
  x <- gsub('（', '(', x)
  x <- gsub('）', ')', x)
  x <- gsub('：', ':', x)
  x <- gsub('∶', ':', x)
  x <- gsub('∶', ':', x)
  
  
  x <- gsub('\\(.*,.*\\)','',x)#括号里有逗号则去掉括号及里面的内容
  x <- gsub('GM','G',x)#GM改成G
  x <- gsub('IU','U',x)#IU改成U
  
  x <- ifelse(grepl(':',x) & grepl(',',x),
              str_extract(x,paste(unlist(str_split(x,',')), collapse = "|")),
              x)
  
  x <- map_chr(x,GtoMG)
  
  
  return(x)
}


## ZB final function

# clean_spec_zb ----
clean_spec_zb <- function(x){
  
  x <- gsub('^ ', '', x)
  x <- gsub(' $', '', x)
  x <- gsub('　', ':', x)
  x <- gsub('μ','u',x)
  x <- toupper(x)#改成大写
  x <- str_replace(x, "([[:digit:]]|\\.)*(?=万)",as.character(as.numeric(str_extract(x,"([[:digit:]]|\\.)*(?=万)"))*10000)) %>% str_replace( "(?<=[[:digit:]]|\\.)万","")
  x <- gsub('（', '(', x)
  x <- gsub('）', ')', x)
  x <- gsub('，', '', x)
  x <- gsub('、', ':', x)
  x <- gsub('：', ':', x)
  x <- gsub('∶', ':', x)
  x <- gsub('×', '*', x)
  
  x <- gsub('\\(I+.*\\)|\\(V+.*\\)', '', x)#去掉罗马数字 - IV
  x <- gsub('[\U2160-\U2169]|[\U216A]|[\U216B]', '', x)#去掉罗马数字 - Ⅻ
  x <- gsub('([\u4E00-\u9FA5]+)','',x)#去掉中文
  
  x <- gsub('\\([[:alpha:]].*\\)','',x)#'40MG(C24H25C1FN503)' 
  x <- gsub('\\(\\)', '', x)# ()
  x <- gsub('\\(\\)', '', x)# (())
  
  x <- gsub('(\\*[[:digit:]]*)$', '', x)#去掉最后的价格转换比 - *10
  x <- gsub('(\\*[[:digit:]]*/)$', '', x)#去掉最后的价格转换比 - *10/
  x <- gsub('GM','G',x)#GM改成G
  x <- gsub('AXa','',x,ignore.case = T)#'AXa'改成''
  x <- gsub('IU','U',x)#IU改成U
  
  x <- map_chr(x,GtoMG)
  
  x <- gsub('^(.*ML):(.*MG)','\\2:\\1',x)
  x <- gsub('^(.*ML):(.*U)','\\2:\\1',x)
  
  x <- str_replace(x, '^(.*)\\(([[:digit:]]|\\.)+%\\)', paste(str_extract(x, '([[:digit:]]|\\.)+%'), '\\1', sep = " ")) %>% str_replace_all('\\(([[:digit:]]|\\.)+%\\)','')#'15G:7.5MG(0.05%)' 改成 0.05% 15G:7.5MG    
  

  
  return(x)
}
