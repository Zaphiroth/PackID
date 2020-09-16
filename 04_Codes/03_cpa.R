mapping <- read.xlsx('02_Inputs/mapping/Product standardization master data-A-S-0708_v1.xlsx',
                     sheet =  '标准文件')%>%
  select(1:13)%>%
  distinct()
mapping$SPEC[which(is.na(mapping$SPEC))] <- 1

pfizer <- read.xlsx('02_Inputs/pfizer_test_new_匹配上的部分查错误v2.xlsx',
                    sheet = 'test')
all <- pfizer%>%
  filter(!grepl('Others', ORIGIN_MOLE_NAME, ignore.case = TRUE))%>%
  select(1:9,12,20,28:33)

###########################################################################
# functions ---------------------------------------------------------------
clean_spec <- function(x){
  x <- gsub(' ', '', x)
  x <- toupper(x)
  x <- gsub('（', '(', x)
  x <- gsub('）', ')', x)
  x <- gsub('：', ':', x)
}


f1 <- function(rowdata, mapping){
  a <- list()
  a[[1]] <- as.matrix(stringdistmatrix(rowdata$ORIGIN_MOLE_NAME, mapping$MOLE_NAME_CH,method="lv"))
  a[[2]] <- as.matrix(stringdistmatrix(rowdata$ORIGIN_PRODUCT_NAME, mapping$PROD_NAME_CH,method="lcs"))
  a[[3]] <- as.matrix(stringdistmatrix(rowdata$ORIGIN_DOSAGE, mapping$DOSAGE,method="lv"))
  a[[4]] <- as.matrix(stringdistmatrix(rowdata$ORIGIN_SPEC, mapping$SPEC,method="lv"))
  a[[5]] <- as.matrix(stringdistmatrix(rowdata$ORIGIN_MANUFACTURER_NAME, mapping$CORP_NAME_CH,method="lv"))
  a[[6]] <- t(as.matrix(abs(rowdata$ORIGIN_PACK_QTY - mapping$PACK)/mapping$PACK))
  
  a[[4]] <- t(apply(a[[4]], 2, function(x) {ifelse(is.na(x), max(x, na.rm = TRUE), x)}))
  a[1:5] <- lapply(a[1:5], function(x) x/max(x))
  b <- t(do.call(rbind, a))
  b1 <- as.data.frame(b)
  colnames(b1) <- c('匹配名', '商品名', '剂型', '规格', '生产企业', 'pack_number')
  b2 <- b1%>%
    mutate(p = 0.2*匹配名 + 0.3*商品名 + 0.1*剂型 + 0.1*规格 + 0.1*生产企业 + 0.2*pack_number)%>%
    mutate(packid = mapping$PACK_ID)%>%
    filter(p == min(p))
  if(dim(b2)[1]>1){
    b2 <- b2[1,]
  }
  return(list(b2$packid, b2$p))
}


# parallel ----------------------------------------------------------------
all$ORIGIN_SPEC <- clean_spec(all$ORIGIN_SPEC)
mapping$SPEC <- clean_spec(mapping$SPEC)
all$packid <- NA
start_time = Sys.time()
cl <- makeCluster(30)
registerDoParallel(cl)
result <- foreach (i = 1:dim(all)[1], #.export = c("f1"),
                   .packages = c("tidyverse","stringdist","Matrix")) %dopar%{
                     f1(all[i,], mapping)
                   }
stopImplicitCluster()
end_time = Sys.time()
end_time-start_time

result1 <- do.call(rbind,result)
result2 <- as.data.frame(result1)
all$PACK_ID <- stri_pad_left(all$PACK_ID, 7, '0') 

all1 <- all%>%
  mutate(packid = result2$V1,
         p = result2$V2)%>%
  mutate(flag人工 = packid == PACK.ID人工)%>%
  mutate(flag程序 = packid == PACK.ID程序)%>%
  mutate(flag人程序 = PACK.ID人工==PACK.ID程序)
table(all1$flag人工)
table(all1$flag程序)
table(all1$flag人程序)

all1$packid <- unlist(all1$packid)
all1$p <- unlist(all1$p)

all2 <- all1%>%
  left_join(mapping, by = c('packid' = 'PACK_ID'))

chk <- all2%>%
  filter(flag人工 == 'FALSE')
write.xlsx(chk, '03_Outputs/cpa/cpa_chk.xlsx')


# 英文企业 --------------------------------------------------------------------
all2_en <- all2_pack%>%
  filter(grepl('[a-z]', 生产企业, ignore.case = TRUE))





manu <- unique(all_f$生产企业)
stand_manu <- unique(mapping$CORP_NAME_CH, mapping$MNF_NAME_CH)
a <- stringdistmatrix(manu, stand_manu)
df <- data.frame(a)
colnames(df)[1:33469] <- 1:33469
colnumber <- colnames(df)[1:33469][apply(df[,1:33469],1,which.max)]

chk <- data.frame(生产企业 = manu, number = colnumber)
chk <- chk%>%
  mutate(匹配企业名 = stand_manu[number])







# chk ---------------------------------------------------------------------
tmp <- mapping%>%
  group_by(MOLE_NAME_CH, PROD_NAME_CH, CORP_NAME_CH, DOSAGE, SPEC, PACK)%>%
  count()%>%
  filter(n>1)





manu <- unique(all_f$生产企业)
stand_manu <- unique(mapping$CORP_NAME_CH, mapping$MNF_NAME_CH)
a <- stringdistmatrix(manu, stand_manu)
df <- data.frame(a)
colnames(df)[1:33469] <- 1:33469
colnumber <- colnames(df)[1:33469][apply(df[,1:33469],1,which.max)]

chk <- data.frame(生产企业 = manu, number = colnumber)
chk <- chk%>%
  mutate(匹配企业名 = stand_manu[number])







