mapping <- read.xlsx('02_Inputs/mapping/Product standardization master data-A-S-0708_v1.xlsx',
                     sheet =  '标准文件')%>%
  select(1:13)%>%
  distinct()
mapping$SPEC[which(is.na(mapping$SPEC))] <- 1

ims <- read.xlsx('02_Inputs/ims_chpa_to20Q1.xlsx', sheet = 'cleaned')

cpa <- read_feather('02_Inputs/data/target/target_cpagy201911MAT.feather')
zb <- read_feather('02_Inputs/data/target/target_zb.feather')
colnames(cpa)[1:8] <- c('生产企业', '商品名', '剂型', '匹配名', '规格', 'Pack Number',
                        'Road', 'dataset')

all <- bind_rows(cpa, zb)


# functions ---------------------------------------------------------------
clean_pack <- function(x){
  x <- gsub(' ', '', x)
  x <- toupper(x)
}


# 整合数据 --------------------------------------------------------------------
all_f <- all%>%
  mutate(pack_number = gsub('[^0-9]', '', 包装))
sum(is.na(all_f$pack_number))
all_f[which(is.na(all_f$pack_number)), "pack_number"] <- 
  all_f[which(is.na(all_f$pack_number)), "价格转换比"]
sum(is.na(all_f$pack_number))
all_f[which(is.na(all_f$pack_number)), "pack_number"] <- 
  all_f[which(is.na(all_f$pack_number)), "Pack Number"]
sum(is.na(all_f$pack_number))


all_f1 <- all_f%>%
  select(-`Pack Number`, -包装, -价格转换比)

all_f1[which(all_f1$商品名=='无'), "商品名"] <- NA
# view(table(all_f1$商品名, useNA = 'always'))

sum(is.na(all_f1$生产企业))
sum(is.na(all_f1$商品名))
sum(is.na(all_f1$剂型))
sum(is.na(all_f1$匹配名))
sum(is.na(all_f1$规格))

mapping$PACK <- as.numeric(mapping$PACK)

mapping$SPEC <- clean_pack(mapping$SPEC)
all_f1$规格 <- clean_pack(all_f1$规格)

all2 <- all_f1%>%
  filter(!grepl('Others', 匹配名, ignore.case = TRUE))
all2$pack_number <- as.numeric(all2$pack_number)

# 标准化数据 -------------------------------------------------------------------
stand_mole <- unique(mapping$MOLE_NAME_CH)
stand_prod <- unique(mapping$PROD_NAME_CH)
stand_spec <- unique(mapping$SPEC)


# pack number -------------------------------------------------------------
#有pack number
all2_pack <- all2%>%
  filter(!is.na(pack_number))

#无pack number
all2_nopack <- all2%>%
  filter(is.na(pack_number))


# 中文企业 --------------------------------------------------------------------
all2_ch <- all2_pack%>%
  filter(!grepl('[a-z]', 生产企业, ignore.case = TRUE))

#有商品名
all2_ch_prod <- all2_ch%>%
  filter(!is.na(商品名))
sum(is.na(all2_ch_prod[,c(1:5,9)]))

#无商品名
all2_ch_noprod <- all2_ch%>%
  filter(is.na(商品名))
table(all2_ch_noprod$dataset)

# chk <- all2_ch_prod%>%
#   filter(grepl('×|*', 规格))
# view(table(all2_ch_prod$规格))

tmp <- all2_ch_prod[2,]
tmp <- all2_ch_prod%>%
  filter(dataset == 'AZ_ZB')
tmp <- tmp[1,]

a <- list()
# a[[1]] <- as.matrix(stringdistmatrix(tmp$匹配名, mapping$MOLE_NAME_CH,useNames="strings",method="lv"))
# a[[2]] <- as.matrix(stringdistmatrix(tmp$商品名, mapping$PROD_NAME_CH,useNames="strings",method="lcs"))
# a[[3]] <- as.matrix(stringdistmatrix(tmp$剂型, mapping$DOSAGE,useNames="strings",method="lv"))
# a[[4]] <- as.matrix(stringdistmatrix(tmp$规格, mapping$SPEC,useNames="strings",method="lv"))
# a[[5]] <- as.matrix(stringdistmatrix(tmp$生产企业, mapping$CORP_NAME_CH,useNames="strings",method="lv"))
a[[1]] <- as.matrix(stringdistmatrix(tmp$匹配名, mapping$MOLE_NAME_CH,method="lv"))
a[[2]] <- as.matrix(stringdistmatrix(tmp$商品名, mapping$PROD_NAME_CH,method="lcs"))
a[[3]] <- as.matrix(stringdistmatrix(tmp$剂型, mapping$DOSAGE,method="lv"))
a[[4]] <- as.matrix(stringdistmatrix(tmp$规格, mapping$SPEC,method="lv"))
a[[5]] <- as.matrix(stringdistmatrix(tmp$生产企业, mapping$CORP_NAME_CH,method="lv"))
a[[6]] <- t(as.matrix(abs(tmp$pack_number - mapping$PACK)/mapping$PACK))

a[[4]] <- t(apply(a[[4]], 2, function(x) {ifelse(is.na(x), max(x, na.rm = TRUE), x)}))
a[1:5] <- lapply(a[1:5], function(x) x/max(x))

b <- t(do.call(rbind, a))
b1 <- as.data.frame(b)
colnames(b1) <- c('匹配名', '商品名', '剂型', '规格', '生产企业', 'pack_number')
b2 <- b1%>%
  mutate(p = 0.2*匹配名 + 0.3*商品名 + 0.1*剂型 + 0.1*规格 + 0.1*生产企业 + 0.2*pack_number)%>%
  mutate(packid = mapping$PACK_ID)%>%
  filter(p == min(p))



f1 <- function(rowdata, mapping){
  a <- list()
  a[[1]] <- as.matrix(stringdistmatrix(rowdata$匹配名, mapping$MOLE_NAME_CH,method="lv"))
  a[[2]] <- as.matrix(stringdistmatrix(rowdata$商品名, mapping$PROD_NAME_CH,method="lcs"))
  a[[3]] <- as.matrix(stringdistmatrix(rowdata$剂型, mapping$DOSAGE,method="lv"))
  a[[4]] <- as.matrix(stringdistmatrix(rowdata$规格, mapping$SPEC,method="lv"))
  a[[5]] <- as.matrix(stringdistmatrix(rowdata$生产企业, mapping$CORP_NAME_CH,method="lv"))
  a[[6]] <- t(as.matrix(abs(rowdata$pack_number - mapping$PACK)/mapping$PACK))
  
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
  return(b2$packid)
}

chk <- all2_ch_prod%>%
  filter(!dataset %in% c('CPA', 'GYCX'))
chk1 <- chk[1:100,]
chk1$packid <- NA

chk1$packid <- apply(chk1, 1, FUN = f1(chk1, mapping))
for(i in 1:100){
  chk1$packid[i] <- f1(chk1[i,], mapping)
}
chk2 <- chk1%>%
  mutate(flag = ifelse(packcode_m == packid,'same','diff'))
table(chk2$flag)
tmp <- chk2%>%
  filter(flag == 'diff')%>%
  left_join(mapping, by = c('packid' = 'PACK_ID'))
write.xlsx(tmp, '03_Outputs/chk/100chk.xlsx')


# parallel ----------------------------------------------------------------
chk3 <- chk
cl <- makeCluster(7)
registerDoParallel(cl)
result <- foreach (i = 1:dim(chk3)[1], #.export = c("f1"),
         .packages = c("tidyverse","stringdist","Matrix")) %dopar%{
          f1(chk3[i,], mapping)
         }
stopImplicitCluster()
result1 <- t(bind_cols(result))
chk3 <- chk3%>%
  mutate(packid = result1)
chk3 <- chk3%>%
  mutate(flag = ifelse(packcode_m == packid,'same','diff'))
table(chk3$flag)

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







