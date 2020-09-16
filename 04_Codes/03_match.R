mapping <- read.xlsx('02_Inputs/mapping/Product standardization master data-A-S-0708_v1.xlsx',
                     sheet =  '标准文件')%>%
  select(1:13)%>%
  distinct()
ims <- read.xlsx('02_Inputs/ims_chpa_to20Q1.xlsx', sheet = 'cleaned')

cpa <- read_feather('02_Inputs/data/target/target_cpagy201911MAT.feather')
zb <- read_feather('02_Inputs/data/target/target_zb.feather')
colnames(cpa)[1:8] <- c('生产企业', '商品名', '剂型', '匹配名', '规格', 'Pack Number',
                        'Road', 'dataset')

all <- bind_rows(cpa, zb)

# 直接匹配 --------------------------------------------------------------------
all_f <- all%>%
  mutate(pack_number = gsub('[^0-9]', '', 包装))
sum(is.na(all_f$pack_number))
all_f[which(is.na(all_f$pack_number)), "pack_number"] <- 
  all_f[which(is.na(all_f$pack_number)), "价格转换比"]
sum(is.na(all_f$pack_number))
all_f[which(is.na(all_f$pack_number)), "pack_number"] <- 
  all_f[which(is.na(all_f$pack_number)), "Pack Number"]
sum(is.na(all_f$pack_number))

#left1
nopack <- all_f%>%
  filter(is.na(pack_number))

withpack <- all_f%>%
  anti_join(nopack)

all_f1 <- withpack%>%
  select(-`Pack Number`, -包装, -价格转换比)

mapping$PACK <- as.character(mapping$PACK)

all_f2 <- all_f1%>%
  left_join(mapping, by = c('生产企业' = 'CORP_NAME_CH', '商品名' = 'PROD_NAME_CH',
                            '匹配名' = 'MOLE_NAME_CH', '规格' = 'SPEC',
                            'pack_number' = 'PACK'))
bingo1 <- all_f2%>%
  filter(!is.na(PACK_ID))
all1 <- all_f2%>%
  anti_join(bingo1)%>%
  select(1:9)

# 标准分子名 -------------------------------------------------------------------
mole_name_cn <- unique(mapping$MOLE_NAME_CH)

all1 <- all1%>%
  mutate(flag_mole = ifelse(匹配名 %in% mole_name_cn, 'TRUE', 'FALSE'))

all1[which(all1$商品名=='无'), "商品名"] <- NA
view(table(all1$商品名, useNA = 'always'))

all1_t <- all1%>%
  filter(flag_mole == 'TRUE')%>%
  mutate(分子名 = 匹配名)

all_f <- all1%>%
  filter(flag_mole == 'FALSE')

# all_f$分子名 <- apply(all_f$匹配名, 1, function(x) first(agrep(x, mole_name_cn, value = TRUE)))

all_f$分子名 <- amatch(all_f$匹配名, mole_name_cn, maxDist = 10)
all_f <- all_f%>%
  mutate(分子名 = ifelse(is.na(分子名), NA, mole_name_cn[分子名]))

all2 <- bind_rows(all1_t, all_f)


# 商品名 ---------------------------------------------------------------------
#剩余没用分子名匹配pack的
all2_left <- all2%>%
  filter(is.na(分子名))%>%
  filter(!grepl('Others', 匹配名))
#with pack_id
view(table(all2_left$packcode_m, all2_left$dataset, useNA = 'alway'))

all2_have <- all2%>%
  filter(!is.na(分子名))

brand <- unique(mapping$PROD_NAME_CH)

#有分子名
all2_have <- all2_have%>%
  mutate(flag_brand = ifelse(商品名 %in% brand, 'TRUE', 'FALSE'))
table(all2_have$flag_brand)

#无分子名
all2_left <- all2_left%>%
  mutate(flag_brand = ifelse(商品名 %in% brand, 'TRUE', 'FALSE'))
table(all2_left$flag_brand)


# all2_left ---------------------------------------------------------------
#有商品名直接用商品名匹配
all2_left_t <- all2_left%>%
  filter(flag_brand == 'TRUE')%>%
  mutate(标准商品名 = 商品名)

all2_left_t$规格 <- gsub(' ', '', all2_left_t$规格)
all2_left_t$规格 <- toupper(all2_left_t$规格)

all2_left_t1 <- all2_left_t%>%
  left_join(mapping, by = c('生产企业' = 'CORP_NAME_CH',
                            '标准商品名' = 'PROD_NAME_CH',
                            '规格' = 'SPEC',
                            'pack_number' = 'PACK',
                            '剂型' = 'DOSAGE'))
chk <- all2_left_t1%>%
  group_by(生产企业, 标准商品名, 匹配名, 规格, pack_number)%>%
  count()%>%
  filter(n>1)


#有分子名无产品名
all2_left_f <- all2_left%>%
  filter(flag_brand == 'FALSE')


all2_have_f$标准商品名 <- amatch(all2_have_f$商品名, brand, maxDist = 10)
all2_have_f <- all2_have_f%>%
  mutate(标准商品名 = ifelse(is.na(标准商品名), NA, brand[标准商品名]))



# all2_have ---------------------------------------------------------------
#有分子名有商品名
all2_have_t <- all2_have%>%
  filter(flag_brand == 'TRUE')%>%
  mutate(标准商品名 = 商品名)

#有分子名无产品名
all2_have_f <- all2_have%>%
  filter(flag_brand == 'FALSE')


all2_have_f$标准商品名 <- amatch(all2_have_f$商品名, brand, maxDist = 10)
all2_have_f <- all2_have_f%>%
  mutate(标准商品名 = ifelse(is.na(标准商品名), NA, brand[标准商品名]))

all3 <- bind_rows(all2_have_t, all2_have_f)

all3_f <- all3%>%
  left_join(mapping, by = c('生产企业' = 'CORP_NAME_CH', '标准商品名' = 'PROD_NAME_CH',
                            '分子名' = 'MOLE_NAME_CH', '规格' = 'SPEC',
                            'pack_number' = 'PACK'))

bingo2 <- all3_f%>%
  filter(!is.na(PACK_ID))
all4 <- all3_f%>%
  anti_join(bingo2)%>%
  select(1:13)


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







