mapping <- read.xlsx('02_Inputs/mapping/Product standardization master data-A-S-0708_v1.xlsx',
                     sheet =  '标准文件')%>%
  select(1:13)%>%
  distinct()
ims <- read.xlsx('02_Inputs/ims_chpa_to20Q1.xlsx', sheet = 'cleaned')


# ZB ----------------------------------------------------------------------
CA <- read.xlsx('02_Inputs/data/all_raw_data_packid_CA_19.xlsx')
# tmp <- CA%>%
#   select(通用名, 匹配名, 商品名, 剂型, 规格, 包装, 生产企业, packcode_m)%>%
#   distinct()%>%
#   filter(!is.na(packcode_m))
# nopack <- CA%>%
#   select(通用名, 匹配名, 商品名, 剂型, 规格, 包装, 生产企业, packcode_m)%>%
#   distinct()%>%
#   filter(is.na(packcode_m))

CA1 <- CA%>%
  select(匹配名, 商品名, 剂型, 规格, 包装, 价格转换比, 生产企业, packcode_m)%>%
  distinct()%>%
  mutate(dataset = 'CA_ZB')

servier <- read_feather('02_Inputs/data/all_raw_data_packid_Servier_171819_v4.feather')
colnames(servier)<-c("Year","Month","季度","省份","城市",
                     "区县","医院名称","医院库类型","通用名","商品名",
                     "化学名","匹配名","药品名","剂型","规格",
                     "生产企业","采购价","采购数量","采购金额","价格转换比",
                     "最小使用单位数量","packcode")
servier1 <- servier%>%
  mutate(匹配名 = ifelse(!is.na(匹配名), 匹配名,化学名))%>%
  select(匹配名, 商品名, 剂型, 规格, 生产企业, 价格转换比, packcode_m = packcode)%>%
  distinct()%>%
  mutate(dataset = 'servier_ZB')

AZ <- read_feather('02_Inputs/data/all_raw_data_m3_AZ_6p_171819_all_v2.feather')
colnames(AZ) <- c("Year","Month","季度","省份","城市","区县","医院库类型","医院名称","通用名","药品名",
                  "商品名","剂型","规格","包装","生产企业","采购价","采购数量","采购金额","价格转换比",
                  "最小使用单位数量","name","产品名称","packcode","Corp_ID","MNF_ID","ATC4_Code",
                  "NFC1_Code","NFC12_Code","NFC123_Code","NFC2_Code","NFC23_Code","IntPrd_Desc","Prd_desc",
                  "Str_Desc","IntStr_Desc","Pck_Desc","PckSize_Desc","IntSize_Desc","PckVol_Desc","IntVol_Desc",
                  "IntPck_Desc","PckLaunchDate","MKT-TYPE_Desc","PRES_Desc","GEN_PRD_Desc","PROTECTION_Desc",
                  "COMP_Desc","PrdLaunchDate","Molecule_Desc","项目","序号","医院级别","匹配名","flag","化学名")
tmp <- AZ%>%
  select(通用名, 药品名, 商品名)%>%
  distinct()

AZ1 <- AZ%>%
  mutate(匹配名 = ifelse(!is.na(药品名), 药品名, 通用名))%>%
  select(匹配名, 商品名, 剂型, 规格, 包装, 价格转换比, 生产企业, packcode_m = packcode)%>%
  distinct()%>%
  mutate(dataset = 'AZ_ZB')

pfizer <- read_feather('02_Inputs/data/all_raw_data_packid_Pfizer_171819_m_v3.feather')
colnames(pfizer)[1:28] <- c("序号","Year","Month","季度","省份", "城市","区县",
                            "医院库类型","医院名称","通用名","商品名","化学名","剂型",
                            "规格","包装","生产企业","采购价","采购数量","采购金额",
                            "价格转换比","最小使用单位数量","flag","name","PHA",
                            "医疗机构","药品名","匹配名","医院级别")
tmp <- pfizer%>%
  select(通用名, 化学名, 药品名,匹配名)%>%
  distinct()

pfizer1 <- pfizer%>%
  mutate(匹配名 = ifelse(!is.na(匹配名), 匹配名, 化学名))%>%
  select(匹配名, 商品名, 剂型, 规格, 包装, 价格转换比, 生产企业, packcode_m)%>%
  distinct()%>%
  mutate(dataset = 'pfizer_ZB')

all <- bind_rows(CA1, servier1, AZ1, pfizer1)

chk <- all%>%
  filter(is.na(价格转换比))
write_feather(all, '02_Inputs/data/target/target_zb.feather')

# cpa ---------------------------------------------------------------------
cpa <- read_feather('02_Inputs/data/cpagy201911MAT.feather')
cpa1 <- cpa%>%
  select(Atccorpname, Atcproductname, Dosage, `Mole Name`, `Pack Des`,
         `Pack Number`, Road, `Original Dataset`)%>%
  distinct()
write_feather(cpa1, '02_Inputs/data/target_cpagy201911MAT.feather')



chk <- mapping%>%
  group_by(PACK_ID)%>%
  count()%>%
  filter(n>1)

combined <- mapping%>%
  left_join(ims, by = c('PACK_ID' = 'Pack_Id'))


mole_name <- unique(cpa$`Mole Name`)


tmp <- agrep(mole_name[10], unique(combined$MOLE_NAME_CH), value = TRUE, 
             ignore.case = TRUE)
