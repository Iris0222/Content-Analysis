
wkDir = "C:/文本分析"
setwd(wkDir)
library(jiebaR);     library(xlsx);    library(stringr);   library(text2vec);   library(data.table)

# ================ 讀取數據 ===============
RR = read.csv('new.csv', header = TRUE, sep = ',', fileEncoding = "utf-8" ) ; head( RR )


RR <- filter( RR, RR$sub.author != "" )
RR$gp[ RR$gp == "-" ] = 0
RR$bp[ RR$bp == "-" ] = 0

RR$gp[ RR$gp == "爆" ] = as.integer( 1000 )
RR$bp[ RR$bp == "X" ] = as.integer( 1000 )

RR$sub.gp[ RR$sub.gp == "-" ] = 0
RR$sub.bp[ RR$sub.bp == "-" ] = 0

RR$gp[ is.na( RR$gp ) ] = 0
RR$bp[ is.na( RR$bp ) ] = 0
RR$sub.gp[ is.na( RR$sub.gp ) ] = 0
RR$sub.bp[ is.na( RR$sub.bp ) ] = 0

RR$gp = as.integer( RR$gp ) 
RR$bp = as.integer( RR$bp )
RR$sub.gp = as.integer( RR$sub.gp )
RR$sub.bp = as.integer( RR$sub.bp )

RR$sub.time[ RR$sub.time == ""  ] = NA

RR$nch   = nchar( RR$mainText )

RR$sub.nch   = nchar( RR$sub.Text )

RR$Rdate = as.Date(RR$sub.time)

# colnames(RR) -> "title","author","time","gp","bp","mainText","sub.author","sub.time","sub.gp","sub.bp","sub.Text","nch","sub.nch","Rdate"




# ================ 數據轉換 ===============
RRtitle = unique(RR$title)
length(RRtitle)   # 共有2214個討論主題

content = NULL;   Ncontent = NULL            

for (title in RRtitle) {                                   #-- (2) 同一討論主題的回文內容(RR$text),以"\n\n"串接
  content = c(content, paste(title, paste(RR$sub.Text[which(RR$title==title)],collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))   #-- (3) length(which(條件)): 表示滿足條件的個數
}


head(content,1)
# [1] "20歲到30歲沒專長的年輕人都在做甚麼工作 輪班囉設備工程...."


#####===== (3) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(A)取關鍵n詞 (Seg$title-->Dword2-->dtm-->group) (GOOD!!!) =====#####

cutter = worker( stop_word = "stop.txt", type="tag" ) 
Dword = sapply(content, function(x)segment(tolower(as.character(x)),cutter)) 
length(Dword) # 2214   
head(Dword[[1]],15)  
# m       zg        v        m       zg        v        n       uj        n        d        p        v        r       vn        n 
# "20"     "歲"     "到"     "30"     "歲"     "沒"   "專長"     "的" "年輕人"     "都"     "在"     "做"   "甚麼"   "工作"   "輪班" 


nDword = lapply(Dword,nchar);    head(nDword[[1]],15)
# m zg  v  m zg  v  n uj  n  d  p  v  r vn  n 
# 2  1  1  2  1  1  2  1  3  1  1  1  2  2  2

# Dword2B -> 找出大於2的單字
Dword2B = lapply(Dword, function(x){  ## 只保留字數大於2的單字
  nx=sapply(x,nchar); 
  xA=x[nx>=2]; 
  return(xA[setdiff((1:length(x)),grep("[[:digit:]]+",xA))])
} )

head(Dword2B[[1]],15) 
# n        n        r        n       vn        n        n        n        n        n        v        v        n        x       vn 
# "專長" "年輕人"   "甚麼"   "輪班"   "設備"   "工程"   "產品"   "工程" "技術員"   "學校"   "不會"   "組裝"   "業界"   "機台"   "分析" 

length(Dword2B) # 2214


# Dword2 -> 找出名詞跟代名詞
Dword2 = lapply(Dword2B, function(xB) return(xB[grep(c("n","r"),names(xB))])) # r:代名詞
length(Dword2)
head(Dword2,1)



removeKW <- function(KWlist,KWvec) { 
  OUTlist = list()
  for (kl in 1:length(KWlist)) {
    ind = NULL
    for (k in 1:length(KWvec)) 
    {
      ind = c( ind, as.vector(which(KWvec[k]==KWlist[[kl]])) ) # ind: 有出現在關鍵詞裡的所有INDEX
    }
    OUTlist[[kl]] = KWlist[[kl]][setdiff( (1:length(KWlist[[kl]])), ind )]
  }
  return(OUTlist)
}


# Dword2 -> 找出名詞跟代名詞, Dword2B -> 找出大於2的單字

## Dword3 -> "Dword2" 刪除 keyword : ( "https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣" ) 
Dword3 = removeKW(Dword2, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))

## Dword3B -> "Dword2B" 刪除 keyword : ( "https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣" ) 
Dword3B = removeKW(Dword2B, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))

keys = worker("keywords",topn=5) # 關鍵詞提取
Dword4B = lapply(Dword3B, FUN=function(x)vector_keywords(x,keys)) # 每個list前5大key words
length(Dword4B)
head(Dword4B[[1]]) 
# 1232.62   58.696  46.9568  35.2176  33.5652 
# "NA"   "輪班" "技術員" "作業員"   "保全"



Dword4 = lapply(Dword3, FUN=function(x)vector_keywords(x,keys))
length(Dword4)
head(Dword4[[1]]) 

# Dword4:每篇文章中名詞跟代名詞出現的前5名
# Dword4B: 每篇文章中 字數大於2的單字 出現的前5名

##########===== (B).詞語向量(vector_keywords())到文件詞語矩陣(dtm) =====########## 

#####===== 生成文檔-詞矩陣（DTM ) =====##### difficult again
tokens = Filter(function(x) !any( x == "NA"), Dword4)
length(tokens) # 2210
head(tokens[[1]])   
# 58.696  46.9568  35.2176  23.4784  23.4784 
# "輪班" "技術員" "作業員"   "保險"    "cnc"


it = itoken(tokens, progressbar = FALSE)  #-- 可在此處加上參數 preprocessor = tolower 可以轉換成小寫，也可以在此處自行設置 tokenizer 
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 3L) # 保留term_count >= 3
dim(vocab)
# term term_count doc_count
# 1:     ae          3         3
# 2:    bsn          3         3
# 3: coding          3         3

## from here ================

vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer) 
dim(dtm)   # 2210  717

as.matrix(dtm[60:70,150:200]) # sparse matrix -> dense matrix
# 臨時工 蓋章 藉口 裁員 補貨 製造業 西點 規則 規模 觀念 訂單 計程車 記帳 論文 諮詢 證件 議題 變化 豆花 負債 貨櫃 車子 車輛 輪機 農業 連鎖 運輸 違規 邏輯 配電 酒店
# 60      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 61      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 62      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 63      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 64      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 65      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 66      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 67      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 68      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 69      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 70      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 醫師 醫生 金融業 銷售 錄音 除草 階段 集團 電信 額度 顧客 馬達 體驗 高鐵 點數 eat forum gov qq rd
# 60    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 61    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 62    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 63    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 64    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 65    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  1
# 66    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 67    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 68    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 69    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 70    0    0      0    0    0    0    0    0    0    0    0    1    0    0    0   0     0   0  0  0



fit_hc = hclust(dist(as.matrix(dtm)), method="ward.D2" ) # 階層聚類分析
for (kk in 20:150) { group = cutree(fit_hc, k=kk);   print(table(group)) }  
Ncls=86
group = cutree(fit_hc, k=Ncls)
print(table(group))  # 每一簇的樣本數
# group
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28 
# 12  20  19  32  47  10  48  17  62  49   8  15  25  53  14 376  56  18  33  19  28  22  17  18  19  12  31  16 

# 29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56 
# 25  10  38  35  18  18  19  29  18  24   7  24  29  27  21  24  21  21  29  17  26  13  29   9  20  14  19  14 

# 57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84 
# 24  17  13  50  16  29  23  10  10  20  17  12  12  16  22  16  35   8  18  17   9  17  11   6  28  22   5  14 

# 85  86 
# 8  11 

RRtitle[group==11]    
# [1] "【討論】最近正考慮換工作"                                        
# [2] "【問題】禮拜五離職，離職日壓禮拜天會不會很哭?"                   
# [3] "【問題】請教有無認識或正在東南亞做博弈類工作的朋友（想了解現況）"
# [4] "【心得】營造業工作3年的看法(抱怨)"                               
# [5] "【問題】偉康科技這間公司如何？"                                  
# [6] "【問題】請問有人可以推薦一下好用的2D工業設計繪圖軟體嗎？"        
# [7] "【心得】台灣固定薪資被越南超車！"                                
# [8] "【情報】熊貓、Uber Eats 4月全台齊大改制 外送員哀嚎質疑變相減薪" 




#####===== 為每個聚類組合計算出關鍵詞 =====##### 
DTM = as.matrix(dtm)
KWgroup = NULL
for (k in 1:Ncls) {   # Ncls=86, Ncontent: 每個title的回文個數
  ind = as.vector(which(group==k))   #--  [1]  112  242  332  399  534  931 1558
  nContent = sum(Ncontent[ind]) # 屬於k這個group的文回總數
  if (length(ind)==1) { 
    colDTM0 = DTM[ind,] 
  } else { 
    colDTM0 = colSums(DTM[ind,]) # 每個kw出現次數加總
  }

  colDTM = colDTM0[which(colDTM0>0)] # 出現次數>0的關鍵詞
  
  sortDTM = colDTM[order(colDTM,decreasing=TRUE)]
  sortDTM[1:20] # 取出最高頻的前 20 個詞頻
  
  max2 = as.integer(sortDTM[2])
  selDTM = sortDTM[which(sortDTM>=max2)]
  selDTM
  
  KWgroup = rbind( KWgroup, c(k, paste(names(selDTM),collapse="/"), length(ind), nContent, paste(ind,collapse="/")) )
}

KWgroup.df = as.data.frame(KWgroup)
colnames(KWgroup.df) = c("k","KW","count","nContent","ind")
dim(KWgroup.df)
# k          KW count nContent                                                                                                 ind
# 22 22   興趣/薪水    22      129 30/86/201/338/584/701/732/739/1073/1084/1115/1269/1500/1561/1653/1687/1874/1923/1946/2085/2136/2208
# 37 37 技術/工程師    18       91                          54/62/295/515/538/543/578/612/646/821/853/856/896/1140/1727/1860/2018/2049
# 45 45   內場/辦法    21      173          69/79/117/356/445/579/654/671/757/834/965/969/1018/1149/1357/1407/1501/1560/1940/2017/2163                                                                                                                                                                                                                                                                                                                                                 

paste0(KWgroup.df$KW,"(",KWgroup.df$count,"/",KWgroup.df$nContent,")")
write.xlsx(KWgroup.df,"./KWgroup1.xlsx")







#####===== kwKind: 關鍵詞屬於甚麼分類 =====#####
kwKind = list()
kwKind$"老闆" = c("老闆","機車","主管","政府","業主","廠商","店長","客戶","朋友")
kwKind$"同事" = c("老鳥","新人","朋友","學長","人員","同事","機車","員工","人員","對方","部門")
kwKind$"薪水工時" = c("彈性","業績","價值","全額","薪水","天數","年資","月薪","夜班","薪資","加班費","輪班","日班","全職","行業")
kwKind$"請假休假" = c("事假","天數","小時","mueller","tomtom","puma","nike")
kwKind$"員工福利" = c("保險","投保","國民","年金","合約","契約","勞保","規定","勞基法","勞工","失業","獎金","福利")
kwKind$"工作機會" = c("公職","人生","待業","offer","創業","跳槽","運氣","機會","工作","大運","大學","內容","賽","球員","網站")
kwKind$"工作環境" = c("內場","單位","餐廳","中國","社區","業界","市場","疫苗","職場","環境","壓力","企業","發展","機構","國營","醫院","工廠","辦公室","加油站","疫情","日本","現場","銀行")
kwKind$"器材設備" = c("cnc","軟體","裝潢","電腦","手機","設備","檢查","系統","品管")
kwKind$"面試" = c("面試","面試官","課程","會員","次數","姿勢","強度","感覺")
kwKind$"學校科系" = c("學歷","老師","學生","學校","科系","老師","碩士","大學","畢業","感覺","興趣","經歷","讀書","當兵")
kwKind$"證照專長" = c("堆高機","程式","證照","乙級","課程","專長","考試")
# 公職/產業/行業/職業
kwKind$"職業" = c("清潔隊","職業","產業","遊戲","公務員","船員","郵差","計程車","設計師","助理","外勤","職業","作業員","行銷","企劃","工程師",
                "司機","業務","uber","eat","技師","電機","熊貓","工程師","餐飲","line")
kwKind$"語文" = c("英文","基礎","語言","日文" )
kwKind$"學徒" = c("經驗","師傅","基礎","學徒","年輕人" )
kwKind$"健康" = c("身體","狀況" )
kwKind$"程式語言" = c("java","php", "game" )
kwKind$"投資" = c("投資","股票", "保險","情況","規劃" )

length(kwKind)  # 17
KWgroup.df$KWkind = ""
for (k in 1:length(kwKind)) {
  for (j in 1:length(kwKind[[k]])) 
    KWgroup.df$KWkind[ grep(kwKind[[k]][j],KWgroup.df$KW) ] = names(kwKind)[k] # 看group屬於甚麼分類
}
KWgroup.df$KWkind

KWgroup.df[c(3,15,17,20,26:27),]
# k             KW count nContent                                                                                                         ind   KWkind
# 3   3      主管/員工    24      227  3/30/33/154/382/604/636/640/863/1171/1210/1221/1265/1305/1847/1897/2075/2308/2362/2547/2627/2707/2765/2795     同事
# 15 15      部門/主管    10       79                                                                 19/141/315/621/895/1167/1331/1539/1777/2122     同事
# 17 17 待業/帳號/文章    14       80                                            21/218/431/803/1473/1543/1831/1843/1922/2087/2140/2242/2298/2672 工作機會
# 20 20      客戶/老闆    21      116                26/271/391/441/443/493/602/807/822/852/855/1010/1243/1431/1452/1820/2171/2240/2292/2414/2440     老闆
# 26 26      興趣/工作    24      171 36/64/246/419/426/628/867/907/915/982/1238/1407/1743/1828/1908/1932/1942/2105/2154/2243/2389/2459/2486/2818 學校科系
# 27 27      薪水/老闆    16      169                                   37/139/716/874/943/1118/1550/1683/1952/1954/2041/2190/2320/2445/2625/2772 薪水工時

write.xlsx(KWgroup.df,"KWgroup2.xlsx")



########## (C).文本分析中的知識架構:單類別知識分析 ##########

#####===== group6的分析 =====#####
DTM1 = DTM[which(group %in% c(6)),]
dim(DTM1)   #-- [1]  17 798
length(which(colSums(as.matrix(DTM1))>0))    #-- [1] 32
DTM2 = DTM1[,which(colSums(as.matrix(DTM1))>1)]
dim(DTM2)   # 10  7
colSums(DTM2)     
# 職員 副業 資訊 國營 考試 單位 薪水 
# 3    2    2   10    5    5    5 

#####===== (C2) (KDD3) 數據轉換 (matrix DTM2-->list L2-->set S2) =====#####
library(kst)
L2 = list()
for (i in 1:dim(DTM2)[1]) { L2[[i]] = as.set(colnames(DTM2)[which(DTM2[i,]>0)]) }
L2

# [[1]]
# {"單位", "國營", "考試"}
# 
# [[2]]
# {"單位", "國營", "職員", "資訊"}
# 
# [[3]]
# {"副業", "國營", "薪水"}
# 
# [[4]]
# {"國營", "考試", "薪水"}
# 
# [[5]]
# {"國營", "考試", "薪水"}
# 
# [[6]]
# {"國營", "薪水"}
# 
# [[7]]
# {"單位", "國營", "考試"}
# 
# [[8]]
# {"單位", "國營", "職員", "資訊"}
# 
# [[9]]
# {"副業", "國營", "薪水"}
# 
# [[10]]
# {"單位", "國營", "考試", "職員"}



#####===== (C3) (KDD4) 數據模型 (S2-->kst2-->ksp2) =====#####
S2 = as.set(unique(L2)) # 10個裡面有重複組合的去掉
S2 
kst2 = kstructure(S2)
kst2   
# { {}, 
#   {"國營", "薪水"}, 
#   {"副業", "國營", "薪水"}, 
#   {"單位", "國營", "考試"}, 
#   {"國營", "考試", "薪水"},
#   {"單位", "國營", "考試", "職員"}, 
#   {"單位", "國營", "職員", "資訊"}, 
#   <<set(7)>>
# }

kdomain(kst2)   # {"副業", "單位", "國營", "考試", "職員", "薪水", "資訊"}
ksp2 = kspace(kst2)
ksp2     
# {{}, {"國營", "薪水"}, {"副業", "國營", "薪水"}, {"單位", "國營", "考試"}, {"國營", "考試", "薪水"},
#   {"副業", "國營", "考試", "薪水"}, {"單位", "國營", "考試", "職員"}, {"單位", "國營", "考試", "薪水"},
#   {"單位", "國營", "職員", "資訊"}, {"副業", "單位", "國營", "考試", "薪水"}, {"單位", "國營", "考試",
#     "職員", "薪水"}, {"單位", "國營", "考試", "職員", "資訊"}, {"單位", "國營", "職員", "薪水", "資訊"},
#   <<set(6)>>, <<set(6)>>, <<set(6)>>, <<set(7)>>}

katoms(kst2, items=kdomain(kst2))
# $副業
# {{"副業", "國營", "薪水"}}
# $單位
# {{"單位", "國營", "考試"}, {"單位", "國營", "職員", "資訊"}}
# $國營
# {{"國營", "薪水"}, {"單位", "國營", "考試"}, {"單位", "國營", "職員", "資訊"}}
# $考試
# {{"單位", "國營", "考試"}, {"國營", "考試", "薪水"}}
# $職員
# {{"單位", "國營", "考試", "職員"}, {"單位", "國營", "職員", "資訊"}}
# $薪水
# {{"國營", "薪水"}}
# $資訊
# {{"單位", "國營", "職員", "資訊"}}


plot(kst2, main = "知識結構 group = 6 " )


#####===== (C4) (KDD5) 知識結構的應用之一 (ksp2) =====#####

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")

BiocManager::install("Rgraphviz")
library(Rgraphviz)
lpath(ksp2)
install.packages("kst")
library(kst) 
kst2_relation = as.relation_ensemble( ksp2 ) # 可視化



plot( kst2_relation ) 
# { {}, {"國營", "薪水"}, {"副業", "國營", "薪水"}, {"單位", "國營", "考試"}, {"國營", "考試", "薪水"},
#   {"副業", "國營", "考試", "薪水"}, {"單位", "國營", "考試", "職員"}, {"單位", "國營", "考試", "薪水"},
#   {"單位", "國營", "職員", "資訊"}, {"副業", "單位", "國營", "考試", "薪水"}, {"單位", "國營", "考試",
#     "職員", "薪水"}, {"單位", "國營", "考試", "職員", "資訊"}, {"單位", "國營", "職員", "薪水", "資訊"},
#   <<set(6)>>, <<set(6)>>, <<set(6)>>, <<set(7)>>
# }

install.packages("igraph")  
library(igraph)              

gg = make_graph( edges = c( c("國營"),
                            c("國營", "薪水"),
                            c("副業", "國營", "薪水"),
                            c("單位", "國營", "考試"),
                            c("國營", "考試", "薪水"),
                            c("副業", "國營", "考試", "薪水"),
                            c("單位", "國營", "考試", "職員"),
                            c("單位", "國營", "考試", "薪水"),
                            c("單位", "國營", "職員", "資訊"),
                            c("副業", "單位", "國營", "考試", "薪水"),
                            c("單位", "國營", "考試", "職員", "薪水")
                            # c("單位", "國營", "考試", "職員", "資訊")
                            # c("單位", "國營", "職員", "薪水", "資訊"),
                            # c("副業", "單位", "國營", "考試", "薪水", "職員"),
                            # c("副業", "單位", "國營", "考試", "職員", "薪水"),
                            # c("副業", "單位", "國營", "考試", "職員", "資訊"),
                            # c("副業", "單位", "國營", "職員", "薪水", "資訊")
) )

gg = make_graph( edges = c(
  c("國營", "薪水"),
  c("副業", "國營", "薪水"),
  c("單位", "國營", "考試"),
  c("國營", "考試", "薪水"),
  c("單位", "國營", "考試", "職員"),
  c("單位", "國營", "職員", "資訊"),
  c("單位", "國營", "考試", "職員", "資訊")
) )

#   {}, 
#   {"國營", "薪水"}, 
#   {"副業", "國營", "薪水"}, 
#   {"單位", "國營", "考試"}, 
#   {"國營", "考試", "薪水"},
#   {"單位", "國營", "考試", "職員"}, 
#   {"單位", "國營", "職員", "資訊"}, 
#   <<set(7)>>
plot( gg )



install.packages("sna")
install.packages("network")
library(sna)           
library(network)
A10 <- get.adjacency(gg, sparse=FALSE) # 鄰接矩陣
gg10 <- network::as.network.matrix(A10)
gg10
cores10 = coreness(gg) # 計算每個節點的核心度 有越多條邊的越核心
cores10


n <- network.size(gg10)
theta <- seq(0, 2*pi, length.out = n+1)[-(n+1)]
layout <- matrix(c(cos(theta), sin(theta)), ncol=2)

sna::gplot(gg10,
           coord = layout,
           vertex.cex = cores10,
           vertex.col = cores10,
           edge.col = "darkgray",
           main = "中心度圖",
           usearrows = FALSE,
           displaylabels = TRUE)



      
