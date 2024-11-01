wkDir = "C:/文本分析"
setwd(wkDir)


library(data.table)

RR = read.csv('new.csv', header = TRUE, sep = ',', fileEncoding = "utf-8" ) 

library(dplyr)
RR <- filter( RR, RR$sub.author != "" ); head( RR )

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

dim(RR);   head(RR,2)       #-- [1] 13945    14
# colnames(RR) -> "title","author","time","gp","bp","mainText","sub.author","sub.time","sub.gp","sub.bp","sub.Text","nch","sub.nch","Rdate"


from here

#####===== (1B) 中文文本分析工具jieba =====#####
##== jieba軟件包：來自百度的某開發者, 基于MIT协议，免费且Open Source, 支援簡體、繁體
#    -- C++库以R封装，並通过Rcpp进行调用，效率很好
##== jieba原理
#    -- (1)將輸入字串進行正規式(regular expression)分析
#    -- (2)比對字典樹(trie)建立句子切分組合的有向無環圖(DAG, Directed Acyclic Graph)
#    -- (3)以隱式馬克夫模型(HMM, Hidden Markov Model) Viterbi動態規劃演算法，計算最佳切分組合

#####=====*(1C) 啟動jieba分詞引擎 (worker()) =====#####
install.packages("jiebaR") # 中文分詞（斷詞）
library("jiebaR")     
wkr1 = worker()     # jieba 分詞引擎


########## (2) 斷詞 ##########
RR$title[1]   #-- [1] "20歲到30歲沒專長的年輕人都在做甚麼工作"

#####===== 結巴分詞的三種調用方法 =====#####
##== (方法1) worker引擎(一種數據結構)直接分詞：
wkr1[ RR$title[1] ]            
#-- [1] "20"     "歲"     "到"     "30"     "歲"     "沒"     "專長"   "的"     "年輕人" "都"     "在"     "做"     "甚麼"   "工作"   


##== (方法2) 詞語導向worker引擎分詞：
wkr1 <= RR$title[1]           
#-- [1] "20"     "歲"     "到"     "30"     "歲"     "沒"     "專長"   "的"     "年輕人" "都"     "在"     "做"     "甚麼"   "工作" 


##== (方法3) 用segment()函數調用worker()解析詞語：
segment( RR$title[1], wkr1 )   
#-- [1] "20"     "歲"     "到"     "30"     "歲"     "沒"     "專長"   "的"     "年輕人" "都"     "在"     "做"     "甚麼"   "工作" 



#####===== (2C) (KDD3) 實務數據的斷詞 (RR$title/RR$text-->content/Ncontent) =====#####
##== 只就 RR$title 關鍵字太少，難以分類，所以要用 RR$text

RRtitle = unique(RR$title);   length(RRtitle)   #-- (1) 共有2821個討論主題

content = NULL;   Ncontent = NULL            

for (title in RRtitle) {                                   #-- (2) 同一討論主題的回文內容(RR$text),以"\n\n"串接
  content = c(content, paste(title, paste(RR$sub.Text[which(RR$title==title)],collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))   #-- (3) length(which(條件)): 表示滿足條件的個數
}

length(content);   head(content,2)     #-- [1] 2821        #-- (4) content:討論主題的回文內容(RR$text)集合,
# [1] "20歲到30歲沒專長的年輕人都在做甚麼工作 輪班囉設備工程、產品工程、技術員。學校不會教你組裝業界機台、看良率分析啦！大多數文憑只是張面試門票\n\n就大叔我的觀察有幾種外送員貨運電子廠輪班作業員餐飲超商派遣直銷賣車房仲藍藍路差不多就這樣了\n\n車手宮廟跳8+9保險房仲保全\n\n導遊 特助 保全直銷造勢\n\n我自己身邊的小學、國中同學跳8+9（家裡開宮廟）輪班操作技術員賣車保險房仲木工水電開店（生意不錯）里長（最近要參選）全職主婦（嫁得好的那種）免洗作業員、品保（大宗）服務業店員酒促小姐CNC技術員混得越好的人越不想讓人知道他做什麼做業務這幾年真的很差的樣子\n\n服務業、夜班保全爽滑\n\n輪班星人啊...有新鮮的肝就可以做了\n\n做蝦皮代購  月薪十萬\n\n做露天代購  月薪二十萬\n\n做自己，月入O萬\n\n在家養粉絲賺流量錢...\n\n船員\n\n志願役 退伍後 一間小間的科技公司 後來又回役了\n\n修手機\n\n輪班技術員\n\n仲介、服務生、作業員、工廠技師(焊接、CNC等)很多人28~3x說好聽是刷工作經驗，說難聽點一直換工作資料來源:面試這年齡層很容易看到光履歷就有一頁半到二頁的\n\n經營網路賣場賺仲介費自家有家業幫忙做\n\n我做文字編輯，還在想下一步要幹嘛\n\n保全吧，或者警衛做到退休"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [2] "【問題】［保全］哪種固定日班類型比較好 ※ 引述《qwer50123 (ㄚ肥)》之銘言> 目前y25做過傳產一段時間 > 發現又熱又累又危險錢也沒多少 > 目前想找日班保全做一輩子 > 目前有科技業保全 > 豪宅保全 > 社區保全 > 工廠保全 > 請問哪個比較推？ 做一輩子，好像想了太遠了些，目前退休年限65歲，也就是還得經歷40年。保全基本上還是「看點」，商辦的夜班保全基本上都是睡得很開心，而且一般上班族也比較不會管保全，倘若是「社區保全」，一個社區來個幾戶腦子有病的住戶，整天都在找其他住戶、總幹事、保全的麻煩，自以為很厲害，碰到這種社區光上班就覺得煩。而「豪宅保全」除了一般門禁管制外，對於各住戶的習慣、喜好…等也要稍微知悉，搭配物管秘書一起合作服務住戶，初期踏入需要學習很多，薪水也是頗高的，而且三節、過年、宴會服務…等，住戶多少都會給紅包，不過「薪水、額外」多就表示「很容易畢業」倘若可以的話，去豪宅學習一下，再去一般的案場會覺得輕鬆很多，不過薪水最起碼少一萬以上，至於我說的豪宅，是北部一坪至少100萬的那種，室內坪數至少100坪起跳，有些高級住宅往往自抬身價也說是豪宅XDDD \n\n科技業保全豪宅保全這兩個比較硬，錢比較多，要求也比較多有的要求要軍警退休...這方面背景的門面(長相)也會有一定要求長期走動+緊迫盯人，安檢沒過可能就要被HL或者掰掰社區大樓保全工廠保全這兩個看點只能多找多看有哪個點適合你了\n\n每個保全類型都有其特定的工作環境和任務，無法給予明確的推薦。您需要先考慮自己的興趣和優勢，然後了解每種保全類型的工作內容，再考慮哪一個適合您。以下是各種保全類型的一些相關資訊：科技業保全：需要具備一定的技術知識和對資訊安全的了解，能夠保障公司設施的安全和穩定運作。豪宅保全：需要有良好的服務態度和溝通能力，能夠照顧居民的需求和維護豪宅的安全管理。社區保全：需要熟悉社區的各種設施和活動，能夠保持社區的安寧和維護社區秩序。工廠保全：需要具備安全知識和急救技能，能夠預防事故和處理突發事件。最終，選擇哪種保全類型需要評估您自己的能力和興趣，請依照自己個人情況作出選擇。\n\n還不如去找台塑企業自聘的警衛雖然要排班輪班但是一天也就8小時一個月幾天紅就休幾天我堂弟做9年，111年扣繳憑單97萬，加一堆禮券福利年薪超過100萬據說他有個老同事140幾萬但是沒什麼時間摸魚工作很精實\n\n去工地工廠混一段時間休息一下還可以，等你實際去當保全一段時間後，才能確定你是不是願意做這垃圾工作一輩子每天12小時，時薪低沒福利休假又少，同事基本上都是底層邏輯思維，住戶把你當狗在看……做久了真的會與社會脫節你還年輕建議再考慮，當兵都比當保全好，保全等當完兵四五十歲再做都來得及"

Ncontent[1:10]   #-- [1] 19  5  1  2  1  5  3  5 19 19     #-- (5) Ncontent: 每個討論主題的回文數



########## (3) 詞性標註 (詞語標籤) ##########

#####===== (3A) 分詞引擎產生詞性(詞語標籤) (worker(type="tag"): content-->Dword0) =====#####

##== (1) 在工作引擎中加上"tag"參數，使分詞時同時產生詞性
wkr2 = worker(type="tag")   

##== (2) 以隱式迴圈指令 sapply()，對向量content的各個元素，分別操作 wkr2() (content-->Dword0)
Dword0 = sapply(content, function(x) segment(tolower(as.character(x)), wkr2));   
#-- 要先轉成文字型式(as.character)，並將其中的所有英文字轉為小寫(tolower)

##== (3) 切出的詞語 (第一個元素為Dword0[[1]]) 中 附有標籤
length(Dword0) # 2214
head(Dword0[[1]],10)   #-- [1] 2821  #-- 所得到的 Dword0 為由 content各個元素斷詞的表列(list)，共2821個表列元素
# m         zg        v        m       zg        v        n       uj        n        d 
# "20"     "歲"     "到"     "30"     "歲"     "沒"   "專長"     "的" "年輕人"     "都" 


##== 過濾掉 單字詞 與 數字帶頭的詞語 (Dword0-->Dword)
Dword = lapply(Dword0, function(x){ 
  nx = sapply(x,nchar); ## num of x 
  xA = x[nx>=2];      #-- 過濾掉 單字詞，取出字長大於2的詞語 nchar(x)>=2
  return( xA[-grep("[[:digit:]]+",xA)] ) 
} );   

length(Dword) # 2214
head(Dword[[1]],10) 
# n        n        r       vn        n       vn        n        n        n        n 
# "專長" "年輕人"   "甚麼"   "工作"   "輪班"   "設備"   "工程"   "產品"   "工程" "技術員"


#####===== 只保留動詞跟名詞 =====#####

Dword = lapply(Dword0, function(x){ 
                         nx=sapply(x,nchar); 
                         xA=x[nx>=2]; 
                         return( xA[substr(names(xA),1,1) %in% c("n","v")] )  
                       } );   

head(Dword[[1]],10)
# n        n       vn        n       vn        n        n        n        n        n 
# "專長" "年輕人"   "工作"   "輪班"   "設備"   "工程"   "產品"   "工程" "技術員"   "學校"



#####===== (3F) 停止詞庫 =====#####

wkr_stop = worker(stop_word="stop.txt",type="tag")
Dwords = sapply(content, function(x) segment(tolower(as.character(x)), wkr_stop));   #-- 要先轉成文字型式(as.character)，並將其中的所有英文字轉為小寫(tolower)

Dwords = lapply(Dwords, function(x){ nx=sapply(x,nchar); 
xA=x[nx>=2]; 
return( xA[substr(names(xA),1,1) %in% c("n")] ) } );   

Dwords[[1]]   


library(RColorBrewer)
library(wordcloud)
Dword = lapply(Dwords, function(x){ nx=sapply(x,nchar); xA=x[nx>=2]; 
return( xA[substr(names(xA),1,1) %in% c("n")] ) } );   
# n          n          n          n          n         nr          n          n          n          n          n          n          n 
head(Dword[[1]],10)

DWL = lapply(Dword, as.vector)
length(DWL)  # 2214

DW  = unlist(DWL)
length(DW)   #-- [1] 228757

TDW = table(DW) # 計算 DW 中每個詞語的出現頻率，生成一個頻率表 TDW
TDW100 = TDW[order(TDW,decreasing=T)]
TDW100[1:30]
# 薪水   老闆   時間   主管   薪資   建議   經驗   能力   同事   員工   小時   個人   社會   朋友 工程師   畢業   機會   環境   辦法   人生   業務   大學 
# 3149   2844   2699   2225   1710   1643   1557   1530   1470   1463   1321   1210   1163   1105   1028   1026   1008    982    954    848    831    811 
# 狀況   新人   職場   學歷   興趣   技術   專業   對方 
# 798    791    760    758    745    744    731    709
wordcloud(names(TDW100[1:30]), TDW100[1:30], col= rainbow(length(TDW100[1:30])), family="STKaiti")



