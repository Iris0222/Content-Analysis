
wkDir = "C:/文本分析"
setwd(wkDir)
library( "stringr" ) 
library(RCurl)
library(XML)
library( openxlsx )
library(readxl)
library( rio )
library( stringr )

library(flextable)
library(ggplot2)
library(data.table)
# install.packages("vctrs")
library(dplyr)
# ======== Data Acquisition =========== "爆" -> 1000, "-" -> 0 

RR = read.csv('new.csv', header = TRUE, sep = ',', fileEncoding = "utf-8" )  # nrow(RR) = 14959

RR <- filter( RR, sub.author != "" ) # 保留 sub.author 欄位不為空的資料列，確保有回文作者 # nrow(RR) = 13945

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

# dim(RR)
# head(RR,2)   #-- [1] 13945    13  
# colnames(RR) : "title", "author", "time", "gp", "bp", "mainText", "sub.author", "sub.time", "sub.gp", "sub.bp", "sub.Text", "nch", "sub.nch", "Rdate"
# write.csv(RR, "output.csv", row.names = TRUE, fileEncoding = "BIG5")


# ================= Data Exploration ==============================
#################### 不同 title（標題）和 author（作者）的數量。
#################### 計算發文和回文時間的範圍。
length(unique(RR$title))         # 2214篇貼文
length(unique(RR$author))        ## 1492個人參與發文
length(unique(RR$sub.author))    ## 4316個作者回覆貼文

range(RR$time,na.rm=T)           ## remove NA:忽略空值, time的範圍:"2013-06-06 14:35:23" "2023-04-20 16:06:01"


# ================= Data Transformation ==============================
# 資料按作者進行聚合，並計算每位作者的回文次數（FF）、回文總字數（MM）、首次和最後發文時間（D0 和 Df），以及總 GP（Sgp）和 BP（Sbp）。

# setDT( RR, key="author" )
setDT(RR)

Ra = RR[, .(FF=length(unique(sub.time)), MM=sum(sub.nch), D0=min(Rdate), Df=max(Rdate), Sgp=sum(sub.gp), Sbp=sum(sub.bp)), by=sub.author]

# Ra$Sgp[ is.na( Ra$Sgp ) ] = 0
# Ra$Sbp[ is.na( Ra$Sbp ) ] = 0

dim(Ra) # 7項:sub.author, FF, MM, D0, Df, Sgp, Sbp


range(Ra$FF)   # 回文次數 1-284
range(Ra$MM)   # 回文總字數: 0-109813  

range( Ra$Sgp ) # gp_sum: 0-855      
range( Ra$Sbp ) # bp_sum: 0-2136    


## 切割回文篇數
Ra$FF0 = cut(Ra$FF, breaks=c( -1,0,1,9,19,39,99, 299 ) )   
table(Ra$FF0)
# (-1,0]    (0,1]    (1,9]   (9,19]  (19,39]  (39,99] (99,299] 
# 0          2685     1389      140       66       31        5  


## 切割回文字數
Ra$MM0 = cut(Ra$MM, breaks=c(-1,0,1,99,999,9999,59999, 79999, 99999, 110000 ))
table(Ra$MM0)
# (-1,0]           (0,1]          (1,99]        (99,999]     (999,1e+04]   (1e+04,6e+04]   (6e+04,8e+04]   (8e+04,1e+05] (1e+05,1.1e+05] 
# 11               5            1510            2255             510              21               2               1               1 

## 切割GP
Ra$Sgp0 = cut(Ra$Sgp, breaks=c(-1,0,1,9,19,39,99, 299, 499, 699, 899  ) )
table(Ra$Sgp0)
# (-1,0]     (0,1]     (1,9]    (9,19]   (19,39]   (39,99]  (99,299] (299,499] (499,699] (699,899] 
# 1020         761      1655       386       243       174        67         3         4         3 


## 切割BP
Ra$Sbp0 = cut( Ra$Sbp, breaks=c(-1,0,1,9,19,39,99, 299, 499, 699, 899, 2199 ) )
table(Ra$Sbp0)
# (-1,0]         (0,1]         (1,9]        (9,19]       (19,39]       (39,99]      (99,299]     (299,499]     (499,699]     (699,899] (899,2.2e+03] 
#   4272             0             6            11            12             7             6             1             0             0             1


# year 年分  Y0: min year, Yf: max year 
Ra$Y0 = as.integer( substr(Ra$D0,1,4) ) # print(Ra$D0) = "2021-04-03" -> 2021
Ra$Yf = as.integer( substr(Ra$Df,1,4) )

dim(Ra)
head(Ra,2)   #-- [1] 4316   13
# sub.author        FF   MM         D0           Df            Sgp    Sbp     FF0         MM0           Sgp0    Sbp0    Y0    Yf
# 1: sonysonyson2    7   747        2021-04-03   2022-10-20    14       0     (1,9]       (99,999]      (9,19]  (-1,0]  2021  2022
# 2:       o89406   48   3009       2020-11-09   2023-03-31    26       0     (39,99]     (999,1e+04]   (19,39] (-1,0]  2020  2023


# ================= Data Modeling ============================== 
# 探討回覆文章的作者使用巴哈姆特的年份 從?到?
addmargins( table(Ra$Y0,Ra$Yf) )
#     結束 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023  Sum
# 開始    
# 2013      3    0    0    0    0    0    0    0    0    0    0     3
# 2014      0   10    0    0    1    0    0    0    0    2    4     17
# 2015      0    0   30    1    1    0    0    2    9    3    4     50
# 2016      0    0    0   49    0    0    1    1    7    9   13     80
# 2017      0    0    0    0   31    2    0    2    4    8    9     56
# 2018      0    0    0    0    0   64    3    0    8    9   12     96
# 2019      0    0    0    0    0    0  141    5   27   37   34     244
# 2020      0    0    0    0    0    0    0  240   74   58   58     430
# 2021      0    0    0    0    0    0    0    0 1345  308  183     1836
# 2022      0    0    0    0    0    0    0    0    0  998  139     1137
# 2023      0    0    0    0    0    0    0    0    0    0  367     367
#
# Sum       3   10   30   50   33   66  145  250 1474 1432  823     4316


# ================= Data Modeling ============================== 
# 每個user得到的gp bp總和
addmargins( table(Ra$Sbp0,Ra$Sgp0) )
#           GP  (-1,0] (0,1] (1,9] (9,19] (19,39] (39,99] (99,299] (299,499] (499,699] (699,899]  Sum
#   BP
# (-1,0]          1019   760  1646    378     239     164       59         1         4         2 4272
# (0,1]              0     0     0      0       0       0        0         0         0         0    0
# (1,9]              1     0     1      2       0       0        2         0         0         0    6
# (9,19]             0     1     4      1       0       4        1         0         0         0   11
# (19,39]            0     0     2      2       2       3        2         1         0         0   12
# (39,99]            0     0     0      2       0       3        2         0         0         0    7
# (99,299]           0     0     2      1       2       0        1         0         0         0    6
# (299,499]          0     0     0      0       0       0        0         1         0         0    1
# (499,699]          0     0     0      0       0       0        0         0         0         0    0
# (699,899]          0     0     0      0       0       0        0         0         0         0    0
# (899,2.2e+03]      0     0     0      0       0       0        0         0         0         1    1
# Sum             1020   761  1655    386     243     174       67         3         4         3 4316


# ================= GP & BP Result Interpretation ====================== from here

# (1) 有超過9成的文章都沒有 BP, 代表這個社群應該蠻友善的
# (2) GP 拿最多的人-> qbgeneral
Ra[ Ra$Sgp == max(Ra$Sgp) ]
# sub.author  FF    MM         D0         Df         Sgp   Sbp   Sbp0      Sgp0        FF0
# qbgeneral   183   22612      2020-11-02 2023-04-13 855   0     (-1,0]    (699,899]   (99,299]
## 總共發表183次文，而且bp是0, 我猜這個人的回應可能讓大家很有共鳴。

# (3) BP 拿最多的人 -> abcgg12
Ra[ Ra$Sbp == max(Ra$Sbp) ]
#   sub.author  FF    MM         D0           Df           Sgp  Sbp            Sbp0            Sgp0       FF0
#   abcgg12     284   32359      2020-02-06   2023-02-04   837  2136           (2e+03,2.2e+03] (699,899]  (99,299]
##  總共發表284次文, 但是gp也不少，我猜這個人可能發了很多篇廢文所以bp才會這麼多，或著是在巴哈裡面認同他的想法大概佔1/3。


# ================= Result Interpretation ==============================
# colnames(RR) = "title","author","time","gp","bp","mainText","sub.author","sub.time","sub.gp","sub.bp","sub.Text","nch","sub.nch","Rdate"
# 使用次數最高的user

author_years <- RR[, .(D0 = min(Rdate), Df = max(Rdate)), by = sub.author]
# sub.author         D0         Df
# 1:   dh5987462 2022-02-10 2023-04-13
# 2:   qbgeneral 2020-11-02 2023-04-13
# 3:      ccc886 2021-04-25 2023-01-20
# 4:  ns02931302 2022-07-24 2023-03-15
# 5:  n48904040n 2019-03-06 2023-04-14
# ---                                  
#   4312: thaliaangel 2021-03-21 2021-03-21
# 4313:   x68422486 2021-03-22 2021-03-22
# 4314:    wbqp2678 2021-03-22 2021-03-22
# 4315:      joself 2021-03-22 2021-03-22
# 4316:   freestyle 2021-03-23 2021-03-23


author_years[, years_active := as.integer(substr(Df, 1, 4)) - as.integer(substr(D0, 1, 4))]
# sub.author         D0         Df years_active
# 1:   dh5987462 2022-02-10 2023-04-13            1
# 2:   qbgeneral 2020-11-02 2023-04-13            3
# 3:      ccc886 2021-04-25 2023-01-20            2
# 4:  ns02931302 2022-07-24 2023-03-15            1
# 5:  n48904040n 2019-03-06 2023-04-14            4
# ---                                               
#   4312: thaliaangel 2021-03-21 2021-03-21            0
# 4313:   x68422486 2021-03-22 2021-03-22            0
# 4314:    wbqp2678 2021-03-22 2021-03-22            0
# 4315:      joself 2021-03-22 2021-03-22            0
# 4316:   freestyle 2021-03-23 2021-03-23            0

unique(author_years$years_active) # 1 3 2 4 0 7 5 9 6 8
nine_years_users <- author_years[years_active == 9, sub.author] 
# 使用9年的user: "D109424", "konyosam", "play1128", "edgarfigard"



D109424_plot <- ggplot(
  data.frame(year = substr(RR$Rdate[RR$sub.author == "D109424"], 1, 4)), 
  aes(x = year) 
) + geom_bar()  
  
konyosam_plot <- ggplot(
  data.frame(year = substr(RR$Rdate[RR$sub.author == "konyosam"], 1, 4)), 
  aes(x = year) 
) + geom_bar()  

play1128_plot <- ggplot(
  data.frame(year = substr(RR$Rdate[RR$sub.author == "play1128"], 1, 4)), 
  aes(x = year) 
) + geom_bar()  

edgarfigard_plot <- ggplot(
  data.frame(year = substr(RR$Rdate[RR$sub.author == "edgarfigard"], 1, 4)), 
  aes(x = year) 
) + geom_bar()  




# ================= Data Modeling ==============================
addmargins( table(Ra$FF0,Ra$MM0) ) # FF回文次數, MM: 回文總字數
#      字數 (-1,0] (0,1] (1,99] (99,999] (999,1e+04] (1e+04,6e+04] (6e+04,8e+04] (8e+04,1e+05] (1e+05,1.1e+05]  Sum
# 篇數
# (-1,0]        0     0      0        0           0             0             0             0               0    0
# (0,1]        11     5   1336     1271          62             0             0             0               0 2685
# (1,9]         0     0    174      948         265             2             0             0               0 1389
# (9,19]        0     0      0       32         105             3             0             0               0  140
# (19,39]       0     0      0        4          58             4             0             0               0   66
# (39,99]       0     0      0        0          20             9             1             1               0   31
# (99,299]      0     0      0        0           0             3             1             0               1    5
#
# Sum          11     5   1510     2255         510            21             2             1               1 4316


Ra[ as.numeric(Ra$MM0)==9 ] #十萬-十一萬
RR[ RR$sub.author == "tt700910" ]
# sub.author  FF     MM         D0         Df Sgp Sbp      FF0             MM0      Sgp0   Sbp0   Y0   Yf
# 1:   tt700910 226 109813 2021-01-02 2023-04-20 696   0 (99,299] (1e+05,1.1e+05] (499,699] (-1,0] 2021 2023


# ================= text analyze ==============================
setDT( RR, key = "title" )
Rt = RR[, .( tt=unique(mainText), MM=unique( nch ), gp=unique(gp), bp=unique(bp), res=length(sub.author), D0= as.Date(unique(time)) , Df=max(Rdate) ), by=title ]
# res: title下的回文作者數量, D0: 發文時間, Df: 最後回文時間

# year 年分  Y0: min year, Yf: max year 
Rt$Y0 = as.integer( substr(Rt$D0,1,4) ) # 發文年分
Rt$Yf = as.integer( substr(Rt$Df,1,4) ) # 最後回文年分

head( Rt, 2 )
dim( Rt ) 
# colnames(Rt) -> "title","tt(mainText)","MM(nch)","gp","bp","res(title下的回文作者數量)","D0(發文時間)","Df(最後回文時間)","Y0(發文年分)","Yf(最後回文年分)"




range(Rt$MM)   #-- [1] 0   12499  主文總字數範圍

range( Rt$res )   #-- [1]  1   19  回文篇數範圍

range( Rt$gp ) #-- [1]   0 1000    gp

range( Rt$bp ) #-- [1]   0 156    bp


## 切割回文篇數
Rt$res0 = cut(Rt$res, breaks=c( -1,0,1,9,15,20 ) )
table(Rt$res0)
# (-1,0]   (0,1]   (1,9]  (9,15] (15,20] 
#      0     376    1335     278     227 


## 切割回文字數
Rt$MM0 = cut(Rt$MM, breaks=c( -1,0,1,99,999,1999,3999,7999,9999, 19999 ) ) 
table(Rt$MM0)
# (-1,0]    (0,1]        (1,99]      (99,999]   (999,2e+03] (2e+03,4e+03] (4e+03,8e+03] (8e+03,1e+04] (1e+04,2e+04] 
#      2        0           421          1555           160            52            21             4             1 


## 切割GP
Rt$gp0 = cut(Rt$gp, breaks=c(-1,0,1,9,19,39,99, 299, 499, 699, 999, 1099  ) )
table(Rt$gp0)
# (-1,0]         (0,1]         (1,9]        (9,19]       (19,39]       (39,99]      (99,299]     (299,499]     (499,699]     (699,999]  (999,1.1e+03] 
#    600           408           771           141           122            83            69            12             6             3          1


## 切割BP
Rt$bp0 = cut( Rt$bp, breaks=c(-1,0,1,9,19,39,99, 199 ) )
table(Rt$bp0)
# (-1,0]    (0,1]    (1,9]   (9,19]  (19,39]  (39,99] (99,199] 
#   1942        0       70       57       78       68        1 



addmargins( table(Rt$Y0,Rt$Yf) ) # 主題討論年分 ? to ?
#      2015 2016 2017 2018 2019 2020 2021 2022 2023  Sum
# 2013    2    1    0    0    0    0    0    0    0    3
# 2015    3    0    0    0    0    0    0    1    0    4
# 2016    0    5    0    0    0    0    0    0    1    6
# 2017    0    0    4    1    0    0    0    0    0    5
# 2018    0    0    0    4    0    0    3    3    3   13
# 2019    0    0    0    0   10    2   13    9    0   34
# 2020    0    0    0    0    0   20   23   29    8   80
# 2021    0    0    0    0    0    0  822   53   10  885
# 2022    0    0    0    0    0    0    0  859   35  894
# 2023    0    0    0    0    0    0    0    0  292  292
# Sum     5    6    4    5   10   22  861  954  349 2216


addmargins( table(Rt$gp0,Rt$bp0) )
#               (-1,0] (0,1] (1,9] (9,19] (19,39] (39,99] (99,199]  Sum
# (-1,0]           563     0     4     10      14       9        0  600
# (0,1]            369     0     5      9      15      10        0  408
# (1,9]            670     0    24     15      25      37        0  771
# (9,19]           118     0     9      4       7       3        0  141
# (19,39]           97     0    11      3       6       5        0  122
# (39,99]           65     0     8      5       2       2        1   83
# (99,299]          47     0     6      6       8       2        0   69
# (299,499]          8     0     2      2       0       0        0   12
# (499,699]          3     0     1      1       1       0        0    6
# (699,999]          1     0     0      2       0       0        0    3
# (999,1.1e+03]      1     0     0      0       0       0        0    1
# Sum             1942     0    70     57      78      68        1 2216



# ================= 主題數據框到主題價值模型(Topic Value Model) ==============================


setDT(RR, key = "title") 
Pv = RR[, .(postNo=length(sub.time), D0=min(sub.time), Df=max(sub.time), Nauthor=length(unique(sub.author)), 
            chCount=sum(nch,na.rm=T)), by=title]


# colnames(Pv) -> "title"  "postNo(回覆數量)"  "D0(最早回覆時間)"      "Df(最後回覆時間)"      "Nauthor(回覆貼文的作者數量)" "chCount(貼文字數)"


Pv$postNo0 = cut(Pv$postNo, breaks=c(0,1,9,99,999,7999))
table(Pv$postNo0)
# (0,1]       (1,9]      (9,99]    (99,999] (999,8e+03] 
# 376        1334         504           0           0


Pv$chCount0 = cut(Pv$chCount, breaks=c(-1,0,9,99,999,9999,99999,999999))
table(Pv$chCount0)
# (-1,0]         (0,9]        (9,99]      (99,999]   (999,1e+04] (1e+04,1e+05] (1e+05,1e+06] 
# 2             2           143           945           934           182             6


Pv$nDay = as.Date(Pv$Df)-as.Date(Pv$D0) # title討論幾天
range(Pv$nDay)   #-- 0 2629天


Pv$nDay0 = cut(as.numeric(Pv$nDay), breaks=c(-1,0,7,30,100,400,1200,4300))
table(Pv$nDay0)
# (-1,0]             (0,7]            (7,30]          (30,100]         (100,400]     (400,1.2e+03] (1.2e+03,4.3e+03] 
# 657              1055               245                87                79                80                11



addmargins( table(Pv$nDay0, Pv$chCount0) )
#  nDay0\chCount0   (-1,0] (0,9] (9,99] (99,999] (999,1e+04] (1e+04,1e+05] (1e+05,1e+06]  Sum
# (-1,0]                 0     1    119      414         111            12             0  657   #-- 一日話題
# (0,7]                  1     1     20      406         563            63             1 1055   #-- 一週
# (7,30]                 0     0      3       72         137            32             1  245   #-- 一月
# (30,100]               0     0      0       22          39            25             1   87   #-- 三月
# (100,400]              1     0      1       18          39            18             2   79   #-- 一年
# (400,1.2e+03]          0     0      0       12          41            26             1   80   #-- 三年
# (1.2e+03,4.3e+03]      0     0      0        1           4             6             0   11   #-- 逾三年
# Sum                    2     2    143      945         934           182             6 2214
#                            數十字話題  數百字..    數千字..     數萬字..      數十萬字..




