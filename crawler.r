# 巴哈姆特職場版 爬蟲


wkDir = "C:/文本分析"
setwd(wkDir)
# 
# library(RCurl)
# 
# library(XML)
# response = getURL( "https://forum.gamer.com.tw/B.php?bsn=60561", .encoding = 'utf8' ) 
# response.parser = htmlParse(response, asText=TRUE)
# print(response.parser) # 輸出的是帶有階層結構的解析內容
# 
# 
# title <- xpathSApply(response.parser, "//*[@id='BH-master']
#                                          //*[@class='b-list__row b-list-item b-imglist-item']
#                                          //*[@class='imglist-text']
#                                          /div/p")
# 
# 
# print(title) # <p data-gtm="B頁文章列表-縮圖" href="C.php?bsn=60561&amp;snA=25453&amp;tnum=5" class="b-list__main__title">【問題】寺廟打雜美編(想離職)</p>
# 
# 
# author <- xpathSApply(response.parser, "//*[@id='BH-master']
#                                          //*[@class='b-list__row b-list-item b-imglist-item']
#                                          //*[@class='b-list__count__user']", xmlValue )
# 
# 
# # print(author)
# # print(author[1])
# author <- gsub("\n", "", author) # global substitution
# 
# # ============ 寫檔 ================
# library( openxlsx )
# titles <- sapply(title, xmlValue)
# links <- sapply(title, function(x) xpathSApply(x, "@href"))
# links <- paste0("https://forum.gamer.com.tw/", links)
# title.df <- data.frame(Title = titles, Link = links, Author = author, stringsAsFactors = FALSE)
# write.xlsx(title.df, "title.xlsx")
# 
# # # ================================================
# 
# 
# library(openxlsx)
# library( stringr )
# 
# GetContentList <- function( x ) # 文章內容、日期時間、GP與BP, list結構:標題 作者 時間 gp bp 內文
# {
#   print( str_c("標題 : ", x[1] ) ) 
#   
#   url = x[2]
#   author = x[3] 
#   content = getURL( url, .encoding = 'utf8' )
#   content.parser = htmlParse( content, asText=TRUE)
#   # print(content.parser)
#   
#   ##### content #####
#   content.list <- xpathSApply( content.parser,  "//*[@id='BH-master']
#                                                  //*[@class='c-section']
#                                                  //*[@class='c-section__main c-post ']
#                                                  //*[@class='c-post__body']
#                                                  //*[@class='c-article FM-P2']
#                                                  //*[@class='c-article__content']", xmlValue )
#   
#   
#   
#   content.list <- gsub("\n", "", content.list)
#   content.list <- paste(unlist(content.list), collapse = " ")
#   # print(content.list)
#   
#   ##### date time #####
#   
#   time = getURL( url, .encoding = 'utf8' )
#   time.parser = htmlParse( time, asText=TRUE)
#   # print(time.parser)
#   time.value <- xpathSApply( time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[3]/a/attribute::data-mtime" )  
#                         
# 
#   if (  is.null(time.value) ) 
#   {
#     print( "is null" ) 
#     time.value <- xpathSApply( time.parser, "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[3]/a/attribute::data-mtime" )
#     
#   } # if 
#   
#   if( is.null(time.value) )
#   {
#     time.value = "Date Time Error" 
#   } 
#   # print(content.list)
#   # print( time.value )
#   # readline(prompt = "按 Enter 鍵繼續...")
#   content.list <- append( content.list, time.value, after = 0 ) # 插入索引 0 的位置
#   # print(content.list)
#   
#   # ------------------gp bp--------------------
#   
#   gp.value <- xpathSApply( time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue )  
# 
#   if ( is.null( gp.value ) )
#   {
#     gp.value <- xpathSApply( time.parser,  "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue )
#   } 
#   
#   
#   bp.value <- xpathSApply( time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue )
# 
#   if ( is.null(bp.value) )
#   {
#     bp.value <- xpathSApply( time.parser,  "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue )
#   }
# 
#   if ( is.null( gp.value ) )
#   {
#     gp.value = "GP Error"
#   }
#   
#   if( is.null( bp.value ) ) 
#   {
#     bp.value = "BP Error"
#   }
#   
#   # print( c( str_c( "gp :", gp.value), str_c( "bp :", bp.value))  )
#   content.list <- append( content.list, gp.value, after = 1 )
#   content.list <- append( content.list, bp.value, after = 2 )
# 
#   ##### 加入標題與作者 #####
#   content.list <- append(content.list, author, after = 0)  # 插入作者
#   content.list <- append(content.list, x[1], after = 0)    # 插入標題
#   
#   return ( content.list  ) 
#   
# } 
# 
# # apply : 1 -> row, 2 -> column
# # title <- read.xlsx("title.xlsx", sheet = 1)
# 
# ll <- lapply(1:nrow(title.df), function(i) GetContentList(title.df[i, ]))
# # print(ll[[5]][1])
# 
# 
# # ------------------ 儲存 --------------------------
# library(openxlsx)
# 
# # 創建 Excel 工作簿
# wb <- createWorkbook(creator = 'iris', title = '巴哈姆特爬蟲') 
# addWorksheet(wb, sheetName = 'sheet1')
# 
# # 將 ll 轉換為 data.frame
# df <- do.call(rbind, lapply(ll, function(row) {
#   # 確保每個欄位的長度不會太長，如果太長就截斷
#   truncated_row <- lapply(row, function(x) {
#     if(is.character(x) && nchar(x) > 32000) {
#       return(substr(x, 1, 32000))
#     }
#     return(x)
#   })
#   return(truncated_row)
# }))
# 
# # 轉換為 data.frame 並加上欄位名稱
# df <- as.data.frame(df, stringsAsFactors = FALSE)
# colnames(df) <- c("標題", "作者", "發文日期", "GP", "BP", "內文")
# 
# # 一次寫入所有資料
# writeData(wb, sheet = 'sheet1', x = df)
# 
# # 保存工作簿
# saveWorkbook(wb, "巴哈姆特爬蟲.xlsx", overwrite = TRUE)




# ----------------- 抓取20頁 ---------------

wkDir = "C:/文本分析"
setwd(wkDir)

library(RCurl)
library(XML)
library(openxlsx)
library(stringr)

# 函數：抓取單一頁面的標題列表
getPageTitles <- function(page_num) {
  url <- sprintf("https://forum.gamer.com.tw/B.php?bsn=60561&page=%d", page_num)
  response = getURL(url, .encoding = 'utf8')
  response.parser = htmlParse(response, asText=TRUE)
  
  title <- xpathSApply(response.parser, "//*[@id='BH-master']
                       //*[@class='b-list__row b-list-item b-imglist-item']
                       //*[@class='imglist-text']
                       /div/p")
  
  author <- xpathSApply(response.parser, "//*[@id='BH-master']
                        //*[@class='b-list__row b-list-item b-imglist-item']
                        //*[@class='b-list__count__user']", xmlValue)
  
  author <- gsub("\n", "", author)
  
  titles <- sapply(title, xmlValue)
  links <- sapply(title, function(x) xpathSApply(x, "@href"))
  links <- paste0("https://forum.gamer.com.tw/", links)
  
  return(data.frame(Title = titles, Link = links, Author = author, stringsAsFactors = FALSE))
}

GetContentList <- function(x) # 文章內容、日期時間、GP與BP, list結構:標題 作者 時間 gp bp 內文 url
{
  print(str_c("標題 : ", x[1]))
  
  url = x[2]
  author = x[3]
  content = getURL(url, .encoding = 'utf8')
  content.parser = htmlParse(content, asText=TRUE)
  
  ##### content #####
  content.list <- xpathSApply(content.parser, "//*[@id='BH-master']
                             //*[@class='c-section']
                             //*[@class='c-section__main c-post ']
                             //*[@class='c-post__body']
                             //*[@class='c-article FM-P2']
                             //*[@class='c-article__content']", xmlValue)
  
  content.list <- gsub("\n", "", content.list)
  content.list <- paste(unlist(content.list), collapse = " ")
  
  ##### date time #####
  time = getURL(url, .encoding = 'utf8')
  time.parser = htmlParse(time, asText=TRUE)
  time.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[3]/a/attribute::data-mtime")
  
  if (is.null(time.value)) {
    print("is null")
    time.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[3]/a/attribute::data-mtime")
  }
  
  if(is.null(time.value)) {
    time.value = "Date Time Error"
  }
  
  content.list <- append(content.list, time.value, after = 0)
  
  # ------------------gp bp--------------------
  gp.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue)
  
  if (is.null(gp.value)) {
    gp.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue)
  }
  
  bp.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue)
  
  if (is.null(bp.value)) {
    bp.value <- xpathSApply(time.parser, "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue)
  }
  
  if (is.null(gp.value)) {
    gp.value = "GP Error"
  }
  
  if(is.null(bp.value)) {
    bp.value = "BP Error"
  }
  
  content.list <- append(content.list, gp.value, after = 1)
  content.list <- append(content.list, bp.value, after = 2)
  
  ##### 加入標題與作者 #####
  content.list <- append(content.list, author, after = 0)
  content.list <- append(content.list, x[1], after = 0)
  content.list <- append(content.list, x[2], after = 6)
 
  return(content.list)
}

# 主程式：抓取20頁資料
all_titles <- data.frame()
for(page in 1:1) {
  print(sprintf("正在抓取第 %d 頁...", page))
  page_data <- getPageTitles(page)
  all_titles <- rbind(all_titles, page_data)
  Sys.sleep(2)  # 避免過快請求
}

# 保存標題列表
write.xlsx(all_titles, "title.xlsx")

# 抓取所有文章內容
ll <- lapply(1:nrow(all_titles), function(i) {
  print(sprintf("正在抓取第 %d 篇文章...", i))
  result <- GetContentList(all_titles[i, ])
  Sys.sleep(1)  # 避免過快請求
  return(result)
})

# 創建 Excel 工作簿並保存
wb <- createWorkbook(creator = 'iris', title = '巴哈姆特爬蟲')
addWorksheet(wb, sheetName = 'sheet1')

# 將 ll 轉換為 data.frame
df <- do.call(rbind, lapply(ll, function(row) {
  truncated_row <- lapply(row, function(x) {
    if(is.character(x) && nchar(x) > 32000) {
      return(substr(x, 1, 32000))
    }
    return(x)
  })
  return(truncated_row)
}))

# 轉換為 data.frame 並加上欄位名稱
df <- as.data.frame(df, stringsAsFactors = FALSE)
colnames(df) <- c("標題", "作者", "發文日期", "GP", "BP", "內文", "URL")

# 寫入資料
writeData(wb, sheet = 'sheet1', x = df)

# 保存工作簿
saveWorkbook(wb, "巴哈姆特爬蟲.xlsx", overwrite = TRUE)

