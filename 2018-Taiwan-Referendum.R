library(dplyr)
library(xml2)
library(rvest)

###公投第十案：台北市資料按里別###

##1.建置開票所與里別對照表

sc <- read.csv("里別投開票所.csv")
sc[,2] <- substring(sc[,2], 1, 3)
sc[,3] <- paste0(sc[,3], "區")

df1 <- group_by(sc, 鄉鎮市區, 村里) %>% summarise(min(票所編號))
df2 <- group_by(sc, 鄉鎮市區, 村里) %>% summarise(max(票所編號))
df <- left_join(df1, df2, by = c("村里", "鄉鎮市區")) %>% mutate("編號" = 鄉鎮市區)

a <- c("松山區", "信義區", "大安區", "中山區", "中正區", "大同區", "萬華區", "文山區", "南港區", "內湖區", "士林區", "北投區")
for (i in 1:12) {
  b <- df$鄉鎮市區 == a[i]
  df[b,5] = paste0("6300000", sprintf("%02d", i), "0000")
}

source <- data.frame("縣市"="台北市", "鄉鎮市區"=df$鄉鎮市區, "村里"=df$村里, "編號"=df$編號, "票所始"=df$`min(票所編號)`, "票所末"=df$`max(票所編號)`)


##2.將各開票所結果並依照里別儲存

#爬中選會網站各開票所結果
result <- list()
for (i in 1:456) {
  result[[i]] <- character()
  s <- sprintf("%04d", (source[i,5]:source[i,6]))
  for (j in s) result[[i]] <-
      c(result[[i]], paste0("http://referendum.2018.nat.gov.tw/pc/zh_TW/04/", source[i,4], j, ".html") %>%
      read_html() %>% xml_find_all("//tr[@class='trT']") %>% html_nodes("td") %>% html_text())
  result[[i]] <- as.numeric(gsub(",", "", result[[i]]))
}

#將投票結果依照里別儲存
output <- data.frame(source[,1:3], "同意票數"=4, "不同意票數"=5, "投票數"=6, "投票權人數"=7)
for (i in 1:456) {
  l <- c(1:length(source[i,5]:source[i,6]))
  output[i,4] <- sum(result[[i]][(8*l-7)]) #同意票數
  output[i,5] <- sum(result[[i]][(8*l-6)]) #不同意票數
  output[i,6] <- sum(result[[i]][(8*l-3)]) #投票數
  output[i,7] <- sum(result[[i]][(8*l-2)]) #投票權人數
}


##3.讀取里別所得與人口資料，將其併入原先檔案，並且將合併檔輸出

#讀取里別所得資料
income <- read.csv("里別所得資料.csv") %>% select(c(1,2,5,6,9)) 
colnames(income) <- c("鄉鎮市區", "村里", "所得平均數", "所得中位數", "所得標準差")

#讀取里別人口結構
population <- read.csv("里別人口結構.csv") %>% filter(性別 == "計") %>%
  mutate(
    "人口總數" = 總計,
    "0到9歲" = (X0到4歲+X5到9歲),
    "10到19歲" = (X10到14歲+X15到19歲),
    "20到29歲" = (X20到24歲+X25到29歲),
    "30到39歲" = (X30到34歲+X35到39歲),
    "40到49歲" = (X40到44歲+X45到49歲),
    "50到59歲" = (X50到54歲+X55到59歲),
    "60到69歲" = (X60到64歲+X65到69歲),
    "70到79歲" = (X70到74歲+X75到79歲),
    "80歲以上" = (X80到84歲+X85到89歲+X90到94歲+X95到99歲+X100歲以上)
  ) %>% select(1:2,26:35)

#合併檔案
merge <- left_join(output, y = income, by = c("鄉鎮市區", "村里")) %>%
  left_join(merge, y = population, by = c("鄉鎮市區", "村里"))

#檔案輸出
write.csv(merge, file = "台北市第十案公投資料.csv")
  
  
