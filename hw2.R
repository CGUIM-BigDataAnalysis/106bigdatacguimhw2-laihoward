library(readr)
library(dplyr)

Data103Country<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
Data104Country<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
Data105Country<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
Data106Country<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
Data103school<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
Data104school<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
Data105school<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
Data106school<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")
Data103Country<-mutate(Data103Country,Total_103 = rowSums(Data103Country[,c(-1,-2)]))
Data104Country<-mutate(Data104Country,Total_104 = rowSums(Data104Country[,c(-1,-2)]))
Data105Country<-mutate(Data105Country,Total_105 = rowSums(Data105Country[,c(-1,-2)]))
Data106Country<-mutate(Data106Country,Total_106 = rowSums(Data106Country[,c(-1,-2)]))
CountryyearData<-inner_join(Data103Country[,c(1,2,12)],Data104Country[,c(1,2,12)],by=c("洲別","國別"))
CountryyearData<-inner_join(CountryyearData,Data105Country[,c(1,2,12)],by=c("洲別","國別"))
CountryyearData<-inner_join(CountryyearData,Data106Country[,c(1,2,12)],by=c("洲別","國別"))

Data103school$`非學位生-大陸研修生`<-as.numeric(gsub("…",NA,Data103school$`非學位生-大陸研修生`))
Data103school<-mutate(Data103school,Total_103 = rowSums(Data103school[,c(-1,-2,-3)], na.rm = TRUE))
Data104school$`非學位生-大陸研修生`<-as.numeric(gsub("…",NA,Data104school$`非學位生-大陸研修生`))
Data104school<-mutate(Data104school,Total_104 = rowSums(Data104school[,c(-1,-2,-3)], na.rm = TRUE))
Data105school<-mutate(Data105school,Total_105 = rowSums(Data105school[,c(-1,-2,-3)], na.rm = TRUE))
Data106school<-mutate(Data106school,Total_106 = rowSums(Data106school[,c(-1,-2,-3)], na.rm = TRUE))
schoolyearData<-inner_join(Data103school[,c(3,13)],Data104school[,c(3,13)],by = "學校名稱")
schoolyearData<-inner_join(schoolyearData,Data105school[,c(3,13)],by = "學校名稱")
schoolyearData<-inner_join(schoolyearData,Data106school[,c(3,13)],by = "學校名稱")
CountryyearData<- mutate(CountryyearData,total = rowSums(CountryyearData[,c(-1,-2)]))%>%
                 arrange(desc(total),desc(Total_106))

EZ <- read_csv("EZ.csv", 
               locale = locale(encoding = "BIG5"))
colnames(EZ)<-c("國別",	"GEC代碼","二位字母代碼","iso_a3","ISO 3166-1三位數字代碼","STANAG_1059Stanag標準化國碼","網際網路"	,"註說")
countryCode<-left_join(CountryyearData[,c(1,2,7)],EZ[,c(1,4)] ,by = "國別")

grep("[A-Z]{3}",countryCode$`iso_a3`,invert=T)
countryCode[grep("[A-Z]{3}",countryCode$iso_a3,invert=T),]
countryCode$iso_a3[grep("[A-Z]{3}",countryCode$iso_a3,invert=T)]<-
  c("CHN","KOR","ANZ","VCT","KNA","MHL","COD","SRB","UAR")

schoolyearData<-mutate(schoolyearData,Total = rowSums(schoolyearData[,-1], na.rm = TRUE))%>%
                arrange(desc(Total))
knitr::kable(subset(schoolyearData[,c(1,6)],`學校名稱`=="國立台灣師大僑生先修部"))
knitr::kable(head(schoolyearData[,c(1,6)],10))



library(ggmap)
library(readODS)
library(rio)
library(RColorBrewer)
library(choroplethr)
library(ggthemes)

data(country.map)
final.plot<-merge(country.map,
                  countryCode,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
