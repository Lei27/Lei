lei <- function(url, xpath){
	result <- data.frame('标签'=NA,'链接地址'=NA)
	x <- getURLContent(url,encoding="UTF-8")
#	xx <- gsub("&nbsp;"," ",x)
	url_parse <- htmlParse(x, encoding="UTF-8")
	node <- getNodeSet(url_parse, xpath)
	n <- length(node)
	print(n)
	if(n==0){
		print(paste("我操，没有找到节点！"))
	}else{
		for (i in 1:n){
			result[i,1] <- xmlValue(node[[i]])
			result[i,2] <- xmlGetAttr(node[[i]],name="href")
		}
		result
		write.table(result,file="CSDN_TAGS.r")
	}
}
url <- "http://www.csdn.net/tag/"
xpath <- "/html/body/div/div/ul/li[1]/div/div/a"
lei(url,xpath)


#依次获取字母分类、字母分类具体标签、文章标题、文章来源、文章标签、文章发布者
xpath <- "//div[@class="line_list"]/a"  #一个类名为line_list的div标签中的text内容就是一条新闻

xpath <- "//div[@class="not_search"]"  #是否没有内容，判断是否存在 '很抱歉，该页暂时没有"xxx字母分类具体标签名"的相关内容'

xpath <- "//a[@class="tag_news"]"  #从二级页爬到资讯页链接地址

xpath <- "//div[@class='not_search']"  #找不到更多的新闻了的页面，搜索内容判断

http://www.csdn.net/tag/%E5%A4%A7%E6%95%B0%E6%8D%AE/news-75

> /html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div
> /html/body/div[2]/div/div[3]/div[2]/ul/li[2]/div
> /html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[2]
> /html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[2]/a[1]

/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[1]
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[2]/a[1]
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[3]/a

/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[1]/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[2]/a[1]
/html/body/div[2]/div/div[3]/div[2]/ul/li[1]/div/div/span[3]/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[2]/div/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[2]/div/div/span[1]/a
/html/body/div[2]/div/div[3]/div[2]/ul/li[2]/div/div/span[2]/a[1]
/html/body/div[2]/div/div[3]/div[2]/ul/li[2]/div/div/span[3]/a

//*[@id="A"]/div/div/a[1]
//*[@id="A"]/div/div
/html/body/div[2]/div/ul



//*[@id="A"]/div/div/a[1]

CSDN服务器维护中页面xpath
/html/body/div[2]/dl/dd[1]/h1

百度蜘蛛：
Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.93 Safari/537.36
Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)

rm(list=ls())
gc()
library(bitops)
library(RCurl)
library(curl)
library(XML)
myHeader <- c("User-Agent"="Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)","Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8","Accept-Language"="zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3","Accept-Encoding"="gzip, deflate","Connection"="keep-alive")
alphabet <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","#")
tags <- data.frame('字母分类具体标签'=NA)
temp <- data.frame('循环多个标签临时'=NA)
result <- data.frame('文章标题'=NA,'文章来源'=NA,'文章标签'=NA,'文章发布者'=NA,'字母分类'=NA,'字母分类具体标签'=NA)
x <- getURLContent("http://www.csdn.net/tag/",encoding="UTF-8")
url_tag <- htmlParse(x, encoding="UTF-8")
num <- 0  #断点续爬节点1，根据断点处R GUI的屏幕输出内容，输入num值
for (i in 1:27){  #断点续爬节点0，输入i值
	xpath_tag <- paste("/html/body/div/div/ul/li[",i,"]/div/div/a",sep="")
	node_tag <- getNodeSet(url_tag, xpath_tag)
	m <- length(node_tag)
	print(paste("#####  '",alphabet[i],"'  字母分类有  '",m,"'  个具体标签！",sep="   "))
	if(m==0){
		print(paste("!!!!!，没有找到  '",alphabet[i],"'  字母分类的具体标签相关节点！~~~",sep=""))
	}else{
		for (j in 1:m){  #断点续爬节点2，输入j值(如果该标签j的资讯条目n的值小于10条，证明改标签已经爬取完毕，则此处输入j+1值)
			if (i==4 && j==18) next  #CSDN已把Docker标签建立单独域名docker.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==8 && j==2) next #CSDN已把Hadoop标签建立单独域名hadoop.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==8 && j==13) next #同名标签，CSDN已把Hadoop标签建立单独域名hadoop.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==15 && j==3) next #CSDN已把OpenStack标签建立单独域名openstack.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==15 && j==38) next #CSDN已把OpenStack Swift标签划入单独域名openstack.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==19 && j==78) next #CSDN已把spark标签建立单独域名spark.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			if (i==19 && j==96) next #CSDN已把Swift标签建立单独域名swift.csdn.net，此行代码跳过这个坑，暂不抓取此标签
			tags[j,] <- xmlValue(node_tag[[j]])
			k <- 0  #断点续爬节点3，输入k值(如果该页已经爬完，输入k+1值)
			for(k in 1:10000){  #断点续爬节点4，值需与同节点3相同
				url <- paste("http://www.csdn.net/tag/",tags[j,1],"/news-",k,sep="")
				if(url.exists(url) || url.exists(url) || url.exists(url)){  #此处验证url是否存在(验证3遍进行或运算，一定程度上确保非CSDN服务器网络/性能原因导致无响应，返回为空，页面被误判为不存在，在此脚本实际执行中发现确实存在这样的情况)，为了跳过CSDN提示页面；
					y <- getURLContent(url,encoding="UTF-8")
					url_news <- htmlParse(y, encoding="UTF-8")
					node_not_exists <- getNodeSet(url_news,"//div[@class='not_search']")
					if(length(node_not_exists)!=0){
						break
					}else{
						node_news <- getNodeSet(url_news, "/html/body/div/div/div[3]//div[@class='tag_news']//div[@class='line_list']")
						n <- length(node_news)
						if(n==0){
							y <- getURLContent(url,encoding="UTF-8")
							url_news <- htmlParse(y, encoding="UTF-8")
							print(paste("!!!!!,  '",alphabet[i],"'  字母分类具体标签  '",tags[j,1],"'  的资讯不存在！~~~",sep=""))
						}else{
							print(paste("  '",alphabet[i],"'  字母分类的具体标签  '",tags[j,1],"'  第  '",k,"'  页有  '",n,"'  条资讯！",sep=""))
							for(p in 1:n){
								num <- num+1
								node_title <- getNodeSet(url_news,paste('/html/body/div/div/div[3]/div[2]/ul/li[',p,']/div/a',sep=""))
								node_source <- getNodeSet(url_news,paste('/html/body/div/div/div[3]/div[2]/ul/li[',p,']/div/div/span[1]/a',sep=""))
								node_tags <- getNodeSet(url_news,paste('/html/body/div/div/div[3]/div[2]/ul/li[',p,']/div/div/span[2]/a',sep=""))
								node_author <- getNodeSet(url_news,paste('/html/body/div/div/div[3]/div[2]/ul/li[',p,']/div/div/span[3]/a',sep=""))
								result[num,1] <- xmlValue(node_title[[1]])
								result[num,2] <- xmlValue(node_source[[1]])
								for(q in 1:length(node_tags)){
									temp[q,1] <- xmlValue(node_tags[[q]])
								}
								result[num,3] <- paste(temp[1],sep=",")
								result[num,4] <- xmlValue(node_author[[1]])
								result[num,5] <- alphabet[i]
								result[num,6] <- tags[j,1]
							}
							if(num <= 10){
								write.table(result[1:num,1:6],file=paste(alphabet[i],".r",sep=""),append=TRUE,col.names=TRUE)
							}else{
								write.table(result[(num-n+1):num,1:6],file=paste(alphabet[i],".r",sep=""),append=TRUE,col.names=FALSE)
							}
#							Sys.sleep(2) #The time interval to suspend execution for, in seconds.
							rm(result)
#							Sys.sleep(2) #The time interval to suspend execution for, in seconds.
							gc()
							Sys.sleep(1) #The time interval to suspend execution for, in seconds.
							result <- data.frame('文章标题'=NA,'文章来源'=NA,'文章标签'=NA,'文章发布者'=NA,'字母分类'=NA,'字母分类具体标签'=NA)
							print(paste("  '",alphabet[i],"'  字母分类的具体标签  '",tags[j,1],"'  第  '",k,"'  页的  '",n,"'  条资讯已抓取完毕！",sep=""))
							print(paste("字母分类i数字:  '",i,"'/27  ,字母分类的具体标签j数字:  '",j,"'/",m,"  ,资讯页码k数字:  '",k,"'/NA  ,资讯条目n数字:  '",n,"'/",n,"  ,已抓取条数num数字:  '",num,"'  ",sep=""))
							print(paste("--------------------------------------------------------------------------------------------------------------------------"))
						}
					}
				}else{
					break
				}
			}
		}
	}
}



分类标签数量
A：110，B：114，C：181，D：129，E：31，F：65，G：113，H：92，I：76，J:127，K：77，L：97，M：104，N：50，O：40，P：64，Q：64，R：74，S：281，T：88，U：19，V：22，W：178，X：88，Y：236，Z：121，#：47







