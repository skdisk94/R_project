par(mfrow=c(2,2))
data <- read.csv("examples/기온/서울연평균.csv")
data <- data[-c(8),]
tmp <- subset(data, gender=1)

평균기온 <- tmp[[2]]
t.test(평균기온, mu=12.5)

n <- length(평균기온)
mu <- 12.5
t <- (mean(평균기온) - mu)/(sd(평균기온)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(1.5776, df=n-1)

x <- seq(-4, 4, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-4, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 4), c(0, y[x>ul], 0), col=2)
arrows(t, 0.02, t, 0, length=0.1)
text(t, 0.04, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.447))
text(ul, -0.02, expression(t[0.025]==2.447))

#-------
최고기온 <- tmp[[3]]
t.test(최고기온, mu=17)

n <- length(최고기온)
mu <- 17
t <- (mean(최고기온) - mu)/(sd(최고기온)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(1.8197, df=n-1)

x <- seq(-4, 4, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-4, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 4), c(0, y[x>ul], 0), col=2)
arrows(t, 0.02, t, 0, length=0.1)
text(t, 0.04, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.447))
text(ul, -0.02, expression(t[0.025]==2.447))

#-------
최저기온 <- tmp[[6]]
t.test(최저기온, mu=8.6)

n <- length(최저기온)
mu <- 8.6
t <- (mean(최저기온) - mu)/(sd(최저기온)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(2.0083, df=n-1)

x <- seq(-4, 4, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-4, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 4), c(0, y[x>ul], 0), col=2)
arrows(t, 0.02, t, 0, length=0.1)
text(t, 0.04, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.447))
text(ul, -0.02, expression(t[0.025]==2.447))

#------
data <- read.csv("examples/기온/서울강수량.csv")
data <- data[-c(1,2,3),]
tmp <- subset(data, gender=1)

강수량 <- tmp[[2]]
t.test(강수량, mu=1450.5)

n <- length(강수량)
mu <- 1450.5
t <- (mean(강수량) - mu)/(sd(강수량)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(-1.1654, df=n-1)

x <- seq(-4, 4, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-4, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 4), c(0, y[x>ul], 0), col=2)
arrows(t, 0.05, t, 0, length=0.1)
text(t, 0.07, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.365))
text(ul, -0.02, expression(t[0.025]==2.365))

#-----
data <- read.csv("examples/기온/서울평균습도.csv")
tmp <- subset(data, gender=1)

습도 <- tmp[[2]]
t.test(습도, mu=64.4)

n <- length(습도)
mu <- 64.4
t <- (mean(습도) - mu)/(sd(습도)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(-6.7285, df=n-1)

x <- seq(-8, 8, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-8, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 8), c(0, y[x>ul], 0), col=2)
arrows(t, 0.05, t, 0, length=0.1)
text(t, 0.07, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.447))
text(ul, -0.02, expression(t[0.025]==2.447))

#-----
data <- read.csv("examples/기온/일조시간.csv")
tmp <- subset(data, gender=1)

일조시간 <- tmp[[2]]
t.test(일조시간, mu=2066)

n <- length(일조시간)
mu <- 2066
t <- (mean(일조시간) - mu)/(sd(일조시간)/sqrt(n))
qt(0.025, df=n-1)
1 - pt(5.4362, df=n-1)

x <- seq(-6, 6, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.5), main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=n-1)
ll <- -ul
polygon(c(-6, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 6), c(0, y[x>ul], 0), col=2)
arrows(t, 0.05, t, 0, length=0.1)
text(t, 0.07, paste("t=", round(t, 3)))
text(ll, -0.02, expression(-t[0.025]==-2.447))
text(ul, -0.02, expression(t[0.025]==2.447))

#시계열- 1990~2017
#평균기온, 최고기온, 최저기온
data <- read.csv("examples/기온/서울시계열.csv")
data$일시 <- as.Date(data$일시)
data <- xts(data, order.by=data$일시)
data <- data[-c(96),c(2,4,6)]
dygraph(data)%>% dyRangeSelector%>% 
  dyAxis("y",valueRange=c(-20,40)) %>%
  dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey") 

#강수량
data <- read.csv("examples/기온/서울시계열.csv")
data$일시 <- as.Date(data$일시)
data <- xts(data, order.by=data$일시)
data <- data[,c(7)]
dygraph(data)%>% dyRangeSelector%>% 
  dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey",fillGraph=TRUE)

#습도
data <- read.csv("examples/기온/서울평균습도.csv")
data$일시 <- as.Date(data$일시)
data <- xts(data, order.by=data$일시)
data <- data[,c(2)]
dygraph(data)%>% dyRangeSelector%>% 
  dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey",fillGraph=TRUE)

#일조시간
data <- read.csv("examples/기온/일조시간.csv")
data$일시 <- as.Date(data$일시)
data <- xts(data, order.by=data$일시)
data <- data[,c(2)]
dygraph(data)%>% dyRangeSelector%>% 
  dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey",fillGraph=TRUE)

#
data <- read.csv("examples/기온/연별평년값.csv")
data <- data[,-c(5)]
dygraph(data)%>% dyRangeSelector %>% dyOptions(drawXAxis=F,drawPoints=T,pointSize =3,strokeWidth=3)

#도시별
data <- read.csv("examples/기온/도시별.csv")
data1 <- data[,c(1,4)]
data1 <- data1 %>% arrange(desc(평균기온)) %>% head(10)
ggplot(data1, aes(x=reorder(지점정보, desc(평균기온)), y=평균기온,fill=지점정보)) +
  geom_bar(stat="identity") + geom_text(mapping = aes(label = 평균기온), size = 5, vjust = 1, hjust=0.5) +
  coord_cartesian(ylim=c(14,17))

data2 <- data[,c(1,5)]
data2 <- data2 %>% arrange(최저기온) %>% head(10)
ggplot(data2, aes(x=reorder(지점정보, 최저기온), y=최저기온,fill=지점정보)) +
  geom_bar(stat="identity",alpha=0.7) + geom_text(mapping = aes(label = 최저기온), size = 5, vjust = 1, hjust=0.5)

data3 <- data[,c(1,6)]
data3 <- data3 %>% arrange(desc(최고기온)) %>% head(10)
ggplot(data3, aes(x=reorder(지점정보, desc(최고기온)), y=최고기온,fill=지점정보)) +
  geom_bar(stat="identity", alpha=0.7) + 
  geom_text(mapping = aes(label = 최고기온), size = 5, vjust = 1, hjust=0.5)+
  coord_cartesian(ylim=c(19,21.5))
  
