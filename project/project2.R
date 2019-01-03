library(ggplot2)
library(dplyr)
library(dygraphs)
library(xts)
library(ggiraphExtra)


#전과자별재범퍼센트- 1990~2017
data <- read.csv("examples/previous_conviction/전과자별시계열.csv")
data$year <- as.Date(data$year)
data <- xts(data, order.by=data$year)
data <- data[,-c(1)]

dygraph(data)%>% dyRangeSelector %>% dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey")

#범죄 검거율- 1990~2017
data <- read.csv("examples/previous_conviction/전국범죄검거율.csv")
# data <- data %>% ungroup %>% arrange(year) %>% ts(start = c(1990,1))
# data <- data[,-c(1)]
# dygraph(data)%>% dyRangeSelector %>% dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey", stemPlot=T)
# data <- data %>% filter(!year %in% c(1991,1992,1994,1995,1997,1998))
data <- data %>% filter(year %in% c(1990,1995,2000,2005,2010,2015))
data1 <- data %>% filter(종류 == "발생건수")
data2 <- data %>% filter(종류 == "검거")
ggplot(data=data, aes(x=year,y=건, fill=종류)) + geom_bar(stat="identity", position = 'dodge') 
#전체-검거 => 위로 올리면 전체에 대한 비율?
ggplot(data=data, aes(x=year,y=건, fill=종류)) + geom_bar(stat="identity", position = 'dodge') +
  geom_col()
#  theme(legend.position="none")

data <- read.csv("examples/previous_conviction/전국범죄검거율2.csv")
plot(발생건수 ~ year, data=data, type = "h", axes = FALSE, xlab = "year",ylim=c(1000000,2100000), 
       ylab = "발생건수", col="cyan",lwd=10)
lines(data$year, data$검거, col = "red",lwd=2)
axis(side = 1, at = 1990:2017, labels = data$year, line =1)
axis(side = 2,col="cyan" )
text(data$year, data$발생건수, paste(round(data$검거율,digits=1),"%"),cex=1.1)

#2017년 범죄 종류별 검거
data <- read.csv("examples/previous_conviction/2017년범죄종류별검거.csv")
ggplot(data=data,aes(x=reorder(종류,발생건수),y=발생건수)) + 
  geom_col(mapping = aes(fill = 종류),alpha = 0.7)+coord_flip() +
  geom_text(mapping = aes(label = 발생건수), size = 4, vjust = 0, hjust=1) + 
  scale_y_continuous(breaks=seq(0, 18000, 510000)) +  
  ggtitle("2017년 범죄 종류별 발생 건수") + xlab("범죄 종류")+ylab("발생건수")+ 
  theme(axis.text.y=element_text(face = "bold",size=20))

ggplot(data=data,aes(x=reorder(종류,검거),y=검거)) + 
  geom_col(mapping = aes(fill = 종류),alpha = 0.7)+coord_flip() +
  geom_text(mapping = aes(label = 검거), size = 4, vjust = 0, hjust=1) + 
  scale_y_continuous(breaks=seq(0, 11000, 470000)) +  
  ggtitle("2017년 범죄 종류별 검거수") + xlab("범죄 종류")+ylab("검거수") + 
  theme(axis.text.y=element_text(face = "bold",size=20))

#행정구역별 범죄
crime <- read.csv("examples/previous_conviction/2017행정구역별범죄.csv")

korpop1<-rename(korpop1,
                pop=총인구_명,
                name=행정구역별_읍면동)

kor_crime <-inner_join(changeCode(korpop1),crime, by='name')
kor_crime <- kor_crime[,c(2,25,26)]
ggChoropleth(data = kor_crime,
             aes(fill = 범죄, map_id = code, tooltip = name),
             map = kormap1,
             interactive = T)

#전과자통계
data <- read.csv("examples/previous_conviction/전과수별_통계.csv")
data <- data %>% filter(전과 != "total")
ggplot(data=data, aes(x=year, y=범죄수, fill=전과))+geom_bar(stat="identity", position = 'dodge')
ggplot(data=data, aes(x=year, y=범죄수, fill=전과))+geom_bar(stat="identity", position = 'dodge') +
  geom_col()

ggplot(data=data, aes(x=year, y=pec, group(전과), color=factor(전과))) + geom_line()

#범재 전과 비율
data <- read.csv("examples/previous_conviction/범죄_전과.csv")
ggplot(data=data, aes(x=year, y=전과자범죄비율)) + geom_bar(stat="identity") + geom_line() +
  geom_point()
ggplot(data=data, aes(x=year, y=전과자범죄비율)) +  geom_line(color="red",size=2) + geom_smooth(method = loess)

mu <- mean(data$전과자범죄비율)
sigma <- sd(data$전과자범죄비율)
ll <- mu -3*sigma
ul <- mu +3*sigma 
# x<-seq(ll, ul, by=0.01)
# nd <- dnorm(x, mean=mu, sd=sigma)
#hx <- dnorm(x,mu,sigma)
a <- qnorm(0.025, mean = mu, sd = sigma)
b <- qnorm(0.975, mean = mu, sd = sigma)

x <- seq(ll, ul, length=1000)
y <- dnorm(x= x, mean=mu, sd=sigma)

#신뢰구간 95% - 38~64%로 전과자들이 다시 범죄를 저지른다
#편차가 많이 남
plot(x, y, type="l", axes=F,ylim =c(-0.03, 0.12) , main="", xlab="", ylab="")
abline(h=0)
polygon(c(a, x[x>a & x< b], b), c(0, y[x>a & x< b], 0), col="ivory2")
text(a, -0.03, paste("ll=", round(a,digits = 1)))
arrows(a, -0.025, a, 0, length=0.1)
text(mu, -0.03, paste("mu=", round(mu,digits = 1)))
arrows(mu, -0.025, mu, 0, length=0.1)
text(b, -0.03, paste("ul=", round(b,digits = 1)))
arrows(b, -0.025, b, 0, length=0.1)


##
# x<- c(~~~~)
#chisq.test(x, p=c(1,1,1,1)/4)
# => x-squared df p-value
#  0.025 < x-squared < 0.975 | p-value < 0.05 영가설 기각
# 영가설기각 => 비율이 같지않다 (목표는 0.05>)


#
data <- read.csv("examples/previous_conviction/전과자별재범퍼센트.csv")
data <- data %>% filter((year %in% c(2010:2016))& 전과!="소계") 
data_mean <- data %>% group_by(전과) %>% summarise(mean=mean(pec))
sum(data_mean$mean)
sum(c(0.232,0.161,0.118,0.088,0.069,0.054,0.042,0.034,0.202))
x <- c(178307, 124017, 90642, 68040, 53403, 42364, 32452, 26421, 171807)
chisq.test(x, p=data_mean$mean)
1-pchisq(1297.4, df=8)
#p <- round(p, digits = 8)
sum(p)
#2016년 확률 
p <- c(0.226864093, 0.15703756, 0.115056904, 0.086588107, 0.069060479, 0.054014224,  0.042143255,
        0.033730406, 0.215504971 )
#2017년 확률 
p <- c(0.226435102, 0.157491304, 0.115107822, 0.086405157, 0.067817381, 0.053798766, 0.041211348, 
        0.033552479, 0.218180641)
chisq.test(x, p=p)
qchisq(0.95,df=8)

#2~8, 1+9이상
x <- c(124017, 90642, 68040, 53403, 42364, 32452, 26421,305114)
p <- c(0.15703756, 0.115056904, 0.086588107, 0.069060479, 0.054014224,  0.042143255,
       0.033730406, 0.442369064 )
chisq.test(x, p=p)



#그림 
px <- seq(0, 65, by=0.01)
plot(px, dchisq(px, df=8),type='l',col="blue")
xa <- qchisq(0.95,df=8)
lines(c(xa,xa), c(1,0),lty=2)
xc <- qchisq(1-(9.919e-11), df=8)
lines(c(xc,xc), c(1,0),col="red")
abline(h=-0.001)
dc <- dchisq(px, df=8)
tol <- qchisq(0.95, df=8)
tol.g <- round(tol, 2)
polygon(c(tol.g, px[px>tol.g], 65), c(0, dc[px>tol.g], 0), col="red")
text(0, 0.005, "0", cex=0.8)
text(tol, 0.03, expression(chi[0.05]^{2}==15.51), cex=0.8)
text(xc, 0.01, round(xc,digits = 4), cex=0.8)

# 그림
px <- seq(0, 30, by=0.01)
dc <- dchisq(px, df=8)

alpha <- 0.05
tol <- qchisq(0.95, df=8)

par(mar=c(0,1,1,1))
plot(px, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, px[px>tol.g], 30), c(0, dc[px>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==15.51), cex=0.8)

#
data <- read.csv("examples/previous_conviction/두려움.csv")
data$시점 <- as.Date(data$시점)
data <- xts(data, order.by=data$시점)
data <- data[,-c(1)]
dygraph(data)%>% dyRangeSelector %>% dyOptions(drawPoints=T,pointSize = 2,strokeBorderWidth=1,strokeBorderColor="ligthgrey")
