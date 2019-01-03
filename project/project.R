library(ggplot2)
library(dplyr)

composition_ratio <- read.csv("examples/aging/구성비.csv")
composition_ratio <- composition_ratio %>% filter(year == 2000|year == 2005|year == 2010|year == 2015)
ggplot(composition_ratio, aes(x =year,y=pec, fill=나이)) + geom_bar(stat="identity") + 
  geom_text(aes(label=pec), vjust=1.6, color="white", size=10,position = "stack") + 
  scale_fill_brewer(palette = "Set2")+
  theme(panel.grid=element_blank(), axis.ticks=element_blank())


composition_ratio2017 <- read.csv("examples/aging/2017년구성비.csv")

ggplot(data= composition_ratio2017, aes(x="1",y=pec,fill=나이)) +
  coord_polar(theta="y") + geom_bar(width = 1, size = 1, color = "white",stat="identity") +
  scale_fill_brewer(palette="Pastel2") + 
  geom_text(aes(x=1.0,label = paste(pec, "%")), size = 6, position = position_stack(vjust = 0.5),check_overlap = T) + 
  geom_text(aes(x=1.2,label = 나이), size = 5, position = position_stack(vjust = 0.5),check_overlap = T) +
  theme_bw() + theme_void() + theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())


library(ggiraphExtra)
library(kormaps2014)

aging_rate <- read.csv("examples/aging/고령인구비율시도별.csv")
colnames(aging_rate) <- c("name", "a", "b", "고령인구비율_2017년")
aging_rate <- aging_rate[-c(18),]
# str(aging_rate)
# str(changeCode(kormap1))
# View(changeCode(korpop1))
# View(changeCode(kormap1))

korpop1<-rename(korpop1,
                pop=총인구_명,
                name=행정구역별_읍면동)

kor_aging <-inner_join(changeCode(korpop1),aging_rate, by='name')
kor_aging <- kor_aging[,c(2,25,28)]
ggChoropleth(data = kor_aging,
             aes(fill = 고령인구비율_2017년, map_id = code, tooltip = name),
             map = kormap1,
             interactive = T)
#전라남도 21.54%로 가장 높음


aging_index <- read.csv("examples/aging/노령화지수.csv")
ggplot(data = aging_index,aes(x=year,y=노령화지수)) + geom_line(color="green")  + geom_point(color="darkgreen") +
  theme_light() + ggtitle("노령화 지수(1970~2017년)")+
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15, color="darkblue"))


data <- read.csv("examples/aging/고령화.csv")
str(data)

#고령화 비율과 기대수명 그래프
par(mar = c(5, 4, 4, 6) + 0.1)
plot(aging_rate ~ year, data=data, type = "l", axes = FALSE, xlab = "year", ylab = "고령화 비율", col="blue")
lines(data$year, data$life_expectancy, col = "red")
axis(side = 1, at = 1970:2017, labels = data$year, line =1)
axis(side = 2,col="blue" )
par(new = TRUE)
plot(life_expectancy ~ year, data=data, type = "p", axes = FALSE, xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red")
mtext("기대수명", side = 4)

#표본공분산 / 표본상관계수
mean.l <- mean(data$life_expectancy)
mean.p <- mean(data$aging_rate)
cov(data$aging_rate, data$life_expectancy) # 표본공분산
cor(data$aging_rate, data$life_expectancy) # 표본상관계수

sxy <- sum((data$life_expectancy - mean.l) * (data$aging_rate - mean.p))
sxx <- sum((data$life_expectancy - mean.l)^2)

(b1 <- sxy / sxx)
(b0 <- mean.p - b1 * mean.l)

lm(aging_rate ~ life_expectancy, data = data)

out1 <- lm(aging_rate ~ life_expectancy, data = data)
anova(out1)
summary(out1)

par(mfrow=c(2,2))
plot(out1)
par(mfrow=c(1,1))

plot(aging_rate~life_expectancy, pch=20, data=data, xlab="기대수명", ylab="고령화 비율", col="blue")
abline(lm(aging_rate~life_expectancy, data=data), col=2, lwd=2)

ci <- predict(out1, interval="confidence")
lines(data$life_expectancy, ci[,2], lty=3, lwd=1.5, col="green")
lines(data$life_expectancy, ci[,3], lty=3, lwd=1.5, col="cyan1")
# 95% 신뢰구간

library(stringr)
# abline(v=165, col=2, lty=2)
# abline(h=179.6, col=2, lty=2)
# text(165, 176, paste("x=", 165))
# arrows(165, 175.5, 165, 175, length=0.1)
# text(155, 179.6, paste("y=", 179.6))
# arrows(151, 179.6, 149, 179.6, length=0.1)
text.eq <- str_c("Ŷ = ", round(b0,digits = 3), " + ", round(b1,digits = 3), " x")
text(75, 12.5, text.eq, cex=3)

#회귀식 -> 이차식 그래프 95%신뢰구간
lm(aging_rate ~ life_expectancy + I(life_expectancy^2), data=data)
out2 <- lm(aging_rate ~ life_expectancy + I(life_expectancy^2), data=data)
plot(aging_rate~life_expectancy, data=data)
lines(data$life_expectancy, fitted(out2), col="green", lwd=2)

ci <- predict(out2, interval="confidence")
lines(data$life_expectancy, ci[,2], lty=3, lwd=1.5, col="red")
lines(data$life_expectancy, ci[,3], lty=3, lwd=1.5, col="red")
text.eq <- str_c("Ŷ = ", 125, " + ", -3.778, " x", " + ", 0.029, " x^2")
text(70, 10, text.eq, cex=1.2)

summary(out2)
par(mfrow=c(2,2))
plot(out2)  
par(mfrow=c(1,1))



fertility_rate <- read.csv("examples/aging/출산율.csv")
data <- inner_join(data, fertility_rate, by='year')
lm(aging_rate ~ life_expectancy+fertility_rate, data=data)
out3 <- lm(aging_rate ~ life_expectancy+fertility_rate, data=data)
summary(out3)
par(mfrow=c(2,2))
plot(out3)
par(mfrow=c(1,1))

birth_rate <- read.csv("examples/aging/출생률.csv")
data <- inner_join(data, birth_rate, by='year')
lm(aging_rate ~ life_expectancy+birth_rate, data=data)
out4 <- lm(aging_rate ~ life_expectancy+birth_rate, data=data)
summary(out4)
par(mfrow=c(2,2))
plot(out4)
par(mfrow=c(1,1))


AIC(out1, out2, out3,out4)



#--------------------------------------------------------------------------------------------
install.packages("dygraphs")
library(dygraphs)
library(xts)
library(RColorBrewer)

#폐암 사망률 추세 - 성별 1930~2015
death_trend <- read.csv("examples/lungcancer/DeathTrend.csv")
death_trend$Year <- as.Date(death_trend$Year)
death_trend <- xts(death_trend, order.by=death_trend$Year)
death_trend <- death_trend[,-c(1)]

# data <- death_trend %>% ungroup %>% arrange(Year) %>% ts(start = c(1930,1))
# data <- data[,-c(1)]
dygraph(death_trend)%>% dyRangeSelector%>%
  dyShading(from="1987-1-1", to="1993-1-1", color="#FFE6E6") %>%
  dyShading(from="1997-1-1", to="2003-1-1", color="#CCEBD6")


#2018년 암 종류별 새로운 환자 예상
new_case_estimates <- read.csv("examples/lungcancer/예상사례.csv")
new_case_estimates <- new_case_estimates[-c(1),]
top <- new_case_estimates %>% arrange(-Both_sexes_combined) %>% head(20)
top <- top[c(3,5,6,8,9,13),]

ggplot(data=top,aes(x=reorder(암종류,Both_sexes_combined),y=Both_sexes_combined)) + 
  geom_col(mapping = aes(fill = 암종류),alpha = 0.7)+coord_flip() +
  geom_text(mapping = aes(label = Both_sexes_combined), size = 4, vjust = 0, hjust=2) + 
  ggtitle("예상 사례") + xlab("암 종류")+ylab("새로운 환자")

ggplot(data=top,aes(x=reorder(암종류,Both_sexes_combined),y=Both_sexes_combined)) + 
  geom_bar(stat = "identity", mapping = aes(fill = 암종류), alpha = .6)+coord_flip() +
  geom_text(mapping = aes(label = Both_sexes_combined), size = 4, vjust = 0, hjust=2) + 
  scale_y_continuous(breaks=seq(70000, 270000, 50000)) +  
  ggtitle("예상 사례") + xlab("암 종류")+ylab("새로운 환자")
  
#2018년 암 종류별 예상 사망자
death_estimates <- read.csv("examples/lungcancer/예상사망.csv")
death_estimates <- death_estimates[-c(1),]
top <- death_estimates %>% arrange(-Both_sexes_combined) %>% head(20)
top <- top[c(3,5,7,8,10,11),]

ggplot(data=top,aes(x=reorder(암종류,Both_sexes_combined),y=Both_sexes_combined)) + 
  geom_col(mapping = aes(fill = 암종류),alpha = 0.7)+coord_flip() +
  geom_text(mapping = aes(label = Both_sexes_combined), size = 4, vjust = 0, hjust=1.5) + 
  ggtitle("예상 사망") + xlab("암 종류")+ylab("사망자")

#2018년 암 유형별새로운 사망
ggplot(data=top,aes(x=reorder(암종류,Both_sexes_combined),y=Both_sexes_combined)) + 
  geom_bar(stat = "identity", mapping = aes(fill = 암종류), alpha = .6)+coord_flip() +
  geom_text(mapping = aes(label = Both_sexes_combined), size = 4, vjust = 0, hjust=1.5) + 
  scale_y_continuous(breaks=seq(20000, 160000, 70000)) +  
  ggtitle("예상 사망") + xlab("암 종류")+ylab("사망자 환자")

#2000년 주별 성인 흡연량
state_smoking <- read.csv("examples/lungcancer/미국주별흡연율.csv")
state_smoking <- state_smoking %>% filter(year==1998) 
states_map <- map_data("state")
state_smoking <-inner_join(states_map,state_smoking, by='region')
ggChoropleth(data=state_smoking, aes(fill=total_mean, map_id=region), palette = "PuBu",
             map = states_map, interactive = T)

#2008년 주별 폐암 사망률
Lung_cancer_death <- read.csv("examples/lungcancer/미국주별폐암사망률.csv")
colnames(Lung_cancer_death) <- c("region", "사망2008", "사망2009", "사망2010", "사망2011", "사망2012")
Lung_cancer_death <- Lung_cancer_death[-c(52),]
states_map <- map_data("state")
usa_lung <-inner_join(states_map,Lung_cancer_death, by='region')
ggChoropleth(data=usa_lung, aes(fill=사망2008, map_id=region), palette = "PuBu",
             map = states_map, interactive = T)


# smoking <- read.csv("examples/lungcancer/주별흡연량.csv")
# smoking <- smoking[-c(1),]
# states_map <- map_data("state")
# smoking <-inner_join(states_map,smoking, by='region')
# ggChoropleth(data=smoking, aes(fill=smoking, map_id=region),map = states_map, interactive = T)





# #미국 폐암 사망자
# lung_cancer_mortality_rate <- read.csv("examples/lungcancer/미국폐암사망률.csv")
# colnames(lung_cancer_mortality_rate) <- c("year", "number_of_deaths", "death_rate")
# lung_cancer_mortality_rate <- lung_cancer_mortality_rate[-c(1),]
# str(lung_cancer_mortality_rate)
# 
# ggplot(data = lung_cancer_mortality_rate, aes(x=year, y=paste(round(number_of_deaths/10000,digits=3)))) + 
#   geom_bar(stat = "identity") + theme_light() +ggtitle("미국 폐암 사망자 수") + 
#   theme(plot.title = element_text(face="bold", hjust=0.5, size=15, color="darkblue"))
# 
# ggplot(data = lung_cancer_mortality_rate, aes(x=year, y=number_of_deaths)) + 
#   geom_bar(stat = "identity") + theme_light() +ggtitle("미국 폐암 사망자 수") + 
#   theme(plot.title = element_text(face="bold", hjust=0.5, size=15, color="darkblue"))



smoking <- read.csv("examples/lungcancer/미국흡연율.csv")
smoking <- smoking %>% filter(sex=="Both")
trend <- read.csv("examples/lungcancer/발생률 추세.csv")
data <- inner_join(smoking,trend, by='year' )

par(mar = c(5, 4, 4, 6) + 0.1)
plot(pec ~ year, data=data, type = "l", axes = FALSE, xlab = "year", ylab = "흡연율", col="blue")
lines(data$year, data$pec2, col = "red")
axis(side = 1, at = 1996:2012, labels = data$year, line =1)
axis(side = 2,col="blue" )
par(new = TRUE)
plot(pec2 ~ year, data=data, type = "p", axes = FALSE, xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red")
mtext("폐암 발생률", side = 4)

#표본공분산 / 표본상관계수
mean.x <- mean(data$pec)
mean.y <- mean(data$pec2)
cov(data$pec, data$pec2) # 표본공분산
cor(data$pec, data$pec2) # 표본상관계수

sxy <- sum((data$pec - mean.x) * (data$pec2 - mean.y))
sxx <- sum((data$pec - mean.x)^2)

(b1 <- sxy / sxx)
(b0 <- mean.y - b1 * mean.x)

lm(pec2 ~ pec, data = data)

out1 <- lm(pec2 ~ pec, data = data)
anova(out1)
summary(out1)

par(mfrow=c(2,2))
plot(out1)
par(mfrow=c(1,1))

plot(pec2~pec, pch=20, data=data, xlab="흡연율", ylab="폐암 발생률", col="blue")
abline(lm(pec2~pec, data=data), col=2, lwd=2)

ci <- predict(out1, interval="confidence")
lines(data$pec, ci[,2], lty=3, lwd=1.5, col="green")
lines(data$pec, ci[,3], lty=3, lwd=1.5, col="cyan1")
# 95% 신뢰구간

library(stringr)
# abline(v=165, col=2, lty=2)
# abline(h=179.6, col=2, lty=2)
# text(165, 176, paste("x=", 165))
# arrows(165, 175.5, 165, 175, length=0.1)
# text(155, 179.6, paste("y=", 179.6))
# arrows(151, 179.6, 149, 179.6, length=0.1)
text.eq <- str_c("Ŷ = ", round(b0,digits = 3), " + ", round(b1,digits = 3), " x")
text(23.5, 60, text.eq, cex=1.2)


#회귀식 -> 이차식 그래프 95%신뢰구간
lm(pec2~pec + I(pec^2), data=data)
out2 <- lm(pec2~pec + I(pec^2), data=data)
plot(pec2~pec, data=data)
lines(data$pec, fitted(out2), col="green", lwd=2)

ci <- predict(out2, interval="confidence")
lines(data$pec, ci[,2], lty=3, lwd=1.5, col="red")
lines(data$pec, ci[,3], lty=3, lwd=1.5, col="red")
text.eq <- str_c("Ŷ = ", 125, " + ", -3.778, " x", " + ", 0.029, " x^2")
text(23, 60, text.eq, cex=1.2)

summary(out2)
par(mfrow=c(2,2))
plot(out2)  
par(mfrow=c(1,1))

AIC(out, out2)


#회귀식 -> 3차식 그래프 95%신뢰구간
lm(pec2~pec + I(pec^2)+I(pec^3), data=data)
out3 <- lm(pec2~pec + I(pec^2)+I(pec^3), data=data)
plot(pec2~pec, data=data)
lines(data$pec, fitted(out3), col="green", lwd=2)

ci <- predict(out3, interval="confidence")
lines(data$pec, ci[,2], lty=3, lwd=1.5, col="red")
lines(data$pec, ci[,3], lty=3, lwd=1.5, col="red")
text.eq <- str_c("Ŷ = ", 125, " + ", -3.778, " x", " + ", 0.029, " x^2")
text(23, 60, text.eq, cex=1.2)

AIC(out1, out2,out3)

smoking <- read.csv("examples/lungcancer/미국흡연율.csv")
smoking <- smoking %>% filter(sex=="Both")
ggplot(data = smoking,aes(x=year,y=pec)) + geom_line(color="green")  + geom_point(color="darkgreen") +
  theme_light() 
