data<-read.table("C:/Users/suheda/OneDrive/Masaüstü/data.txt",header = FALSE)
names(data)
names(data)<-c("y","x1","x2","x3","x4")
attach(data)
x4<-as.factor(x4)
head(data)

#tanýmlayýcý istatisitkleri
summary(data)
#qq plot grafik çizimi
qqnorm(y)
qqline(y)
shapiro.test(y)
ks.test(y,"pnorm")#normallik testi
#normal çýkmadýðýndan dönüþüm
library(tidyr)
lny<-log(y)
yenidata<-cbind(lny,x1,x2,x3,x4)
new_df<- na.omit(yenidata,c("lny"))
new_df<-as.data.frame(new_df)
names(new_df)
names(new_df)<-c("lny","x1","x2","x3","x4")
attach(new_df)
qqnorm(new_df$lny)
qqline(new_df$lny)
ks.test(lny,"pnorm")
boxplot(new_df$lny,col="blue")
boxplot.stats(new_df$lny)$out

write.table(new_df,file = 'cikti.txt',sep="\t")

newdata<-read.table("C:/Users/suheda/OneDrive/Masaüstü/newdata.txt",header = FALSE)

names(newdata)
names(newdata)<-c("dy","talep","maliyet","para","ulke")
ulke<-as.factor(ulke)
str(ulke)
attach(newdata)
newdata<-as.data.frame(newdata)
head(newdata)
qqnorm(dy)
qqline(dy)
ks.test(dy , "pnorm")
boxplot(newdata$dy,horizontal = FALSE,col = "blue")
summary(newdata)

#doðrusallýk
pairs(newdata)

#model kurma
install.packages("MASS")
library(MASS)
sonuc<-lm(dy~talep+maliyet+para+ulke)
summary(sonuc)
#Aykýrý deðer incelenmesi
inf<-ls.diag(sonuc)

influence.measures(sonuc)

par(mfrow=c(2,2))
plot(predict(sonuc),abs(inf$stud.res),ylab = "Student-türü artýklar",xlab = "Tahmini deðerler")

#Cook Uzaklýðý
n<-107
k<-4
cooksd<-cooks.distance(sonuc)
plot(cooksd, pch="*",cex=2,main="Cook uzaklýðý ile aykýrý deðer incelemesi")
abline(h=if(n>50) 4/n else 4/(n-k-1),col="blue")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")


#Aykýrý deðerlerin çýkarýlmasý ve tekrar modelin kurulmasý

newdata<-read.table("C:/Users/suheda/OneDrive/Masaüstü/newdata.txt",header = FALSE)
names(newdata)
names(newdata)<-c("enforan","talepe","maliyete","parae","ulkeler")
attach(newdata)
ulkeler<-as.factor(ulkeler)
sonuc<-lm(enforan~talepe+maliyete+parae+ulkeler)
summary(sonuc)

#Güven Aralýklarý
confint(sonuc,level = .99)

#Deðiþen Varyanslýlýk
summary(lm(abs(residuals(sonuc)) ~fitted(sonuc)))
par(mfrow=c(2,2))
plot(predict(sonuc),abs(inf$stud.res),ylab = "Student-türü artýklar",xlab = "Tahmini deðerler")

install.packages("lmtest")
library(lmtest)
bptest(sonuc)

#White test

#Artýklarýn karesi 
res=residuals(sonuc)
sqres = res^2
sqtalepe=talepe*talepe
sqmaliyete=maliyete*maliyete
sqparae=parae*parae

talepmaliyet=talepe*maliyete
taleppara=talepe*parae
maliyetpara=maliyete*parae

# Artýk karesi üzerinden regresyon modellemesi

WH = lm(sqres ~ talepe + maliyete+ parae +sqtalepe+sqmaliyete+sqparae+talepmaliyet+taleppara+maliyetpara)

WHs = summary(WH)

# Lagrange Çarpýmý Hesaplamasý

WHts = WHs$r.squared*length(WH$residuals) 

# Ki-kare daðýlýmýndan p deðeri hesaplamasý (sd=2)

WHpv = 1-pchisq(WHts,df=WH$rank-1)

# Güven aralýklarýnýn elde edilmesi

if (WHpv < 0.05) {
  
  cat("Deðiþen varyanslýlýðý ifade eden H0 hipotezi reddedilir.\n",
      
      "WH = ",WHts,"\n","p-value = ",WHpv)
  
} else  {
  
  cat("Deðiþen varyanslýlýðý ifade eden H0 hipotezi reddedilemez.\n",
      
      "WH = ",WHts,"\n","p-value = ",WHpv)

}

# Öziliþki sorunu

dwtest(sonuc) # Durbin-watson testi 

#Çoklu Baðlantý Sorunu
install.packages("corrplot")
library(corrplot)
korelasyon_matrisi<-cor(newdata)
corrplot(korelasyon_matrisi,method = "circle")  


install.packages("car")
library(car)
install.packages("olsrr")
library(olsrr)
#výf elde etmek
ols_vif_tol(sonuc)

#Koþul sayýsý elde etmek
ols_eigen_cindex(sonuc)

#öZdeðer ve Özvektör
install.packages("fastDummies")
library(fastDummies)
dummmy<-dummy_cols(ulkeler)
ulkeler1<-dummmy$.data_1
ulkeler2<-dummmy$.data_2
ulkeler3<- dummmy$.data_3

ort1<-mean(talepe)
kt1<-sum((talepe-ort1)^2)
skx1<-(talepe-ort1)/(kt1^0.5)
ort2<-mean(maliyete)
kt2<-sum((maliyete-ort2)^2)
skx2<-(maliyete-ort2)/(kt2^0.5)
ort3<-mean(parae)
kt3<-sum((parae-ort3)^2)
skx3<-(parae-ort3)/(kt3^0.5)
ort42<-mean(ulkeler2)
kt42<-sum((ulkeler2-ort42)^2)
skx42<-(ulkeler2-ort42)/(kt42^0.5)
ort43<-mean(ulkeler3)
kt43<-sum((ulkeler3-ort43)^2)
skx43<-(ulkeler3-ort43)/(kt43^0.5)
x<-cbind(skx1,skx2,skx3,skx42,skx43)
sm<- eigen (t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)
#Özvektör ve özdeðer iliski matrisi
V<-sm$vectors
t(V)%*%V
V%*%diag(sm$values) %*%  t(V)
#Güven aralýk

newdat <-as.data.frame(newdata)
attach(newdat)
ulkeler<-as.numeric(ulkeler)
sonuc1<-lm(enforan~talepe+maliyete+parae+ulkeler)
str(ulkeler)
predict(sonuc1, newdata = newdat , interval = "confidence" )

#Deðiþken Seçimi
#Ýleriye Doðru Seçim 
newdata
attach(newdata)
ulkeler<-as.factor(ulkeler)
library(stats)

lm.null <- lm(enforan ~ 1)

forward <- step(lm.null,enforan~talepe+maliyete+parae+ulkeler, direction = "forward")

summary(forward)

# Geriye Doðru Seçim 
backward<-step(sonuc,direction="backward")

summary(backward)

# Adýmsal Seçim Yöntemi 

library(MASS)

step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)

summary(step.model)

#Ridge Regresyon

library(MASS)

ridge <- lm.ridge(enforan~talepe+maliyete+parae+ulkeler ,lambda = seq(0,1,0.05))

matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
        
        ylab=expression(hat(beta)))

abline(h=0,lwd=2)

ridge$coef

select(ridge)

ridge$coef[,ridge$lam == 0.4]