setwd("E:/job/interm/CSHO_data/")

DT<- read.csv(file = 'DT11_15.csv')
catchment_name <- c("Adams", "Berks", "Blair", "Carbon","Centre","Clinton","Columbia","Cumberland","Dauphin","Franklin",
                    "Fulton","Huntingdon", "Juniata", "Lancaster", "Lebanon", "Lehigh", "Luzerne", "Lycoming", "Mifflin",
                    "Montour","Northumberland", "Perry", "Schuylkill", "Snyder", "Sullivan", "Union", "Wyoming","York")
DT$catchment<- ifelse(DT$County %in% catchment_name,"catchment","noncatch")
# group Latino/Hispanic: is defined as hispanic_orig between 1-8. This is separate from race.
DT$Latino<- ifelse(DT$Hispanic %in% c(1,2,3,4,5,6,7,8),"Latino","NonLatino")

#### group AficanAmerican Race1==2
DT$Afican<-ifelse(DT$Race1==2,"Afican","Non")

# remove duplicate id 
sDT<-unique(setDT(DT),by=c("PatientID","PSite","DxDate"))
# only serious case
#limit to serious case--> sDT is a data set with only serious cases and no replicated tumor
sDT$ICO3<-ifelse(sDT$Cancer=="bladder"& (sDT$BehaviorICDO3==2|sDT$BehaviorICDO3==3),1,ifelse(sDT$BehaviorICDO3==3,1,0))
sDT<-subset(sDT,sDT$ICO3==1)

#catchment area(DT1 data set)
DT1<-subset(sDT,sDT$catchment=="catchment")
#Average number of cancers/year
t<- melt(table(Cancer=DT1$Cancer))
sum(melt(t$value))/5

###Table-Catchment######
#Average number of cancers/year age-adjusted with 95% CI
#
#
#Average number of cancers/year for African Americans
aDT1<-subset(DT1,DT1$Afican=="Afican")
a<-melt(table(Cancer=aDT1$Cancer))
sa<-sum(melt(a$value))/5
round(sa, digits = 0)


#Average number of cancers/year for Hispanic/Latinos
LDT1<-subset(DT1,DT1$Latino=="Latino")
l<-melt(table(Cancer=LDT1$Cancer))
sum(melt(l$value))/5
#Number of cases for each site (top 10)
t10<-t[order(-t$value ),]
t10$avg<-with(t10,round(value/5,digits=0))
st10<-t10[1:10,]
ot10<-t10[11:23,]
sum(ot10$avg)
#Number of cases for each site (top 10) for African Americans
a10<-a[order(-a$value ),]
a10$avg<-with(a10,round(value/5,digits=0))
sa10<-a10[1:10,]
oa10<-a10[11:23,]
sum(oa10$avg)

#Number of cases for each site (top 10) for Hispanic/Latinos
l10<-l[order(-l$value ),]
l10$avg<-with(l10,round(value/5,digits=0))
sl10<-l10[1:10,]
ol10<-l10[11:23,]
sum(ol10$avg)
#Percent of total cases for each site (top 10)
st10$percent<-with(st10,avg/(sum(avg)+4448)*100)

#Percent of total cases for each site (top 10) for African Americans
sa10$percent<-with(sa10,avg/(sum(avg)+125)*100)
#Percent of total cases for each site (top 10) for Hispanic/Latinos
sl10$percent<-with(sl10,avg/(sum(avg)+117)*100)

#### Table-County-aarate #####

#Number of cancers/year age-adjusted with 95% CI
setwd("E:/job/interm/Pop_data")
pop5<- read.csv(file = 'pop5_acs.csv')
t1 <- table(Age=sDT$agegroups,County=sDT$County)
t2<-melt(t1,id=c('Age'))
colnames(pop5)[2] <-"County"
Total1<- merge(t2,pop5,by=c("Age","County"))
#### US 2000 standard popoulation(produce popjs, totalpopjs);
pop_2000 <- c(18986520, 19919840,  20056779,	19819518, 18257225, 17722067, 19511370,	 22179956,  22479229,  19805793, 17224359, 13307234,	 10654272, 9409940, 8725574, 7414559,  4900234, 4259173)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
sum_pop2000<-sum(pop_2000)
popjs<-cbind((agelabels),melt(pop_2000))
popjs$sumpopjs<-sum(pop_2000)
colnames(popjs)[1] <-"Age"
colnames(popjs)[2] <-"popjs"
total1<-merge(Total1,popjs,by=c("Age"))
setnames(total1, "total", "popj") 

total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs)*100000)
total1$sei<-with(total1,(popjs/sumpopjs)^2*(value/popj^2))
s1<-aggregate(total1$ajrate,by=list(total1$County),FUN='sum')
s2<-aggregate(total1$sei,by=list(total1$County),FUN='sum')
s3<-aggregate(total1$value,by=list(total1$County),FUN='sum')
s4<-aggregate(total1$popj,by=list(total1$County),FUN='sum')

summary<-merge(s1,s2,by=c('Group.1'))
summary<-merge(s4,summary,by=c('Group.1'))
summary<-merge(s3,summary,by=c('Group.1'))
colnames(summary)[1] <-"County"
colnames(summary)[2] <-"No"
colnames(summary)[3] <-"Pop"
colnames(summary)[4] <-"AjRate"
summary

county_name <- c("Adams","Allegheny","Armstrong","Beaver","Bedford","Berks","Blair","Bradford","Bucks","Butler",
                 "Cambria","Cameron","Carbon","Centre","Chester","Clarion","Clearfield","Clinton","Columbia",
                 "Crawford","Cumberland","Dauphin","Delaware","Elk","Erie","Fayette","Forest","Franklin","Fulton",
                 "Greene","Huntingdon","Indiana","Jefferson","Juniata","Lackawanna","Lancaster","Lawrence","Lebanon",
                 "Lehigh","Luzerne","Lycoming","McKean","Mercer","Mifflin","Monroe","Montgomery","Montour","Northampton",
                 "Northumberland","Perry","Philadelphia","Pike","Potter","Schuylkill","Snyder","Somerset","Sullivan",
                 "Susquehanna","Tioga","Union","Venango","Warren","Washington","Wayne","Westmoreland","Wyoming","York")

# Set up result dataframe before the loop
#install.packages("magicfor")
library(magicfor)               # load library
library(dsrTest)

magic_for(silent = TRUE)

for (x in county_name){
  test<-subset(total1,total1$County==x)
  a<-dsrTest(test$value, test$popj, test$popjs, method = "gamma"  , mult = 1e5)
  # df<-data.frame(x,a$conf.int)
  put (a$estimate ,a$conf.int[1], a$conf.int[2])
}

result<-magic_result_as_dataframe()     # get the result

#
#### Table-County #####
#County Number
#Average Number of cancers/year
tc<-melt(table(County=sDT$DxCounty))
tc$avg<-with(tc,round(value/5,digits=0))
#Average number of cancers/year for African Americans
asDT<-subset(sDT,sDT$Afican=="Afican")
ac<-melt(table(County=asDT$DxCounty))
ac$avg<-with(ac,round(value/5,digits=0)) 
#Average number of cancers/year for Hispanic/Latinos
lsDT<-subset(sDT,sDT$Latino=="Latino")
lc<-melt(table(County=lsDT$DxCounty))
lc$avg<-with(lc,round(value/5,digits=0))

avg_county<-merge(tc,ac,by=c("County"),all = T)
avg_county<-merge(avg_county,lc,by=c("County"),all = T)
avg_county<-avg_county[,c(1,3,5,7)]
colnames(avg_county)[2]<-"total"
colnames(avg_county)[3]<-"Black"
colnames(avg_county)[4]<-"Latino"

#Number of cases for each site (top 10)
###change to race group sDT->asDT for african american
######################  sDT->lsDT for Latino

tc10<-melt(table(County=lsDT$DxCounty,Cancer=lsDT$Cancer))
tc10$value<-with(tc10,round(value/5,digits=0))

require(plyr)
ttc10<-ddply(tc10, "County", function(x) head(x[order(x$value, decreasing = TRUE) , ], 10))
otc<-ddply(tc10, "County", function(x) head(x[order(x$value),], 13))
otc<-aggregate(otc$value,by=list(otc$County),FUN=sum)
otc$Cancer<-"others"
colnames(otc)[1]<-"County"
colnames(otc)[2]<-"value"
r<-rbind(ttc10,otc)

tem1<-aggregate(tc10$value,by=list(tc10$County),FUN="sum")
colnames(tem1)[1]<-"County"
r<-merge(r,tem1,list=c("County"))
r$rate<-with(r,value/x*100)
r<-r[,-c(4)]
write.csv(r, file = 'E:/job/interm/Story Map/temp.csv')


#Number of cases for each site (top 10) for African Americans
#Number of cases for each site (top 10) for Hispanic/Latinos
#Percent of total cases for each site (top 10)  
#Percent of total cases for each site (top 10) for African Americans
#Percent of total cases for each site (top 10) for Hispanic/Latinos

###Table-Tract####
###  
#County Number
#Tract Number
#Average number of cancers/year
setwd("E:/job/interm/Pop_data/ACS_15_5yr")
pop15<- read.csv("R11743209_SL140.csv")

tt<-melt(table(County=sDT$DxCounty,CenTract=sDT$CenTract2010),id=c("CenTract"))
tt$avgvalue<-with(tt,round(value/5,digits=0))
x1<-aggregate(pop15$SE_T013_001 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
#tt<-cbind(tt,x1,)
tt<-merge(tt,x1,list=c("county","CenTract"))
tt<-subset(tt,tt$x>300)
write.csv(tt, file = 'E:/job/interm/Story Map/avecases_tract.csv')

#Average number of cancers/year for African Americans (if stratum population is >300)
ta<-melt(table(County=asDT$DxCounty,CenTract=asDT$CenTract2010),id=c("CenTract"))
ta$value<-with(ta,round(value/5,digits=0))

xa<-aggregate(pop15$SE_T013_003 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
ta<-merge(ta,xa,list=c("county","CenTract"))
ta<-subset(ta,ta$x>300)
write.csv(ta, file = 'E:/job/interm/Story Map/avecases_tract_Black.csv')

#Average number of cancers/year for Hispanic/Latinos (if stratum population is >300)
tl<-melt(table(County=lsDT$DxCounty,CenTract=lsDT$CenTract2010),id=c("CenTract"))
tl$value<-with(tl,round(value/5,digits=0))
xl<-aggregate(pop15$SE_T015_003 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
tl<-merge(tl,xl,list=c("county","CenTract"))
tl<-subset(tl,tl$x>300)
write.csv(tl, file = 'E:/job/interm/Story Map/avecases_tract_Latino.csv.csv')

#Number of cases for each site (top 10) (if total population is >300)
#Number of cases for each site (top 10)
###change to race group sDT->asDT for african american
######################  sDT->lsDT for Latino

tc10<-melt(table(County=sDT$DxCounty,CenTract=sDT$CenTract2010,Cancer=sDT$Cancer),id=c("County","CenTract"))
tc10$value<-with(tc10,round(value/5,digits=0))

require(plyr)
ttc10<-ddply(tc10, c("County","CenTract"), function(x) head(x[order(x$value, decreasing = TRUE) , ], 10))
otc<-ddply(tc10, c("County","CenTract"), function(x) head(x[order(x$value),], 13))
otc<-aggregate(otc$value,by=list(otc$County, otc$CenTract ),FUN=sum)
otc$Cancer<-"others"
colnames(otc)[1]<-"County"
colnames(otc)[2]<-"CenTract"
colnames(otc)[3]<-"value"
r<-rbind(ttc10,otc)

tem1<-aggregate(tc10$value,by=list(tc10$County,tc10$CenTract),FUN="sum")
colnames(tem1)[1]<-"County"
colnames(tem1)[2]<-"CenTract"
r<-merge(r,tem1,list=c("County","CenTrat"))
r$rate<-with(r,value/x*100)
r<-r[,-c(5)]

x1<-aggregate(pop15$SE_T013_001 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
tt<-merge(r,x1,list=c("County","CenTract","Cancer"))
tt<-subset(tt,tt$x>300)

#xa<-aggregate(pop15$SE_T013_003 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
#tt<-merge(r,xa,list=c("County","CenTract"))
#tt<-subset(tt,tt$x>300)

#xl<-aggregate(pop15$SE_T015_003 ,by=list(County=pop15$Geo_COUNTY,CenTract=pop15$Geo_TRACT),FUN="sum")
#tt<-merge(r,xl,list=c("County","CenTract"))
#tt<-subset(tt,tt$x>300)


write.csv(tt, file = 'E:/job/interm/Story Map/temp.csv')


#Percent of total cases for each site (top 10) (if total population is >300)

