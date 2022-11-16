# Analysis Master Thesis Annika Frach


#set working directory

setwd("C:/MyData/UNI Leiden/Master/thesis/data stage 4")

#install.packages("vroom", dependencies = TRUE, repos = "https://cran.rstudio.com")

library(vroom)
library(dplyr)
library(tidyr)
#load plac data (which are txt files with all components)


list_of_files <- list.files(path = "C:/MyData/UNI Leiden/Master/thesis/data stage 4/plac", recursive = TRUE,
                            pattern = "\\.txt$", 
                            
                            full.names = TRUE)


df <- vroom(list_of_files, col_names = "X1", delim = "  ", id = "FileName") 

df<-df %>%
rename(DMN = X1, c2 = X2, c3 = X3, c4 = X4, c5 = X5, c6 = X6, CEN = X7, c8 = X8, c9 = X9, SN = X10, c11 = X11, c12 = X12, c13 = X13, c14 = X14, c15 = X15)

Correct_components_plac<- df %>%
  select(FileName, SN, CEN, DMN) %>%
   sapply(gsub,pattern="C:/MyData/UNI Leiden/Master/thesis/data stage 4/plac/dr_stage4_",replacement="")%>%
  cbind(condition = "1") 


uniplac<-unique(Correct_components_plac)
PPTnumber <- cbind(Correct_components_plac, c(1:25))  
  
###load oxy data


list_of_files <- list.files(path = "C:/MyData/UNI Leiden/Master/thesis/data stage 4/oxy", recursive = TRUE,
                            pattern = "\\.txt$", 
                            
                            full.names = TRUE)


df <- vroom(list_of_files, col_names = "X1", delim = "  ", id = "FileName") 

df<-df %>%
  rename(DMN = X1, c2 = X2, c3 = X3, c4 = X4, c5 = X5, c6 = X6, CEN = X7, c8 = X8, c9 = X9, SN = X10, c11 = X11, c12 = X12, c13 = X13, c14 = X14, c15 = X15)

Correct_components_oxy<- df %>%
  select(FileName, SN, CEN, DMN) %>%
  sapply(gsub,pattern="C:/MyData/UNI Leiden/Master/thesis/data stage 4/oxy/dr_stage4_",replacement="")%>%
  cbind(condition = "2") 


###combine data sets
joinedata<-rbind(Correct_components_oxy, Correct_components_plac)



# combined<- rbind.data.frame(as.numeric(Correct_components_plac, Correct_components_oxy, by.x = "SN", by.y = "SN") )
combined<- rbind.data.frame(Correct_components_plac, Correct_components_oxy)
combined$SN <- as.numeric(combined$SN)
combined$DMN <- as.numeric(combined$DMN)
combined$CEN <- as.numeric(combined$CEN)
combined$condition <- as.numeric(combined$condition)

pcount<-unique(combined$FileName)%>%
  cbind(c(1:25,1:25))%>%
  colnames(c("FileName", "PPTnumber"))


combined$pptnumber <- as.numeric(combined$pptnumber)


  combined<-combined%>%
    cbind("pptnumber"= c(rep(1:25, each=450),rep(1:25, each=450)))  ## add correct participant number
  
  



combined$FileName<-gsub("subject([0-4]+).txt", "\\1", combined$FileName) ### to remove unnecessary file name info

###check wether number of data entries correct

clist<-combined%>%
  group_by(FileName)%>%
  count()%>%
  unique()
  arrange((n))
  #filter(n!=450)

  
  clist<-combined%>%
    unique(group_by(combined$FileName))
    
 
  #filter(n!=450)  
  

###summary stats








 
 


###can be removed

#longdata<-pivot_longer(combined, cols = c(DMN, SN,CEN), 
 #            names_to = "Region", 
  #           values_to = "Activation")

#long2<- longdata%>%
#  select(Region, Activation)

#test1<- combined%>%
#  select(DMN, CEN, SN, FileName)

##cormat for plac  
  
cormat<-combined%>%
  select(SN,DMN,CEN) %>%
  filter(combined$condition==1)
as.numeric(unlist(cormat))
cond1cormat<-cor(cormat, method = c("pearson"))

#cormat for oxy

cormat2<-combined%>%
  select(SN,DMN,CEN) %>%
  filter(combined$condition==2)
as.numeric(unlist(cormat))

cond2cormat<-cor(cormat2, method = c("pearson"))
  

library(psych)  # for fishers r to z transformation

cond1fish<-fisherz(cond1cormat)  
cond2fish<-fisherz(cond2cormat)


uniquelist<-unique(combined$pptnumber)
#string_interp

combined$FileName<-as.numeric(combined$FileName)

###filter out conditon 2
cond1corr<-combined %>%
  filter(condition==1)

###filter out conditon 1
cond2corr<-combined %>%
  filter(condition==2)




###define resultsingleppt
resultsingleppt1<-list()
resultsingleppt2<-list()




##matrix conditon1





for ( i in uniquelist){
  cordata1<-filter(cond1corr, pptnumber==i)
  resultsingleppt1[[i]]<-(cor(cordata1))
  print(i)
}

for ( i in uniquelist){
  cordata2<-filter(cond2corr, pptnumber==i)
  resultsingleppt2[[i]]<-(cor(cordata2))
  print(i)
}

#resultsingleppt1<-resultsingleppt1[c(1:25)]
#resultsingleppt2<-resultsingleppt2[c(1:25)] 


###tible for conditon 1
PPTnumber<-uniquelist
Conditon<-c(rep(1, each = 25))


tibblecond1<-tibble(PPTnumber, Conditon)

###tibble for condition 2


Conditon<-c(rep(2, each = 25))

tibblecond2<-tibble(PPTnumber, Conditon)

##combine tibbles

combinedtibble<-rbind(tibblecond1,tibblecond2)

### make it an array  
  
singlearray1<- array(unlist(resultsingleppt1), dim = c(6, 6, 25))
singlearray2<- array(unlist(resultsingleppt2), dim = c(6, 6, 25))


###CN*SN

#extractedCN_SN<-rbind(singlearray1[2,3,1:25],singlearray2[2,3,1:25]) 
 
extractedCN_SN<-singlearray1[2,3,1:25]


SN_CEN_cond1<-cbind(tibblecond1,extractedCN_SN)

extractedCN_SN<-singlearray2[2,3,1:25]
SN_CEN_cond2<-cbind(tibblecond2,extractedCN_SN)

combinedfulltibblesSN_CEN<-rbind(SN_CEN_cond1,SN_CEN_cond2)


###SN*DMN
extractedDMN_SN<-singlearray1[2,4,1:25]


DMN_SN_cond1<-cbind(tibblecond1,extractedDMN_SN)

extractedDMN_SN<-singlearray2[2,4,1:25]
DMN_SN_cond2<-cbind(tibblecond2,extractedDMN_SN)

combinedfulltibblesDMN_SN<-rbind(DMN_SN_cond1,DMN_SN_cond2)


###CN_DMN
extractedCN_DMN<-singlearray1[3,4,1:25]


CN_DMN_cond1<-cbind(tibblecond1,extractedCN_DMN)

extractedCN_DMN<-singlearray2[3,4,1:25]
CN_DMN_cond2<-cbind(tibblecond2,extractedCN_DMN)

combinedfulltibblesCN_DMN<-rbind(CN_DMN_cond1,CN_DMN_cond2)




##mean of pearson correlation
#conditon1
MeanPearson1<-rowMeans(singlearray1, dim = 2)
#conditon 2
MeanPearson2<-rowMeans(singlearray2, dim = 2)






row<-Mean[c(2,3,4), ]
column<-row[,c(2,3,4)]
library(papaja)

install.packages("corrplot")
library(corrplot)
corrplot(cor(row), method="color")
library(ggcorrplot)


#clean matrix conditon 1

rowP1<-MeanPearson1[c(2,3,4), ]
columnP1<-rowP1[,c(2,3,4)]


#clean matrix condiion 2

rowP2<-MeanPearson2[c(2,3,4), ]
columnP2<-rowP2[,c(2,3,4)]

###corplots


library(RColorBrewer)

####To Do : scale plot

###corrplot condtion1

ggcorrplot(columnP1, ggtheme = papaja::theme_apa, method = "square",col=rev(brewer.pal(n=3, name="RdYlBu")), title ="Correlation matrix condition 1",  tl.col = "white", outline="black") +
   scale_x_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))+
  scale_y_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))
 
 
#### corrplot conditon2

ggcorrplot(columnP2, method = "square",col=rev(brewer.pal(n=3, name="RdYlBu")) ,
           ggtheme = papaja::theme_apa, title ="Correlation matrix condition 2",  tl.col = "white", outline="black") +
  scale_x_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))+
  scale_y_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))

### difference matrix

differencedata2<-columnP2-columnP1

ggcorrplot(differencedata2, method = "square",col=rev(brewer.pal(n=3, name="RdYlBu")) ,
           ggtheme = papaja::theme_apa, title ="Correlation matrix difference Oxytocin-Placebo",  tl.col = "white", outline="black") +
  scale_x_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))+
  scale_y_discrete(limit = c("I1", "SI2", "SI1"),labels=c("SN","CEN","DMN"))

col.lim = c(0,1)
library(wesanderson) 






###make everything for joined data
resultsingleppt<-list()
singlematarray<-array(c(resultsingleppt), dim=c(6,6,25))



for ( i in uniquelist){
  cordata<-filter(combined, pptnumber==i)
  resultsingleppt[[i]]<-(cor(cordata))
  print(i)
}



resultsingleppt<-resultsingleppt[c(1:25)] 



### make it an array  

singlearray<- array(unlist(resultsingleppt), dim = c(6, 6, 25))

tiblledata<-tibble(singlearray1)

### z transform SNXCEN

combinedfulltibblesSN_CEN$extractedCN_SN<-fisherz(combinedfulltibblesSN_CEN$extractedCN_SN)

combinedfulltibblesSN_CENz<-combinedfulltibblesSN_CEN

### z transform SNXCEN

combinedfulltibblesDMN_SN$extractedDMN_SN<-fisherz(combinedfulltibblesDMN_SN$extractedDMN_SN)

combinedfulltibblesDMN_SNz<-combinedfulltibblesDMN_SN

### ztransform CN_DMN

combinedfulltibblesCN_DMN$extractedCN_DMN<-fisherz(combinedfulltibblesCN_DMN$extractedCN_DMN)

combinedfulltibblesCN_DMNz<-combinedfulltibblesCN_DMN





combinedfulltibblesSN_CEN$extractedCN_SN<-fisherz(combinedfulltibblesSN_CEN$extractedCN_SN)

combinedfulltibblesSN_CENz<-combinedfulltibblesSN_CEN
#ztransformed1<-fisherz(singlearray1)
#ztransformed2<-fisherz(singlearray2)

#Meanz1<-rowMeans(ztransformed1, dim = 2)
#Meanz2<-rowMeans(ztransformed2, dim = 2)

rowz1<-ztransformed1[c(2,3,4), ,1:25]
CleanMeaz1<-rowz1[,c(2,3,4),1:25]

rowz2<-ztransformed2[c(2,3,4), 1:25]
CleanMeaz2<-rowz2[,c(2,3,4),1:25]

CleanMeaz1<-CleanMeaz1%>%
  abind(condition = "1") 

CleanMeaz2<-CleanMeaz2%>%
  cbind(condition = "2") 


regions<-c("SN","CEN","DMN")
regionscor<-c("SN+CEN","SN+DMN","DMN+CEN")
for(index in 1:length(CleanMeaz1)) {
  print(CleanMeaz1[index, 1, 1])
}
  
data1<-abind(CleanMeaz1, new.names=regions)
data1<-cbind(regions, CleanMeaz1)

ztransjoinedata<-rbind(CleanMeaz1, CleanMeaz2)



combinedZmean<-
  
  
ztransformed<-fisherz(columnP2)
  pairwise.t.test(x = ztransjoinedata, g = longdata$Activation,
                         p.adjust.method = "bonf", paired = TRUE)


  
  
  
pivot_wider(table2, 
            names_from = "type", 
            values_from = "count")




#### t-test CNXSN



t.test(extractedCN_SN ~ Conditon, data=combinedfulltibblesSN_CENz, paired = T)


#### t-test CNXDMN
t.test(extractedCN_DMN ~ Conditon, data=combinedfulltibblesCN_DMNz, paired = T)

###t-test DMNXSN

t.test(extractedDMN_SN ~ Conditon, data=combinedfulltibblesDMN_SNz, paired = T)