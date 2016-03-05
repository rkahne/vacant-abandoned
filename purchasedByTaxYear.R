options(stringsAsFactors = F)


data<-read.csv('./TaxDue_TextFormat.csv')

yearlyPurchasers<-data.frame(owner=data$PDBYSOLDTO, due=data$BILLTOT, year=data$TAXYEAR)
yearlyPurchasers$owner<-sapply(yearlyPurchasers$owner, function(i){
  if(i==''){
    'Metro Louisville'
  }else{
    i
  }
})


### FUNCTIONS
yearbydf<-function(y){
  temp<-subset(yearlyPurchasers, year==y)
  owners<-unique(temp$owner)
  due<-sapply(owners,function(i){
    sum(temp$due[which(temp$owner==i)])
  })
  return(data.frame(owners,due, row.names = NULL))
}


duebyyear<-function(df){
  return(sapply(AllPurchasers$owner, function(i){
    if(length(which(df$owners == i))>0){
      sum(df$due[which(df$owners==i)])
    }else{
      0
    }
  }))
}


### PLUMBING
Purchasers07<-yearbydf(2007)
Purchasers08<-yearbydf(2008)
Purchasers09<-yearbydf(2009)
Purchasers10<-yearbydf(2010)
Purchasers11<-yearbydf(2011)
Purchasers12<-yearbydf(2012)
Purchasers13<-yearbydf(2013)
Purchasers14<-yearbydf(2014)

AllPurchasers<-data.frame(owner=unique(c(Purchasers07$owners,
                        Purchasers08$owners,
                        Purchasers09$owners,
                        Purchasers10$owners,
                        Purchasers11$owners,
                        Purchasers12$owners,
                        Purchasers13$owners,
                        Purchasers14$owners)))

AllPurchasers$Seven<-duebyyear(Purchasers07)
AllPurchasers$Eight<-duebyyear(Purchasers08)
AllPurchasers$Nine<-duebyyear(Purchasers09)
AllPurchasers$Ten<-duebyyear(Purchasers10)
AllPurchasers$Elevent<-duebyyear(Purchasers11)
AllPurchasers$Twelve<-duebyyear(Purchasers12)
AllPurchasers$Thirteen<-duebyyear(Purchasers13)
AllPurchasers$Fourteen<-duebyyear(Purchasers14)
AllPurchasers$Total<-sapply(AllPurchasers$owner,function(i){
  AllPurchasers$Seven[which(AllPurchasers$owner==i)]+
    AllPurchasers$Eight[which(AllPurchasers$owner==i)]+
    AllPurchasers$Nine[which(AllPurchasers$owner==i)]+
    AllPurchasers$Ten[which(AllPurchasers$owner==i)]+
    AllPurchasers$Eleven[which(AllPurchasers$owner==i)]+
    AllPurchasers$Twelve[which(AllPurchasers$owner==i)]+
    AllPurchasers$Thirteen[which(AllPurchasers$owner==i)]+
    AllPurchasers$Fourteen[which(AllPurchasers$owner==i)]
})

write.csv(AllPurchasers, 'PurchasersByYear.csv')
