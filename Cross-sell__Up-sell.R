#Data preparation 
#change the working directory Accordingly
setwd('D:/Study/s_assingment')
pdata <- read.csv(file="Final_Data.csv", header = T,as.is = T)

#install.packages("sqldf")
#install.packages("reshape2")
library(sqldf)
 
qry <- 'SELECT CustomerID, Product, SUM(Quantity) AS total_Q FROM pdata
        GROUP BY CustomerID, Product'
pp_n <- sqldf(qry);

# substract avg.value function
sub_avg <- function(x){x-mean(x,na.rm=T)}

#Cross table for Customer and Product
library(reshape2)
ppi_n <- acast(pp_n, CustomerID ~ Product)

# Correlation for Customers/Users
ppi_n1 <- apply(ppi_n,1,sub_avg)
ppi_n1[is.na(ppi_n1)] <- 0
corrc <- cor(ppi_n1)

# Correlation for product
ppi_n2 <- apply(ppi_n,2,sub_avg)
ppi_n2[is.na(ppi_n2)] <- 0
corrp <- cor(ppi_n2)

gc();

#fuction to find corrlated customer with a certain customer and cutoff of correlation
cor_custn <- function(customer,corr=corr1, ctof=0.4){
  x <- corr[,customer]
  x <- x[-(which('1'== x))]
  m_t <- x[which(x>ctof)]
  custid_mc <- rownames(as.data.frame(m_t))
  return(custid_mc)
} 

#loop for grouping customers based on correlated customers
cust_g <- list()
corr1 <- corrc
i <- 1
z=c()
while(length(colnames(corr1))!=0){
  A <- cor_custn(colnames(corr1)[1])
  
  if(length(as.vector(A))==0){
    z=c(z,colnames(corr1)[1])
  corr1 <- corr1[-1, -1]
  i=i
 }
  else {
    cust_g[[i]] <- c(colnames(corr1)[1],sort(A))
    corr1 <-  corr1[-(which(colnames(corr1) %in% cust_g[[i]])),-(which(colnames(corr1) %in% cust_g[[i]]))]
    i=i+1
  }
}

length(z)/length(unique(pdata$CustomerID))

###################
#fuction to find corrlated products with a certain product and cutoff of correlation
cor_product <- function(product="Aloe Relief",corr=corr2, ctof=0.40){
  x <- corr[,product]
  x <- x[-(which('1'== x))]
  m_t <- x[which(x>ctof)]
  pdmc <- rownames(as.data.frame(m_t))
  return(pdmc)
} 

#loop for grouping products based on correlated products
prod_g <- list()
corr2 <- corrp
i <- 1
w=c()
while(length(colnames(corr2))!=0){
  A <- cor_product(colnames(corr2)[1])
  
  if(length(as.vector(A))==0){
    w=c(w,colnames(corr2)[1])
    corr2 <- corr2[-1, -1]
    i=i
  }
  else {
    prod_g[[i]] <- c(colnames(corr2)[1],sort(A))
    corr2 <-  corr2[-(which(colnames(corr2) %in% prod_g[[i]])),-(which(colnames(corr2) %in% prod_g[[i]]))]
    i=i+1
  }
}
length(w)/length(unique(pdata$Product))


#Cross-selling using item based collaborative filtering 
#Cross-Sell- recomending products which can be purchased together, product group made by item based colaborative filtering indicates producst under same group can be purchased together.

rm(list=setdiff(ls(), c("pdata",'cust_g','prod_g')));gc()
cross <- function(cust='17122'){

    for(i in 1:length(cust_g)){
        if(cust%in%cust_g[[i]]) {
          a <- cust_g[[i]]
          break
        }
    }
  p1 <- unique(pdata$Product[pdata$CustomerID==cust]) 
  p_all <- unique(pdata$Product[pdata$CustomerID%in%a])
  p_other <- setdiff(p_all,p1)
  
  p1_tag <- data.frame(p1=p1,tag=rep(0,length(p1)))
  for(i in 1:length(p1)){
  for(j in 1:length(prod_g)){
    if(p1[i]%in%prod_g[[j]]) {
      p1_tag$tag[i] <- j
      break
    }}}
  p_other_tag <- data.frame(p_other=p_other,tag=rep(0,length(p_other)))
  for(i in 1:length(p_other)){
    for(j in 1:length(prod_g)){
      if(p_other[i]%in%prod_g[[j]]) {
        p_other_tag$tag[i] <- j
        break
      }}}
  p_other_tag1 <- p_other_tag[-which(p_other_tag$tag%in%setdiff(p_other_tag$tag,p1_tag$tag)),]
  p_cross <- setdiff(p_other_tag1$p_other,p1_tag$p1)
  return(p_cross)
}


########################################
#Upsell- recomending a higher priced product contaning similar features purchased by a customer. We are classify the products based on their features(kmeans Cluster) and recomend a higher priced product having same features.

source("product_Group.R")
rm(list=setdiff(ls(), c("pdata",'cust_g','prod_g','prod_clus',"cross")));gc()

up_sell <- function(cust="17122"){
  
  for(i in 1:length(cust_g)){
    if(cust%in%cust_g[[i]]) {
      a <- cust_g[[i]]
      break
    }
  }
  p1 <- unique(pdata$Product[pdata$CustomerID==cust]) 
  p_all <- unique(pdata$Product[pdata$CustomerID%in%a])
  p_other <- setdiff(p_all,p1)
  
  prod_gr <- prod_clus[,c("Product","UnitPrice","group")]
  
  p1_tag <- data.frame(p1=p1,pgroup=rep(0,length(p1)), unit_price=rep(0,length(p1)))
  for(i in 1:length(p1)){
    p1_tag$pgroup[i] <- prod_gr$group[prod_gr$Product%in%p1_tag$p1[i]]
    p1_tag$unit_price[i] <- prod_gr$UnitPrice[prod_gr$Product%in%p1_tag$p1[i]]
  }
  
  p_other_tag <- data.frame(p_other=p_other,pgroup=rep(0,length(p_other)), unit_price=rep(0,length(p_other)))
  for(i in 1:length(p_other)){
    p_other_tag$pgroup[i] <- prod_gr$group[prod_gr$Product%in%p_other_tag$p_other[i]]
    p_other_tag$unit_price[i] <- prod_gr$UnitPrice[prod_gr$Product%in%p_other_tag$p_other[i]]
  }
  p_other_tag1 <- p_other_tag[-which(p_other_tag$pgroup%in%setdiff(unique(p_other_tag$pgroup),unique(p1_tag$pgroup))),]
  
  qry1 <- 'SELECT p1, pgroup,MAX(Unit_Price) as Max_UP FROM p1_tag
        GROUP BY pgroup'
  p1_max <- sqldf(qry1)
  
  y1 <- merge(x = p_other_tag1, y = p1_max, by = "pgroup", all.x = TRUE)
    y1$ind <- ifelse(y1$unit_price>y1$Max_UP,1,0)
  y1 <- y1[which(y1$ind==1),]
  
  upsell = sqldf('SELECT pgroup, p_other, MIN(unit_price) as Unit_Price FROM y1
            GROUP BY pgroup')
 return(as.character(upsell$p_other))
}
## Cross_sell & Up_sell Function 

cross('15942') #For different customer, Cross_sell products

up_sell('15942') #For different customer, Up_sell products


