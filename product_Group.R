library(sqldf)
qry <- 'SELECT Product,COUNT(DISTINCT CustomerID) AS customers,COUNT(DISTINCT InvoiceNo) Frequency,
        SUM(Quantity) AS Total_Q,Unit_Price AS UnitPrice,Product_type
         FROM pdata
         GROUP BY Product'
unik_Product <- sqldf(qry)
df <- data.frame(Product_type= unique(unik_Product$Product_type),Product_type_tag= 1:length(unique(unik_Product$Product_type)))
unik_Product <- merge(x = unik_Product, y = df, by = "Product_type", all.x = TRUE)

cdata <- unik_Product[,-which(names(unik_Product)%in%c("Product_type","Product","Frequency","Revenue"))]
cdata_scaled <- scale(cdata)
set.seed(123)
wss <- numeric()
for (i in 1:15){
  wss[i] <- kmeans(cdata_scaled,centers = i)$tot.withinss
}
plot(x=1:15,wss,type = "l")


set.seed(123)
c <- kmeans(cdata_scaled,centers = 6)
group <- c$cluster
table(group)
prod_clus <- data.frame(unik_Product,group)

Avg <- aggregate(prod_clus,by=list(group),mean)
Avg <- data.frame(Avg,"Size_of_group"=c$size)

c$withinss/c$betweenss

# unik_Product$Revenue <- unik_Product$Total_Q*unik_Product$UnitPrice
# vif(lm(Revenue~ customers+Frequency+Total_Q+UnitPrice+Product_type_tag , data=unik_Product))

#plot(unik_Product$customers,unik_Product$Frequency)
#plot(unik_Product$customers,unik_Product$Total_Q)
#plot(unik_Product$customers,unik_Product$UnitPrice)
#plot(unik_Product$customers,unik_Product$Product_type_tag)

#plot(unik_Product$Frequency,unik_Product$Total_Q)
#plot(unik_Product$Frequency,unik_Product$UnitPrice)
#plot(unik_Product$Frequency,unik_Product$Product_type_tag)

#plot(unik_Product$Total_Q,unik_Product$UnitPrice)
#plot(unik_Product$Total_Q,unik_Product$Product_type_tag)

#plot(unik_Product$Product_type_tag,unik_Product$UnitPrice)

#unik_Product$revenue <- unik_Product$Total_Q*unik_Product$UnitPrice
#library(car)
#vif(lm(revenue ~ customers+Frequency+Total_Q+UnitPrice+Product_type_tag , data=unik_Product))
#vif(lm(revenue ~ customers+Total_Q+UnitPrice+Product_type_tag , data=unik_Product))
#vif(lm(revenue ~ Frequency+Total_Q+UnitPrice+Product_type_tag , data=unik_Product))
 



