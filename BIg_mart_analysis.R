library(caret)
library(tidyverse)
library(cowplot)
data = read.csv(file.choose())
train = data
test = read.csv(file.choose())
View(data)
dim(data)
dim(train)
dim(test)
glimpse(train)

prop.table(table( train$Outlet_Type))
plot(log(train$Item_Outlet_Sales) ~ log(train$Item_Visibility),col= train$Outlet_Type)

ggplot(train, aes(x=Item_Visibility,y=Item_Outlet_Sales, col = Outlet_Location_Type))+ geom_point()+ facet_grid(~Outlet_Type)

ggplot(train)+geom_histogram(mapping = aes(Item_Outlet_Sales, col = 'red'),binwidth = 150)



glimpse(train)
p1 = ggplot(train)+geom_histogram(mapping = aes(train$Item_Weight, col = 'red'))
p2= ggplot(train)+geom_histogram(mapping = aes(train$Item_Visibility, col = 'red'))
p3 = ggplot(train)+geom_histogram(mapping = aes(train$Item_MRP, col = 'red'))

library(cowplot)
plot_grid(p1,p2,p3,nrow=1)


table(train$Item_Fat_Content)

##  TWo types of ggplot of explanatory variable

ggplot(train %>% group_by(Item_Fat_Content) %>% summarise(cont = n())) + geom_bar(aes(x = Item_Fat_Content, y = cont,label=cont), stat = "identity", fill= "green") 

train %>% 
  group_by(Item_Fat_Content) %>%
  count() %>% ggplot()+ geom_bar(aes(Item_Fat_Content, n), stat = "identity", fill = "green", color = "red")
##


table(train$Outlet_Establishment_Year)

train %>%
  group_by(Outlet_Establishment_Year) %>% 
  count() %>%
  ggplot() + geom_bar(aes(Outlet_Establishment_Year, n), stat = 'identity') + geom_label(aes(Outlet_Establishment_Year, n, label = n),vjust=1,hjust=.5)+theme(axis.text.x = element_text(angle=45))



# Target variable vs dependent variable


t1 = ggplot(train, aes( y=Item_Outlet_Sales , x = Outlet_Size )) + geom_violin()
t2 = ggplot(train, aes( y=Item_Outlet_Sales , x = Outlet_Location_Type )) + geom_violin()
t3 = ggplot(train, aes( y=Item_Outlet_Sales , x = Outlet_Size )) + geom_violin()
t4 = ggplot(train, aes( y=Item_Outlet_Sales , x = Outlet_Identifier )) + geom_violin()
plot_grid(t1,t2,t3,t4, ncol =2, nrow = 2)


#### Missing value handeling

## Chgecking missing values

prop.table(table(is.na(train)))



library(mice)

mis = mice(train , method = 'rf')
mis

## See all variables with total number of missing values

for(i in 1:ncol(train)){
  cat(names(train)[i],":",sum(is.na(train[,i])),"\n")
}



## Weighted mean missing value imputation


mis_ind = which(is.na(train$Item_Weight))
for(i in mis_ind){
  item = train$Item_Identifier[i]
  
  train$Item_Weight[i] = mean(train$Item_Weight[train$Item_Identifier==item],na.rm=T)
}

for(i in mis_ind){
  item = train$Item_Identifier[i]
  
 
}
item


###############  model building 

## linear model
names(train)
linear_model <-  lm(Item_Outlet_Sales ~ . , data = train[,!names(train) %in% "Item_Identifier"])
broom::tidy(linear_model)

