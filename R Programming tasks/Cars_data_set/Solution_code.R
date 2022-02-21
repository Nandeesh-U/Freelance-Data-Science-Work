setwd("C:/Users/Umesha/Desktop/HW1")

#############Q1#############
#b)creating the required vector
vec = c(2100,1,1,30,165, 2101,2,1,39,880, 2102,1,3,39,902, 2103,1,3,33,674, 2104,2,3,40,260,
        2105,1,3,40,100, 2106,1,1,30,229, 2107,2,3,25,764, 2108,1,3,33,901,
        2109,1,1,54,868, 2110,1,3,39,472, 2111,2,1,49,254, 2112,2,3,44,739,
        2113,1,1,32,845, 2114,2,3,32,694, 2115,2,3,35,555, 2116,1,3,66,287,
        2117,1,3,58,872, 2118,1,3,60,981, 2119,1,3,42,395, 2120,2,5,48,344)
#c) Creation of cost matrix
cost_mat = matrix(vec,nrow = 21,ncol = 5,byrow = TRUE)

#d) dimension of the matrix
dim(cost_mat)

#e) changing the column names
colnames(cost_mat)= c("SubjectID", "Gender", "Race", "Age","Price")

#f)price vector
price = cost_mat[,5]

#g)Tax calculation
Tax = c()
for (i in 1:length(price)) {
  if(price[i]<600){
    Tax[i] = price[i]*0.06
  }else{
    Tax[i] = price[i]*0.08
  }
}

#h)New matrix
cost_tax = cbind(cost_mat,Tax)

#i)Total cost matrix
Total_price = cost_tax[,5]+cost_tax[,6]
total_cost = cbind(cost_tax[,-6],Total_price)

#j)Converting into data frame
cost_data = data.frame(total_cost)

#k) Sex vector
Sex = cost_data$Gender
for (i in 1:length(Sex)) {
  if(Sex[i]==1){
    Sex[i] = "Female"
  }else{
    Sex[i] = "Male"
  }
}
Sex = as.factor(Sex)

#l)Ethnicity vector
Ethnicity = cost_data$Race
for (i in 1:length(Ethnicity)) {
  if(Ethnicity[i]==1){
    Ethnicity[i] = "Whites"
  }else if(Ethnicity[i]==3){
    Ethnicity[i] = "AAs"
  }else if(Ethnicity[i]==5){
    Ethnicity[i] = "Others"
  }
}
Ethnicity = as.factor(Ethnicity)

#m) Price Range
tmp = cost_data$Total_price
Price_range = c()
for (i in 1:length(tmp)) {
  if(tmp[i]<300){
    Price_range[i] = "Low"
  }else if(tmp[i]<600){
    Price_range[i] = "Affordable"
  }else{
    Price_range[i] = "High"
  }
}
Price_range=as.factor(Price_range)

#n) Pr_range
tmp = cost_data$Total_price
Pr_range = c()
for (i in 1:length(tmp)) {
  if(tmp[i]<300){
    Pr_range[i] = "Low"
  }else if(tmp[i]<600){
    Pr_range[i] = "Affordable"
  }else{
    Pr_range[i] = "High"
  }
}
Pr_range=as.factor(Pr_range)

#o)Final_cost
final_cost = data.frame(cost_data,Sex,Ethnicity,Price_range) 

#p)Removing the columns
final_cost = final_cost[,c(-2,-3)]

#q)count_male and count_whites
count_male = table(final_cost$Sex)[2]
names(count_male) = NULL
count_whites = table(final_cost$Ethnicity)[3]
names(count_whites) = NULL

#r)Exporting to csv
write.csv(final_cost,"expt_cost.csv")

###########Q2#################
#a) reading the dataset
cars = read.delim("Cars.txt")

#b)num_cars
num_cars = cars[,c(-1,-3)]

#c)Averages of columns
sapply(num_cars,mean)

#d)Average MSRP by car type
sapply(split(cars[2],cars$Make),function(t) mean(t$MSRP))

#e) Acura
Acura = cars$MSRP[cars$Make=="Acura"]

#f) Scatter plot
plot(cars$MPG_Highway,cars$MSRP,xlab = "Highway Mileage",ylab = "MSRP ($)",main = "MSRP VS Highway Mileage",col = "blue")

#h) mileage_line
mileage_line = 95000 - (2300*cars$MPG_Highway)
plot(cars$MPG_Highway,cars$MSRP,xlab = "Highway Mileage",ylab = "MSRP ($)",main = "MSRP VS Highway Mileage",col="blue")+lines(cars$MPG_Highway,mileage_line,type = "l",col="red")

#i)sumf function
sumf = function(x){
  if(length(x)<3|!is.numeric(x)){
    return("Non-numeric or length is less than 3")
  }else{
    return(sum(x[1:3]))
  }
}
a1 = c(1:4)
a2 = c("A","B","C")
a3 = c(1,2)
sumf(a1)
sumf(a2)
sumf(a3)
