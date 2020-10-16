
---
title: " Customer intelligence report"
author: "Felipe Gonzalez. ID 11246645"
output:
  html_document: default
  pdf_document: default
---
### Exploratory analysis
To explore a dataset and be able to obtain powerful insights from the data it is important to understand the meaning of the variables and handle missing values. The following attributes were validated in a first stage before proceding to the analysis: 1) In the variables nbr is over data/voice 0 represent people that simply have not gotten overvoice/overdata, thus NAs in the variables, time since last overvoice/overdata were replaced by 117 which is the month in which the variables were collected. 2) By looking at the frequency chart of the variables Cash down, Phone price and Phone balance (positive values), the following logic baceme clear: People who bring their phones do not pay for anything and therefore have 0 in all three variables. People who buy their phones pay the full price at once and therefore have 0 in Cash down and Phone balance. Lastly, people that rent have a Phone Price but also a balance that remains to be paid and therefore they have a value in Cash down, Phone price and Phone balance. Then, NAs were replaced by accordingly.
```{r setup, include=FALSE}
#library(tidyverse,verbose=FALSE)
library("knitr")
library("dplyr")
knitr::opts_chunk$set(echo = TRUE)
library("markdown",verbose=FALSE)
library("rmarkdown",verbose=FALSE)
options(tinytex.verbose = TRUE)
library(ggplot2)
```

```{r,include=FALSE}
# Reading the files and exploring the data
setwd("C:/Users/admin/Dropbox/Statistical Learning/Assignment 3")
retentiontrain=read.csv("Retention-train_fixed.csv")
summary(retentiontrain)
```

```{r,include=FALSE}
clients_CDR=retentiontrain %>%
  filter(planType=="rent" & (cashDown>0)) %>%
  nrow() 
clients_PPR=retentiontrain %>%
  filter(planType=="rent"& (phonePrice>0)) %>%
  nrow() 
clients_PBR=retentiontrain %>%
  filter(planType=="rent"& (phoneBalance>0)) %>%
  nrow() 

clients_CDB=retentiontrain %>%
  filter(planType=="bring" & (cashDown>0)) %>%
  nrow() 
clients_PPB=retentiontrain %>%
  filter(planType=="bring"& (phonePrice>0)) %>%
  nrow() 
clients_PBB=retentiontrain %>%
  filter(planType=="bring"& (phoneBalance>0)) %>%
  nrow() 

clients_CDBu=retentiontrain %>%
  filter(planType=="buy" & (cashDown>0)) %>%
  nrow() 
clients_PPBu=retentiontrain %>%
  filter(planType=="buy"& (phonePrice>0)) %>%
  nrow() 
clients_PBBu=retentiontrain %>%
  filter(planType=="buy"& (phoneBalance>0)) %>%
  nrow() 

chart_plan <- matrix(c(clients_PPB,clients_PPBu,clients_PPR,clients_CDB,clients_CDBu,clients_CDR,clients_PBB,clients_PBBu,clients_PBR),ncol=3,byrow=TRUE)
colnames(chart_plan) <- c("Bring","Buy","Rent")
rownames(chart_plan) <- c("Phone price","Cash down","Phone balance")
chart_plan <- as.table(chart_plan)
chart_plan
```

```{r ,include=FALSE}
#Replace NAs in phone price, phone balance, time since last complaints and time since last technical problems with 0

retentiontrain$phoneBalance[is.na(retentiontrain$phoneBalance)] <- 0
retentiontrain$phonePrice[is.na(retentiontrain$phonePrice)] <- 0
retentiontrain$cashDown[is.na(retentiontrain$cashDown)] <- 0
retentiontrain$timeSinceLastComplaints[is.na(retentiontrain$timeSinceLastComplaints)] <- 117
retentiontrain$timeSinceLastTechProb[is.na(retentiontrain$timeSinceLastTechProb)] <- 117
retentiontrain$timeSinceLastIsOverData[is.na(retentiontrain$timeSinceLastIsOverData)] <- 117
retentiontrain$timeSinceLastIsOverVoice[is.na(retentiontrain$timeSinceLastIsOverVoice)] <- 117
retentiontrain$minutesVoice[is.na(retentiontrain$minutesVoice)] <- 0
```

```{r,include=FALSE}
# Are there are missing values on this data set 
retentiontrain=retentiontrain %>%
  select(-unlimitedText)

apply(is.na(retentiontrain),2,sum)
```
### Methodology for client selection
Client targeting is useful for many purposes such as marketing campaigns, promotions, discounts or client loyalty campaigns. The purpose of this analysis is to maximize the result of any such initiative by making sure we follow some fundamental principles: i) Don't waste efforts on clients that are not likely to leave, ii) Don't waste efforts on clients that are a lost cause iii) Avoid disturbing clients that don't appreciate being included in this type of campaigns (sleeping dogs).  In this case we used data from a previous (Pilot) where randomly selected clients were invited with their families to a dinner. For this purpose, two separate datasets were created for clients belonging or not to that Pilot sample. Following this, uplift modeling was performed. In that sense, the best performing model was chosen for each scenario. Then I computed the *uplift* : P. churn (if not invited) -  P. churn (if invited). After this we want the clients with highest uplift as they are more likely to live company if not invited. However, we want to prioritize clients more valuable for the company. Following that logic, an expected revenue was calculated as :*uplift***monthly rate for plan* and families with the top expected revenue were invited.

#### Model selection and evaluation criteria
An additional step before fitting the models was rebalancing the data to make the churn probability was similar for the Pilot and No Pilot datasets. In practice, this was achieved by undersampling over the Pilot set; since the Pilot set was much larger than the No Pilot set, 8% of the data where Churnin3month =1 was dismissed. This balanced dataset allowed a much better performance of the models and of the uplift calculation in general. To develope the models, the data set was randomly splited between traiining and validation (75% and 25% respectively).  

The evaluation criteria was the area under the ROC curve, AUC, of the models in the training set. Since the AUC of the models was similar, the best models were re-run in the entire training set and the selection criteria was the Net profit. The best combination turned out to be logistic regression with interactions and cuadratic powers of certain variables for both cases (Pilot and No Pilot). 

```{r, include=FALSE}
# Set of clients out of the pilot
retentiontrainnp=retentiontrain %>%
  filter(promo==0)

# Delete random rows where churn is equal to 1
totalchurn = sum(retentiontrainnp$churnIn3Month==1)

(churnindex <- which(retentiontrainnp$churnIn3Month == 1))
(deleteindex <- sample(churnindex, length(churnindex) - totalchurn*0.92)) 
retentiontrainnp = retentiontrainnp[-deleteindex, ]

```

```{r, ,include=FALSE}
#  Set of clients in the pilot
retentiontrainpilot=retentiontrain %>%
  filter(promo==1)
summary(retentiontrainpilot)
```

```{r, ,include=FALSE}
#Training and validation sets from no pilot
set.seed(20606)
trainingIDNP=sample(1:nrow(retentiontrainnp),nrow(retentiontrainnp)*0.7)
trainingNP=retentiontrainnp[trainingIDNP,]
validationNP=retentiontrainnp[-trainingIDNP,]
```

```{r ,include=FALSE}
#Training and validation sets from pilot
set.seed(20606)
trainingIDP=sample(1:nrow(retentiontrainpilot),nrow(retentiontrainpilot)*0.7)
trainingPILOT=retentiontrainpilot[trainingIDP,]
validationPILOT=retentiontrainpilot[-trainingIDP,]
```

```{r,include=FALSE}
#Compares mean churn in pilot and no pilot
churnNP = mean(trainingNP$churnIn3Month)
churnPILOT = mean(trainingPILOT$churnIn3Month)
```

```{r,include=FALSE}
# Who are the people that buy
meanAge_buy = mean(retentiontrain$age[retentiontrain$planType=="buy"])
meanLife_buy = mean(retentiontrain$lifeTime[retentiontrain$planType=="buy"])
meanRate_buy = mean(retentiontrain$baseMonthlyRateForPlan[retentiontrain$planType=="buy"])
meanWork_buy = mean(retentiontrain$isWorkPhone[retentiontrain$planType=="buy"])
churn_buy = mean(retentiontrain$churnIn3Month[retentiontrain$planType=="buy"])
adultAvg_buy = mean(retentiontrain$nbAdultAvg[retentiontrain$planType=="buy"])

table1 <- table(retentiontrain$gender[retentiontrain$planType=="buy"])
prop.table(table1)

```

```{r,include=FALSE}
# Who are the people that rent
meanAge_rent = mean(retentiontrain$age[retentiontrain$planType=="rent"])
meanLife_rent = mean(retentiontrain$lifeTime[retentiontrain$planType=="rent"])
meanRate_rent = mean(retentiontrain$baseMonthlyRateForPlan[retentiontrain$planType=="rent"])
meanWork_rent = mean(retentiontrain$isWorkPhone[retentiontrain$planType=="rent"])
churn_rent = mean(retentiontrain$churnIn3Month[retentiontrain$planType=="rent"])
adultAvg_rent = mean(retentiontrain$nbAdultAvg[retentiontrain$planType=="rent"])

table2 <- table(retentiontrain$gender[retentiontrain$planType=="rent"])
prop.table(table2)

```

```{r,include=FALSE}
# Who are the people that bring
meanAge_bring = mean(retentiontrain$age[retentiontrain$planType=="bring"])
meanLife_bring = mean(retentiontrain$lifeTime[retentiontrain$planType=="bring"])
meanRate_bring = mean(retentiontrain$baseMonthlyRateForPlan[retentiontrain$planType=="bring"])
meanWork_bring = mean(retentiontrain$isWorkPhone[retentiontrain$planType=="bring"])
churn_bring = mean(retentiontrain$churnIn3Month[retentiontrain$planType=="bring"])
adultAvg_bring = mean(retentiontrain$nbAdultAvg[retentiontrain$planType=="bring"])

table3 <- table(retentiontrain$gender[retentiontrain$planType=="bring"])
gender_bring = prop.table(table3)

```
### Understanding types of clients by plan
The designation of plans for mobile service is one of the most powerful tools that cell phone providers have to engage clients and differenciate their offer with respect to oher market participants. The features offered on the plans are some of the most heavily weighted aspects by clients and thus mobile providers draw their lineup of plans very carefully to be relevant to their client base. With that in mind, it is of great importance to know the type of client that is engaged under each plan type.
```{r,include=FALSE}
# Chart with added parameters
plan = data.frame(Plan_type= c("Bring","Buy","Rent"))
age = data.frame(Age=c(meanAge_bring,meanAge_buy,meanAge_rent))
life = data.frame(Lifetime=c(meanLife_bring,meanLife_buy,meanLife_rent))
rate = data.frame(Monthly_rate=c(meanRate_bring,meanRate_buy,meanRate_rent))
work = data.frame(IsWork=c(meanWork_bring,meanWork_buy,meanWork_rent))
churn = data.frame(Churn=c(churn_bring,churn_buy,churn_rent))
genderF = data.frame(GenderF=c(prop.table(table1)[1],prop.table(table2)[1],prop.table(table3)[1]))
genderM = data.frame(GenderM=c(prop.table(table1)[2],prop.table(table2)[2],prop.table(table3)[2]))
nbavg = data.frame(nbavg=c(adultAvg_bring,adultAvg_buy,adultAvg_rent))

statsby_plan <- cbind(plan,age,life,rate,work,churn,genderF,genderM,nbavg)

# Demographic variables
demo <- matrix(c(meanAge_bring,meanAge_buy,meanAge_rent,prop.table(table1)[1],prop.table(table2)[1],prop.table(table3)[1],adultAvg_bring,adultAvg_buy,adultAvg_rent),ncol=3,byrow=TRUE)
colnames(demo) <- c("Bring","Buy","Rent")
rownames(demo) <- c("Age","Females %","Avg. num of adults")
demo <- as.table(demo)
demo

```
  
```{r,include=TRUE, echo=FALSE}
# Segmentation variables  
# create a dataset

avg_variable <- c( rep("Life time" , 3) , rep("Churn %" , 3),rep("Is work phone %",3) )
condition <- rep(c("Bring" , "Buy" , "Rent") , 3)
value <- c(statsby_plan$Lifetime,(statsby_plan$Churn)*100,(statsby_plan$IsWork)*100)
data <- data.frame(avg_variable,condition,value)
 
# Table
chart_var <- matrix(c(churn_bring*100,churn_buy*100,churn_rent*100,meanWork_bring*100,meanWork_buy*100,meanWork_rent*100,meanLife_bring,meanLife_buy,meanLife_rent),ncol=3,byrow=TRUE)
colnames(chart_var) <- c("Bring","Buy","Rent")
rownames(chart_var) <- c("Churn %","Is work phone%","Life time (months)")
chart_var <- as.table(chart_var)
chart_var
```
To start the analysis with some interesting variables, we can see how the clientele of each type of plan takes different shapes: 1) We can infer that clients under a Bring contract are more likely to leave and in fact remain less time since they don't have any contract. However, since their monthly fees (47.7) are comparable to those of the Buy(48.6) and Rent(49.7), Telcel could try to engage those clients to buy or rent newest phones with the objective of keeping them in the company for a longer time. 2) By looking into the work phone variable we can see that the business clients are mostly attracted in renting, perhaps because they like having edge technology phones while financing their purchases. Expanding into corporate clients seems like a good idea since the rent plan has the highest average revenue, and extended Life time and a low churn rate.

```{r,include=FALSE}
# People that (H= bring)

Highrev = retentiontrain %>%
  filter(planType=="bring")
mean_dataH = mean(Highrev$data) 
mean_dataconsumptionH = mean(Highrev$dataAvgConsumption)
mean_unlvoiceH = mean(Highrev$unlimitedVoice) 
mean_voiceH = mean(Highrev$minutesVoice[Highrev$unlimitedVoice==0])
mean_nboverdataH = mean(Highrev$nbrIsOverData)
mean_lifeH = mean(Highrev$lifeTime)
mean_techprH = mean(Highrev$nbrTechnicalProblems)
Num_timetechprH3m = nrow(Highrev[Highrev$timeSinceLastTechProb >= 117,])
techpr_3m = Num_timetechprH3m/nrow(Highrev)
Num_complH3m = nrow(Highrev[Highrev$timeSinceLastComplaints >= 117,])
compl_3m = Num_complH3m/nrow(Highrev)

mean_rentH = nrow(Highrev[Highrev$planType == "rent",])/nrow(Highrev)
mean_buyH = nrow(Highrev[Highrev$planType == "buy",])/nrow(Highrev)
mean_bringH = nrow(Highrev[Highrev$planType == "bring",])/nrow(Highrev)

```

```{r,include=FALSE}
# People that (M=buy)
Medrev = retentiontrain %>%
  filter(planType=="buy")
mean_dataM = mean(Medrev$data) 
mean_dataconsumptionM = mean(Medrev$dataAvgConsumption)
mean_unlvoiceM = mean(Medrev$unlimitedVoice) 
mean_voiceM = mean(Medrev$minutesVoice[Medrev$unlimitedVoice==0])
mean_nboverdataM = mean(Medrev$nbrIsOverData)
mean_lifeM = mean(Medrev$lifeTime)
mean_techprM = mean(Medrev$nbrTechnicalProblems)
Num_timetechprM3m = nrow(Medrev[Medrev$timeSinceLastTechProb >= 117,])
techpr_3mM = Num_timetechprM3m/nrow(Medrev)
Num_complM3m = nrow(Medrev[Medrev$timeSinceLastComplaints >= 117,])
compl_3mM = Num_complM3m/nrow(Medrev)

```

### High value and Low value segments
As the company must prioritize their efforts to improve their offer, it is important to understand what type of services clients consume and in what areas they have service issues. For this, clients were classified according to their plan type.
```{r,include=TRUE, echo=FALSE}
# Who are the people that (L=rent)

Lowrev = retentiontrain %>%
  filter(planType=="rent")
mean_dataL = mean(Lowrev$data) 
mean_dataconsumptionL = mean(Lowrev$dataAvgConsumption)
mean_unlvoiceL = mean(Lowrev$unlimitedVoice) 
mean_voiceL = mean(Lowrev$minutesVoice[Lowrev$unlimitedVoice==0])
mean_nbovervoiceL = mean(Lowrev$nbrIsOverVoice)
mean_nboverdataL = mean(Lowrev$nbrIsOverData)
mean_lifeL = mean(Lowrev$lifeTime)
Num_timetechprL3m = nrow(Highrev[Highrev$timeSinceLastTechProb >= 117,])
techpr_3mL = Num_timetechprL3m/nrow(Lowrev)
Num_complL3m = nrow(Lowrev[Lowrev$timeSinceLastComplaints >= 117,])
compl_3mL = Num_complL3m/nrow(Lowrev)

mean_rentL = nrow(Lowrev[Lowrev$planType == "rent",])/nrow(Lowrev)
mean_buyL = nrow(Lowrev[Lowrev$planType == "buy",])/nrow(Lowrev)
mean_bringL = nrow(Lowrev[Lowrev$planType == "bring",])/nrow(Lowrev)

# Revenue variables
rev_chart <- matrix(c(mean_dataH,mean_dataconsumptionH,techpr_3m,compl_3m,mean_dataM,mean_dataconsumptionM,techpr_3mM,compl_3mM,mean_dataL,mean_dataconsumptionL,techpr_3mL,compl_3mL),ncol=4,byrow=TRUE)
colnames(rev_chart) <- c("Data","Data_consumption","Prop._tech_problems_3M","Prop._comp_3M")
rownames(rev_chart) <- c("Bring","Buy","Rent")
rev_chart <- as.table(rev_chart)
rev_chart
```
This chart includes variables regarding voice and data consumption as well as service related variables. The 5th and 6th column refers to the proportion of clients in each category that experienced technical problems and filed complaints during the last 3 months. In general, we can see that in all categories clients have more resources than what they actually consume, especially data, and therefore it doesn't seem like they're looking to expand their offer in any segment. On the other hand, the service experience seems to be a bit troublesome especially for the Bring and Buy categories perhaps because cell phones might be older and less reliable. Facilitating access to newer phones could be an alternative way to reduce technical related issues.

```{r,include=FALSE}
# Scaling example
set.seed(1)
X <- runif(7)

X.man.scaled <- (X-mean(X))/sd(X)
X.aut.scaled <- scale(X, center = T)

```

```{r,include=FALSE}
#Create matrix with numeric variables
distance_mat = retentiontrainpilot %>%
  select(dataAvgConsumption,voiceAvgConsumption,textoAvgConsumption,data)

deleteindex2 <- sample(nrow(distance_mat), 136647)
distance_mat = distance_mat[-deleteindex2, ]
distance_mat = scale(distance_mat, center = TRUE, scale = TRUE)

#--------------------------------------------------------------------------
distance_mat2 = retentiontrainnp %>%
  select(dataAvgConsumption,voiceAvgConsumption,textoAvgConsumption,data)

deleteindex3 <- sample(nrow(distance_mat2), 603510)
distance_mat2 = distance_mat2[-deleteindex3, ]
distance_mat2 = scale(distance_mat2, center = TRUE, scale = TRUE)

```

```{r,include=FALSE}
#Hierarchical clustering
#clientclusH=hclust(dist(distance_mat),method="ward.D2")
#plot(clientclusH)
```

```{r,include=FALSE}
# Elbow method
library(factoextra)
library(NbClust)

#fviz_nbclust(distance_mat2, kmeans, method = "wss") +
#    geom_vline(xintercept = 6, linetype = 2)+
#  labs(subtitle = "Elbow method")
```
### Clustering
An interesting way to cluster the clients for a mobile service provider is according to the phone usage of its clients. With that purpose, the K-means algorithm was used to perform clustering over the variables: Data, Data avg consumption, Voice avg consumption and texto avg consumption. The idea is to get a sense of who the clients are in terms of how and why they use their mobile phones. The number of clusters used was 6; this number was determined by using the elbow mehod and the hierarchical clustering over two different random samples of 10,000 clients. The elbow mwethod gives the total within-cluster sum of square (WSS). As the number of clusters gets larger the WSS gets reduced, however, after 6 clusters the WSS starts to flatten down. In the hierarchical clustering we see a large enough space between clusters for 6 clusters, which means is a reasonable choice.

```{r,include=FALSE}
#K means algorithm
distance_mat3 = retentiontrain %>%
  select(data,dataAvgConsumption,voiceAvgConsumption,textoAvgConsumption)

distance_mat3 = scale(distance_mat3, center = TRUE, scale = TRUE)

set.seed(2020)
clientclusNOH=kmeans(distance_mat3,6)
print(clientclusNOH)

initial = clientclusNOH$centers
```

```{r, include=TRUE, echo=FALSE}
# output of the 6 clusters
initial[,1]= (clientclusNOH$centers[,1]*sd(retentiontrain$data))+(mean(retentiontrain$data))
initial[,2]= (clientclusNOH$centers[,2]*sd(retentiontrain$dataAvgConsumption))+(mean(retentiontrain$dataAvgConsumption))
initial[,3]= (clientclusNOH$centers[,3]*sd(retentiontrain$voiceAvgConsumption))+(mean(retentiontrain$voiceAvgConsumption))
initial[,4]= (clientclusNOH$centers[,4]*sd(retentiontrain$textoAvgConsumption))+(mean(retentiontrain$textoAvgConsumption))

initial
```
Average values of the entire dataset
```{r, include=TRUE, echo=FALSE}
# Average of the attributes
Averages = retentiontrain %>%
  select(data,dataAvgConsumption,voiceAvgConsumption,textoAvgConsumption)

apply(Averages,2,mean)
```

According to what the values tell, the following names were asigned to each of the clusters so that their interpretation becomes more meaningful: 1) High productivity, 2) Massive speaker ,3) Sporadic user, 4) Social media geek, 5) Average guy 6) Classic outsider. Based on these labels we can explore further the categories: 
```{r,include=TRUE, echo=FALSE}
# Evaluationg clusters in terms of demographic variables
cluster_N = as.numeric(clientclusNOH$cluster)
woman = ifelse(retentiontrain$gender=="F",1,0)

cbind(retentiontrain,cluster_N,woman) %>%
  select(cluster_N,age,woman,lifeTime,nbAdultAvg) %>%
  group_by(cluster_N) %>%
  summarise_all(mean)
```
In terms of sex and number of adults in the family all clusters are fairly similar. Regarding the age, the sporadic users and massive speaker have a higher age, which in part explains why the first group is not a big fan of phone usage and why the second group prefers to have voice conversations to texting. Interestingly, high productivity people, social media geeks and average guys tend to stay longer at the company, perhaps because they have active contracts.
```{r,include=TRUE, echo=FALSE}
# Evaluationg clusters in terms of phone usage
cbind(retentiontrain,cluster_N) %>%
  select(cluster_N,phonePrice,isWorkPhone,baseMonthlyRateForPlan,phoneBalance) %>%
  group_by(cluster_N) %>%
  summarise_all(mean)

```
When looking into variables related to the spending, social media geeks and high productivity people have the more expensive phones and this is a key feature to engage them. They also have the highest Phone Balance wich yields additional revenue to the company in terms of interest.Since they have the highest percentage of work usage, possible campaings directed at companies is a good way to target these segments which are the most profitable based on the monthly rates and phone balances.

```{r,include=TRUE, echo=FALSE}
# Evaluationg clusters in terms of phone usage
cbind(retentiontrain,cluster_N) %>%
  select(cluster_N,churnIn3Month) %>%
  group_by(cluster_N) %>%
  summarise_all(mean)

```
As a final cluster analysis, we can see that Classic outsiders, Sporadic users and Massive speakers have larger churn rates. The company could think about creating plans more suited for these categories and that match their expectations better, perhaps less GB of data with a more competitive price point. Regarding Social media geeks, High productivity and Average guys churn rate are lower although not quite low in any case, and thus, reinforce the previous point that service and quality related issues might be driving this indicator up.

```{r, include=FALSE,eval=FALSE}
render("Inteligence.Rmd","pdf_document")
```
