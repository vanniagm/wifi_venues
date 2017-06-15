# Reading dataset
setwd("/home/vanniagm/drop/Dropbox/DataScience/Klustera/")
df_e<-read.csv("data/e.csv",header = TRUE)

#Libraries
library(lubridate)
library(plyr)
library(reshape2)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)
library(scales)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Let's look at the frequency of visitor and non visitors
names(df_e)
table(df_e$visitor)
#Per branch
table(df_e$visitor,df_e$branch_office)

#Mean, median, max of session time for visitors and non visitors

simplestats<-function(x){
        c(mean(x),median(x),max(x))}
#Non-visitors
simplestats(df_e$tiempodeses[df_e$visitor=="false"])

#Visitors
simplestats(df_e$tiempodeses[df_e$visitor=="true"])

plot(df_e$visitor,df_e$tiempodeses)

#NA's or Null's
checknanull<-function(x){c(sum(is.na(x)),sum(is.null(x)))}
apply(df_e,2,checknanull)

# New date column
df_e$Date<-as.POSIXct(
        paste(
                paste(df_e$month_tz,df_e$day_tz,"2016",sep="/"),df_e$hour_tz,sep=" "
        ),
        format="%m/%d/%Y %H",tz="CST"
        )

#Cooroborate that the year is 2016 since this was not detailed
df_e$Wkd<-weekdays(df_e$Date)
df_e$day_of_week_tz<-as.character(df_e$day_of_week_tz)
sum(!(df_e$day_of_week_tz==df_e$Wkd))
# 0 

# Visitors per weekday
table(df_e$visitor,weekdays(df_e$Date))

table(df_e$visitor[df_e$branch_office==1],weekdays(df_e$Date[df_e$branch_office==1]))

table(as.Date(df_e$Date[df_e$branch_office==1]),df_e$visitor[df_e$branch_office==1])


gral_theme <- function(size=12,family='sans'){
        theme_minimal(base_size =size, base_family = family) +
                theme(
                        axis.text = element_text(size = 10),
                        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
                        axis.title = element_text(size = 12),
                        panel.grid.major = element_line(color = "lightgrey"),
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "aliceblue"),
                        strip.background = element_rect(fill = "lightgrey", color = "lightgrey"),
                        strip.text = element_text(face = "bold", size = 12, color = "black"),
                        legend.position = "bottom",
                        legend.justification = "top",
                        legend.box = "horizontal",
                        legend.background = element_blank(),
                        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
                )}



#Peak day
g4<-ggplot(data=df_e,aes(x=as.factor(as.Date(Date)),fill=visitor))+geom_bar(position='dodge')
g4+
        facet_grid(branch_office~.)+
        gral_theme()+theme(axis.text.x = element_text( angle=90))+
        scale_fill_brewer(palette="Set1")+
        labs(x="Date",y="Number of devices",title="Klustera Venues Wifi-Data",subtitle="Visitors and non visitors per day")

#Since the 2nd and 3th branches have no data before the 2nd of Nov, I am going to subset the data
#starting that day

#Subset of data
dfe_sub<-df_e[df_e$Date>="2016-11-02",]


# Peak day of the week
g3<-ggplot(data=dfe_sub,aes(x=branch_office,fill=visitor))+geom_bar(position='dodge')
g3+
        facet_grid(.~weekdays(dfe_sub$Date))+
        labs(x='Branches',y='Number of devices',fill='Visitor')+
        gral_theme()


#peak hour accumulative
g5<-ggplot(data=dfe_sub,aes(x=hour(Date),fill=visitor))+geom_bar(position='dodge')
g5+facet_grid(branch_office~.)+
        labs(x='Hour',y='Number of devices',fill='Visitor')+
        gral_theme()

#Absolute peak hour

df_hour<-dfe_sub %>% group_by(Date,visitor,branch_office)%>% summarise(count=n())
max(df_hour$count)
df_hour$hour<-hour(df_hour$Date)
sum(df_hour$count[df_hour$hour==15 & df_hour$visitor=='true' & df_hour$branch_office==1])
#as dataframe
df_agg<-aggregate(dfe_sub,by=list(dfe_sub$Date,dfe_sub$branch_office,dfe_sub$visitor),length)[1:4]
df_agg$hour<-hour(df_agg$Group.1)
#Mean of reported devices by hour
df_meanh<-aggregate(df_agg$X,by=list(df_agg$hour,df_agg$Group.2,df_agg$Group.3),mean)
#df_meanh<-aggregate(X~hour+Group.2+Group.3,df_agg,mean)
df_meanh$branches<-mapvalues(df_meanh$Group.2,from=c(1,2,3),to=c("Branch_1","Branch_2","Branch_3"))
g6<-ggplot(df_meanh,aes(x=Group.1,y=x,fill=Group.3))
g6+
        geom_bar(position='dodge',stat="identity")+
        facet_grid(branches~.)+
        labs(x='Hour',y='Mean number of devices',fill="Visitor")+
        gral_theme()


# Devices by hour and weekday (accumulative devices and mean num. of devices)
g5<-ggplot(data=dfe_sub,aes(x=as.factor(hour(Date)),fill=visitor))+geom_bar(alpha=.5)
g5+facet_grid(weekdays(Date)~branch_office)+
        labs(x='Hour',y='Number of devices',fill='Visitor')+
        gral_theme()


df_agg<-aggregate(dfe_sub,by=list(dfe_sub$Date,dfe_sub$branch_office,dfe_sub$visitor,dfe_sub$day_of_week_tz),length)[1:5]
df_agg$hour<-hour(df_agg$Group.1)
#Mean of reported devices by hour
df_meanh<-aggregate(df_agg$X,by=list(df_agg$hour,df_agg$Group.2,df_agg$Group.3,df_agg$Group.4),mean)
#df_meanh<-aggregate(X~hour+Group.2+Group.3,df_agg,mean)
df_meanh$branches<-mapvalues(df_meanh$Group.2,from=c(1,2,3),to=c("Branch_1","Branch_2","Branch_3"))

g5<-ggplot(data=df_meanh,aes(x=as.factor(Group.1),y=x,fill=Group.3))+geom_bar(stat='identity',alpha=.5)
g5+facet_grid(Group.4~Group.2)+
        labs(x='Hour',y='Mean number of devices',fill='Visitor',title='Mean number of devices by hour')+
        gral_theme()


#### Session times

#Session time by hour of day
df_ses<-aggregate(tiempodeses~hour(Date)+visitor+branch_office,dfe_sub,mean)
dfe_sub$tiemlog0<-mapvalues(dfe_sub$tiempodeses,from=0,to=1)
g<-ggplot(
        data=dfe_sub,
        aes(x=as.factor(hour(Date)),y=log10(as.numeric(tiemlog0)),fill=visitor,col=visitor))
g+
        geom_boxplot(alpha=.5)+
        facet_grid(branch_office~.)+
        labs(x='Hour',y='Session time (log10)',fill='Visitor')+
        gral_theme()

#Session time by day of observation
dfe_sub$tiemlog0<-mapvalues(dfe_sub$tiempodeses,from=0,to=1)
g<-ggplot(
        data=dfe_sub,
        aes(x=as.factor(as.Date(Date)),y=log10(as.numeric(tiemlog0)),fill=visitor,col=visitor))
g+
        geom_boxplot(alpha=.5)+
        scale_fill_brewer()+
        facet_grid(branch_office~.)+
        labs(x='Date',y='Session time (log10)',fill='Visitor',col='')+
        gral_theme()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

#Session time by day of the week
dfe_sub$tiemlog0<-mapvalues(dfe_sub$tiempodeses,from=0,to=1)
g<-ggplot(
        data=dfe_sub,
        aes(x=weekdays(Date),y=log10(as.numeric(tiemlog0)),fill=visitor,col=visitor))
g+
        geom_boxplot(alpha=.5)+
        scale_fill_brewer()+
        facet_grid(branch_office~.)+
        labs(x='Day of the week',y='Session time (log10)',fill='Visitor',col='')+
        gral_theme()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


#Session time by day of the week and hour
g<-ggplot(
        data=dfe_sub,
        aes(x=as.factor(hour(Date)),y=log10(as.numeric(tiemlog0)),fill=visitor,col=visitor))
g+
        geom_boxplot(alpha=.5)+
        scale_fill_brewer()+
        facet_grid(weekdays(Date)~branch_office)+
        labs(x='Hour',y='Session time (log10)',fill='Visitor',col='')+
        gral_theme()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

###### Slight difference

###### NOTE: WHY IS THERE A CONSISTENT PEAK IN THE EARLY MORNIG??????? 

#Session time by hour and month for branch office 1
df_e$tiemlog0<-mapvalues(df_e$tiempodeses,from=0,to=1)
g<-ggplot(
        data=df_e[df_e$branch_office==1,],
        aes(x=as.factor(hour(Date)),y=log10(as.numeric(tiemlog0)),fill=visitor,col=visitor))
g+
        geom_boxplot(alpha=.5)+
        scale_fill_brewer()+
        facet_grid(branch_office~month(Date))+
        labs(x='Hour',y='Session time (log10)',fill='Visitor',col='')+
        gral_theme()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
# No difference

################################
################################
### Test data
df_v<-read.csv("data/v.csv",header = TRUE)

df_v$Date<-as.POSIXct(
        paste(
                paste(df_v$month_tz,df_v$day_tz,"2016",sep="/"),df_v$hour_tz,sep=" "
        ),
        format="%m/%d/%Y %H",tz="CST"
)
g4<-ggplot(data=df_v,aes(x=as.factor(as.Date(Date))))+geom_bar(position='dodge')
g4+facet_grid(branch_office~.)+gral_theme()+theme(axis.text.x = element_text( angle=90))

#Session times by hour and weekday
#map cero values to 1 for log plot
df_v$seslog0<-mapvalues(df_v$tiempodeses,from=0,to=1)
gses<-ggplot(
        data=df_v,
        aes(x=as.factor(hour(Date)),y=log10(as.numeric(seslog0))))
gses+
        geom_boxplot(alpha=.5)+
        scale_fill_brewer()+
        facet_grid(weekdays(Date)~branch_office)+
        labs(x='Hour',y='Session time (log10)')+
        gral_theme()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

# Devices by hour and weekday (accumulative devices and mean num. of devices)
g5<-ggplot(data=df_v,aes(x=as.factor(hour(Date))))+geom_bar(alpha=.5)
g5+facet_grid(weekdays(Date)~branch_office)+
        labs(x='Hour',y='Number of devices')+
        gral_theme()


df_agg<-aggregate(df_v,by=list(df_v$Date,df_v$branch_office,df_v$day_of_week_tz),length)[1:4]
df_agg$hour<-hour(df_agg$Group.1)
#Mean of reported devices by hour
df_meanh<-aggregate(df_agg$X,by=list(df_agg$hour,df_agg$Group.2,df_agg$Group.3),mean)
#df_meanh<-aggregate(X~hour+Group.2+Group.3,df_agg,mean)
df_meanh$branches<-mapvalues(df_meanh$Group.2,from=c(1,2,3),to=c("Branch_1","Branch_2","Branch_3"))

g5<-ggplot(data=df_meanh,aes(x=as.factor(Group.1),y=x))+geom_bar(stat='identity',alpha=.5)
g5+facet_grid(Group.3~Group.2)+
        labs(x='Hour',y='Mean number of devices Test',title='Mean number of devices by hour')+
        gral_theme()


### Features
# The are not many features so I will create new variables:

# 1. There may be users that are frequent clients, I will create a frequent variable for users
# or devices that appear 4 or more times on the period the data was collected ~ 1 month (for different days on the total period of time).
# 2. I will convert Weekdays to binaries
# 3. I will convert branch_office to binaries
# 4. I will leave hour as categorical


# I will use the categorical variables: Weekdays, branch_offices and hour of the day 


# Frequent users
df_eDays<-aggregate(df_e$X,by=list(df_e$device_mac,as.Date(df_e$Date)),length)
df_eDays<-aggregate(Group.2~Group.1,df_eDays,length)
names(df_eDays)<-c("dev","days")
max(df_eDays$days)
length(df_eDays$dev[df_eDays$days==2])
length(df_eDays$dev[df_eDays$days==1]) #100975
summary(df_eDays$days) #median is ~ 1.5
ggplot(df_eDays,aes(x=days))+geom_bar()
# So around 80 % of the devices are visitors/non-visitors for only one day
# I will asume that any user (device) that is a visitor/non-visitor for 4 or more days is frequent (in a 32 day period)
# Let's see if frequent users are visitors or non visitors
# For this I subset the data with those frequent users (devices)
df_freq<-df_eDays$dev[df_eDays$days>3] #length=6468 
df_efreq<-df_e[df_e$device_mac %in% df_freq,] # frequent users
df_enofreq<-df_e[!df_e$device_mac %in% df_freq,] #not frequent

g4<-ggplot(data=df_efreq,aes(x=as.factor(as.Date(Date)),fill=visitor))+geom_bar(position='dodge')
g4+
        facet_grid(branch_office~.)+
        gral_theme()+
        theme(axis.text.x = element_text( angle=90))+
        labs(x="Date",y="Number of devices",title="Visits/Non-visits of frequent devices per day")


# Preparindg data
#training data
df_pre<-df_e %>%
        mutate(frequent=as.factor(ifelse(df_e$device_mac%in%df_freq,1,0)),
               branch_1=as.factor(ifelse(df_e$branch_office==1,1,0)),
               branch_2=as.factor(ifelse(df_e$branch_office==2,1,0)),
               branch_3=as.factor(ifelse(df_e$branch_office==3,1,0)),
               monday=as.factor(ifelse(df_e$day_of_week_tz=="Monday",1,0)),
               tuesday=as.factor(ifelse(df_e$day_of_week_tz=="Tuesday",1,0)),
               wednesday=as.factor(ifelse(df_e$day_of_week_tz=="Wednesday",1,0)),
               thursday=as.factor(ifelse(df_e$day_of_week_tz=="Thursday",1,0)),
               friday=as.factor(ifelse(df_e$day_of_week_tz=="Friday",1,0)),
               saturday=as.factor(ifelse(df_e$day_of_week_tz=="Saturday",1,0)),
               sunday=as.factor(ifelse(df_e$day_of_week_tz=="Sunday",1,0)),
               hour=as.factor(df_e$hour_tz)
        )%>% 
        subset(select=-c(1:7,10:12))
rownames(df_pre)<-paste("index",df_e$X,sep="_")

#Test data

#Data prep

# Frequent users
# Frequent users
df_vDays<-aggregate(df_v$X,by=list(df_v$device_mac,as.Date(df_v$Date)),length)
df_vDays<-aggregate(Group.2~Group.1,df_vDays,length)
names(df_vDays)<-c("dev","days")
max(df_vDays$days)
length(df_vDays$dev[df_vDays$days==2])
length(df_vDays$dev[df_vDays$days==1]) #100975
summary(df_vDays$days) #median is ~ 1.5
ggplot(df_vDays,aes(x=days))+geom_bar()

df_freqv<-df_vDays$dev[df_vDays$days>3] #length=6468 
df_vfreq<-df_v[df_v$device_mac %in% df_freqv,] # frequent users
df_vnofreq<-df_v[!df_v$device_mac %in% df_freqv,] #not frequent

df_prev<-df_v %>%
        mutate(frequent=as.factor(ifelse(df_v$device_mac%in%df_freqv,1,0)),
               branch_1=as.factor(ifelse(df_v$branch_office==1,1,0)),
               branch_2=as.factor(ifelse(df_v$branch_office==2,1,0)),
               branch_3=as.factor(ifelse(df_v$branch_office==3,1,0)),
               monday=as.factor(ifelse(df_v$day_of_week_tz=="Monday",1,0)),
               tuesday=as.factor(ifelse(df_v$day_of_week_tz=="Tuesday",1,0)),
               wednesday=as.factor(ifelse(df_v$day_of_week_tz=="Wednesday",1,0)),
               thursday=as.factor(ifelse(df_v$day_of_week_tz=="Thursday",1,0)),
               friday=as.factor(ifelse(df_v$day_of_week_tz=="Friday",1,0)),
               saturday=as.factor(ifelse(df_v$day_of_week_tz=="Saturday",1,0)),
               sunday=as.factor(ifelse(df_v$day_of_week_tz=="Sunday",1,0)),
               hour=as.factor(df_v$hour_tz)
        )%>% 
        subset(select=-c(1:7,10:12))
rownames(df_prev)<-paste("index",df_v$X,sep="_")

#Model testing and validation

set.seed(123)

# Visitor variable
train<-df_pre[,-2]

intrain <- createDataPartition(train$visitor, p = 0.7, list=FALSE)
mod_train <- train[intrain, ]
mod_test <- train[-intrain, ]
mod_train_X <- mod_train[,-1]
mod_test_X <- mod_test[,-1]

test<-df_prev[,-2]

# Let us look at the classification tree using rpart 

set.seed(123)
# strat with a very small cp
dt <- rpart(visitor ~ .,
             data = train,
             method = "class",control=rpart.control(cp=1e-05))

#Now look at the complexity parameter

printcp(dt)

best_cp<-dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
# ~ 28% (xerror * node error) minimizes misclassification rate
# ~ 72% accuracy
dt <- rpart(visitor ~ .,
            data = train,
            method = "class",control=rpart.control(cp=best_cp))
fancyRpartPlot(dt)

# Feature relative significance

#training set

control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
# traininig
set.seed(123)
predictors<-as.factor(names(train)[-1])
#sampling train data to perform faster calculation
model <- train(visitor ~ ., data = sample_n(train,200), method = "rf", preProcess = NULL, trControl = control)
# estimate variable importance
signif <- varImp(model, scale=TRUE)
# prepare for plotting
signif_I <- signif$importance
signif_I$group <- rownames(signif_I)
levorder<- signif_I[order(signif_I$Overall, decreasing = FALSE), "group"]
signif_2 <- signif_I
signif_2$Overall <- 0
signif_df <- rbind(signif_I, signif_2)
signif_I$group<-factor(signif_I$group,levels=levorder)
signif_df$group<-factor(signif_df$group,levels=levorder)
ggplot() +
        geom_point(data = signif_I, aes(x = Overall, y = group, color = group)) +
        geom_path(data = signif_df, aes(x = Overall, y = group, color = group, group = group)) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
        labs(
                x = "Importance",
                y = "",
                title = "Detection of phone devices in local businesses",
                subtitle = "Scaled feature importance",
                caption = "\nRandom Forest with repeated cross validation (10 repeats, 10 times)"
        )



# Reduce features in order to apply model 
# I will categorize the hours of the day in three in order to reduce the number of features 


#Preparindg data
#training data
df_pre<-df_e %>%
        mutate(frequent=as.factor(ifelse(df_e$device_mac%in%df_freq,1,0)),
               branch_1=as.factor(ifelse(df_e$branch_office==1,1,0)),
               branch_2=as.factor(ifelse(df_e$branch_office==2,1,0)),
               branch_3=as.factor(ifelse(df_e$branch_office==3,1,0)),
               monday=as.factor(ifelse(df_e$day_of_week_tz=="Monday",1,0)),
               tuesday=as.factor(ifelse(df_e$day_of_week_tz=="Tuesday",1,0)),
               wednesday=as.factor(ifelse(df_e$day_of_week_tz=="Wednesday",1,0)),
               thursday=as.factor(ifelse(df_e$day_of_week_tz=="Thursday",1,0)),
               friday=as.factor(ifelse(df_e$day_of_week_tz=="Friday",1,0)),
               saturday=as.factor(ifelse(df_e$day_of_week_tz=="Saturday",1,0)),
               sunday=as.factor(ifelse(df_e$day_of_week_tz=="Sunday",1,0)),
               morning=as.factor(ifelse(df_e$hour_tz<8,1,0)),
               day=as.factor(ifelse(df_e$hour_tz<16 & df_e$hour_tz>7,1,0)),
               evening=as.factor(ifelse(df_e$hour_tz<24 & df_e$hour_tz>15,1,0))
        )%>% 
        subset(select=-c(1:7,10:12))
rownames(df_pre)<-paste("index",df_e$X,sep="_")

#Test data

#Data prep

# Frequent users
# Frequent users
df_vDays<-aggregate(df_v$X,by=list(df_v$device_mac,as.Date(df_v$Date)),length)
df_vDays<-aggregate(Group.2~Group.1,df_vDays,length)
names(df_vDays)<-c("dev","days")
max(df_vDays$days)
length(df_vDays$dev[df_vDays$days==2])
length(df_vDays$dev[df_vDays$days==1]) #100975
summary(df_vDays$days) #median is ~ 1.5
ggplot(df_vDays,aes(x=days))+geom_bar()

df_freqv<-df_vDays$dev[df_vDays$days>3] #length=6468 
df_vfreq<-df_v[df_v$device_mac %in% df_freqv,] # frequent users
df_vnofreq<-df_v[!df_v$device_mac %in% df_freqv,] #not frequent

df_prev<-df_v %>%
        mutate(frequent=as.factor(ifelse(df_v$device_mac%in%df_freqv,1,0)),
               branch_1=as.factor(ifelse(df_v$branch_office==1,1,0)),
               branch_2=as.factor(ifelse(df_v$branch_office==2,1,0)),
               branch_3=as.factor(ifelse(df_v$branch_office==3,1,0)),
               monday=as.factor(ifelse(df_v$day_of_week_tz=="Monday",1,0)),
               tuesday=as.factor(ifelse(df_v$day_of_week_tz=="Tuesday",1,0)),
               wednesday=as.factor(ifelse(df_v$day_of_week_tz=="Wednesday",1,0)),
               thursday=as.factor(ifelse(df_v$day_of_week_tz=="Thursday",1,0)),
               friday=as.factor(ifelse(df_v$day_of_week_tz=="Friday",1,0)),
               saturday=as.factor(ifelse(df_v$day_of_week_tz=="Saturday",1,0)),
               sunday=as.factor(ifelse(df_v$day_of_week_tz=="Sunday",1,0)),
               morning=as.factor(ifelse(df_v$hour_tz<8,1,0)),
               day=as.factor(ifelse(df_v$hour_tz<16 & df_v$hour_tz>7,1,0)),
               evening=as.factor(ifelse(df_v$hour_tz<24 & df_v$hour_tz>15,1,0))
        )%>% subset(select=-c(1:10))
rownames(df_prev)<-paste("index",df_v$X,sep="_")

#Model testing and validation

set.seed(123)

# Visitor variable
train<-df_pre[,-2]

intrain <- createDataPartition(train$visitor, p = 0.7, list=FALSE)
mod_train <- train[intrain, ]
mod_test <- train[-intrain, ]
mod_train_X <- mod_train[,-1]
mod_test_X <- mod_test[,-1]

test<-df_prev[,-2]

# Let us look at the classification tree using rpart 

set.seed(123)
# strat with a very small cp
dt <- rpart(visitor ~ .,
            data = train,
            method = "class",control=rpart.control(cp=1e-05))

#Now look at the complexity parameter

printcp(dt)

best_cp<-dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
# ~ 28% (xerror * node error) minimizes misclassification rate
# ~ 72% accuracy
dt <- rpart(visitor ~ .,
            data = train,
            method = "class",control=rpart.control(cp=best_cp))
fancyRpartPlot(dt)

# Feature relative significance

#training set

control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
# traininig
set.seed(123)
predictors<-as.factor(names(train)[-1])
#sampling train data to perform faster calculation
model <- train(visitor ~ ., data = sample_n(train,200), method = "rf", preProcess = NULL, trControl = control)
# estimate variable importance
signif <- varImp(model, scale=TRUE)
# prepare for plotting
signif_I <- signif$importance
signif_I$group <- rownames(signif_I)
levorder<- signif_I[order(signif_I$Overall, decreasing = FALSE), "group"]
signif_2 <- signif_I
signif_2$Overall <- 0
signif_df <- rbind(signif_I, signif_2)
signif_I$group<-factor(signif_I$group,levels=levorder)
signif_df$group<-factor(signif_df$group,levels=levorder)
ggplot() +
        geom_point(data = signif_I, aes(x = Overall, y = group, color = group)) +
        geom_path(data = signif_df, aes(x = Overall, y = group, color = group, group = group)) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
        labs(
                x = "Importance",
                y = "",
                title = "Detection of phone devices in local businesses",
                subtitle = "Scaled feature importance",
                caption = "\nRandom Forest with repeated cross validation (10 repeats, 10 times)"
        )

set.seed(123)
control<-trainControl(method = "repeatedcv", number = 10, repeats = 10,verboseIter = FALSE)
model_rf <- caret::train(visitor ~ .,
                         data = mod_train,
                         method = "rf",
                         preProcess = NULL,
                         trControl = control)

