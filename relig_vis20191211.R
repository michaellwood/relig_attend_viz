
#set working drive
setwd("C:/Users/mlwood/Box/Research/Relig viz")

#load necessary packages
pacman::p_load(ggplot2, tidyr, readxl, dplyr, reshape2)

#import xls file
gss18 <- readxl::read_xls("GSS.xls")

#rename variables
names(gss18)[1] <- "year"
names(gss18)[3] <- "attend"
names(gss18)[5] <- "gender"


#subset the dataframe. I create a separate df for a gender example
gss18gender <- gss18[,c(1,5)]
gss18 <- gss18[,c(1,3)]

#convert the gender and attendance variables to factors
#gss18$gender   <- as.factor(gss18$gender)
gss18$attend   <- as.factor(gss18$attend)

#remove "DK" and "NA" responses from the attendance variable
gss18<-gss18[!(gss18$attend=='Dk,na'),]
gss18$attend <- droplevels(gss18$attend)


#remove two rows with NA in the year
gss18<-gss18[!(is.na(gss18$year)==TRUE),] 


#check the factor levels for the attendance variable
levels(gss18$attend)

#reorder the factor levels for the attendance variable, because they are out of order
gss18$attend <- factor(gss18$attend,levels(gss18$attend)[c(5,3,8,9,7,1,6,2,4)])

#gss18$attend <- factor(gss18$attend,levels(gss18$attend)[c(6,4,9,10,8,1,7,3,5,2)])

#check the new level order
levels(gss18$attend)


#Data Wrangling I
#To create the visualization we need a dataframe that gives the proportion of responses for each repsonse category by year 
#This is done in two steps. First, we reshape the data from wide to long and tally up the responses.
#Then we go back to wide format, giving each response category its own column with a sum

#inspect our current dataframe
head(gss18)
#create an ID variable preparatory to reshaping the data
gss18$id <- rownames(gss18)


#reshape from wide to long and tally. 
#The new dataframe shows how many cases there are in each attendance response category by year
gss18_l <- gss18 %>%
  group_by(year, attend) %>%
  tally()

#inspect the new dataframe
head(gss18_l)

#Reshape the dataframe again, creating a column for each factor level of the attend variable
gss18_attend <- dcast(gss18_l,year~attend)
#inspect our new dataframe
head(gss18_attend)

#Data Wrangling II
#We now have our data in the right general format, but we need to make a few other changes before creating the plot.


#get the total number of cases for each year
gss18_attend$tot <- rowSums(gss18_attend[,c(2:10)])

#divide each cell by the total and multiply by 100
gss18_attend[,-1] <- lapply(gss18_attend[,-1], function(x){(x/gss18_attend$tot)*100})

#drop the "total" column
gss18_attend <- gss18_attend[,-11]

#reinspect the dataframe
head(gss18_attend)

#First, because we have an odd number of categories, we cannot split our attendance variable into
#High and Low  without first splitting our middle category, which here is "Once a month." 
#Practically, this means that "Once a month" is a neutral category, falling right in the middle of each bar. 

#split the "Once a month" into two categories
gss18_attend$`OnceMonthLo` <- gss18_attend$`Once a month`/2
gss18_attend$`OnceMonthHi` <- gss18_attend$`OnceMonthLo`
gss18_attend <- gss18_attend[,-6]
gss18_attend <- gss18_attend[,c(1:5,10:11,6:9)]


#We want to plot the proportions of each category rather than the raw numbers,
#so we divide each cell by the total number of cases for that year




#Preparing the Plot
#Before we create the plot, we need to get our colors right. I chose a color palette from
#the Urban Institute's Data Visualization Style Guide https://urbaninstitute.github.io/graphics-styleguide/
#Because our plot is technically TWO plots, we need to create separate palettes for the "high" and "low" bars.

#create an object that stores the colors for the low bar
pal_lo <- c("#0a4c6a","#1696d2","#73bfe2","#cfe8f3","#f5f5f5")

#create an object that stores the colors for the low bar
pal_hi <- c("#f5f5f5","#fff2cf", "#fdd870", "#fdaa11", "#ca5800")
#The Urban isntitute uses #fdbf11, but I chose #fdaa11 to make it easier for color-blind viewers

#Next we need to create two separate dataframes: one for the "high" bars and one for the "low"

#Create a dataframe that goes from "never" to "once a month-lo"
gss_att_lo <- gss18_attend[,1:6]

#Reshape wide to long
gss_att_lo <- melt(gss_att_lo,id="year")

#make the values negative, so that they get plotted on the left side of the graph
gss_att_lo$value <- -gss_att_lo$value

#assigns one of the five colors from our palette to each category
gss_att_lo$col <- rep(pal_lo,each=(160/5))
#change these to color values to factors and make sure they are in the right order
gss_att_lo$col <- factor(gss_att_lo$col, levels = c("#0a4c6a","#1696d2","#73bfe2","#cfe8f3","#f5f5f5"))

#Now do the same, but for the "high" plot
gss_att_hi <- gss18_attend[,c(1,7:11)]
gss_att_hi <- melt(gss_att_hi,id="year")
#We need to reverse the levels here so they are plotted in the right order
gss_att_hi$variable <- factor(gss_att_hi$variable,levels(gss_att_hi$variable)[c(5,4,3,2,1)])
gss_att_hi$col <- rep(pal_hi,each=(160/5))
gss_att_hi$col <- factor(gss_att_hi$col, levels = c("#ca5800","#fdaa11","#fdd870","#fff2cf","#f5f5f5"))

#Create the text for the legend
mylevels <- c("More than once a week","Every week","Nearly every week","2-3x a month","Once a month",
              "Several times a year","Once a year","Less than once a year","Never")


#creat the palette for the legend.
legend.pal <- c("#ca5800", "#fdaa11","#fdd870","#fff2cf","#f5f5f5","#cfe8f3","#73bfe2","#1696d2", "#0a4c6a")

#When we make the plot, we want to label each category with its value.
#To do this, we need to calculate WHERE each label should appear. 
#To get these labels near the center of each category, we add up the values of the preceding categories and 
#then add 1/2 of the value of the category we are working on. These will be the "y" locations of our annotations
#We don't need to do this for the middle category, because we already know that the text should be at 0. 



labeldf <- gss18_attend
labeldf <- labeldf[,-6]
#names(labeldf) <- c("year","1","2","3","4","5","6","7","8","9")
#labeldfraw <- labeldf
labeldf[,2] <- -(((labeldf[,2])/2)+rowSums(labeldf[,3:6]))
labeldf[,3] <- -(((labeldf[,3])/2)+rowSums(labeldf[,4:6]))
labeldf[,4] <- -(((labeldf[,4])/2)+rowSums(labeldf[,5:6]))
labeldf[,5] <- -(((labeldf[,5])/2)+labeldf[,6])

labeldf[,10] <- ((labeldf[,10])/2)+rowSums(labeldf[,6:9])
labeldf[,9] <- ((labeldf[,9])/2)+rowSums(labeldf[,6:8])
labeldf[,8] <- ((labeldf[,8])/2)+rowSums(labeldf[,6:7])
labeldf[,7] <- ((labeldf[,7])/2)+labeldf[,6]

#Function to make the x axis pretty
abs_percent <- function(x,digits=2)
{paste0(round(abs(x),digits), "%")}


#Main Plot
jpeg("Religious Attendance 1972-2018 (20191211).jpg", units="in", width=11, height=8.5, res=400,quality=100)

ggplot() +  
  geom_bar(data = gss_att_lo, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+ 
  geom_bar(data = gss_att_hi, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+
  geom_hline(yintercept = 0, color =c("white"))+
  scale_fill_identity(labels = mylevels, breaks=legend.pal, guide="legend")+
  theme_classic()+
  scale_x_continuous(breaks=c(1972:1978,1980,1982:1991,1993,seq(1994,2018,2)))+
  scale_y_continuous(breaks=seq(-50,50,10),
                     labels=abs_percent)+
  geom_hline(yintercept=c(-50,50),linetype="dashed",color="grey")+
  coord_flip()+
  labs(title = "How often do you attend religious services?",
       caption = "General Social Survey 1972-2018",
       y = "",
       x = "Year")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_text(hjust=.54))+
  theme(plot.title=element_text(size=20),
        plot.caption=element_text(size=10),
        axis.title.y=element_text(size=15),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        legend.text=element_text(size=10))

dev.off()


#Main plot, with values annotated
jpeg("Religious Attendance 1972-2018 (Annotated)(20191211).jpg", units="in", width=11, height=8.5, res=400,quality=100)

ggplot() +  
  geom_bar(data = gss_att_lo, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+ 
  geom_bar(data = gss_att_hi, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+
  geom_hline(yintercept = 0, color =c("white"))+
  scale_fill_identity(labels = mylevels, breaks=legend.pal, guide="legend")+
  theme_classic()+
  annotate("text", x = labeldf$year,y = 0,label = round((2*labeldf$OnceMonthHi),1))+
  annotate("text", x = labeldf$year, y = labeldf$Never, hjust = .5, label = round(gss18_attend$Never,1), color = "#f5f5f5") +
  annotate("text", x = labeldf$year, y = labeldf$`Lt once a year`, hjust = .5, label = round(gss18_attend$`Lt once a year`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`Once a year`, hjust = .5, label = round(gss18_attend$`Once a year`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`Sevrl times a yr`, hjust = .5, label = round(gss18_attend$`Sevrl times a yr`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`2-3x a month`, hjust = .5, label = round(gss18_attend$`2-3x a month`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`Nrly every week`, hjust = .5, label = round(gss18_attend$`Nrly every week`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`Every week`, hjust = .5, label = round(gss18_attend$`Every week`,1))+
  annotate("text", x = labeldf$year, y = labeldf$`More thn once wk`, hjust = .5, label = round(gss18_attend$`More thn once wk`,1), color="#f5f5f5")+
  scale_x_continuous(breaks=c(1972:1978,1980,1982:1991,1993,seq(1994,2018,2)))+
  scale_y_continuous(breaks=seq(-50,50,10),
                     labels=abs_percent)+
  geom_hline(yintercept=c(-50,50),linetype="dashed",color="grey")+
  coord_flip()+
  labs(title = "How often do you attend religious services?",
       caption = "General Social Survey 1972-2018",
       y = "",
       x = "Year")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_text(hjust=.54))+
  theme(plot.title=element_text(size=20),
        plot.caption=element_text(size=10),
        axis.title.y=element_text(size=15),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        legend.text=element_text(size=10))





dev.off()


#Do everything again, but plot gender instead of religion
gss18gender$gender   <- as.factor(gss18gender$gender)

#remove "DK" and "NA" responses from the attendance variable
gss18gender<-gss18gender[!(gss18gender$gender=='Dk,na'),]
gss18gender$gender <- droplevels(gss18gender$gender)


#remove two rows with NA in the year
gss18gender<-gss18gender[!(is.na(gss18gender$year)==TRUE),] 


#check the factor levels for the attendance variable
levels(gss18gender$gender)

head(gss18gender)
#create an ID variable preparatory to reshaping the data
gss18gender$id <- rownames(gss18gender)


#reshape from wide to long and tally. 
#The new dataframe shows how many cases there are in each attendance response category by year
gss18gender_l <- gss18gender %>%
  group_by(year, gender) %>%
  tally()

#inspect the new dataframe
head(gss18gender_l)

#Reshape the dataframe again, creating a column for each factor level of the attend variable
gss18_gender <- dcast(gss18gender_l,year~gender)
#inspect our new dataframe
head(gss18_gender)

#Data Wrangling II
#We now have our data in the right general format, but we need to make a few other changes before creating the plot.


#get the total number of cases for each year
gss18_gender$tot <- rowSums(gss18_gender[,c(2:3)])

#divide each cell by the total and multiply by 100
gss18_gender[,-1] <- lapply(gss18_gender[,-1], function(x){(x/gss18_gender$tot)*100})

#drop the "total" column
gss18_gender <- gss18_gender[,-4]

#reinspect the dataframe
head(gss18_gender)


#create an object that stores the colors for the low bar
gndr_pal_lo <- c("#1696d2")

#create an object that stores the colors for the low bar
gndr_pal_hi <- c("#fdbf11")


#Next we need to create two separate dataframes: one for the "high" bars and one for the "low"

#Create a dataframe that goes from "never" to "once a month-lo"
gss_gndr_lo <- gss18_gender[,1:2]

#Reshape wide to long
gss_gndr_lo <- melt(gss_gndr_lo,id="year")

#make the values negative, so that they get plotted on the left side of the graph
gss_gndr_lo$value <- -gss_gndr_lo$value

#assigns one of the five colors from our palette to each category
gss_gndr_lo$col <- rep(gndr_pal_lo,each=(32/1))

#Now do the same, but for the "high" plot
gss_gndr_hi <- gss18_gender[,c(1,3)]
gss_gndr_hi <- melt(gss_gndr_hi,id="year")

gss_gndr_hi$col <- rep(gndr_pal_hi,each=(32/1))

#Create the text for the legend
mylevels_gndr <- c("Men","Women")

#creat the palette for the legend.
gndr_legend.pal <- c("#fdbf11","#1696d2")


gndr_labeldf <- gss18_gender
gndr_labeldf$Female <- gndr_labeldf$Female/2
gndr_labeldf$Male <- gndr_labeldf$Male/2


jpeg("Gender 1972-2018 (Annotated)(20191211).jpg", units="in", width=11, height=8.5, res=400,quality=100)

ggplot() +  
  geom_bar(data = gss_gndr_lo, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+ 
  geom_bar(data = gss_gndr_hi, aes(x=year, y=value,fill=col), position="stack", stat="identity",width=.95)+
  geom_hline(yintercept = 0, color =c("white"))+
  scale_fill_identity(labels = mylevels_gndr, breaks=gndr_legend.pal, guide="legend")+
  theme_classic()+
  annotate("text", x = gndr_labeldf$year, y=-gndr_labeldf$Female, hjust = .5, label = round((gndr_labeldf$Female*2),1))+
  annotate("text", x = gndr_labeldf$year, y=gndr_labeldf$Male, hjust = .5, label = round((gndr_labeldf$Male*2),1))+
  scale_x_continuous(breaks=c(1972:1978,1980,1982:1991,1993,seq(1994,2018,2)))+
  scale_y_continuous(breaks=seq(-50,50,10),
                     labels=abs_percent)+
  geom_hline(yintercept=c(-50,50),linetype="dashed",color="grey")+
  coord_flip()+
  labs(title = "Gender of GSS Respondents",
       caption = "General Social Survey 1972-2018",
       y = "",
       x = "Year")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_text(hjust=.54))+
  theme(plot.title=element_text(size=20),
        plot.caption=element_text(size=10),
        axis.title.y=element_text(size=15),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        legend.text=element_text(size=10))

dev.off()
