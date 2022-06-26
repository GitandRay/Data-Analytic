View(EssayMarks)
colnames(EssayMarks)##Check the names of the column in the data frame

##Scatter plot - essay marks against hours spent
scat1<-ggplot(EssayMarks,aes(essay,hours))
a1<-scat1+geom_point()+labs(x="essay",y="hours")+ggtitle("Essay marks and hours spent")+theme(plot.title=element_text(hjust=0.5))

##Scatter plot with best fit straight line
a2<-scat1+geom_point()+labs(x="essay",y="hours")+geom_smooth(method="lm",colour="Purple",se=F,alpha=0.1,fill="Red")+ggtitle("Essay marks and hours spent")+theme(plot.title=element_text(hjust=0.5))

##Scatter plot with best fit curve
a3<-scat1+geom_point()+labs(x="essay",y="hours")+geom_smooth(method="loess",colour="Purple",se=F,alpha=0.1,fill="Red")+ggtitle("Essay marks and hours spent")+theme(plot.title=element_text(hjust=0.5))

##Scatter plot with third variable grade in different colours 
scat2<-ggplot(EssayMarks,aes(essay,hours,colour=grade))
b<-scat2+geom_point()+labs(x="essay",y="hours")+ggtitle("Essay marks and hours spent")+theme(plot.title=element_text(hjust=0.5))

##Histogram showing the distribution of essay marks
histo1<-ggplot(EssayMarks,aes(essay))
c1<-histo1 + geom_histogram(binwidth = 0.5, colour="green", fill="yellow")+labs(x="essay marks",y="frequency")+ggtitle("Essay mark distribution")+theme(plot.title=element_text(hjust=0.5))  

##Histogram showing the distribution of hours spent
histo2<-ggplot(EssayMarks,aes(hours))
c2<-histo2 + geom_histogram(binwidth = 0.5, colour="red", fill="cyan")+labs(x="hours",y="frequency")+ggtitle("Hours spent on essay")+theme(plot.title=element_text(hjust=0.5)) 

##Bar chart showing the essay marks at each grade
bar1<-ggplot(EssayMarks,aes(grade,essay))
d1<-bar1+stat_summary(fun="mean",geom="bar",fill="blue",colour="cyan")+stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.1)+labs(x="grade",y="essay marks")+ggtitle("Essay marks and grade")+theme(plot.title=element_text(hjust=0.5))

##Bar chart showing the hours spent at each grade
bar2<-ggplot(EssayMarks,aes(grade,hours))
d2<-bar2+stat_summary(fun="mean",geom="bar",fill="pink",colour="magenta")+stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.1)+labs(x="grade",y="essay marks")+ggtitle("Hours spent and grade")+theme(plot.title=element_text(hjust=0.5))

##Bar chart showing the number of students at each grade
##Summary statistics using table followed by creating the dataframe
gradename<-c("First Class","Upper Second C","Lower Second Class","Third Class")
gradefreq<-c(10,23,10,2)
gradet<-data.frame(gradename,gradefreq)
bar3<-ggplot(gradet,aes(gradename,gradefreq))
d3<-bar3+geom_bar(stat="identity",fill="orange",colour="brown")+labs(x="grade",y="frequency")+ggtitle("grade")+theme(plot.title=element_text(hjust=0.5))

##Boxplot showing the essay marks at each grade
box1<-ggplot(EssayMarks,aes(grade,essay,color=grade))
e1<-box1+geom_boxplot()+labs(x="grade",y="essay marks")+ggtitle("Essay marks at each grade")+theme(plot.title=element_text(hjust=0.5))

##Boxplot showing the hours spent at each grade
box2<-ggplot(EssayMarks,aes(grade,hours,fill=grade))
e2<-box2+geom_boxplot()+labs(x="grade",y="hours spent")+ggtitle("Hours spent at each grade")+theme(plot.title=element_text(hjust=0.5))

##Boxplot showing the essay marks at each grade (reordered)
EssayMarks$grade<-factor(EssayMarks$grade, levels=c("First Class","Upper Second Class","Lower Second Class","Third Class"))
levels(EssayMarks$grade)                         
box1<-ggplot(EssayMarks,aes(grade,essay,color=grade))
e3<-box1+geom_boxplot()+labs(x="grade",y="essay marks")+ggtitle("Essay marks at each grade")+theme(plot.title=element_text(hjust=0.5))

##Line graph (mean essay marks across each grade) with error bar
##Very similar to bar chart
EssayMarks$grade<-factor(EssayMarks$grade, levels=c("First Class","Upper Second Class","Lower Second Class","Third Class"))
line1<-ggplot(EssayMarks,aes(grade,essay))
f1<-line1+stat_summary(fun="mean",geom="point")+stat_summary(fun="mean",geom="line",aes(group=1),colour="Navy",linetype="dashed") +stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.1)+labs(x="grade",y="essay marks")+ggtitle("Essay marks and grade")+theme(plot.title=element_text(hjust=0.5))

##Line graph (mean hours spent across each grade) with error bar
##Very similar to bar chart
EssayMarks$grade<-factor(EssayMarks$grade, levels=c("First Class","Upper Second Class","Lower Second Class","Third Class"))
line2<-ggplot(EssayMarks,aes(grade,hours))
f2<-line2+stat_summary(fun="mean",geom="point")+stat_summary(fun="mean",geom="line",aes(group=2),colour="Red",linetype=3) +stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.1)+labs(x="grade",y="hours spent")+ggtitle("Hours spent and grade")+theme(plot.title=element_text(hjust=0.5))

##Pie chart showing essay marks (from bar chart)
pc1 <- ggplot(EssayMarks, aes(x="", y=essay, fill=grade))
g1<-pc1 + geom_bar(width = 1 ,stat="identity")+coord_polar("y", start=0, direction = 1)+ theme_classic()+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + theme_void()+ggtitle("Essay marks and grade")+theme(plot.title=element_text(hjust=0.5))

##Pie chart showing hours spent (from bar chart)
pc2 <- ggplot(EssayMarks, aes(x="", y=hours, fill=grade))
g2<-pc2 + geom_bar(width = 1 ,stat="identity")+coord_polar("y", start=0, direction = 1)+ theme_classic()+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + theme_void()+ggtitle("Hours spent and grade")+theme(plot.title=element_text(hjust=0.5))

pdf("Week 5 graphs.pdf")
print(a1)
print(a2)
print(a3)
print(b)
print(c1)
print(c2)
print(d1)
print(d2)
print(e2)
print(e3)
print(f1)
print(f2)
print(g1)
print(g2)
dev.off()
