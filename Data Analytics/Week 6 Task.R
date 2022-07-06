library(Hmisc)
library(ggplot2)

# Exam Anxiety
# Draw a scatter plot between Exam and Anxiety variables
scatter1<-ggplot(ExamAnxiety,aes(Exam,Anxiety))+geom_point()+labs(x="Exam Mark",y="Anxiety level")

#Draw a scatter plot between Exam and Revise variables
scatter2<-ggplot(ExamAnxiety,aes(Exam,Revise))+geom_point()+labs(x="Exam Mark",y="Revise")

#Pearson correlation and p values between all the numerical variables. Problem is that all the values are corrected to 2 decimal places
rcorr(as.matrix(ExamAnxiety[,c("Exam", "Anxiety","Revise")]))

#Create a matrix called ExamCor. The matrix contains 3 parameters r, n and p. Use View with $ to generate a matrix summarising the r and p values, each with 7 decimal places
ExamCor <- rcorr(as.matrix(ExamAnxiety[,c("Exam", "Anxiety","Revise")]), type = "pearson")
View(ExamCor$r)
View(ExamCor$P)

# Essay Marks
# Draw a scatter plot between Essay marks and hours spent
scatter3<-ggplot(EssayMarks,aes(essay,hours))+geom_point()+labs(x="Essay Mark",y="Hours spent")

#Change the variable Grade into a numerical categorical variable - Add a new column called gradeno using ifelse
EssayMarks$gradeno <- ifelse(EssayMarks$grade=="First Class",1,ifelse(EssayMarks$grade=="Upper Second Class",2,ifelse(EssayMarks$grade=="Lower Second Class",3,4)))


#Draw the scatter plot between Essay and Grade variables.
scatter4<-ggplot(EssayMarks,aes(essay,gradeno))+geom_point()+labs(x="Essay Mark",y="Grade achieved")

#Pearson correlation - essay marks and hours   
EssayCor1 <- rcorr(as.matrix(EssayMarks[,c("essay","hours")] ,type="pearson"))
View(EssayCor1$r)
View(EssayCor1$P)

#Spearman correlation - grade and essay
EssayMarks$gradeno2 <- ifelse(EssayMarks$grade=="First Class",4,ifelse(EssayMarks$grade=="Upper Second Class",3,ifelse(EssayMarks$grade=="Lower Second Class",2,1)))

EssayCor2 <- rcorr(as.matrix(EssayMarks[,c("gradeno2","essay")],type="spearman"))
View(EssayCor2$r)
View(EssayCor2$P)

#Spearman correlation - grade and hours
EssayCor3 <- rcorr(as.matrix(EssayMarks[,c("gradeno2","hours")],type="spearman"))
View(EssayCor3$r)
View(EssayCor3$P)

#Film
#check if the sample has an equal number of male and female who prefer each film.
filmtable<-table(film$gender,film$film) #No point to check for the correlation between film and gender as the numbers are equal.

#box plot between gender and enjoyment
boxfilm1<-ggplot(film,aes(gender,enjoyment))+geom_boxplot()+labs(x="gender",y="enjoyment")
boxfilm2<-ggplot(film,aes(film,enjoyment))+geom_boxplot()+labs(x="film",y="enjoyment")

#Convert the variables into numerical variables
film$genderno <- ifelse(film$gender=="Female",0,1)
film$filmno <- ifelse(film$film=="Bridget Jones' Diary",0,1)


#Two categorical variables showing no continuum, should use point-biserial correlation - install the following packages
library(polycor)
library(ltm)

#Gender and enjoyment - use biserial correlation to work out r
r_gen_enjoy <-biserial.cor(film$enjoyment,film$genderno,use = c("all.obs" , "complete.obs"), level=2)

#Film and enjoyment - use biserial correlation to work out r
r_film_enjoy <-biserial.cor(film$enjoyment,film$filmno,use = c("all.obs" , "complete.obs"),level=2)

#Create a table summarising the r values
filmvariables<-c("gender","film")
filmenjoyment<-c(r_gen_enjoy,r_film_enjoy)
filmrvalues<-data.frame(filmvariables,filmenjoyment)

#Gender and film (both dichotomous/ binary categorical variables - use Tetrachoric Correlation)
library(psych)
r_gen_film<-tetrachoric(as.matrix(film[,c("genderno","filmno")]))
View(r_gen_film$rho)


#Gender, film, enjoyment - use rcorr to find p values. It does not matter which type to work out p values.
filmCor <- rcorr(as.matrix(film[,c("genderno","filmno","enjoyment")],type="pearson"))
View(filmCor$r)
View(filmCor$P)


# Trees - existing database called trees
# Find the correlations between all three variables using appropriate methods (create a table). 
View(trees)
head(trees)

# Check normality of data - use q-q plot
library("ggpubr")
ggqqplot(trees$Girth, ylab = "Girth") #normal
ggqqplot(trees$Height, ylab = "Height") #normal
ggqqplot(trees$Volume, ylab = "Volume") #reasonably normal

#Use of correlation plot to show the correlation between each pair of variables in the database trees. All correlations are positive, ranging from medium to strong.
install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(trees),lower = "number",upper = "circle",tl.col = "black")

#Normal data means we can use Pearson, a parametric test
treesCor <- rcorr(as.matrix(trees[,c("Girth", "Height","Volume")]), type = "pearson")
View(treesCor$r)
View(treesCor$P)