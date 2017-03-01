data=read.csv('/Users/hola/Desktop/Speed\ Dating/Data/speed-dating-experiment/Speed\ Dating\ Data.csv',header=TRUE,sep=";")


# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
        ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col))
        }
    }
}

============================================================================================================================================

#Raison de la participation en fonction du revenu

data <- read.csv("../input/Speed Dating Data.csv")
data$income2 <- sub(",", "", data$income)
data$income2 <- as.numeric(data$income2)

method1 <- data[data$wave>5 & data$wave<10,]
method2 <- data[data$wave<6 | data$wave>9,]



par(mar=c(12,5,3,3))
boxplot(data$income2~data$goal, ylab="Income", xlab="", xaxt="n")
axis(1, at=c(1,2,3,4,5,6), labels=c("Fun", "Meet people", "Date", "Serious relationship", "Experience", "Other"), las=2)
mtext("Reason for participating", side=1, line=10)

#BON
============================================================================================================================================

# does income relate to how frequently you date/go out?

data$incomesplit <- cut(data$income2, c(0,30000,50000,70000,90000,110000))
data$date <- as.numeric(data$date)

#par(fig=c(0,0.5,0,1))
#boxplot(data$income2~data$date, xlab="Dates a lot    ---    Seldom dates", ylab="Income")
lm(data$income2~data$date)
p1 <- ggplot(na.omit(data[,c("incomesplit", "date")])[!duplicated(data$iid),], aes(incomesplit, date)) + geom_boxplot() + geom_jitter(alpha=1/2)

#par(fig=c(0.5,1,0,1), new=T)
#boxplot(data$income2~data$go_out, xlab="Goes out a lot  ---  Seldom goes out", ylab="Income")
lm(data$income2~data$go_out)
p2 <- ggplot(na.omit(data[,c("incomesplit", "go_out")])[!duplicated(data$iid),], aes(incomesplit, go_out)) + geom_boxplot() + geom_jitter(alpha=1/2)



multiplot(p1,p2, ncol=2)

#MOYEN
============================================================================================================================================

# what do the top earners do?

topearn <- unique(data[data$income2>quantile(data$income2, 0.95, na.rm=T) & !is.na(data$income2),c(49,196)])
topearn[order(topearn$income2, decreasing=T),]


#BON

============================================================================================================================================

# what are the exercise habits of the wealthy? - slight trend for high value of exercise in the ultra-wealthy

ggplot(na.omit(data[,c("incomesplit", "exercise")])[!duplicated(data$iid),], aes(exercise, x=incomesplit)) +  geom_boxplot() + geom_jitter()


#plot(data$income2, data$exercise)
#boxplot(data$income2~data$exercise, ylab="Income", xlab="Exercise interest")

#MOYEN


============================================================================================================================================

# how about reading?

boxplot(data$income2~data$reading, ylab="Income", xlab="Reading interest")

summary(lm(data$income2~data$reading))

#MOYEN

============================================================================================================================================

# do the wealthy value shopping more?

boxplot(data$income2~data$shopping, ylab="Income", xlab="Shopping interest")

summary(lm(data$income2~data$shopping))

#BON

============================================================================================================================================

# the study group think they're smarter than the average person, and the average person they dated

hist(data$intel3_1, xlab="Percieved intelligence", breaks=6, xlim=c(0,10))
hist(data$intel, xlab="Dates' intelligence", breaks=10, xlim=c(0,10))

#MOYEN

============================================================================================================================================

# how did people rate their own intelligence in comparison to their date's?

ggplot(data, aes(y=intel, x=intel3_1)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank()) + ylab("Date's intelligence") + xlab("Own intelligence")

summary(lm(data$intel~data$intel3_1))

#MOYEN

============================================================================================================================================

# how do mate quality preferences vary across career choices?

# attractiveness
par(mar=c(10,5,3,3))
#ggplot(data, aes(factor(career_c), attr1_1)) + geom_boxplot()

boxplot(method2$attr1_1~method2$career_c, xaxt="n")
abline(h=median(method2$attr1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)

#TRAVAILLER


============================================================================================================================================
============================================================================================================================================
============================================================================================================================================
======================================================REGLES DASSOCIATION===================================================================
============================================================================================================================================
============================================================================================================================================
============================================================================================================================================
============================================================================================================================================

library(arules)
library(arulesViz)

data <- read.transactions("/Users/hola/Desktop/scirep-cuisines-detail/allr_recipes.txt", format="basket", sep = " ")

rules <- apriori(data, parameter = list(supp = 0.01, conf = 0.2,minlen=4))
plot (rules[1:19],method="graph",shading="confidence")


table=read.csv('/Users/hola/Desktop/Speed\ Dating/Data/ARULES/Match.csv',header=TRUE,sep=";")
table=as(as.list(table),"transactions")
rules <- apriori(table,parameter = list(supp = 0.8,minlen=4))

plot(rules, method="graph", control=list(type="items"))


table=read.csv(' /Users/hola/Desktop/M2\ Data\ Science/Data\ Mining/Projet/Speed\ Dating/TEST.csv',header=TRUE,sep=";")





table=read.transactions("/Users/hola/Desktop/TEST.csv", sep = ";", format="basket",rm.duplicates=TRUE)

rules <- apriori(table,parameter = list(supp = 0.01,conf=0.01,minlen=3))
rules.sorted <- sort(rules, by="lift")


subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
plot(rules.pruned, method="graph")


rules <- apriori(table,parameter = list(minlen=2, supp=0.1, conf=0.8))

plot(
abbreviate(rules, minlength=10),
method="grouped",
measure="support",
shading="lift",
control=list(k=10)
)

Ambiance=rules[inspect(rules)$rhs=="{Ambiance}"]
plot(sort(Ambiance, by="lift")[1:5], method="graph", control=list(type="items"))

Brocouille=rules[inspect(rules)$rhs=="{Brocouille}"]
plot(sort(Brocouille, by="lift")[1:5], method="graph", control=list(type="items"), interactive=TRUE, shading=NA)

======================================================================================================================================================
library(arules)
library(arulesViz)

#Importer les données sous forme de transaction et visualiser les fréquences absolues de chaque item#

table=read.transactions("/Users/hola/Desktop/M2\ Data\ Science/Data\ Mining/Projet/Speed\ Dating/TEST.csv", sep = ";", format="basket", rm.duplicates=TRUE)
itemFrequencyPlot(table, topN=60, type= "absolute", cex=0.7, ylab="Fréquence absolue", main="Fréquence absolue de chaque item")

rules <- apriori(table,parameter = list(minlen=4, supp=0.4, conf=0.8))

Apriori

Parameter specification:
confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target   ext
0.8    0.1    1 none FALSE            TRUE       5     0.4      4     10  rules FALSE

Algorithmic control:
filter tree heap memopt load sort verbose
0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 513

set item appearances ...[0 item(s)] done [0.00s].
set transactions ...[48 item(s), 1284 transaction(s)] done [0.00s].
sorting and recoding items ... [30 item(s)] done [0.00s].
creating transaction tree ... done [0.00s].
checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.04s].
writing ... [20370 rule(s)] done [1.06s].
creating S4 object  ... done [0.01s].

inspect(sort(rules, by="lift")[1:5])

lhs                                                           rhs          support   confidence lift
[1] {Entreprenant,Films,Inteligent,Interets_commun,Sincere}    => {Extraverti} 0.4003115 0.9431193  1.323459
[2] {Casanier,Entreprenant,Inteligent,Interets_commun,Sincere} => {Extraverti} 0.4080997 0.9424460  1.322514
[3] {Entreprenant,Films,Interets_commun,Sincere}               => {Extraverti} 0.4018692 0.9416058  1.321335
[4] {Entreprenant,Inteligent,Interets_commun,Sincere}          => {Extraverti} 0.4119938 0.9412811  1.320880
[5] {Casanier,Entreprenant,Interets_commun,Sincere}            => {Extraverti} 0.4096573 0.9409660  1.320438

plot(sort(rules, by="lift")[1:5], method="graph", control=list(type="items"),main="Les 5 règles d'association avec le meilleure lift")

inspect(sort(rules, by="lift")[1:10])

lhs                                                           rhs          support   confidence lift
[1]  {Entreprenant,Films,Inteligent,Interets_commun,Sincere}    => {Extraverti} 0.4003115 0.9431193  1.323459
[2]  {Casanier,Entreprenant,Inteligent,Interets_commun,Sincere} => {Extraverti} 0.4080997 0.9424460  1.322514
[3]  {Entreprenant,Films,Interets_commun,Sincere}               => {Extraverti} 0.4018692 0.9416058  1.321335
[4]  {Entreprenant,Inteligent,Interets_commun,Sincere}          => {Extraverti} 0.4119938 0.9412811  1.320880
[5]  {Casanier,Entreprenant,Interets_commun,Sincere}            => {Extraverti} 0.4096573 0.9409660  1.320438
[6]  {Entreprenant,Interets_commun,Sincere}                     => {Extraverti} 0.4135514 0.9398230  1.318834
[7]  {Casanier,Entreprenant,Films,Inteligent,Interets_commun}   => {Extraverti} 0.4065421 0.9371634  1.315101
[8]  {Entreprenant,Films,Inteligent,Interets_commun}            => {Extraverti} 0.4104361 0.9360568  1.313549
[9]  {Casanier,Entreprenant,Inteligent,Interets_commun}         => {Extraverti} 0.4190031 0.9356522  1.312981
[10] {Entreprenant,Inteligent,Interets_commun}                  => {Extraverti} 0.4228972 0.9345955  1.311498

plot(sort(rules, by="lift")[1:10], method="graph", control=list(type="items"),main="Les 10 règles d'association avec le meilleure lift")



Celib=rules[inspect(rules)$rhs=="{No_Match}"]
plot(sort(Celib, by="lift")[1:5], method="graph", control=list(type="items"), main="Les 5 raisons de ne pas matcher")
plot(sort(Celib, by="lift")[1:10], method="graph", control=list(type="items"), main="Les 10 raisons de ne pas matcher" )


Femme=rules[inspect(sort(rules, by="lift"))$rhs=="{Femme}"]
Homme=rules[inspect(rules)$rhs=="{Homme}"]
plot(sort(Femme, by="lift")[1:10], method="graph", control=list(type="items"), main="Les 10 éléments associés au facteur Femme" )



Beau=rules[inspect(sort(rules, by="lift"))$rhs=="{Beau}"]
plot(sort(Beau, by="lift")[1:10], method="graph", control=list(type="items"), main="Les 10 raisons pour lesquelles on trouve quelqu'un beau" )



======================================================================================================================================================
======================================================================================================================================================
======================================================================================================================================================
===========================================================   ADAMO     ==============================================================================
======================================================================================================================================================
======================================================================================================================================================
======================================================================================================================================================




ibrary(dplyr)        # Data manipulation
library(reshape2)     # Data reshaping for ggplot
library(ggplot2)      # Data visualization
library(plotly)       # Dynamic data visualization
library(RColorBrewer) # Colors on plots
library(readr)        # CSV file I/O, e.g. the read_csv function
library(dataQualityR) # DQR generation
library(randomForest) # Random Forest for variable importance
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(corrplot)
#library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(ade4)
library(randomForest)
library(adabag)
library(xgboost)
library(caret)



missing.types <- c("NA", "")
df <- read.csv("Speed-Dating-Data.csv", header=T,sep=";", stringsAsFactors = F)

#On change
fem <- df[df$gender == 0, ]
mal <- df[df$gender == 1, ]


# Les données ont été recueillies auprès de participants à des événements expérimentaux
#datant de 2002-2004.
#Les sujets sont des étudiants des écoles supérieures et professionnelles de l'Université Columbia.
#À la fin de leur rencontre de speed dating, les participants ont été interrogés s'ils aimeraient voir leur date à nouveau.
# Ils ont également été invités à noter leur date sur six attributs df[, 25:31]


kor <- cor(df[, 25:31], use = "complete.obs") #les attributs sont sur les colonnes 25 à 31
corrplot(kor, method = "color", order = "hclust")

#Le graphique de corrélation montre que l'attractivité, la fantaisie et l'intérêt commun
#sont corrélés dans une plus grande mesure que les autres attributs.
#Alors que l'intelligence est corrélée principalement avec les ambitions et sincérité.

# Comment les hommes évaluent les attributs des femmes
par(mfrow = c(1,2))
par(mfrow = c(1,1))

korF <- cor(df[df$gender == 0, 25:31], use = "complete.obs")
corrplot(korF, method = "color", title = "Attributs des femmes", mar = c(0,0,1,0))

# Comment les femmes évaluent les attributs des hommes
korM <- cor(df[df$gender == 1, 25:31], use = "complete.obs")
corrplot(korM, method = "color", title = "Attributs des hommes", mar = c(0,0,1,0))

#Y a-t-il une différence, visuellement non mais soyons plus précis

max(abs(korM - korF))
#La difference max entre  7%




######################################
#         DECISION TREE              #
######################################

test_dat <- df[1:2000, ]
train_dat <- df[2001:nrow(df), ]

correct <- function(prediction, corr_answer) {
    print(table(prediction, corr_answer))
    
    cat("\n PREDITS CORRECTEMENT: \n")
    (table(prediction, corr_answer)[1,1] +
    table(prediction, corr_answer)[2,2])/length(corr_answer)
}

fit_both <- rpart(dec_o ~ attr_o + intel_o + sinc_o + fun_o + shar_o + gender, data = train_dat, method = "class")

plot(fit_both, mar = c(0,0.1,0,0))
text(fit_both)
summary(fit_both)


pred_both <- predict(fit_both, test_dat, type = "class")
correct(pred_both, test_dat$dec_o)



































