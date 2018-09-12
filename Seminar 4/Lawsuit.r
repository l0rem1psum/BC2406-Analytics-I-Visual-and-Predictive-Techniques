#Clean up workspace
rm(list = ls())
gc(verbose = FALSE)


#Install (if not installed) and attach packages
if (!require(data.table)){install.packages("data.table")}
require(data.table)
if (!require(gridExtra)){install.packages("gridExtra")}
require(gridExtra)
if (!require(devtools)){install.packages("gridExtra")}
require(devtools)
install_github('cttobin/ggthemr')#The package "ggthemr" is still under development
require(ggthemr)


#Set theme for the plot
ggthemr("light")


#Load and clean data
lawsuit.dt <- fread("Lawsuit.csv")
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept, levels = 1:6, labels = c("Biochemistry", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery"))
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender, levels = c(0, 1), labels = c("Female", "Male"))
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin, levels = c(0, 1), labels = c("Research emphasis", "Clinical emphasis"))
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert, levels = c(0, 1), labels = c("Not board certified", "Board certified"))
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank, levels = 1:3, labels = c("Assistant", "Associate", "Full Professor"))


#Create a summarzied data table
lawsuit.summarized <- lawsuit.dt[,.(mean.94 = mean(Sal94), mean.95 = mean(Sal95), mean.prate = mean(Prate), mean.exper = mean(Exper)), by = c("Dept", "Rank", "Gender")]


#Plot graph of average salary (by gender) across each department according to rank.
mean.94 <- ggplot(lawsuit.summarized, aes(Dept, mean.94, color = Gender)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Gender)) + facet_grid(.~Rank) + xlab("Departments") + ylab("Mean salary in 1994")
mean.95 <- ggplot(lawsuit.summarized, aes(Dept, mean.95, color = Gender)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Gender)) + facet_grid(.~Rank) + xlab("Departments") + ylab("Mean salary in 1995 (after increment)")
grid.arrange(mean.94,mean.95, nrow=2)

#Plot graph of publication rate and years after obtaining MD (by gender) across each department according to rank.
mean.Exper <- ggplot(lawsuit.summarized, aes(Dept, mean.exper, color = Gender)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Gender)) + facet_grid(.~Rank) + xlab("Departments") + ylab("Average years since obtaining MD")
mean.Prate <- ggplot(lawsuit.summarized, aes(Dept, mean.prate, color = Gender)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Gender)) + facet_grid(.~Rank) + xlab("Departments") + ylab("Average publication rate")
grid.arrange(mean.Exper, mean.Prate, nrow = 2)


#Reset to ggplot2's default setting
ggthemr_reset()