
# Read the data:   
data = read.table("titanic.csv",sep=",",header = TRUE)
#View data
View(data)

# did passengers who paid more survived more/?
ggplot(data,aes(x=sex,y=fare))+geom_boxplot()+scale_y_log10()+facet_wrap(~survived)
# demographics of children who survived more male child or female child ?
ggplot(data,aes(x=sex,y=age))+geom_violin()+facet_wrap(~survived)
#did females in 3rd class died the most
ggplot(data,aes(x=sex,y=pclass))+geom_violin()+facet_wrap(~survived)
