library(arules)
library(arulesViz)
data(Adult)
Adult_rule<-apriori(Adult,parameter = list(support=0.002, confidence=0.5))
Adult_rule
inspect(head(sort(Adult_rule,by="lift"),2))
plot(Adult_rule,jitter=0)
subrules=Adult_rule[quality(Adult_rule)$confidence>0.8];
subrules
plot(subrules,jitter=0)