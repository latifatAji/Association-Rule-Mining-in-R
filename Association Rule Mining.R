##                                     ##
##          Association Rules          ##
##                                     ##
##### -------------------------------####
require(arules)
require(arulesViz)
require(RColorBrewer)
write.csv(retail_df, "retail_data.csv", quote = FALSE, row.names = TRUE)

## Import Data as Transaction
itembaskets <- read.transactions('retail_data.csv', format = 'basket', sep = ',')
class(itembaskets)
itembaskets
dim(itembaskets)
itemsize <- size(itembaskets)
summary(itemsize)
quantile(itemsize, probs = seq(0, 1, 0.1))
ggplot(data.frame(count = itemsize)) +
  geom_density(aes(x = count), binwidth = 1) +
  scale_x_log10()
itemfreq <- itemFrequency(itembaskets); summary(itemfreq)
sum(itemfreq)
itemcount <- (itemfreq/sum(itemfreq)) * sum(itemsize)
summary(itemcount)
ordereditem <- sort(itemcount, decreasing = T)
ordereditem[1:10]

itemFrequencyPlot(itembaskets, topN=20, type="absolute", main="Item Frequency Plot",
                  col = brewer.pal(8,'Pastel2'))
rules <- apriori(itembaskets, parameter = list(supp=0.001, conf=0.8,maxlen=10))
sorted_rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(sorted_rules)
inspect(sorted_rules[1:10])
redundant_rules <- is.redundant(sorted_rules, measure = 'confidence')
final_rules <- sorted_rules[!redundant_rules]
inspect(final_rules[1:10])
top_10_rules <- head(final_rules, n = 10, by = "confidence")
plot(final_rules)
plot(final_rules, engine = "plotly")
plot(final_rules[1:20], method = "graph")
plot(top_10_rules, method = "grouped")
plot(final_rules,method="two-key plot")

plot(top_10_rules, method="graph", control=list(type="itemsets"), itemLabels=FALSE)
plot(final_rules[1:20], method="graph", control=list(layout=igraph::in_circle()))
## Find coffee rules
# Finding interesting rules-1
rules_coffee <- apriori(itembaskets,parameter = list( maxlen=10,supp=.001, conf=.80),appearance=list(rhs=c("COFFEE"),default="lhs"))
inspect(rules_coffee[1:10])
