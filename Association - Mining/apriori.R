# 
# Arules can help in identifying:
#   - recurring purchasing patterns(fast mover)
#   - complement and substitute products(substitude product for perticular product)
#   - trigger products, buying of which may lead to buying another product with high degree of certainty.
#           (bread triggering the butter)      

# As such, arules might be helpful in:
#   - planning inventory
#   - designing "combo offers"
#   - planning discount programs (discount one product, markup complements)
#   - planning shelf and catalog layout
#   - recommending products and crosselling (especially higher margin products).(along with tea ,chakali is also sold :cross silling)
 # 
# (we use read.transaction to read the transaction data )

library(Matrix)
library(arulesViz)
library("RColorBrewer")
library(arulesSequences)
data("Groceries")
View(Groceries)
summary(Groceries)

#Summary of above may look like :

# there were 9'835 transactions altogether
# 169 different items were bought during the month
# the most frequently bought item was "whole milk": 2'513 purchases
# there were 2'159 single item baskets, the biggest basket included 32 items 
# median basket included 3 items; mean had 4.4 items.

itemLabels(Groceries)

itemFrequencyPlot(Groceries,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="absolute",
                          ylab="Item Frequency (Absolute)")


#Alternatively, we might show least frequently bought items:

barplot(sort(table(unlist(LIST(Groceries))))[1:10],
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, absolute')


#Sometimes it might be interesting to explore data as a simple contingency table.

tbl <- crossTable(Groceries)

#restricting values
tbl[100:105,100:105]

#By default, the table shows absolute counts,
tbl['whole milk','soda']

tbl['whole milk','flour']
tbl['whole milk','flour','soda'] #(only 2 products we can analyse) 

#by sorting with no.
tbl <- crossTable(Groceries, sort=TRUE)
tbl[1:6,1:6]


#Aprori Implemenation:
# 
# # count. Number of times a particular item, or itemset,
# is encountered in the transactions database. 
# # Count for the "whole milk", e.g. is 2513.
# # 
# # support. Support of an item, or an itemset consisting of several items, 
# is frequency of occurrence of a specific item. Support (or frequency) 
# is obtained as count divided by number of transactions. 
# Support for "whole milk' e.g. is 2513/9835 = 0.2555. 
# As a rule, but not necessarily, we want items/itemsets with high support, 
# as high frequency would ensure that our potentially valuable finding (i) 
# is not due to chance 
# 
# # Confidence {A} => {B}. Confidence is probability of purchase B,
# given purchase A happened (in humane language this is conditional probability P(B|A).
#                            For recommending a good rule we prefer higher confidence.
#                            
# 
# # lift. Lift defined as P(AB)P(A)???P(B) and shows how more often the rule under questions
#   happens than if it did simply happen by chance.  Lift defined both for itemsets and 
#   rules. In general, we prefer higher lift over lower lift.
#   The lift value of an association rule is the ratio of the 
#   confidence of the rule and the expected confidence of the rule. 
#The expected confidence of a rule is defined as the product of the support values of the rule body and the rule head divided by the support of the rule body.
#crossTable() function allows showing and sorting items and pairs of items by:
#   
# count
# support
# lift

#crossTable(Groceries, measure='lift',sort=T)[1:5,1:5]

#with mining most frequent itemsets of minimum length equal 2, and frequency of occurrence at least 1 in 1000, i.e. support=.001


itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     confidence =0.6,
                                     minlen=3,
                                     maxlen =10
                                     # to mine for itemsets
                    ))
summary(itemsets)

#We see based on prespecified support 13'335 itemsets of maximum length 6 were built out of 157 items (12 were thrown out due to rarity).
# 
# Support for itemsets is calculated by default, so we can sort by it and print out top itemsets:

inspect(sort(itemsets, by='support', decreasing = T)[190:220])
inspect(sort(itemsets, by='confidence', decreasing = T)[190:220])

# sort(), as implied by name, sorts itemsets and rules by measure specified
# in by=... argument. Usually one sorts by support, lift,  confidence, 
# or any other measure, that could be calculated with interestMeasure() function.
# - inspect() is a command that prints out rules or itemsets of interest.
# 
# # Should we want to add lift and show top 5 results by lift, 
# we may proceed as follows:

quality(itemsets)$lift <- interestMeasure(itemsets, measure='lift', Groceries)
inspect(sort(itemsets, by ='lift', decreasing = T)[1:15])

#The figures above explain very well why fruits and vegetables departments are often found next to diary departments.

itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     minlen=2,
                                     maxlen=5,
                                     target='frequent' # to mine for itemsets
                    ))


summary(itemsets)


quality(itemsets)$lift <- interestMeasure(itemsets, measure='lift', Groceries)
inspect(sort(itemsets, by ='lift', decreasing = T)[1:10])


#When we switch to searching rules we need to change target='frequent itemsets' to target = 'rules'. As well, we should specify  confidence (unless we are satisfied with default confidence=0.8)

rules <- apriori(Groceries,
                 parameter = list(support=.001,
                                  confidence=.5,
                                  minlen=2,
                                  target='rules' # to mine for rules
                 ))
summary(rules)

inspect(sort(rules, by='lift', decreasing = T)[1:5])

#Subsetting rules and itemsets
rules <- apriori(Groceries,
                 parameter = list(support=.001,
                                  confidence=.7,
                                  maxlen=5,
                                  target='rules' # to mine for rules
                 ))
inspect(rules[1:10])

inspect(sort(rules, by="support", decreasing = T)[645:655])

# The power of subset function can be shown by choosing the following subset:
#   
#   rhs should be 'bottled beer'
# confidence should be above .7
# results should be sorted by lift

inspect(sort(subset(rules,
                    subset=rhs %in% 'whole milk' & confidence > .7),
             by = 'lift',
             decreasing = T))

 inspect(subset(canned_rules,
               subset=lhs %ain% c("liquor", "red/blush wine") & rhs %in% 'canned beer' ))

#By now, we must be well aware of the fact that people buying "liquor" and "red wine" are almost certain to buy "bottled beer" (9 times out of 10), but not "canned beer":

#Both "whole milk" and "yogurt" must be present and rule's confidence must be higher than .95
inspect(subset(rules, subset=items %in% c("soda","yogurt") & confidence >.95))

#where milk is the impact
milk.association.rules <- apriori(Groceries, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="whole milk"))
inspect(head(milk.association.rules))

#where milk is the influencer
milk.association.rules <- apriori(Groceries, parameter = list(supp=0.001, conf=0.08),appearance = list(lhs="whole milk",default="rhs"))
inspect(head(milk.association.rules))

#Adding interactivity to graph.
plot(rules, engine='interactive')
