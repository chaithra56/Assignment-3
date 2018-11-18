##I have removed the install.packages("tidyverse"), install.packages("iNEXT") and install.packages("dplyr") from the original script. I have done this as it is not needed. Also, dplyr is a part of the tidyverse group of packages, so it does not need to be loaded separately. 

###In part C module, I have choosen the biodiversity of mammalian taxonomic group. It has different orders and im interested in records of counntry and the number of orders in the data set, to reduce the selected data set and plot graph of reduced data.
library(tidyverse)

Assignmentdata <- read.delim("bold_data.txt")
#to check records per country
countries.count <-Assignmentdata %>%
  group_by(country) %>%
  summarize(count_records2 = length(processid)) %>%
  arrange(desc(count_records2)) %>%
  print()
#plot a histogram of the order_taxID
hist(Assignmentdata$order_taxID)
#to reduce the families
top.families <- Assignmentdata %>% #Changed notation from Assignment1data to Assignmentdata as Assignment1data did not exist. 
  group_by(family_name) %>%
  mutate(count_by_family = length(processid)) %>%
  filter(count_by_family > 120 )
#to check remaining family records
length(top.families$family_name)
#tocheck unique family name
unique(top.families$family_name)
#plot graph of family and count of family
plot(as.factor(top.families$family_name), top.families$count_by_family)
#using vegan package
library(vegan)
comm <- Assignmentdata %>%
  group_by(bin_uri) %>%
  count(bin_uri)
commAssign1 <- specaccum(comm = n, method = "exact", permutations = 100, conditioned = TRUE)

commAssign1 <- spread(comm, bin_uri, n)
#using iNext package

library(iNEXT)
g <- ggplot2::aes(top.families$family_name, top.families$count_by_family)
g1 <- ggplot2::aes(Assignmentdata$species_name, Assignmentdata$species_taxID)
g1

#####the data set is interpreted by checking the records of the country and  plotted the histogram to  analyze the qunatity of different orders. so with the function histogram mostly two orders are in high frequency.The data is reduced and filtered which has more 120 families. Vegan package and inext package are used to plot a graph.
