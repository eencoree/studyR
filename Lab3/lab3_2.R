library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library (data.table)

data1 <- read.table("data1.txt")
new_names = data1$V1
df_t <- transpose(data1)
names(df_t) <- new_names
df_t <- df_t[-1, ]

data2 <- read.csv("data2.csv")
colnames(data2)[1] <- "Sample"
data <- merge(df_t, data2, all.x = TRUE, all.y=TRUE)

# удаляем столбцы с NAs
data <- data[ , colSums(is.na(data))== 0 ]
colich <- data[c(3,5,7),]
cach <- data[c(2, 4),]
colich_1 <- colich[1,]
colich_1 <- colich_1 %>% pivot_longer(names_to = "V", values_to = "val", V_0:V_100)
colich_1$val <- as.double(colich_1$val)
ggplot(colich_1, mapping = aes(x=val)) + geom_histogram(aes(y=..density..),binwidth=5, colour="black", fill="grey")+
  theme(axis.text.x = element_text(angle = 300)) + geom_density(alpha=.5, fill="red") + ggtitle("Height")

colich_2 <- colich[2,]
colich_2 <- colich_2 %>% pivot_longer(names_to = "V", values_to = "val", V_0:V_100)
colich_2$val <- as.double(colich_2$val)
ggplot(colich_2, mapping = aes(x=val)) + geom_histogram(aes(y=..density..),binwidth=5, colour="black", fill="grey")+
  theme(axis.text.x = element_text(angle = 300)) + geom_density(alpha=.5, fill="red") + ggtitle("Oil")

colich_3 <- colich[3,]
colich_3 <- colich_3 %>% pivot_longer(names_to = "V", values_to = "val", V_0:V_100)
colich_3$val <- as.double(colich_3$val)
ggplot(colich_3, mapping = aes(x=val)) + geom_histogram(aes(y=..density..),binwidth=5, colour="black", fill="grey")+
  theme(axis.text.x = element_text(angle = 300)) + geom_density(alpha=.5, fill="red") + ggtitle("Protein")


# Пусть количественная будет Oil, качественные: GrowthType, MaturType
new_names = cach$Sample
df <- transpose(cach)
names(df) <- new_names
row.names(df)<-colnames(cach)
df <- df[-1,]

df <- df%>% pivot_longer(names_to = "CACH", values_to = "val", GrowthType:MaturType)
ggplot(df, mapping = aes(x = val,y = CACH, fill = CACH)) +
  geom_col(position = 'identity',alpha=0.5) +
  ggtitle("GrowthType and MaturType")


ggplot(colich_2, mapping = aes(x=val)) + geom_histogram(aes(y=..density..),binwidth=5, colour="black", fill="grey")+
  theme(axis.text.x = element_text(angle = 300)) + geom_density(alpha=.5, fill="red") + ggtitle("Oil")
ggplot(colich_2, aes(x=val)) + geom_boxplot(colour="black")



