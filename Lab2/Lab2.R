data <- read.csv("Payment_and_value_of_Care-Hospital.csv", header = TRUE)
names(data)
data$Payment
data$Payment.Category
# очищаем от больницы без оценок и несостоятельных ячеек
clean_data <- subset(data, data$Payment != "Not Available")

#убираем знак доллара и запятые из чисел
clean_data$Payment <- as.numeric(gsub("[$,]","",clean_data$Payment))
clean_data$Lower.estimate <- as.numeric(gsub("[$,]", "", clean_data$Lower.estimate))
clean_data$Higher.estimate <- as.numeric(gsub("[$,]", "", clean_data$Lower.estimate))
head(clean_data$Payment)

# возможные услуги 
services <- unique(clean_data$Value.of.Care.Display.Name)
i = 1
hosps <- list()
for (elem in services){
  illness <- subset(clean_data, clean_data$Value.of.Care.Display.Name == elem)
  rec <- subset(illness, illness$Payment == min(illness$Payment))
  hosps[[i]] <- subset(rec, select=c("Facility.Name", "State", "County.Name", "City", "Payment"))
  cat("Показатели для ", elem,"\n")
  print(hosps[i])
  i <- i + 1
}

