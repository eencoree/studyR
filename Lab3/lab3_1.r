library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
load("ExpImp.RData")
data <- ExpImp
flag_s <- 0
# преобразуем в тип double, убираем NA
for (i in 2:length(names(data))) {
  data[[i]] <- as.double(data[[i]])
}
data <- drop_na(data)

district <- grep('федеральный округ', data$Регион)
data <- data[-c(17:74), ]

filt <- !str_detect(data$Регион, 'Российская Федерация|федеральный округ')
data <- filter(data, filt)

export <- select_at(data, vars(matches("Экспорт")))
import <- select_at(data, vars(matches("Импорт")))

export$Total_Sum <- rowSums(export, na.rm = TRUE)
import$Total_Sum <- rowSums(import, na.rm = TRUE)
dif <- export$Total_Sum - import$Total_Sum
data_2 <- data

g_plot <- function(flag_s){
  if (flag_s == 1) data <- data_2
  if (flag_s==0){
    data$СумЭкспорт <- export$Total_Sum
    data$СумИмпорт <- import$Total_Sum
    data$Разница <- dif
    data <- data[,c("Регион", "СумЭкспорт", "СумИмпорт","Разница")]
  }else{
    data$ОбщСум <- export$Total_Sum + import$Total_Sum
    data <- data[,c("Регион", "ОбщСум")]
  }
if (flag_s == 0){
  data <- data%>%pivot_longer(names_to = "Экспорт+Импорт", values_to = "млн. $", СумЭкспорт:СумИмпорт)
  region_sum <- data %>% group_by(Регион, `Экспорт+Импорт`)%>% summarise(total_sum = sum(`млн. $`), total_dif = `Разница`, .groups = 'drop')
  region_sum |>
    ggplot(mapping = aes(x = Регион, y = total_sum, fill = `Экспорт+Импорт`)) +
    geom_col(color = 'black', size = 0.3, position = 'stack',alpha=0.5) +
    ggtitle("Разница между импортом и экспортом")+ 
    ylab('млн. $') + coord_flip() +
    geom_text(aes(y=300000, label = `total_dif`))
}else{
  data |>
    ggplot(mapping = aes(x = Регион, y = `ОбщСум`, fill = `ОбщСум`)) +
    geom_col(color = 'black', size = 0.3, alpha=1) +
    ggtitle("Общее сумма импорт + сумма экспорт") + 
    ylab('млн. $') + coord_flip()
} 
}

g_plot(0)
g_plot(1)
