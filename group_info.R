names <- c("Donovan Tamkin", "Hyojeong (Jenny) Yoo", "Juanshu Wu",  "Muqing Fan", "Xiangyu (Alaric) Wang", "Yuchen Liu")
role <- c("undergrad", "psych grad 1st", "psych grad 1st", "psych grad 1st", "undergrad", "undergrad")
contact <- c("dtamkin","h4yoo", "juw1365", "mufan", "xiw404", "yul862")
covid_group <- data.frame(names, role, contact)

write.csv(covid_group,"C:\\Users\\fan\\Desktop\\covid_group.csv")

