#Мисюрева Катя, 129 группа, вариант 3
#Регион 48, 
#для региона 48 рассчитайте урожайность 
#пшеницы в период с 2005 по 2015 год взяв для рассчета средние сумм
#активных температур за эти годы, с 8 ближайших метеостанций

rm(list=ls())
setwd("C:/kate_in_R")
getwd()
#install.packages("tidyverse")
#install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
#скачиваем станции 
station_data = ghcnd_stations()
#записываем в файл для последующей работы 
#write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")

#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,

#создав таблицу с именем региона и координатами его столицы
lipetsk = data.frame(id = "LIPETSK", latitude = 52.6059,  longitude = 39.5877)
lipetsk_around = meteo_nearby_stations(lat_lon_df = lipetsk, station_data = station_data, limit = 8, var = c("PRCP", "TAVG"), year_min = 2005, year_max = 2015)

#lipetsk_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их 

# удалленности от Липецка, очевидно что первым элементом таблицы будет идентификатор метеостанции Липетцка,

#его то мы и попытаемся получить
? meteo_nearby_stations

lipetsk_id = lipetsk_around[["LIPETSK"]][["id"]][1]

#получение всех данных с метеостанций

summary (lipetsk_id)

str(lipetsk_around)

all_lipetsk_data = meteo_tidy_ghcnd(stationid = lipetsk_id)

#2)чтобы получить таблицу всех метеостанций вокруг Липетцка нужно выбрать целиком первый объект из списка


lipetsk_table = lipetsk_around[[1]]

summary(lipetsk_table)

#в таблице lipetsk_table оказалось 8 объектов, ранжированных по расстоянию от Липецка

#нужно убедится, что этот список включает нужные по условию задачи метеостанции

lipetsk_stations = lipetsk_table 

str(lipetsk_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит


lipetsk_stations$id

# Создание цикла, в который загружаются необходимые данные с метеостанций 

# Промежуточный объект, куда скачиваются данные с конкретной метеостанции
all_lipetsk_data = meteo_tidy_ghcnd(stationid = lipetsk_id)

#посмотрим, что же скачивается
?meteo_tidy_ghcnd

summary(all_lipetsk_data)



#решим, какие из этих данных нам нужны

##нам нужны среднесуточные температуры (tavg) выше 10 за 2005-2015 гг.

###Нужно создать цикл, в котором бы скачивались нужные данные для всех метеостанций из созданного списка

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции


all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций

all_lipetsk_meteodata = data.frame()

#Цикл для всех метеостанций

for(i in 1:8)
  
{
  
  all_i = meteo_tidy_ghcnd(stationid = lipetsk_around[["LIPETSK"]][["id"]][i])
  
  
  
  #выберем нужные свойства
  
  all_i = all_i[ ,c("id","date","tavg")]
  
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном #этапах цикла
  
  print(all_i)
  
  all_lipetsk_meteodata=rbind(all_lipetsk_meteodata, all_i)
  
}
#Записываем полученные результаты

write.csv(all_lipetsk_meteodata,"all_lipetsk_meteodata.csv")


#2 часть

################## 4. Разбивка даты на составляющие(год, месяц, день года)

# считываем данные из файла all_lipetsk_meteodata.csv

all_lipetsk_meteodata = read.csv("all_lipetsk_meteodata.csv")
#посмотрим на данные

str(all_lipetsk_meteodata) 

#ищем библиотеку из tidyverse, которая может помочь с датой

library(lubridate)

# вытащить год

#проверим, что работает

y = year(all_lipetsk_meteodata$date); y

all_lipetsk_meteodata [,"year"]= year(all_lipetsk_meteodata$date)

#добавим месяц

all_lipetsk_meteodata [,"month"]= month(all_lipetsk_meteodata$date)

#вытащить день от начала года

all_lipetsk_meteodata [,"day_of_the_year"]= yday(all_lipetsk_meteodata$date)

#проверим результат

str(all_lipetsk_meteodata)

#отфильтруем данные за 2005-2015

years_lipetsk_meteodata = filter (all_lipetsk_meteodata, year > 2005 & year < 2015 )

#проверим результат

str(years_lipetsk_meteodata)

summary (years_lipetsk_meteodata)

################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц

#Изучаем формулу и видим, что единственное, что нужно расчитать

#- это сумму температур больше 10 град. по месячно, остальное в формуле- константы



#### 1. температурy нужно поделить на 10

years_lipetsk_meteodata[,"tavg"]= years_lipetsk_meteodata$tavg / 10

summary (years_lipetsk_meteodata)

#### 2. Превратим в нули все NA и где tavg больше 10 градусов



years_lipetsk_meteodata [is.na(years_lipetsk_meteodata$tavg), "tavg"] = 0

years_lipetsk_meteodata [years_lipetsk_meteodata$tavg<10, "tavg"] = 0

years_lipetsk_meteodata [years_lipetsk_meteodata$tavg>27, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 10 градусов

summary(years_lipetsk_meteodata)

#### 3. суммарная температура за месяц за 10 лет для всех станций

# группирую по метеостанциям, годам и месяцам

alldays= group_by(years_lipetsk_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным

#с помощью функции group_by

#просуммирую температуру по этим группам с помощью sum

sumT_alldays_lipetsk = summarize(alldays, tsum = sum(tavg))

#Получилось - все года, все месяца присутствуют

# максимальная суммарная температура за месяц 29.85

summary(sumT_alldays_lipetsk)


#Сгруппирем данные по месяцам

groups_lipetsk_months = group_by(sumT_alldays_lipetsk,month)

groups_lipetsk_months

#найду для всех метеостанций и ВСЕХ лет среднее по месяцам

sumT_months= summarize(groups_lipetsk_months , St = mean(tsum))

sumT_months

################## 6. Подготовка к расчету по формуле Урожая

### Ввод констант

afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)

bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)

di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)

y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;

Kf = 300 # - коэффициент использования ФАР посевом;

Qj = 1600 # - калорийность урожая культуры;

Lj = 2.2 # - сумма частей основной и побочной продукции;

Ej = 25 # - стандартная влажность культуры;

# Рассчитаем Fi по месяцам

#Fi= afi+bfi∗y∗(St>10℃)

sumT_months = mutate(sumT_months, Fi= afi+bfi*y*St)

#Рассчитаем Yi

sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

## Расчитываем урожай как сумму по месяцам и думаем разумный ли он

Yield = sum(sumT_months$Yi)

Yield

# Ответ: 17.62 ц/га  


