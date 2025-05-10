
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(tibble)
library(fixest)  
library(purrr)
library(car)




############################## ФИЛЬТРАЦИЯ #############################
# Задаём категории продуктов питания
food_categories <- c(
  "Молочные продукты",
  "Рыба/Морепродукты",
  "Хлебобулочные изделия",
  "Макароны/Крупы",
  "Каши/Мюсли/Хлопья",
  "Овощи/Фрукты/Грибы",
  "Мясо",
  "Чай/Кофе/Какао",
  "Приправы/Соусы",
  "Сладости/К чаю",
  "Консервация",
  "Безалкогольные напитки",
  "Заморозка/Готовая еда"
)
# отбираем только наименования из пищевых категорий
food_products <- Categories %>%
  filter(Категория %in% food_categories)

# фильтруем только нужные товары в датасете цен и присоединяем категории
filtered_data <- Prices %>%
  filter(Наименование %in% food_products$Наименование) %>%
  left_join(food_products, by = "Наименование") %>%
  filter(Ритейлер %in% c("Auchan", "Lenta", "Magnit", "Perekrestok", "Pyaterochka"))


# Создание логической переменной "Соц_значимость" на основе ключевых признаков в наименовании товара

# Определение словаря ключевых слов для отнесения товаров к социально значимым категориям
keywords <- list(
  "Говядина" = c("говядина", "говяжье", "говяжий"),
  "Баранина" = c("баранина", "баранье", "бараний"),
  "Куры" = c("куры", "куриный", "цыпленок", "курица"),
  "Рыба мороженая неразделанная" = c("рыба"),
  "Масло сливочное" = c("масло сливочное", "сливочное масло"),
  "Масло подсолнечное" = c("масло подсолнечное", "подсолнечное масло"),
  "Молоко питьевое" = c("молоко питьевое", "питьевое молоко", "молоко"),
  "Яйца куриные" = c("яйца куриные", "куриные яйца", "яйцо "),
  "Сахар-песок" = c("сахар-песок", "сахар"),
  "Соль поваренная пищевая" = c("соль поваренная", "пищевая соль"),
  "Чай черный байховый" = c("чай черный", "черный чай", "байховый"),
  "Мука пшеничная" = c("мука пшеничная", "пшеничная мука"),
  "Хлеб ржаной, ржано-пшеничный" = c("хлеб ржаной", "ржаной хлеб", "ржано-пшеничный"),
  "Хлеб и булочные изделия из пшеничной муки" = c("хлеб пшеничный", "булочки пшеничные"),
  "Рис шлифованный" = c("рис шлифованный", "шлифованный рис"),
  "Пшено" = c("пшено"),
  "Крупа гречневая - ядрица" = c("гречневая крупа", "гречка", "ядрица"),
  "Вермишель" = c("вермишель"),
  "Картофель" = c("картофель", "картошка"),
  "Капуста белокочанная свежая" = c("капуста белокочанная", "капуста свежая"),
  "Лук репчатый" = c("лук репчатый", "репчатый лук"),
  "Морковь" = c("морковь"),
  "Яблоки" = c("яблоки", "яблоко")
)

# Определение перечня слов-исключений (для удаления продуктов с похожими названиями, но другим назначением)
exclude_words <- c(
  "майонез", "колбаса", "рулет", "фарш", "бульон", "кетчуп",
  "сахарная", "копчено", "копченый", "копченое", "холодец", "бекон",
  "приправа", "смесь", "смеси", "соус", "соусы", "соусный",
  "сыр", "сырок", "сырки", "йогурт", "творог", "творожный",
  "желатин", "язык", "хлебцы", "колбаски", "сосиски", "сардельки",
  "карбонад", "какао", "набор", "роллтон", "отбор", "ваниль",
  "мюсли", "сгущ", "кокос", "каша", "хлопья", "слойка", "пюре",
  "десерт", "мороженое", "печенье", "конфеты", "выпечка", "пирог",
  "крем", "пудинг", "кексы", "батончик", "снеки", "чипсы", "чизкейк",
  "запеканка", "пельмени", "вареники", "замороженное", "Actimel"
)

# Формируем регулярные выражения для исключающих слов
exclude_pattern <- paste(exclude_words, collapse = "|")

# Создание новой логической переменной "Соц_значимость" с изначальным значением FALSE
filtered_data <- filtered_data %>%
  mutate(Соц_значимость = FALSE)

# Цикл по каждой товарной группе из словаря social_keywords
for (product in names(keywords)) {
  
  # Объединение всех ключевых слов в регулярное выражение
  pattern <- paste(keywords[[product]], collapse = "|")
  
  # Пометка строк как социально значимых, если:
  # - строка еще не была помечена
  # - содержит ключевые слова (с учетом регистра)
  # - не содержит ни одного исключающего слова
  filtered_data <- filtered_data %>%
    mutate(Соц_значимость = if_else(
      Соц_значимость == FALSE &
        str_detect(Наименование, regex(pattern, ignore_case = TRUE)) &
        !str_detect(Наименование, regex(exclude_pattern, ignore_case = TRUE)),
      TRUE,
      Соц_значимость
    ))
}

# Добавление переменной "Соц_категория" — наименование конкретной подкатегории
# (например: "Картофель", "Говядина", "Яйца куриные" и т.д.)

# Сначала создаем переменную с пропущенными значениями
filtered_data <- filtered_data %>%
  mutate(Соц_категория = NA_character_)

# Затем в цикле проходим по словарю ключевых слов и присваиваем подкатегории
for (product in names(keywords)) {
  pattern <- paste(keywords[[product]], collapse = "|")
  
  filtered_data <- filtered_data %>%
    mutate(
      Соц_категория = if_else(
        Соц_значимость == TRUE &                            # если уже помечено как соц.значимое
          is.na(Соц_категория) &                               # и категория ещё не задана
          str_detect(Наименование, regex(pattern, ignore_case = TRUE)),  # и подходит по шаблону
        product,                                             # присваиваем текущую категорию
        Соц_категория                                        # иначе оставляем как есть
      )
    )
}

############################## ОБЪЕДИНЕНИЕ ДАТАСЕТОВ #############################

# Присоединение справочника городов (Город → Субъект)
filtered_data <- filtered_data %>%
  left_join(Города, by = "Город")

# Присоединение календаря (по номеру недели)
filtered_data <- filtered_data %>%
  left_join(Dates, by = "Неделя") %>%
  mutate(
    Начало.недели = as.Date(Начало.недели),
    Год = as.numeric(format(Начало.недели, "%Y")),
    Месяц = as.numeric(format(Начало.недели, "%m"))
  )

# Преобразуем Год в числовой формат во всех справочниках
Авто_на_тыс_чел       <- Авто_на_тыс_чел       %>% mutate(Год = as.numeric(Год))
Доходы_по_субъектам   <- Доходы_по_субъектам   %>% mutate(Год = as.numeric(Год))
Зарплаты_скорр_       <- Зарплаты_скорр_       %>% mutate(Год = as.numeric(Год))
Уровень_бедности      <- Уровень_бедности      %>% mutate(Год = as.numeric(Год))
Цены_на_топливо       <- Цены_на_топливо       %>% mutate(Год = as.numeric(Год))

# Приводим названия месяцев в зарплатах к заглавному виду
Зарплаты_скорр_ <- Зарплаты_скорр_ %>%
  mutate(Месяц = stringr::str_to_title(Месяц))

# Заменяем числовой месяц на словесный в основном датафрейме
month_names <- c("1" = "Январь", "2" = "Февраль", "3" = "Март", "4" = "Апрель",
                 "5" = "Май", "6" = "Июнь", "7" = "Июль", "8" = "Август",
                 "9" = "Сентябрь", "10" = "Октябрь", "11" = "Ноябрь", "12" = "Декабрь")
filtered_data <- filtered_data %>%
  mutate(Месяц = month_names[as.character(Месяц)])

# Присоединяем внешние источники по субъекту и году/месяцу
filtered_data <- filtered_data %>%
  left_join(Авто_на_тыс_чел %>% rename(Cars = Значение),
            by = c("Субъект", "Год")) %>%
  left_join(Доходы_по_субъектам %>% rename(RegionIncome = `Среднедушевые денежные доходы, руб.`),
            by = c("Субъект" = "Область", "Год")) %>%
  left_join(Зарплаты_скорр_ %>% select(Субъект, Год, Месяц, `Реальная зп`),
            by = c("Субъект", "Год", "Месяц")) %>%
  left_join(Розничные_объекты %>% rename(Competition = `Количество объектов розничной торговли`),
            by = c("Субъект", "Год")) %>%
  left_join(Уровень_бедности %>% rename(Poverty = `Уровень бедности (% за год)`),
            by = c("Субъект", "Год")) %>%
  left_join(Цены_на_топливо %>%
              select(Субъект, Год, `Дизельное топливо, руб/л`) %>%
              rename(Diesel = `Дизельное топливо, руб/л`),
            by = c("Субъект", "Год"))

# Расчёт квинтилей дохода (по каждому году отдельно)
filtered_data <- filtered_data %>%
  group_by(Год) %>%
  mutate(IncomeQuintile = ntile(RegionIncome, 5)) %>%
  ungroup()

# Удаляем лишние переменные с датами
filtered_data <- filtered_data %>%
  select(-Начало.недели, -Конец.недели)

# Создаём переменную EffectivePrice: фактическая цена с учётом скидки
filtered_data <- filtered_data %>%
  mutate(EffectivePrice = ifelse(!is.na(`Цена.со.скидкой`), `Цена.со.скидкой`, `Цена.без.скидки`)) %>%
  select(-`Цена.со.скидкой`)

# Финальное переименование переменных в соответствии с модельной структурой
filtered_data <- filtered_data %>%
  rename(
    Product       = Наименование,
    Retailer      = Ритейлер,
    City          = Город,
    Week          = Неделя,
    Price         = `Цена.без.скидки`,
    Region        = Субъект,
    Year          = Год,
    Month         = Месяц,
    Wages         = `Реальная зп`,
    Income        = RegionIncome
  )

############################## ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ #############################

# Таблица 1: Описательные статистики по социально-экономическим характеристикам регионов
# Русские названия переменных
var_labels <- c(
  Income = "Среднедушевые доходы, руб.",
  Poverty = "Уровень бедности, %",
  Competition = "Объекты розничной торговли, ед.",
  Diesel = "Цена дизеля, руб./л",
  Wages = "Реальная заработная плата, руб.",
  Cars = "Автомобили на 1000 чел."
)

desc_table <- filtered_data %>%
  summarise(across(
    c(Income, Poverty, Competition, Diesel, Wages, Cars),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(., na.rm = TRUE),
      min  = ~min(., na.rm = TRUE),
      max  = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Показатель", "Статистика"),
               names_sep = "_",
               values_to = "Значение") %>%
  pivot_wider(names_from = Статистика, values_from = Значение) %>%
  mutate(Показатель = recode(Показатель, !!!var_labels)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  select(Показатель, mean, sd, min, max) %>%
  rename(
    `Среднее` = mean,
    `Ст. отклонение` = sd,
    `Минимум` = min,
    `Максимум` = max
  )

# Таблица 2. Структура выборки: количество уникальных наблюдений и доля социально значимых товаров

# Считаем основные метрики структуры датасета
table2 <- tibble(
  `Показатель` = c(
    "Всего наблюдений",
    "Уникальные города",
    "Уникальные регионы",
    "Уникальные недели",
    "Уникальные ритейлеры",
    "Уникальные товары",
    "Уникальные категории",
    "Доля социально значимых товаров"
  ),
  `Значение` = c(
    nrow(filtered_data),
    n_distinct(filtered_data$City),
    n_distinct(filtered_data$Region),
    n_distinct(filtered_data$Week),
    n_distinct(filtered_data$Retailer),
    n_distinct(filtered_data$Product),
    n_distinct(filtered_data$Категория),
    round(mean(filtered_data$Соц_значимость, na.rm = TRUE), 3)
  )
)

# Таблица 3. География охвата торговых сетей 

table3 <- filtered_data %>%
  group_by(Retailer) %>%
  summarise(
    `Число регионов` = n_distinct(Region),
    `Число городов` = n_distinct(City),
    `Число наблюдений` = n()
  ) %>%
  arrange(desc(`Число наблюдений`))

# Таблица 4 (расширенная): по категориям с описательной статистикой цен

table4 <- filtered_data %>%
  group_by(Категория) %>%
  summarise(
    `Число наблюдений` = n(),
    `Доля от выборки` = round(n() / nrow(filtered_data), 3),
    `Доля СЗТ в категории` = round(mean(Соц_значимость, na.rm = TRUE), 5),
    `Средняя цена` = round(mean(EffectivePrice, na.rm = TRUE), 2),
    `Медиана цены` = round(median(EffectivePrice, na.rm = TRUE), 2),
    `Мин. цена` = round(min(EffectivePrice, na.rm = TRUE), 2),
    `Макс. цена` = round(max(EffectivePrice, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(`Число наблюдений`))


# Таблица 5: охват по регионам
coverage_by_region <- filtered_data %>%
  group_by(Region) %>%
  summarise(
    n_cities = n_distinct(City),
    n_products = n_distinct(Product),
    n_categories = n_distinct(Категория),
    n_retailers = n_distinct(Retailer),
    n_obs = n()
  ) %>%
  arrange(desc(n_obs))

# Таблица 6: соц-экон характеристики по регионам
socio_by_region <- filtered_data %>%
  group_by(Region) %>%
  summarise(
    avg_income = mean(Income, na.rm = TRUE),
    avg_poverty = mean(Poverty, na.rm = TRUE),
    avg_wages = mean(Wages, na.rm = TRUE),
    avg_cars = mean(Cars, na.rm = TRUE),
    avg_competition = mean(Competition, na.rm = TRUE),
    avg_diesel = mean(Diesel, na.rm = TRUE)
  )

library(openxlsx)
write.xlsx(list(Coverage = coverage_by_region, Socio = socio_by_region),
           file = "region_summary.xlsx", overwrite = TRUE)


# Строим вспомогательную модель для оценки VIF
vif_raw_model <- lm(
  EffectivePrice ~ Cars + Wages + Competition + Diesel + Poverty,
  data = filtered_data
)

# Считаем VIF
vif(vif_raw_model)

unique(filtered_data$Соц_категория)



############################## МОДЕЛИРОВАНИЕ #############################

# Базовая модель зависимости эффективной цены от уровня бедности региона 
model1 <- feols(
  log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) | 
    Product + Retailer + Week,
  data = filtered_data,
  cluster = ~Region
)
summary(model1)

# Уточняющая модель зависимости эффективной цены от среднего дохода региона
model_income <- feols(
  log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) | 
    Product + Retailer + Week,
  data = filtered_data,
  cluster = ~Region
)
summary(model_income)

# КАТЕГОРИИ

# Вся выборка
# Группируем данные по переменной "Категория" 
# Строим регрессию по каждой категории с кластеризацией
models_by_cat <- filtered_data %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Просмотр результатов для каждой категории
models_by_cat %>% 
  mutate(model_summary = map(model, summary)) %>% 
  select(Категория, model_summary)

# Выводим summary по очереди
for (i in seq_len(nrow(models_by_cat))) {
  cat("\n\n=== Категория:", models_by_cat$Категория[i], "===\n")
  print(summary(models_by_cat$model[[i]]))
}

# СЗТ
# Строим регрессии по каждой категории только для СЗТ
models_by_cat_szt <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория:", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}

#  НЕ СЗТ
# Строим регрессии по каждой категории только для неСЗТ
models_by_cat_nonszt <- filtered_data %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория:", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}



# СЗТ БАЗОВАЯ ЦЕНА
# Строим регрессии по каждой категории только для СЗТ
models_by_cat_szt <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(Price) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Просмотр моделей
models_by_cat_szt %>% 
  mutate(model_summary = map(model, summary)) %>% 
  select(Категория, model_summary)

# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория:", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}

#  НЕ СЗТ БАЗОВАЯ ЦЕНА
# Строим регрессии по каждой категории только для неСЗТ
models_by_cat_nonszt <- filtered_data %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(Price) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория:", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}


# Модели среди СЗТ (по переменной "Соц_категория") БЕДНОСТЬ
models_by_subcat_szt_eff <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Соц_категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )


# Выводим summary для каждой соц. категории среди СЗТ
for (i in seq_len(nrow(models_by_subcat_szt_eff))) {
  cat("\n\n=== СЗТ / Соц. категория:", models_by_subcat_szt_eff$Соц_категория[i], "===\n")
  print(summary(models_by_subcat_szt_eff$model[[i]]))
}


# Модели среди СЗТ (по переменной "Соц_категория") ДОХОД
models_by_subcat_szt_eff <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Соц_категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income)+ log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )


# Выводим summary для каждой соц. категории среди СЗТ
for (i in seq_len(nrow(models_by_subcat_szt_eff))) {
  cat("\n\n=== СЗТ / Соц. категория:", models_by_subcat_szt_eff$Соц_категория[i], "===\n")
  print(summary(models_by_subcat_szt_eff$model[[i]]))
}

# Модели в разрезе Ритейлеров (БЕДНОСТЬ)

models_ret <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      cluster = ~Region
    ))
  )


# Выводим summary для каждой сети
for (i in seq_len(nrow(models_ret))) {
  cat("\n\n=== Ретейлер:", models_ret$Retailer[i], "===\n")
  print(summary(models_ret$model[[i]]))
}



# Модели в разрезе Ритейлеров (Доход)

models_ret <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      cluster = ~Region
    ))
  )



# Выводим summary для каждой сети
for (i in seq_len(nrow(models_ret))) {
  cat("\n\n=== Ретейлер:", models_ret$Retailer[i], "===\n")
  print(summary(models_ret$model[[i]]))
}





############################## ПРОВЕРКА УСТОЙЧИВОСТИ #############################

# Создаём подвыборку без Ленты
data_no_lenta <- filtered_data %>%
  filter(Retailer != "Lenta")

# СЗТ по категориям без Ленты
models_by_cat_szt_nolenta <- data_no_lenta %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )

# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_szt_nolenta))) {
  cat("\n\n=== СЗТ / Категория:", models_by_cat_szt_nolenta$Категория[i], "===\n")
  print(summary(models_by_cat_szt_nolenta$model[[i]]))
}


# неСЗТ по категориям без Ленты
models_by_cat_nonszt_nolenta <- data_no_lenta %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )



# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_nonszt_nolenta))) {
  cat("\n\n=== неСЗТ / Категория:", models_by_cat_nonszt_nolenta$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt_nolenta$model[[i]]))
}


#  Driscoll–Kraay standard errors 

# 1
filtered_data$panel_id <- interaction(filtered_data$Product, filtered_data$City, filtered_data$Retailer)

model_dk <- feols(
  log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
    Product + Retailer + Week,
  data = filtered_data,
  vcov = "DK",
  panel.id = ~ panel_id + Week
)


model_dk

# 2

models_by_cat_szt <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )


# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория:", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}




models_by_cat_nonszt <- filtered_data %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )


# Выводим summary для каждой категории
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория:", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}


# 3

panel.id = ~ interaction(Product, City) + Week

models_ret_pov <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )


# Выводим summary для каждой сети
for (i in seq_len(nrow(models_ret_pov))) {
  cat("\n\n=== Ретейлер:", models_ret_pov$Retailer[i], "===\n")
  print(summary(models_ret_pov$model[[i]]))
}

models_ret_inc <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )

# Выводим summary для каждой сети
for (i in seq_len(nrow(models_ret_inc))) {
  cat("\n\n=== Ретейлер:", models_ret_inc$Retailer[i], "===\n")
  print(summary(models_ret_inc$model[[i]]))
}




