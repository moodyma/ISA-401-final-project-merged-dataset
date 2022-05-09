install.packages("pacman")

pacman::p_load(rvest,
               magrittr,
               tidyverse)

# creating a url to scrape data
f1_url = "https://www.formula1.com/en/results.html"
html_url = read_html(f1_url)
url_cycle_list = list("/2017/", "/2018/", "/2019/", "/2020/", "/2021/")
fast_laps = "fastest-laps.html"

allfastest=tibble()
# loop with permanent tibble
fast_table_final = tibble()
for (i in 1:5){
  full_url = paste0(f1_url, url_cycle_list[i], fast_laps)
  read_html(full_url) %>%
    html_element("table") %>%
    html_table() -> fasttable
  
  allfastest=rbind(allfastest, fasttable)
  
}

empty_columns <- colSums(is.na(allfastest) | allfastest == "") == nrow(allfastest)
allfastest = allfastest[, !empty_columns]

allfastest
# data is in order so we will just make an ID variable to merge the two
allfastest = tibble::rowid_to_column(allfastest, "ID")

# race results



f1_url = "https://www.formula1.com/en/results.html"
html_url = read_html(f1_url)
url_cycle_list = list("/2017/", "/2018/", "/2019/", "/2020/", "/2021/")
winners = "races.html"

allraces=tibble()

fast_table_final = tibble()
for (i in 1:5){
  full_url = paste0(f1_url, url_cycle_list[i], winners)
  read_html(full_url) %>%
    html_element("table") %>%
    html_table() -> racetable
  
  allraces=rbind(allraces, racetable)
  
}
empty_columns <- colSums(is.na(allraces) | allraces == "") == nrow(allraces)
wintable = allraces[, !empty_columns]

wintable = tibble::rowid_to_column(wintable, "ID")
wintable

win_fastest_table =  merge(wintable, allfastest, by = 'ID')
# fixing large spaces in names
win_fastest_table$Winner %>%
  str_split_fixed(pattern = '\n', n =3) -> temp_matrix
win_fastest_table$Driver %>%
  str_split_fixed(pattern = '\n', n =3) -> tem_matrix

win_fastest_table$FirstName_Winner = temp_matrix[, 1] %>% str_trim()
win_fastest_table$LastName_Winner = temp_matrix[, 2] %>% str_trim()
win_fastest_table$FirstName_Driver = tem_matrix[, 1] %>% str_trim()
win_fastest_table$LastName_Driver = tem_matrix[, 2] %>% str_trim()

win_fastest_table = select(win_fastest_table, -"Grand Prix.y",-Winner, -Driver)


write.csv(win_fastest_table,"WinAndFastest.csv", row.names = FALSE)

driver_table = read.csv("drivers.csv")

#creating a vector with just nationality information
driver_country_vector = as.vector((c("forename", "surname", "nationality")))

driver_country <- driver_table[,driver_country_vector]       # Extracting only 3 columns
driver_country


write.csv(win_fastest_table,"WinAndFastest.csv", row.names = FALSE)

write.csv(driver_country,"Driver_Country.csv", row.names = FALSE)

# due to naming convention differences (non-English characters) these will be joined in Tableau prep

