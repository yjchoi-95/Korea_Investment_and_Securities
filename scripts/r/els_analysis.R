## 본 데이터는 한국투자증권 홈페이지에 공개된 ELS 증권 상품에 대한 정보입니다.
rm(list=ls())

## library
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(reshape)) install.packages("reshape"); library(reshape) # functions: cast
if(!require(echarts4r)) install.packages("echarts4r"); library(echarts4r)


## stringr encoding error 발생 시
# 일부 windows 환경에서 stringr 패키지의 최신 버전 사용시 encoding 에러가 발생하는 경우가 존재합니다.
# encoding error 발생 시 아래 코드를 실행하셔서 과거 버전을 받으시면 해결

# sessionInfo()
# detach(package:stringr)
# remove.packages("stringr")
# if(!require(devtools)) install.packages("devtools"); library(devtools) 
# install_version("stringr", version = "0.6.2", repos = "http://cran.us.r-project.org")

## load data

# 01. 데이터 경로를 지정하고 해당 경로 확인하기 ####
setwd("C:/Users/PC0/Desktop/fastcampus/solutions/R_ML_DA/datasets/ch_02_data_analysis")
getwd()


# 02. 데이터 불러오기 ####
els_df <- read.csv("els_df.csv")
els_df <- fread("els_df.csv") %>% as.data.frame()


# 03. 데이터의 구조를 확인 ####
els_df %>% head(3)
els_df %>% str()


# 04. 데이터의 중복된 행의 개수를 확인하고 이를 제거 ####
dup_cnt <- duplicated(els_df) %>% sum; dup_cnt
els_df <- els_df %>% distinct()


# 05. 데이터를 날짜 기준으로 정렬하여 가장 과거의 데이터가 첫행에 위치하도록 변경 ####
els_df <- els_df %>% arrange(date)


# 06. 데이터의 열마다 Na값을 확인 ####
is.na(els_df) %>% colSums()


# 07. 데이터의 컬럼 중 "bools"와 "bools2_list"열은 몇번째 컬럼인지 확인 ####
bools_idx <- which(colnames(els_df) == "bools"); bools_idx
bools_idx2 <- which(colnames(els_df) == "bools2_list"); bools_idx2


# 08. 위에서 확인한 index 정보를 이용하여 열 이름을
# 각각 "guaranteed", "repayment"로 변경 (which 함수 이용) ####
colnames(els_df)[bools_idx] <- "guaranteed"
colnames(els_df)[bools_idx2] <- "repayment"


# 09. repayment 열의 값이 상환일 경우 TRUE, 미상환일경우 FALSE로 변경 ####
els_df$repayment <- ifelse(els_df$repayment == "상환", TRUE, FALSE)
# 동일 결과.
#els_df <- els_df %>% mutate(repayment = ifelse(repayment== "상환", TRUE, FALSE))


# 10. guaranteed 열의 값에 지급이 들어갈 경우 TRUE, 그렇지 않을 경우 FALSE로 변경 ####
els_df$guaranteed <- ifelse(els_df$guaranteed == "비보장", FALSE, TRUE)
# 동일 결과.
#els_df$guaranteed <- ifelse(els_df$guaranteed %in% "지급", TRUE, FALSE)
#els_df <- els_df %>% mutate(guaranteed = ifelse(guaranteed %in% "지급", TRUE, FALSE))


# 11. repayment와 guaranteed 두 열, 각각에 대해서 TRUE와 FALSE의 비율을 확인 ####
els_df$repayment %>% table %>% prop.table()
els_df$guaranteed %>% table %>% prop.table()
# 동일 결과.
els_df %>% select(repayment) %>% table %>% prop.table()
els_df %>% select(guaranteed) %>% table %>% prop.table()


# 12. els_df의 due 변수를 연과 개월로 분리하여 새로운 컬럼을 만들고 due 변수를 제거 ####
select_fn <- function(x, index) {
  return(x[index])
}
els_df$due_year <- els_df$due %>% str_split("/") %>% lapply(select_fn, 1) %>% unlist
els_df$due_month <- els_df$due %>% str_split("/") %>% lapply(select_fn, 2) %>% unlist
els_df$due <- NULL


# 13. 텍스트 처리 ####
# 데이터의 products는 각각의 상품들이 "--"으로 결합된 단일 text입니다.
# 이를 "--"를 기준으로 분해 후 상품별 products의 개수를 products_num 컬럼으로 할당하시오.
els_df <- els_df %>% mutate(products_num = str_split(products, "--") %>% lapply(length) %>% unlist)


# 14. products의 종류는 무엇이 있는지 확인 (unlist 함수 활용)  ####
tot_products <- els_df$products %>% str_split("--") %>% unlist %>% unique()
# 동일 결과.
#tot_products <- els_df %>% select(products) %>% unlist %>% str_split("--") %>% unlist %>% unique()


# 15. vector indexing의 이해 ####
# products는 지수와 종목으로 구분 지을 수 있습니다.  
# 지수가 다음과 같을 때 tot_products 변수에서 종목만을 솎아내시오.
index_list <-  c('HSCEI', 'KOSPI200', 'NIKKEI225', 'EUROSTOXX50', 'HSI', 
              'SPXLTBUP', 'EUROSTOXX BANKS', 'EUROSTOXX5O', 'KOSDAQ150', 
              'HSI(항셍지수)', 'DAX', 'ERUOSTOXX50', 'S&P500', 'KOSPI200지수', 'S&P500I',
              'FTSE100', 'EUROSTOXX', 'EUOSTOXX50', 'EUROSTOXX500', '3지수 월지급식 스텝다운형',
              'KOSP200', 'KOSPI200 지수', 'HSCEI지수')
stock_list <- tot_products[!tot_products %in% index_list]


# 16. lapply의 이해 ####
# 각 행의 products 목록 중 stock_list의 항목이 존재하는 행은 TRUE를,  
# 그렇지 않고 index_list만 존재하는 행은 FALSE 값을 갖는 벡터를 생성하시오. 
check_index <- function(x, list) {
  check_value <- (x %in% list) %>% sum
  return(ifelse(check_value > 0, TRUE, FALSE))
}
stock_idx <- els_df$products %>% str_split("--") %>% lapply(check_index, stock_list) %>% unlist


# 17. Indexing을 통한 데이터 분리 ####
# 앞서 도출한 stock_idx를 바탕으로 종목이 포함되어 있는 data.frame과  
# 지수만으로 구성되어 있는 data.frame을 분리하시오.
stock_df <- els_df[stock_idx,]
index_df <- els_df[!stock_idx,]


# 18. %>% 의 이해 ####
# 위의 두 단계를 간소화 해봅시다. dplyr 라이브러리의 함수 filter를 활용하여
# products 항목 중 종목이 포함 되어 있으면 TRUE, 지수로만 구성되어 있을 경우 FALSE를 갖는
# 열 "check_stock"을 만드시오.
els_df <- els_df %>% 
  mutate(check_stock = str_split(products, "--") %>% lapply(check_index, stock_list) %>% unlist)


# 19. check_stock 별로 평균 수익률(profit)을 도출 ####
# 종목이 포함될 경우 그렇지 않은 경우 보다 수익률이 높은가요?
els_df %>% group_by(check_stock) %>% summarise(m_profit = mean(profit))


# 20. category별 profit의 평균을 구하고 profit 평균의 오름차순대로 정렬 ####
temp_df1 <- els_df %>%
  group_by(category) %>%
  summarise(m_profit = mean(profit)) %>%
  arrange(m_profit); temp_df1
# 원금부분지급형의 경우 유일하게 음수값을 기록하였습니다.
# 해당 category는 어느해에 가장 많이 팔렸는지 확인해봅시다.


# 21. date 열에서 연도와 월을 추출하여 새로운 컬럼을 생성 ####
els_df <- els_df %>% mutate(year = substr(date,1,4),
                            month = substr(date,6,7))


# 22. 연도별로 category의 개수 ####
temp_df2 <- els_df %>% group_by(year,category) %>% summarise(cnts = n()) %>% as.data.frame;temp_df2
# 한눈에 정보를 파악하기 어려운 것 같네요. 시각화를 진행해봅시다.


# 23. temp_df2를 라인 다중 차트로 표현 ####
temp_df2 %>% 
  group_by(category) %>% 
  e_chart(year) %>% 
  e_line(cnts)


# 24. temp_df2의 category 비중을 pie chart를 통해 시각화 (e_pie 함수 활용)####
temp_df2 %>% 
  group_by(category) %>% 
  summarise(cnts = sum(cnts)) %>% 
  e_charts(category) %>% 
  e_tooltip() %>% 
  e_pie(cnts)


# 25. temp_df2의 category 비중을 연도별로 pie chart로 표현 (timeline 옵션 활용) ####
temp_df2 %>% 
  group_by(year) %>% 
  e_charts(category, timeline = TRUE) %>% 
  e_tooltip() %>% 
  e_pie(cnts)


# 26. 위에서 도출한 long format 데이터를 wide format으로 변경하시오####
temp_df3 <- temp_df2 %>% cast(year~category); temp_df3
# 동일 결과.
temp_df3 <- temp_df2 %>% data.table %>% dcast(year~category); temp_df3


# 27. Wide format data에 Na값이 존재할 경우 0으로 대치 ####
temp_df3[is.na(temp_df3)] <- 0; temp_df3


# 28. structure별 상품 개수를 계산 후 내림차순 정렬 ####
str_cnts <- els_df %>% group_by(structure) %>% summarise(cnts = n()) %>% arrange(desc(cnts)); str_cnts


# 29. str_cnts에 대하여 상위 5개 구조의 pie chart ####
str_cnts %>% 
  head(5) %>% 
  group_by(structure) %>% 
  summarise(cnts = sum(cnts)) %>% 
  e_charts(structure) %>% 
  e_pie(cnts) %>% 
  e_tooltip() %>% 
  e_labels(show = TRUE,
           formatter = "{b} \n proportion = {d}%")


# 30. stock_list에서 보통주가 들어간 종목은 보통주를 제거하여 회사명만 남기시오. ####
find_fn <- function(x) {
  return(str_replace(x, "보통주", "") %>% str_replace(" ",""))
}
stock_list <- sapply(stock_list, find_fn, USE.NAMES = FALSE)


# 31. 중복된 종목을 제거 ####
stock_list <- stock_list %>% unique(); stock_list


# 32. 우리금융 -> 우리금융지주, 기아차 -> 기아로 변경 ####
stock_list[which(stock_list == "우리금융")] <- "우리금융지주"
stock_list[which(stock_list == "기아차")] <- "기아"


# 33. stock_list의 주가 정보를 담고 있는 데이터 total_stocks.csv를 불러오고 구조를 확인 ####
# readAny::read.any는 linux 환경이나 기타 encoding 문제가 존재할 때 사용할 수 있는 함수 입니다.
if(!require(readAny)) {
  library(devtools)
  install_github("plgrmr/readAny", force = TRUE)
}; library(readAny)

tot_stock <- read.any("total_stocks.csv", header = TRUE)
tot_stock$Date <- tot_stock$Date %>% as.IDate()

## els_df와 tot_stock을 함께 활용하여 분석을 진행해봅시다.
## 목표하고자 하는 분석은 els_df에 명시되어있는 주가 종목의 date를 기준으로하여
## n개월이 지났을 때 주가의 변화를 살펴보고자 합니다.
## 이를 위해선 다음과 같은 step을 거쳐야 합니다.

# 34. 함수 생성 ####
# tot_stock 데이터에서 stock_list의 임의의 종목을 받아 
# 해당 종목의 columns을 모두 출력하는 함수를 작성하시오.
# 단 Date 열은 항상 포함이 되어야 합니다.
select_cols <- function(df, stocks) {
  return(df %>% select(which(str_detect(colnames(tot_stock), stocks) | colnames(tot_stock) == "Date")))
}
select_cols(tot_stock, stock_list[1])


# 35. 이중 함수 생성 ####
# stock_list 내의 종목명을 입력 시 els_df 내 date와 due_year, due_month를 중복없이 출력하는 함수를 작성하시오.
# 해당 함수를 출력하였다면 현대차 종목의 data를 h_df로 할당하시오.
select_row <- function(x, value) {
  return(value %in% x)  
}

print_inform <- function(df, item) {
  return(df %>% 
             filter(df$products %>%
                      str_split("--") %>%
                      lapply(select_row, item) %>%
                      unlist) %>% 
             select(date, due_year, due_month) %>% 
             distinct()
  ) # end of return
} # end of function

item <- "현대차"
h_df <- print_inform(els_df, item); h_df


# 36. 기준가와 종가 출력 ####
# h_df의 첫번째 행을 기준 시점으로, 그 때의 종가(Close)를 기준가로 삼겠습니다.
# 기준 시점으로부터 3년(180일*6) 간의 종목 최저가(Low)를 6개월(180일) 간격으로 출력해보세요.
# 날짜가 주말, 휴일 등의 이유로 데이터가 출력되지 않는 경우는 별도의 처리를 하지 않겠습니다.
h_stock <- select_cols(tot_stock, item)
start_time <- h_df[1,]$date
time_list <- seq(start_time, start_time+(180*6), 180)
result_df <- h_stock %>% 
  filter(Date %in% time_list) %>% 
  select(Date, paste(item, "Close", sep = "_"), paste(item, "Low", sep = "_"))


# 37. 앞서 도출한 data frame을 바탕으로 기준가 대비 각 시점간의 최저가의 변화량을 산출해. ####
result_df[2:nrow(result_df),3]/result_df[1,2]


# 38. 특정 종목과 시작 기간을 입력시 향후 180일간의 종가를 plotting하는 함수를 작성. ####
# NA값이 존재하는 경우 해당 행을 제거하시오.
temp_df5 <- select_cols(tot_stock, item)
start_date <- h_df[1,]$date
end_date <- start_date + 180*6
for_plot <- temp_df5 %>% 
  filter(Date >= start_date & Date <= end_date) %>% 
  select(Date, paste(item, "Close", sep = "_")) %>% 
  arrange(Date) %>% 
  na.omit()

# 일반화를 위한 column명 변경
colnames(for_plot)[2] <- "price"
for_plot$Date <- as.character(for_plot$Date)
for_plot %>% 
  e_chart(Date) %>% 
  e_line(price)

# 39. 앞에서 출력한 그래프에, 기준가를 표시하는 line을 추가 ####
for_plot$base <- rep(for_plot$price[1], nrow(for_plot))
for_plot %>% 
  e_chart(Date) %>% 
  e_line(price) %>% 
  e_line(base)


### end of script