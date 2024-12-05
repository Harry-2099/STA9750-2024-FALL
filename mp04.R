library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(ggthemes)
setwd("~R/Project_0")
### DEVELOPING AN UNDERSTANDING OF MONTE CARLO METHODS
RETIREMENT <- data.frame(
    r = rnorm(24, mean=0.5) / 100, # Monthly returns
    C = rep(100, 24),            # Monthly savings: 100 per month
    period = 1:24                # Period ID (# of months)
)

RETIREMENT |> 
    mutate(net_total_return = order_by(desc(period), 
                                       cumprod(1 + lead(r, default=0)))) %>% 
    summarize(future_value = sum(C * net_total_return))
# example from non central chi sqaured distribution
set.seed(100)
DATA <- rchisq(250, df=pi, ncp = exp(2))
SAMPLE_MEDIAN <- median(DATA)
#computing the true median using a large sample --- notice the slight difference
TRUE_MEDIAN <- median(rchisq(5e7, df=pi, ncp = exp(2)))

#### finding the variance of our sample using a bootstrap method 
B <- 500 # Number of boostrap samples to create
n <- length(DATA) # Original data size
expand_grid(B = 1:B, 
            n = 1:n) |>
    # Notice here we sample _with replacement_ from DATA
    mutate(x = sample(DATA, n(), replace = TRUE)) |>
    group_by(B) |>
    summarize(f_boot = median(x)) |>
    summarize(var_f = var(f_boot)) |>
    pull(var_f)
1/(4 * n * dchisq(TRUE_MEDIAN, df=pi, ncp = exp(2))^2)

## USING THE REPLICATE FUNCTION TO GET THE ABOVE MEDIAN OF THE CHI SQUARE DIST

var(replicate(10000, {
    median(rchisq(250, df=pi, ncp = exp(2))) 
  #this above chunk intitillizes random number from chi sqaure dist with pi df and non centrality parameter e^2 n = 250 samples
}))


#### EXAMPLES WITH THE JOINT DATA (X,y)
### CHI AND SIN PLOT
x <- rchisq(100, df=3, ncp=2)
y <- x * sin(2 * x) + 15 * log(x)
plot(x, y)

### intitial correlation
cor(x, y, method="kendall")

stopifnot(length(x) == length(y))
n_samp <- length(x)
n_boot <- 400

mc <-data.frame(x = x, y = y) |>
    slice_sample(n = n_samp * n_boot, 
                 replace=TRUE) |>
    mutate(resample_id = rep(1:n_boot, times=n_samp)) |>
    group_by(resample_id) |>
    summarize(kendall_cor = cor(x, y, method="kendall")) |>
    summarize(var(kendall_cor))

##USING THE REPLICATE METHOD TO GET THE VAR -- AGAIN VERY CLOSE

var(replicate(5000, {
    x <- rchisq(100, df=3, ncp=2)
    y <- x * sin(2 * x) + 15 * log(x)
    cor(x, y, method="kendall")
}))

#### DATA AQUISITION###
 
#LETS GET THE T BILLS 1 YEAR FROM FRED USING THE SERIES ID: DTB1YR
install.packages("httr2")
library(httr)
library(jsonlite)
#setting up the api key discrete reading
FRED_key <- readLines("~/R/Project_0/FRED_key.txt")

# RUNNING EXAMPLE URL
res <- GET(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=",FRED_key,"&file_type=json"))
res_content <- content(res, as = "text", encoding = "UTF-8")#converting to text with utf8

ex <- fromJSON(res_content)
ex_data <-ex$observations
#BULIDING FRED FUNCTION
get_fred<- function(id){
  base_url <- "https://api.stlouisfed.org/fred/series/observations?series_id="
  res <- GET(paste0(base_url,id,"&api_key=",FRED_key,"&file_type=json"))
  res_content <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(res_content)
  data <-json$observations
  print(res)# printing allows for easy trouble shooting
  data <- data %>% mutate(value = as.numeric(value),# immediately convert to usable format
                          date = as.Date(date))
  return(data)
}

tbills_1<- get_fred("DTB1YR")#short term debt
inflation <- get_fred("FPCPITOTLZGUSA")

NY_wage <- get_fred("STTMINWGNY")
ggplot(NY_wage,aes(x = date,y= value))+
  geom_point()

#USING THE ALPHA VANTAGE API
# example url

res_content <- content(res, as= "text")
j <- fromJSON(res_content,flatten= TRUE)
data <- j$`Monthly Adjusted Time Series`
## UNPACKING WITH FOR LOOP
close_list<- c()
for(i in seq(1:length(data))){
  close_list <- append(close_list,data[[i]][["4. close"]])
}
close <- data.frame(date = names(data),close = close_list)# data frame with unpacked data

AV_key <- readLines("~/R/Project_0/Alphavantage_key.txt")
### FUNCTION TO GET THE ALPHA VANATGE API DATA

GET_AV <- function(ticker){
  
  url <-paste0("https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED","&symbol=",ticker,"&apikey=",AV_key)
  print(url)# for troubleshooting
  res <- GET(url)
  print(res)
  res_content<-(content(res,as = "text",encoding = "UTF-8"))
  j <- fromJSON(res_content,flatten = TRUE)
  data <- j$`Monthly Adjusted Time Series`
  #### unpacking the data with a for loop
  
  close <- c() #empty list to contain the list
  low <- c()
  volume <- c()
  div <- c()
  
  for(i in seq(1:length(data))){
    close <- append(close,data[[i]][["4. close"]])
    low <- append(low,data[[i]][["3. low"]])
    volume <- append(volume,data[[i]][["6. volume"]])
    div <-append(div,data[[i]][["7. dividend amount"]])
  }
  df <- data.frame(date = as.Date(names(data)),
                   close = as.numeric(close),
                   low = as.numeric(low),
                   volume = as.numeric(volume),
                   dividend = as.numeric(div))
  return(df)
}
#GETTING SOME DATA
ibm <-GET_AV("IBM")#trial 

sp500 <-GET_AV("SPY")
midcap400<- GET_AV("MDY")# s and p medium cap 
smallcap600 <- GET_AV("SPSM")#s and p small cap
#RENAMEING BEFORE WE JOIN ALL THE LOW MID AND LARGE CAP INDEX FUNDS FOR A US MARKET ESTIMATE

rename_col <- function(data,str){
  data <- data %>% 
    rename_with(~ paste0(str, .), c("close", "low", "volume", "dividend"))
  return(data)
}

sp500 <- rename_col(sp500,"SP500_")
smallcap600 <- rename_col(smallcap600,"smallcap600_")
midcap400 <- rename_col(midcap400,"midcap400_")

ice_bonds <- get_fred("BAMLHYH0A0HYM2TRIV")
ice_bonds <- ice_bonds %>% drop_na() %>% select(-1,-2)
VGK_euro <- GET_AV("VGK")

IEV_euro <- GET_AV("IEV")
fid_inter <- GET_AV("FSPSX")

ggplot(fid_inter,aes(x = date,y = close))+
  geom_line()

#creating a ggtheme for ease of use 
my_theme <- theme_clean()+
   theme(axis.title.x = element_text(face = "bold.italic", size = 12),  
    axis.title.y = element_text(face = "bold.italic", size = 12))

ggplot(IEV_euro,aes(x=date,y= close))+
  geom_line()+
  my_theme

#### SOME ANALYSIS
#join all the stock/etf in to one dataframe
IEV_euro<- IEV_euro %>% 
  pivot_longer(cols = -date, names_to = "val_type", values_to  = "value") %>% 
  mutate(index = rep("IEV_euro",length(date))) %>% 
  mutate(value = round(value,3))
#function to make longer and apply a index name col used for id when joined in large stock table
MAKE_LONG <- function(data,str){
  d <- data %>% 
  pivot_longer(cols = -date, names_to = "val_type", values_to  = "value") %>% 
  mutate(index = rep(str,length(date))) %>% 
  mutate(value = round(value,3))
  return(d)
}


sp500_close <- sp500 %>% 
  select(date,close) %>%
  mutate("Index"=rep("SP500",length(date)))

smallcap_close <- smallcap600 %>% 
  select(date,close) %>%
  mutate("Index"=rep("SmallCap600",length(date)))

midcap_close <- midcap400 %>% 
  select(date,close) %>%
  mutate("Index"=rep("Midcap400",length(date)))


### add the IN close when the API allows here 
IN <- fid_inter %>% 
  select(date,close) %>%
  mutate("Index" = rep("Fidelity International",length(date)))
IN_CAGR <- IN %>% reframe(start_close = close[date == "2011-10-31"],
    Index= "Fidelity International",
    end_close = close[date == "2024-11-29"],
    num_years = as.numeric(difftime("2024-11-29", "2011-08-31", units = "days")) / 365,
    CAGR = ((end_close / start_close)^(1 / num_years) - 1)*100)
  


stocks_close <- sp500_close %>% 
  full_join(midcap_close, by = c('date',"Index","close")) %>% 
  full_join(smallcap_close, by = c('date',"Index","close")) %>% 
  full_join(IN, by = c('date',"Index","close"))


### CLOSE PRICE STOCKS 
ggplot(stocks_close,aes(x = date,y= close,color = Index))+
  geom_line()+
  labs(title = "American and International Index Performance (Close Price)", x = "Date",y = "Close Price")+
  my_theme+
  theme(legend.position = "bottom")

#Standard Deviation
stocks_close %>% 
  group_by(Index) %>% 
  summarize(close_std = sd(close)) %>% 
  ggplot(aes(x = Index, y = close_std, fill = Index))+
  geom_bar(stat = "identity")+
  my_theme+
  theme(legend.position = "FALSE")+
  labs(title = "Standard Deviation of Index Close Prices",y = "Close Standard Deviation")

### CAGR lollipop plot code
stocks_close %>%
  group_by(Index) %>%
  reframe(
    start_close = close[date == "2000-08-31"],
    end_close = close[date == "2024-11-29"],
    num_years = as.numeric(difftime("2024-11-29", "2000-08-31", units = "days")) / 365,
    CAGR = ((end_close / start_close)^(1 / num_years) - 1)*100) %>% 
  #plot
  ggplot(aes(x = Index, y = CAGR, fill = Index))+
  geom_bar(stat = "identity")+
  my_theme+
  geom_hline(yintercept = 0, color = "red",linetype = "dashed")+
  theme(legend.position = "FALSE")+
  coord_flip()
  
##BOND ANALYSIS
ggplot(ice_bonds,aes(x = date, y = value))+
  geom_line(color = "blue")+
  geom_area(fill = "#69b3a2", alpha=.5)+
  my_theme+
  labs(title = "ICE Bond Value Over Time", y="Bond Value", x = "Date")
  
###lets add the bond to the other stocks for a side by side comparison
bonds <- ice_bonds %>% mutate(Index = rep("Bond",length(date))) # calling it index to make the join process easier
library(lubridate)
### turning the daily data into a monthly data
bonds <- bonds %>% mutate(month = month(date),
                                  day = day(date),
                                  year = year(date)) %>% 
  group_by(year,month) %>% 
  mutate(close = value[which.max(day)]) %>% 
  ungroup()
  

bonds <- bonds %>% arrange(desc(date)) %>%
  distinct(close,.keep_all = TRUE) %>% 
  select(date,close,Index) %>% 
  filter(date >= as.Date("1999-12-30"))
  
  

investments <- sp500_close %>% 
  full_join(midcap_close, by = c('date',"Index","close")) %>% 
  full_join(smallcap_close, by = c('date',"Index","close")) %>% 
  full_join(IN, by = c('date',"Index","close")) %>% 
  full_join(bonds,by=c('date',"Index","close"))

#visualize investments
ggplot(investments,aes(x = date,y= close,color = Index))+
  geom_line()+
  labs(title = "Equity and Bonds Performance (Monthly Close Price/Value)", x = "Date",y = "Close Price")+
  my_theme+
  theme(legend.position = "bottom")
#Standard Deviation
investments %>% 
  group_by(Index)%>% 
  summarize(close_std = sd(close)) %>% 
  ggplot(aes(x=Index, y = close_std))+
  geom_segment(aes(x=Index,xend= Index,y = 0, yend=close_std),linewidth = 1)+
  geom_point(size = 5, fill = alpha("lightblue",.7),shape =21, stroke= 2,color="black")+
  #geom_point(size = 6,shape = 8, color = "red")+
  my_theme+
  theme(legend.position = "FALSE")+
  labs(title = "Standard Deviation of Investment Close Value",y = "Close Standard Deviation")

stock_CAGR <-investments %>%
  group_by(Index) %>%
  reframe(
    start_close = close[date == "2000-08-31"],
    end_close = close[date == "2024-11-29"],
    num_years = as.numeric(difftime("2024-11-29", "2000-08-31", units = "days")) / 365,
    CAGR =((end_close / start_close)^(1 / num_years) - 1)*100)
    
bond_CAGR <-investments %>%
  group_by(Index) %>%
  reframe(start_close = close[date=="2000-08-31"],
          end_close = close[date =="2024-11-28"],
          num_years = as.numeric(difftime("2024-11-29", "2000-08-31", units = "days")) / 365,
          CAGR =(((end_close / start_close)^(1 / num_years) - 1)*100))
    

CAGR <- stock_CAGR %>%
  full_join(bond_CAGR, by = c("start_close","end_close","num_years","Index","CAGR")) %>% 
  full_join(IN_CAGR,by = c("start_close","end_close","num_years","Index","CAGR"))

###plotting all the cagr ### ANIMATED LOLLIPOP
ggplot(CAGR,aes(x=Index, y = CAGR))+
  geom_bar(stat = "identity",width = .0175)+
  geom_point(size = 5, fill = alpha("lightblue",.5),shape =21, stroke= 2,color="black")+
  my_theme+
  theme(legend.position = "FALSE")+
  labs(title = "CAGR of Investments",y = "CAGR")+
  transition_states(CAGR,transition_length = 4, state_length = 1)+ 
  enter_grow()+
  exit_fade(alpha=.2)

tbills <- tbills_1 %>% 
  select(-1,-2) %>% 
  rename(yield = value)
  
  
tbills <-tbills%>%
  drop_na() %>% 
  mutate(month = month(date),
         day = day(date),
         year = year(date)) %>% 
  group_by(year,month) %>% 
  mutate(close_yield = yield[which.max(day)]) %>% 
  ungroup() %>% 
  arrange(desc(date)) %>%
  distinct(close_yield,.keep_all = TRUE) %>% 
  select(date,close_yield) %>% 
  filter(date >= as.Date("1999-12-30"))

tbills %>% drop_na() %>% 
ggplot(aes(x = date,y= close_yield))+
  geom_line(color = "blue")+
  geom_area(fill = alpha("lightblue",.5))+
  my_theme+
  labs(title= "1 Year T-Bill Monthly Percent Yield", x = "Date",y = "Percent Yield")

#average growth over time
geo_mean <- function(x) {
  mean <- prod(1 + x)^(1 / length(x)) - 1 
  return(mean)
}
### Tbills average monthly growth
tbills %>% drop_na() %>% tbill
  summarise(Avg_Growth_Rate = geo_mean(close_yield))


#implementing the TRS

# $55,001 to $75,000: 4.5%
# $75,001 to $100,000: 5.75%
# $100,001 or more: 6%
wages <- get_fred("FRBATLWGT12MMAWMHWG97O")

wage <- wages %>% select(-1,-2) %>% 
  rename(diff = value)

wage <-wage %>% 
  mutate(month = month(date),
         day = day(date),
         year = year(date)) %>% 
  group_by(year,month) %>% 
  mutate(diff = diff[which.max(day)]) %>% 
  ungroup() %>% 
  arrange(desc(date)) %>%
  distinct(diff,.keep_all = TRUE) %>% 
  select(date,diff)
  

ggplot(wage,aes(x = date,y=diff))+
  geom_line()+
  my_theme+
  labs(title = "Median Wage Growth",x = "Date", y = "Percent Growth in Median")

wage  %>% summarise(Avg_wage_growth = geo_mean(diff/100))# GEO MEAN OF WAGES

head(inflation)
inflation<- inflation %>% select(year,value)
inflation <- inflation %>% mutate(rate = round(value/100,3))
#assuming 60,000 salary
### WAGE FUNCTION
# dynamically calculates the growth of the wage as well as return the final salary(x = salary, t = total time worked)


TRS_system <- function(x,t){
  salary_list <- c()
  TRS <- 0
  #calc the wage growth
  for (i in seq(1,t-1)){# -1 accounts for fixed sal in first year
    x <- x*(1+.045)
    x<- min(x, 275000) ## prevents unrealistc salary growth
    salary_list <- append(salary_list,x)#for FAS
    
   #calc the yearly deposit to TRS
    if(x>= 55001 & x <=75000){# tier 1 salary
       TRS <- TRS + (x * .045)
        }else if(x>= 75001 & x <= 100000){# tier 2 salary
            TRS <- TRS + (x * .0575)
        }else{# tier 3 salary
            TRS <- TRS + (x * .06)
        }
  }
   #benfit pay
   FAS <- mean(tail(salary_list,3))

    if(t <= 20){ #benefit payments
       benefit_pay <- .0167 * FAS* t
         }else if(t == 20){
            benefit_pay <- .0175 * FAS* t
         }
          else{
            benefit_pay <- (.35 +(.02 * t)) * FAS
         }
  
  
  cat("Final Salary:",x,"\n")
  cat("Total Contributions:",TRS)
  cat("\n","FAS:",FAS,"\n")
  cat("Monthly Benefit Pay:  ")
  
  return(benefit_pay/12)
}

TRS_system(60000,25)




ggplot(inflation,aes(x = year,y= value))+
  geom_line()

#ORP FUNCTION 
#Age 25 to Age 49:
# 54% US Equities
# 36% International Equities
# 10% Bonds
# Age 50 to Age 59:
# 47% US Equities
# 32% International Equities
# 21% Bonds
# Age 60 to Age 74:
# 34% US Equities
# 23% International Equities
# 43% Bonds

# Age 75 or older:
# 19% US Equities
# 13% International Equities
# 62% Bonds
# 6% Short-Term Debt


tbills %>% drop_na() %>% summarise(Avg_Growth_Rate = geo_mean(close_yield/100))

ORP_system<- function(x,t,age,est_death,withdrawl_rate){
  salary_list <- c()
  years_worked<-0 
  total_deposit<-0
  TRS <- 0
  profit <- 0
  retirment_age <- age+t
  sal_after_r <- c()
#inner functions
  personal_deposit <- function(salary) {
    if (salary >= 55001 & salary <= 75000){
      return(salary * 0.045)}
    else if (salary > 75000 & salary <= 100000){ 
      return(salary * 0.0575)}
    else{ 
      return(salary * 0.06)}
  }
  investment_return <-function(age,total_deposit){
    if(age %in% seq(25,49)){#us equities(split into sp500 & mid400), foreign equities, bonds; respectively
      profit <- (total_deposit*.27)*.0779 + 
                (total_deposit*.27)*.0583 + 
                ((total_deposit*.36)*.033)+ 
                (total_deposit*.10)*.066
                return(profit)
          }
    else if(age %in% seq(50,59)){
      profit <- (total_deposit*.235)*.0779 + 
                (total_deposit*.235)*.0583 + 
                ((total_deposit*.32)*.033) + 
                (total_deposit*.21)*.066
                return(profit)
          }
    else if(age %in% seq(60,74)){
      profit <-(total_deposit*.17)*.0779 + 
                (total_deposit*.17)*.0583 + 
                ((total_deposit*.23)*.033) + 
                (total_deposit*.43)*.066
                return(profit)
        }
    else if(age > 74){#us equities(split into sp500 & mid400), foreign equities, bonds,treasury; respectively
      profit <- (total_deposit*.095)*.0779 + 
                (total_deposit*.095)*.0583 + 
                ((total_deposit*.13)*.033) + 
                (total_deposit*.62)*.066+ 
                (total_deposit *.06)*.0219
                return(profit)
      }
    }
  
    for(i in seq(1, t)){
      x <- x*(1+.045)
      age <- age +1
      years_worked<- years_worked + 1 
      salary_list <- append(salary_list,x)
      
      if(years_worked <= 7){
           total_deposit <- total_deposit + personal_deposit(x) + .08*x
      }else{
        total_deposit <- total_deposit + personal_deposit(x) + .10*x
      }
      
      balance <- total_deposit + investment_return(age = age,total_deposit = total_deposit)
      print(balance)
    }
      while (age < est_death) {
      age <- age + 1
      withdraw <- balance * withdrawl_rate
      sal_after_r <- append(sal_after_r,withdraw)
      balance <- balance - withdraw
      balance <- balance + investment_return(age = age,total_deposit=balance)
      average_annual <- mean(sal_after_r)
         }
 
  
  cat("Final Balance:", balance, "\n")
  cat("Annual Retirement Salary Until Death:", average_annual, "\n")
  cat("________________________________", "\n")
  cat("Monthly Income After Retirement: ")
  
  return(average_annual / 12)
  
}

ORP_system(60000,50,30,90,.04)

t <- TRS_system(60000,50) 
o<- ORP_system(60000,50,30,90,.04)

head(investments)
tbills <- tbills %>% 
  rename(value,close_yield)

n_samp <- length(investments$date)
n_boot <- 100

# mc <-data.frame(x = x, y = y) |>
#     slice_sample(n = n_samp * n_boot, 
#                  replace=TRUE) |>
### MONTE CARLO SIMLUATION
n_boot <- 6000  # Number of bootstrap samples
n_samp <- 1000 # Number of samples per bootstrap
n_total <- n_boot * n_samp  # Total number of samples to generate
set.seed(123)

sp500MC <- investments %>%
  filter(Index == "SP500") %>%            
  slice_sample(n = n_total, replace = TRUE) %>% 
  mutate(resample_id = rep(1:n_boot, each = n_samp)) %>% 
  group_by(resample_id) %>%
  mutate(close = pmax(70,close + rnorm(n(), mean = 0, sd = 123))) %>% # Add variation with min 70(from data)
  ungroup() %>%
  arrange(date)
  
ggplot(sp500MC,aes(x = date, y = close, group = resample_id, color = resample_id)) +
  geom_line(alpha = 0.075) + # Plot each resample
  theme_minimal() +
  theme(legend.position = "FALSE")
#### MONTE CARLO CAGR CALC####

## SP500 MC CAGR
sp500MC %>%
  filter(date == "1999-12-31" | date == "2024-11-29") %>% 
  group_by(date) %>% 
  summarise(average_close = mean(close)) %>% 
  summarise(
    start = average_close[date == "1999-12-31"],
    end = average_close[date == "2024-11-29"]) %>% 
  mutate( CAGR = (end / start)^(1 / 25) - 1)
#mc plot                           
sp500MC %>%
  filter(date == "2024-11-29") %>% 
  group_by(date) %>% 
  ggplot(aes(x = close))+
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7)+
  theme_minimal()+
  geom_vline(xintercept = 600, color = "red") +
  labs(title ="Monte Carlo Simulated Close Prices",x = "Close Price S&P 500 (2024-11-29)", y = "Freq.")
#BONDS
BondMC <- investments %>%
  filter(Index == "Bond") %>%            
  slice_sample(n = n_total, replace = TRUE) %>% 
  mutate(resample_id = rep(1:n_boot, each = n_samp)) %>% 
  group_by(resample_id) %>%
  mutate(close = pmax(300,close + rnorm(n(), mean = 0, sd = 400))) %>% # Add variation with min (from data)
  ungroup() %>%
  arrange(date)

BondMC %>%
  filter(date == "1999-12-31" | date == "2024-11-28") %>% 
  group_by(date) %>% 
  summarise(average_close = mean(close)) %>% 
  summarise(
    start = average_close[date == "1999-12-31"],
    end = average_close[date == "2024-12-03"]) %>% 
  mutate( CAGR = (end / start)^(1 / 25) - 1) ### this value is a quite bit a lower
### bond plot
BondMC %>%
  filter(date == "2024-12-03") %>% 
  ggplot(aes(x = close))+
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7)+
  theme_minimal()+
  geom_vline(xintercept = 1726, color = "red") +
  labs(title ="Monte Carlo Simulated Bond Returns",x = "Close Price (2024-11-28)", y = "Freq.")

#MID CAP MC
midMC <- investments %>%
  filter(Index == "Midcap400") %>%            
  slice_sample(n = n_total, replace = TRUE) %>% 
  mutate(resample_id = rep(1:n_boot, each = n_samp)) %>% 
  group_by(resample_id) %>%
  mutate(close = pmax(70,close + rnorm(n(), mean = 0, sd = 139))) %>% # Add variation with min (from data)
  ungroup() %>%
  arrange(date)
midMC %>%
  filter(date == "1999-12-31" | date == "2024-11-29") %>% 
  group_by(date) %>% 
  summarise(average_close = mean(close)) %>% 
  summarise(
    start = average_close[date == "1999-12-31"],
    end = average_close[date == "2024-11-29"]) %>% 
  mutate( CAGR = (end / start)^(1 / 25) - 1)# a bit lower

####INTERNATINAL MC
IN_MC <- investments %>%
  filter(Index =="Fidelity International") %>%            
  slice_sample(n = n_total, replace = TRUE) %>% 
  mutate(resample_id = rep(1:n_boot, each = n_samp)) %>% 
  group_by(resample_id) %>%
  mutate(close = pmax(28,close + rnorm(n(), mean = 0, sd = 5))) %>% # Add variation with min (from data)
  ungroup() %>%
  arrange(date)

IN_MC %>%
  filter(date == "2011-10-31" | date == "2024-12-02") %>% 
  group_by(date) %>% 
  summarise(average_close = mean(close)) %>% 
  summarise(
    start = average_close[date == "2011-10-31"],
    end = average_close[date == "2024-12-02"]) %>% 
  mutate( CAGR = (end / start)^(1 / 13) - 1)# slightly lower

### international plot
IN_MC %>%
  filter(date == "2024-12-02") %>% 
  ggplot(aes(x = close))+
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7)+
  theme_minimal()+
  geom_vline(xintercept = 50, color = "red") +
  
  labs(title ="Monte Carlo Simulated Close Prices",
       x = "Close Price Fidelity International Index (2024-12-02)", y = "Freq.")
tbill_MC <- tbills %>% 
  slice_sample(n = n_total, replace = TRUE) %>% 
  mutate(resample_id = rep(1:n_boot, each = n_samp)) %>% 
  group_by(resample_id) %>%
  mutate(close_yield = pmax(.05,close_yield + rnorm(n(), mean = 0, sd = 1.85))) %>% # Add variation with min (from data)
  ungroup() %>%
  arrange(date)

tbill_MC %>% 
  drop_na() %>% 
  summarise(MC_growth_rate = mean(close_yield))



#### RANDOMIZE FUNCTION WITH MONTE CARLO #######
MC_TRS <- function(x,t,maxed,n_simulations){
  set.seed(123)#common seed
  sim_results <- numeric(n_simulations)#this initializes a vector, size of n_sims for better computing
  for(sim in seq(1,n_simulations)){
    salary_list <- c()
    TRS <- 0
    current_sal <- x
   #wage growth
    for (i in seq(1,t-1)){
      growth <- (1+ pmax(rnorm(1,mean = .045, sd=0.04),0))
      current_sal <- current_sal * growth
      current_sal<- min(current_sal,maxed)# applies a realistic max sal
      salary_list <- append(salary_list,current_sal)#for FAS
      
     #calc the yearly deposit to TRS
      if(current_sal>= 55001 & current_sal <=75000){# tier 1 salary
         TRS <- TRS + (current_sal * .045)
        }else if(current_sal>= 75001 & current_sal <= 100000){# tier 2 salary
              TRS <- TRS + (current_sal * .0575)
        }else{# tier 3 salary
              TRS <- TRS + (current_sal * .06)
        }
      
    }
    avg_TRS <- TRS/t
     #benefit pay
    FAS <- mean(tail(salary_list,3))
    if(t <= 20){
        benefit_pay <- .0167 * FAS* t
        }else if(t == 20){
           benefit_pay <- .0175 * FAS* t
        }
        else{
          benefit_pay <- (.35 +(.02 * t)) * FAS
        }
    
    monthly_income <- benefit_pay/12
    sim_results[sim]<- monthly_income
    }
  cat("MC Mean Monthly Salary: ",mean(sim_results),"\n","Average Contribution Yearly:",avg_TRS,"\n",
      "__________________________________________________","\n")
  return(sim_results)
  }


TRS_system(60000,30)


x <- MC_TRS(x = 60000,t =25,maxed =175000,n_simulations = 3000)
TRS_sim <- data.frame(Monthly_Income = x)
mean_income <-mean(TRS_sim$Monthly_Income)

ggplot(TRS_sim,aes(x = Monthly_Income))+
  geom_histogram(bins = 75,fill="purple",alpha=.5)+
  geom_vline(xintercept = mean_income, color = "red")+
  theme_clean()+
  labs(title = "Monte Carlo Simulated Monthly Benefit TRS)",x = "Monthly Income ($)", y = "Freq.")

TRS_sim %>% 
  filter(Monthly_Income < 8000)



#### ORP MONTE CARLO #####

MC_ORP<- function(x,t,maxed,age,est_death,withdrawl_rate,n_simulations){
  set.seed(123)#common seed
  sim_results <- numeric(n_simulations)
#inner functions
  personal_deposit <- function(salary) {
    if (salary >= 55001 & salary <= 75000){
      return(salary * 0.045)}
    else if (salary > 75000 & salary <= 100000){ 
      return(salary * 0.0575)}
    else{ 
      return(salary * 0.06)}
  }
  investment_return <-function(current_age,total_deposit,sp,md400,IN,bond,tbill){
    if(current_age %in% seq(25,49)){#us equities(split into sp500 & mid400), foreign equities, bonds; respectively
      profit <- ((total_deposit*.27)* sp) + 
                ((total_deposit*.27)* md400) + 
                ((total_deposit*.36)* IN) + 
                ((total_deposit*.10)* bond)
                return(profit)
          }
    else if(current_age %in% seq(50,59)){
      profit <- ((total_deposit*.235)* sp) + 
                ((total_deposit*.235)* md400) + 
                ((total_deposit*.32)* IN) + 
                ((total_deposit*.21)* bond)
                return(profit)
          }
    else if(current_age %in% seq(60,74)){
      profit <-((total_deposit*.17)* sp) + 
                ((total_deposit*.17)* md400) + 
                ((total_deposit*.23)* IN) + 
                ((total_deposit*.43)* bond)
                return(profit)
        }
    else if(current_age > 74){#us equities(split into sp500 & mid400), foreign equities, bonds,treasury; respectively
      profit <- ((total_deposit*.095)* sp) + 
                ((total_deposit*.095)* md400) + 
                ((total_deposit*.13)* IN) + 
                ((total_deposit*.62)* bond) + 
                ((total_deposit *.06)* tbill)
                return(profit)
      }
  }
  
   for(sim in seq(1,n_simulations)){ # MC LOOP
      years_worked<-0 
      total_deposit<-0
      death_balance <- numeric(n_simulations)
      current_age <- age
      TRS <- 0
      profit <- 0
      retirment_age <- age+t
      sal_after_r <- c()
      average_annual <- c()
  # while working calc
    for(i in seq(1, t)){
      current_sal <- x
      growth <-(1+ pmax(rnorm(1,mean = .045, sd=0.04),0))
      current_sal <- current_sal*growth
      current_sal <- min(current_sal,maxed)
      current_age <- current_age +1
      years_worked<- years_worked + 1 
      #introducing randomness into our returns
      sp <- (1+ pmax(rnorm(1,mean = 0.0526, sd=0.03),0))
      md400 <- (1 + pmax(rnorm(1,mean =0.0695,sd = .03),0))
      IN <- (1 + pmax(rnorm(1,mean =0.0334,sd = .03),0))
      bond<-(1 + pmax(rnorm(1,mean =0.0513,sd = .03),0))
      tbill<-(1 + pmax(rnorm(1,mean =.0248,sd = .015),0))
      
      if(years_worked <= 7){
           total_deposit <- total_deposit + personal_deposit(current_sal) + .08*current_sal
      }else{
        total_deposit <- total_deposit + personal_deposit(current_sal) + .10*current_sal
      }
      
      balance <- total_deposit + investment_return(current_age = current_age,
                                                   total_deposit = total_deposit,
                                                   sp = sp,
                                                   md400 = md400,
                                                   IN = IN,
                                                   bond = bond,
                                                   tbill = tbill)
      
    } 
      #retirement calculations
      while (current_age < est_death) {
      current_age <- current_age + 1
      withdraw <- balance * withdrawl_rate
      sal_after_r <- append(sal_after_r,withdraw)
      balance <- balance - withdraw
      balance <- balance + investment_return(current_age = current_age,
                                                   total_deposit = total_deposit,
                                                   sp = sp,
                                                   md400 = md400,
                                                   IN = IN,
                                                   bond = bond,
                                                   tbill = tbill)
         
     
      }
         death_balance[sim]<- balance
         average_annual <- mean(sal_after_r)
         sim_results[sim]<- average_annual/12
         
   }
  
  cat("Avg Balance at Death",mean(death_balance),"\n")
 return(sim_results)
}

orp_x <- MC_ORP(x = 60000,age = 30,t = 30,maxed = Inf,est_death= 80,withdrawl_rate = .04,n_simulations = 5000)
ORP_sim <- data.frame(Monthly_Income = orp_x)
mean_income <-mean(TRS_sim$Monthly_Income)
ORP_mean_income <-mean(ORP_sim$Monthly_Income)
ORP_SD <- sd(ORP_sim$Monthly_Income)
### MC PLOT
ggplot(ORP_sim,aes(x = Monthly_Income))+
  geom_histogram(bins = 75,fill="purple",alpha=.5)+
  geom_vline(xintercept = ORP_mean_income, color = "red")+
  theme_clean()+
  labs(title = "Monte Carlo Simulated Monthly Benefit ORP)",x = "Monthly Income at 7.5% Withdrawal ($)", 
       y = "Freq.")

ORP_sim %>% filter(Monthly_Income < 12400) %>% 
  nrow()



ORP_system(60000,t =30,age = 30,est_death = 80,withdrawl_rate = .04)

library(shiny)


# UI
ui <- fluidPage(
  titlePanel("Monte Carlo Retirement Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("x", "Enter Salary ($):", value = 60000, min = 55001, max = 200000),
      numericInput("t", "Years Until Retirement:", value = 35, min = 1, max = 50),
      numericInput("maxed", "Set Salary Growth Cap for Realism(Optional) ($):", value = 10000000, min = 50000, max = 500000),
      numericInput("age", "Current Age:", value = 30, min = 20, max = 70),
      numericInput("est_death", "Estimated Death Age:", value = 85, min = 70, max = 120),
      numericInput("withdrawl_rate", "Withdrawal Rate (%):", value = 4, min = 1, max = 10, step = 0.1),
      numericInput("n_simulations", "Number of Simulations:", value = 100, min = 10, max = 10000),
      actionButton("simulate", "Run Simulation")
    ),
    
        mainPanel(
          plotOutput("resultsPlot")
        )
    )
  )

# Server
server <- function(input, output) {
  observeEvent(input$simulate, {
    # Run the simulation when the button is clicked
    sim_results <- MC_ORP(
      x = input$x,
      t = input$t,
      maxed = input$maxed,
      age = input$age,
      est_death = input$est_death,
      withdrawl_rate = input$withdrawl_rate / 100, # Convert to decimal
      n_simulations = input$n_simulations
    )
    
    # Render the plot
    output$resultsPlot <- renderPlot({
      ggplot(data.frame(Monthly_Income = sim_results), aes(x = Monthly_Income)) + 
        geom_histogram(bins = 75, fill = "purple", alpha = 0.5) +
        geom_vline(xintercept = mean(sim_results), color = "red") +
        theme_minimal() +
        labs(
          title = "Monte Carlo Simulated Monthly Benefit (ORP)",
          x = "Monthly Income at Withdrawal Rate ($)", 
          y = "Frequency"
        )
    })
  })
}


shinyApp(ui = ui, server = server)
install.packages("usethis")




