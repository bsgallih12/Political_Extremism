#Check This!!!

library(tidyverse)
library(haven)
library(tidytext)
library(psych)
library(wordcloud)

data <- data.frame(read.csv("PE_Study1.csv"))
data <- data[-1:-3,]

data <- data %>% select("D1","D2","D2_4_TEXT","D3","D4","D4_8_TEXT","D5","D5_15_TEXT","D6_1",
                        "CSE_1","CSE_2","CSE_3","CSE_4",
                        "SDO_1","SDO_2","SDO_3","SDO_4","SDO_5",
                        "SDO_6","SDO_7","SDO_8","SDO_9","SDO_10","SDO_11","SDO_12","SDO_13","SDO_14",
                        "Ind_1","IndAdj","Dem_1","DemAdj","Rep_1","RepAdj","EDem_1","EDemAdj",
                        "Trump_1","TrumpPromptAdj","ERep_1","ERepAdj","Biden_1","BidenAdj",
                        "Sanders_1","SandersAdj",
                        #ATT CHK
                        "CSE_5", "SDO_15", "ATT_3","ATT_4.","ATT_5","ATT_6","ATT_7.")

# Change Variable Names
data <- data %>% rename(ATT_1 = CSE_5,
                        ATT_2 = SDO_15,
                        ATT_4 = ATT_4.,
                        ATT_7 = ATT_7.)

# Recode Attention Checks (if_else() function)

data <- data %>% mutate(
  ATT_1 = if_else(ATT_1 == "Strongly Disagree",1,0),
  ATT_2 = if_else(ATT_2 == "Slightly Negative",1,0),
  ATT_3 = if_else(ATT_3 == "C",1,0),
  ATT_4 = if_else(ATT_4 == "A",1,0),
  ATT_5 = if_else(ATT_5 == "E",1,0),
  ATT_6 = if_else(ATT_6 == is.na(ATT_6),1,0),
  ATT_7 = if_else(ATT_7 == "B",1,0)
)

data <- data %>% mutate(
  att_total = ATT_1 + ATT_2 + ATT_3 + ATT_4 + ATT_5 + ATT_6 + ATT_7
)

# Filter By Attention Check Cutoff

data <- data %>% filter(att_total >= 5)

# Data Transformation Function (CSE)

cse_transform <- function(variable)
{
  df <- recode({{variable}},
                                         "Strongly Disagree"=1,
                                         "Moderately Disagree"=2,
                                         "Slightly Disagree"=3,
                                         "Neutral"=4,
                                         "Slightly Agree"=5,
                                         "Moderately Agree"=6,
                                         "Strongly Agree"=7)
}

data <- data %>% mutate(across(c(CSE_1,CSE_2,CSE_3,CSE_4),cse_transform))

data$cse_mean <- rowMeans(data[,c("CSE_1","CSE_2","CSE_3","CSE_4")], na.rm = TRUE)


# Data Transformation Function (SDO)

sdo_transform <- function(variable)
{
df <- recode({{variable}},
                    "Very Negative"=1,
                    "Negative"=2,
                    "Slightly Negative"=3,
                    "Neither Positive or Negative"=4,
                    "Slightly Positive"=5,
                    "Positive"=6,
                    "Very Positive"=7)
}

data <- data %>% mutate(across(c(SDO_1:SDO_14),sdo_transform))

# Reverse Code SDO
SDO <- data %>% select(SDO_1,SDO_2,SDO_3,SDO_4,SDO_5,SDO_6,SDO_7,SDO_8,SDO_9,SDO_10,SDO_11,SDO_12,SDO_13,SDO_14)
SDOKeys <- c(1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1)
SDO <- reverse.code(SDOKeys, SDO, mini = 1, maxi = 7)
data$sdo_mean <- rowMeans(SDO, na.rm = TRUE)


# Independent
Independent <- data %>% 
  select("D1","D2","D6_1","Ind_1","IndAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word,IndAdj) %>%  
  count(word,sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))

# Democrat
Democrat <- data %>% 
  select("D1","D2","D6_1","Dem_1","DemAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, DemAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))

# Republican
Republican <- data %>% 
  select("D1","D2","D6_1","Rep_1","RepAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, RepAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))

# Ext Democrat
Extreme_Democrat <- data %>% 
  select("D1","D2","D6_1", "EDem_1","EDemAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, EDemAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))


# Ext Republican
Extreme_Republican <- data %>% 
  select("D1","D2","D6_1","ERep_1","ERepAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, ERepAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))


# Trump
Trump <- data %>% 
  select("D1","D2","D6_1", "Trump_1","TrumpPromptAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, TrumpPromptAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))


# Biden
Biden <- data %>% 
  select("D1","D2","D6_1","Biden_1","BidenAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, BidenAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))


# Sanders
Sanders <- data %>% 
  select("D1","D2","D6_1","Sanders_1","SandersAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, SandersAdj) %>%  
  count(word, sort = TRUE) %>% 
  with(wordcloud(word,n, max.words = 10))



