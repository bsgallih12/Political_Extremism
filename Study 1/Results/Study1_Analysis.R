library(tidyverse)
library(haven)
library(tidytext)
library(psych)
library(wordcloud)
library(textdata)

data <- data.frame(read.csv("PE_Study1.csv"))
data <- data[-1:-2,]

data <- data %>% select("D1","D2","D2_4_TEXT","D3","D4","D4_8_TEXT","D5","D5_15_TEXT","D6_1",
                        "CSE_PP_1","CSE_PP_2","CSE_PP_3","CSE_PP_4","CSE_PP_5","CSE_PP_6","CSE_PP_7","CSE_PP_8",
                        "SDO_1","SDO_2","SDO_3","SDO_4","SDO_5",
                        "SDO_6","SDO_7","SDO_8","SDO_9","SDO_10","SDO_11","SDO_12","SDO_13","SDO_14",
                        "Ind_1","IndAdj","Dem_1","DemAdj","Rep_1","RepAdj","EDem_1","EDemAdj",
                        "Trump_1","TrumpPromptAdj","ERep_1","ERepAdj","Biden_1","BidenAdj",
                        "Sanders_1","SandersAdj","Cheney_1","CheneyAdj",
                        #ATT CHK
                        "SDO_15", "ATT_3","ATT_4.","ATT_5","ATT_6","ATT_7.","ATT_8")

# Change Variable Names
data <- data %>% rename(ATT_1 = SDO_15,
                        ATT_2 = ATT_3,
                        ATT_3 = ATT_4.,
                        ATT_4 = ATT_5,
                       ATT_5 = ATT_6,
                        ATT_6 = ATT_7.,
                        ATT_7 = ATT_8)

str(data)

# Recode Attention Checks (if_else() function)

data <- data %>% mutate(
  ATT_1 = if_else(ATT_1 == "Somewhat Favor",1,0,missing = 0),
  ATT_2 = if_else(ATT_2 == "C",1,0,missing = 0),
  ATT_3 = if_else(ATT_3 == "A",1,0,missing = 0),
  ATT_4 = if_else(ATT_4 == "E",1,0,missing = 0),
  ATT_5 = if_else(ATT_5 == is.na(ATT_5),1,0),
  ATT_6 = if_else(ATT_6 == "B",1,0,missing = 0),
  ATT_7 = if_else(ATT_7 == "E",1,0, missing = 0)
)

data <- data %>% mutate(
  att_total = ATT_1 + ATT_2 + ATT_3 + ATT_4 + ATT_5 + ATT_6 + ATT_7
)

# Filter By Attention Check Cutoff

data_wo_attention <- data
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

data <- data %>% mutate(across(c(CSE_PP_1,CSE_PP_2,CSE_PP_3,CSE_PP_4,CSE_PP_5,CSE_PP_6,CSE_PP_7,CSE_PP_8),cse_transform))

data$cse_mean <- rowMeans(data[,c("CSE_PP_1","CSE_PP_2","CSE_PP_3","CSE_PP_4","CSE_PP_5","CSE_PP_6","CSE_PP_7","CSE_PP_8")], na.rm = FALSE)

# Data Transformation Function (SDO)

sdo_transform <- function(variable)
{
df <- recode({{variable}},
                    "Strongly Oppose"=1,
                    "Somewhat Oppose"=2,
                    "Slightly Oppose"=3,
                    "Neutral"=4,
                    "Slightly Favor"=5,
                    "Somewhat Favor"=6,
                    "Strongly Favor"=7)
}

data <- data %>% mutate(across(c(SDO_1:SDO_14),sdo_transform))

# Reverse Code SDO
SDO <- data %>% select(SDO_1,SDO_2,SDO_3,SDO_4,SDO_5,SDO_6,SDO_7,SDO_8,SDO_9,SDO_10,SDO_11,SDO_12,SDO_13,SDO_14)
SDOKeys <- c(1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1)
SDO <- reverse.code(SDOKeys, SDO, mini = 1, maxi = 7)
data$sdo_mean <- rowMeans(SDO, na.rm = FALSE)

affin <- get_sentiments("afinn")

# Independent
Independent <- data %>% 
  select("D1","D2","D6_1","Ind_1","IndAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word,IndAdj) %>% inner_join(affin)

describe(Independent$value)
  
# Independent Word Cloud
Independent_Word <- Independent %>% 
  count(word,sort = TRUE)

print(Independent_Word)

Independent_Cloud <- Independent_Word %>% 
  with(wordcloud(word,n, max.words = 10))

# Democrat
Democrat <- data %>% 
  select("D1","D2","D6_1","Dem_1","DemAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, DemAdj) %>%  
  inner_join(affin)

describe(Democrat$value)

# Democrat Word Cloud
Democrat_Word <- Democrat %>% 
  count(word, sort = TRUE)

print(Democrat_Word)

Democrat_Cloud <- Democrat_Word %>% 
  with(wordcloud(word,n, max.words = 10))

# Republican
Republican <- data %>% 
  select("D1","D2","D6_1","Rep_1","RepAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, RepAdj) %>%  
  inner_join(affin)

describe(Republican$value)

# Republican Word Cloud
Republican_Word <- Republican %>% 
  count(word, sort = TRUE)

print(Republican_Word)

Republican_Cloud <- Republican_Word %>% 
  with(wordcloud(word,n, max.words = 10))

# Ext Democrat
Extreme_Democrat <- data %>% 
  select("D1","D2","D6_1", "EDem_1","EDemAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, EDemAdj) %>%  
  inner_join(affin)

describe(Extreme_Democrat$value)

# Ext Democrat Word
Extreme_Democrat_Word <- Extreme_Democrat %>% 
  count(word, sort = TRUE)

print(Extreme_Democrat_Word)

Extreme_Democrat_Cloud <- Extreme_Democrat_Word%>% 
  with(wordcloud(word,n, max.words = 10))


# Ext Republican
Extreme_Republican <- data %>% 
  select("D1","D2","D6_1","ERep_1","ERepAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, ERepAdj) %>% 
  inner_join(affin)

describe(Extreme_Republican$value)

# Ext Republican Word
Extreme_Republican_Word <- Extreme_Republican %>% 
  count(word, sort = TRUE)

print(Extreme_Republican_Word)

Extreme_Republican_Cloud <- Extreme_Republican_Word %>% 
  with(wordcloud(word,n, max.words = 10))




# Trump
Trump <- data %>% 
  select("D1","D2","D6_1", "Trump_1","TrumpPromptAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, TrumpPromptAdj) %>%  
  inner_join(affin)

describe(Trump$value)

# Trump Word
Trump_Word <- Trump %>% 
  count(word, sort = TRUE)

print(Trump_Word)

Trump_Cloud <- Trump_Word %>% 
  with(wordcloud(word,n, max.words = 10))


# Biden
Biden <- data %>% 
  select("D1","D2","D6_1","Biden_1","BidenAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, BidenAdj) %>%
  inner_join(affin)

describe(Biden$value)

# Biden Word
Biden_Word <- Biden %>% 
  count(word, sort = TRUE)

print(Biden_Word)

Biden_Cloud <- Biden_Word %>% 
  with(wordcloud(word,n, max.words = 10))

# Sanders
Sanders <- data %>% 
  select("D1","D2","D6_1","Sanders_1","SandersAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, SandersAdj) %>%
  inner_join(affin)

describe(Sanders$value)

# Sanders Word
Sanders_Word <- Sanders %>% 
  count(word, sort = TRUE)

print(Sanders_Word)

Sanders_Cloud <- Sanders_Word %>% 
  with(wordcloud(word,n, max.words = 10))

# Cheney
Cheney <- data %>% 
  select("D1","D2","D6_1","Cheney_1","CheneyAdj","cse_mean","sdo_mean") %>% 
  unnest_tokens(word, CheneyAdj) %>%
  inner_join(affin)

describe(Cheney$value)

# Cheney Word
Cheney_Word <- Cheney %>% 
  count(word, sort = TRUE)

print(Cheney_Word)

Cheney_Cloud <- Cheney_Word %>% 
  with(wordcloud(word,n, max.words = 10))



