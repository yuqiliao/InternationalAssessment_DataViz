### ICILS RISE Webinar Analysis
### Yuqi
### 2/1/23

library(EdSurvey)
library(tidyverse)
library(magrittr)

### read in and clean up #####

ICILS2018 <- readICILS("C:/EdSurveyData/ICILS/ICILS2018_IDB_SPSS/Data", countries = "usa", dataSet = "teacher")
View(showCodebook(ICILS2018))

#it2g14f - Your School/Use of ICT in teaching at your school/There is enough time to prepare lessons that incorporate ICT.
#it2g14g - Your School/Use of ICT in teaching at your school/There is sufficient opportunity for me to develop expertise in ICT.
#it2g14h - Your School/Use of ICT in teaching at your school/There is sufficient technical support to maintain ICT resources.
levelsSDF(data = ICILS2018, varnames = "it2g14f")
levelsSDF(data = ICILS2018, varnames = "it2g14g")
levelsSDF(data = ICILS2018, varnames = "it2g14h")

#it2g11a (a to j - all 10 teaching practices) - a: ICT and Teaching/Use of ICT for the following practices/Presenting information through direct class instruction
levelsSDF(data = ICILS2018, varnames = "it2g11a")
levelsSDF(data = ICILS2018, varnames = "it2g11j")

#first, try to replicate table 2 in the teacher report
t1 <- edsurveyTable(idteach ~ it2g11a , data = ICILS2018, jrrIMax = Inf, recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT")))
t1

# the above result doesn't match with Elaine Li's. after checking with Yan, I learned that I need to exclude students who responded "I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS" for each of the it2g11* variables.


# get light.edusvery.data.frame
ICILS2018df <- getData(ICILS2018, varnames = colnames(ICILS2018), omittedLevels = FALSE, addAttributes = TRUE)

ICILS2018dfNew <- ICILS2018df %>% 
  mutate(it2g14f_new = case_when(
    it2g14f %in% ("STRONGLY AGREE") ~ 2,
    it2g14f %in% ("AGREE") ~ 1,
    it2g14f %in% ("DISAGREE") ~ -1,
    it2g14f %in% ("STRONGLY DISAGREE") ~ -2
  ),
  it2g14g_new = case_when(
    it2g14g %in% ("STRONGLY AGREE") ~ 2,
    it2g14g %in% ("AGREE") ~ 1,
    it2g14g %in% ("DISAGREE") ~ -1,
    it2g14g %in% ("STRONGLY DISAGREE") ~ -2
  ),
  it2g14h_new = case_when(
    it2g14h %in% ("STRONGLY AGREE") ~ 2,
    it2g14h %in% ("AGREE") ~ 1,
    it2g14h %in% ("DISAGREE") ~ -1,
    it2g14h %in% ("STRONGLY DISAGREE") ~ -2
  )) %>% 
  # if any of the it2g14*_new is NA, the sum will be NA and will be dropped later
  mutate(perceptionICT = it2g14f_new+it2g14g_new+it2g14h_new) %>% 
  mutate(perceptionICTCategory = case_when(
    is.na(perceptionICT) ~ "NA",
    perceptionICT > 0 ~ "positive",
    perceptionICT < 0 ~ "negative",
    perceptionICT == 0 ~ "NA"
  )) 
  # %>%
  # select(contains(c("it2g14f", "it2g14g", "it2g14h", "perceptionICT"))) %>% 
  # view()


### analysis #####

#second attemp - try to replicate table 2 in the teacher report
t2 <- edsurveyTable(idteach ~ it2g11a , 
                    data = ICILS2018dfNew %>% 
                      filter(!it2g11a %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")), 
                    jrrIMax = Inf, 
                    recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT")))
t2

# it matched!!!

# try to replicate another table just to be sure - table 5
t3 <- edsurveyTable(idteach ~ it2g14f + it2g11a , 
                    data = ICILS2018dfNew %>% 
                      filter(!it2g11a %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")), 
                    jrrIMax = Inf, 
                    recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT"),
                                  it2g14f = list(from = c("STRONGLY AGREE", "AGREE"), to = "agree or strongly agree"),
                                  it2g14f = list(from = c("STRONGLY DISAGREE", "DISAGREE"), to = "disagree or strongly disagree"))) %>% 
  #similar to t3$data
  extract2("data") %>% 
  #select columns of interests
  select(1,2,5,6) %>% 
  #select rows of interests
  filter(it2g14f == "agree or strongly agree" | it2g14f == "disagree or strongly disagree") %>% 
  filter(it2g11a  == "often or always use this ICT")

t3


# now, modify the table 5 above slightly by dropping is.na(perceptionICT) [92 rows] & perceptionICT==0 [52 rows] as requested by Yan - this is the same as dropping perceptionICTCategory == "NA".

t4 <- edsurveyTable(idteach ~ it2g14f + it2g11a , 
                      data = ICILS2018dfNew %>% 
                      filter(!it2g11a %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")) %>% 
                      filter(!perceptionICTCategory == "NA"),
                    jrrIMax = Inf, 
                    recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT"),
                                  it2g14f = list(from = c("STRONGLY AGREE", "AGREE"), to = "agree or strongly agree"),
                                  it2g14f = list(from = c("STRONGLY DISAGREE", "DISAGREE"), to = "disagree or strongly disagree"))) %>% 
  #similar to t3$data
  extract2("data") %>% 
  #select columns of interests
  select(1,2,5,6) %>% 
  #select rows of interests
  filter(it2g14f == "agree or strongly agree" | it2g14f == "disagree or strongly disagree") %>% 
  filter(it2g11a  == "often or always use this ICT")

t4


# now, create the tables of interest - using `perceptionICT` created earlier and keep the dropping logic as above (perceptionICTCategory == "NA").

row1 <- edsurveyTable(idteach ~ perceptionICTCategory + it2g11a , 
                    data = ICILS2018dfNew %>% 
                      filter(!it2g11a %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")) %>% 
                      filter(!perceptionICTCategory == "NA"),
                    jrrIMax = Inf, 
                    recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT"))) %>% 
  #similar to t3$data
  extract2("data") %>% 
  #select columns of interests
  select(1,2,5,6) %>% 
  #select rows of interests
  filter(it2g11a  == "often or always use this ICT")

row1


# now create a function to get the PCT and SE(PCT) for each row (each teaching practice)
getTeacherPracticePct <- function(teachingPractice, dataframe){
  
  recodeList1 <- list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT")
  #using `setNames` so `teachingPractice` gets evaluated as `it2g11a`, not as the `teachingPractice` string.
  recodeList2 <- setNames(list(recodeList1), teachingPractice)
  
  row <- edsurveyTable(as.formula(paste0("idteach ~ perceptionICTCategory + ", teachingPractice)) , 
                       data = dataframe %>% 
                         filter(!.data[[teachingPractice]] %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")) %>% 
                         filter(!.data[["perceptionICTCategory"]] == "NA"),
                       jrrIMax = Inf, 
                       recode = recodeList2
  )  %>%
    #similar to t3$data
    extract2("data") %>%
    #select columns of interests
    select(all_of( c("perceptionICTCategory",teachingPractice,"PCT", "SE(PCT)") )) %>%
    #select rows of interests
    filter(.data[[teachingPractice]]  == "often or always use this ICT") %>% 
    #round digits for easier result compliation
    mutate(`PCT` = round(`PCT`, 0),
           `SE(PCT)` = round(`SE(PCT)`, 1))
  
  return(row)
}

getTeacherPracticePct(teachingPractice = "it2g11a", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11b", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11c", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11d", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11e", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11f", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11g", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11h", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11i", dataframe = ICILS2018dfNew)
getTeacherPracticePct(teachingPractice = "it2g11j", dataframe = ICILS2018dfNew)


# calculate accurate t-tests (aka not rounding and do "summarize" in the end)

getTeacherPracticePctBtwGrpSig <- function(teachingPractice, dataframe){
  
  recodeList1 <- list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT")
  #using `setNames` so `teachingPractice` gets evaluated as `it2g11a`, not as the `teachingPractice` string.
  recodeList2 <- setNames(list(recodeList1), teachingPractice)
  
  row <- edsurveyTable(as.formula(paste0("idteach ~ perceptionICTCategory + ", teachingPractice)) , 
                       data = dataframe %>% 
                         filter(!.data[[teachingPractice]] %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")) %>% 
                         filter(!.data[["perceptionICTCategory"]] == "NA"),
                       jrrIMax = Inf, 
                       recode = recodeList2
  )  %>%
    #similar to t3$data
    extract2("data") %>%
    #select columns of interests
    select(all_of( c("perceptionICTCategory",teachingPractice,"PCT", "SE(PCT)") )) %>%
    #select rows of interests
    filter(.data[[teachingPractice]]  == "often or always use this ICT") 
  
  #perform independent t-test
  numerator <- row$PCT[1]-row$PCT[2]
  
  denominator <- sqrt( row$`SE(PCT)`[1]^2 + row$`SE(PCT)`[2]^2 )
  
  tScore <- numerator/denominator
  
  return(tScore)
}

getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11a", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11b", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11c", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11d", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11e", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11f", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11g", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11h", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11i", dataframe = ICILS2018dfNew)
getTeacherPracticePctBtwGrpSig(teachingPractice = "it2g11j", dataframe = ICILS2018dfNew)



###experienced - successfully replicate table 5 in the report
t_exles
summary2(ICILS2018dfNew, "t_exles")

it2g05a
summary2(ICILS2018dfNew, "it2g05a") #it looks the same as the derived variable. Yan recommends using this one.

t5 <- edsurveyTable(idteach ~ it2g05a + it2g11a , 
                    data = ICILS2018dfNew %>% 
                      filter(!it2g11a %in% ("I DO NOT USE THIS PRACTICE WITH THE REFERENCE CLASS")) ,
                    jrrIMax = Inf, 
                    recode = list(it2g11a = list(from = c("I OFTEN USE ICT WITH THIS PRACTISE", "I ALWAYS USE ICT WITH THIS PRACTISE"), to = "often or always use this ICT"),
                                  it2g05a = list(from = c("NEVER", "LESS THAN TWO YEARS"), to = "never or less than 2"))) %>% 
  #similar to t3$data
  extract2("data") %>% 
  #select columns of interests
  select(1,2,5,6) %>% 
  #select rows of interests
  filter(it2g11a  == "often or always use this ICT")

t5




