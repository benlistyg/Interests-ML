# Multinomial Logit Model vs Random Forest
# Predicting College Major from Items
library(data.table) # For fread function (v fast!)
library(dplyr) # For pre-processing
library(tm) # For cleaning text / pre-processing
library(nnet) #for MNL
library(randomForest) #For rf
library(caret)

# # Helper function for making training / testing sets.
# outersect <- function(x, y) {
#   sort(c(setdiff(x, y),
#          setdiff(y, x)))
# }
# 
# 

# Normalizing majors (entered as free response data)
# majors <- 

data.table::fread(input = paste(getwd(),'/data.csv',sep=''), sep = '\t',stringsAsFactors = F) %>%
  filter(education %in% c(3,4)) %>% 
  .$major %>% 
  matrix() %>% 
  cbind(.,apply(X = ., MARGIN = 1, FUN = nchar)) %>% 
  data.frame %>% 
  `colnames<-`(c('MAJOR','nchar')) %>% 
  mutate(MAJOR = as.character(MAJOR),
         nchar=as.numeric(nchar)) %>% 
  filter(nchar > 1) %>% 
  dplyr::select(MAJOR) %>% 
  .$MAJOR %>% 
  grep(pattern = 'and', x = ., ignore.case = T, value = T, invert = T) %>%
  grep(pattern = 'Yes', x = ., ignore.case = T, value = T, invert = T) %>%
  # grep(pattern = " ", x = ., ignore.case = T, value = T, invert = T) %>% 
  grep(pattern = 'NO', x = ., ignore.case = T, value = T, invert = T) %>% 
  grep(pattern = 'N/A', x = ., ignore.case = T, value = T, invert = T) %>% 
  grep(pattern = 'NIL', x = ., ignore.case = T, value = T, invert = T) %>% 
  # gsub(pattern = 'IT',replacement = 'INFORMATION TECHNOLOGY', x = ., ignore.case = T) %>%
  gsub(pattern = 'HUMAN RESOURCES',replacement = 'HR', x = ., ignore.case = T) %>% 
  gsub(pattern = 'HUMAN RESOURCE',replacement = 'HR', x = ., ignore.case = T) %>% 
  gsub(pattern = 'HUMAN RESOURCE MANAGEMENT',replacement = 'HR', x = ., ignore.case = T) %>% 
  gsub(pattern = 'MBA',replacement = 'BUSINESS', x = ., ignore.case = T) %>% 
  gsub(pattern = 'MATHEMATICS',replacement = 'MATH', x = ., ignore.case = T) %>% 
  gsub(pattern = 'MATHS',replacement = 'MATH', x = ., ignore.case = T) %>% 
  gsub(pattern = 'PSYCHOLOGY',replacement = 'PSYCH', x = ., ignore.case = T) %>% 
  gsub(pattern = 'BUSINESS ADMINISTRATION',replacement = 'BUSINESS', x = ., ignore.case = T) %>%
  gsub(pattern = 'BUSINESS ADMIN',replacement = 'BUSINESS', x = ., ignore.case = T) %>%
  gsub(pattern = 'POLITICS',replacement = 'POLITICAL SCIENCE', x = ., ignore.case = T) %>%
  gsub(pattern = 'GOVERNMENT',replacement = 'POLITICAL SCIENCE', x = ., ignore.case = T) %>%
  gsub(pattern = 'FILM STUDIES',replacement = 'FILM', x = ., ignore.case = T) %>%
  gsub(pattern = 'BUSINESS MANAGEMENT',replacement = 'BUSINESS', x = ., ignore.case = T) %>%
  gsub(pattern = 'COMMUNICATIONS',replacement = 'COMMUNICATION', x = ., ignore.case = T) %>%
  gsub(pattern = 'INFORMATION SCIENCE',replacement = 'INFORMATICS', x = ., ignore.case = T) %>%
  gsub(pattern = 'INFORMATION SCIENCE',replacement = 'INFORMATICS', x = ., ignore.case = T) %>%
  gsub(pattern = 'HR MANAGEMENT',replacement = 'HR', x = ., ignore.case = T) %>%
  gsub(pattern = 'HRM',replacement = 'HR', x = ., ignore.case = T) %>%
  gsub(pattern = 'LEGAL STUDIES',replacement = 'LAW', x = ., ignore.case = T) %>%
  gsub(pattern = 'FINE ART',replacement = 'ART', x = ., ignore.case = T) %>%
  gsub(pattern = 'ARTS',replacement = 'ART', x = ., ignore.case = T) %>%
  gsub(pattern = 'ACCOUNTANCY',replacement = 'ACCOUNTING', x = ., ignore.case = T) %>%
  gsub(pattern = 'SOCIAL SCIENCES',replacement = 'SOCIAL SCIENCE', x = ., ignore.case = T) %>%
  gsub(pattern = 'IO PSYCH',replacement = 'INDUSTRIAL PSYCH', x = ., ignore.case = T) %>%
  gsub(pattern = 'ORG PSYCH',replacement = 'INDUSTRIAL PSYCH', x = ., ignore.case = T) %>%
  gsub(pattern = 'ORGANIZATIONAL PSYCH',replacement = 'INDUSTRIAL PSYCH', x = ., ignore.case = T) %>%
  gsub(pattern = 'ENGINEER',replacement = 'ENGINEERING', x = ., ignore.case = T) %>%
  gsub(pattern = 'PSYCOLOGY',replacement = 'PSYCHOLOGY', x = ., ignore.case = T) %>%
  gsub(pattern = 'PSICOLOGIA',replacement = 'PSYCHOLOGY', x = ., ignore.case = T) %>%
  gsub(pattern = 'THEATRE',replacement = 'THEATER', x = ., ignore.case = T) %>%
  gsub(pattern = 'PSCHOLOGY',replacement = 'PSYCH', x = ., ignore.case = T) %>%
  gsub(pattern = 'PYSCHOLOGY',replacement = 'PSYCH', x = ., ignore.case = T) %>%
  gsub(pattern = 'COMPUTER SCIENCES',replacement = 'COMPUTER SCIENCE', x = ., ignore.case = T) %>%
  gsub(pattern = 'POLITICAL SCIENCES',replacement = 'POLITICAL SCIENCE', x = ., ignore.case = T) %>%
  gsub(pattern = 'HR DEVELOPMENT', replacement = "HR", x = ., ignore.case = T) %>% 
  gsub(pattern = 'HEALTH CARE ADMINISTRATION', replacement = "HEALTHCARE", x = ., ignore.case = T) %>% 
  gsub(pattern = 'HEALTHCARE MANAGEMENT', replacement = "HEALTHCARE", x = ., ignore.case = T) %>% 
  gsub(pattern = 'BUISNESS', replacement = 'BUSINESS', x = ., ignore.case = T) %>% 
  gsub(pattern = 'LEGAL', replacement = 'LAW', x = ., ignore.case = T) %>% 
  gsub(pattern = 'HEALTH CARE', replacement = 'HEALTHCARE', x = ., ignore.case = T) %>% 
  gsub(pattern = 'MANAGMENT', replacement = 'MANAGEMENT', x = ., ignore.case = T) %>% 
  gsub(pattern = 'HUMAN SERVICES', replacement = 'HUMAN SERVICE', x = ., ignore.case = T) %>% 
  gsub(pattern = 'RELIGIOUS STUDIES', replacement = 'RELIGION', x = ., ignore.case = T) %>% 
  gsub(pattern = 'RELIGION STUDIES', replacement = 'RELIGION', x = ., ignore.case = T) %>% 
  toupper() %>% 
  trimws() %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  table() %>% 
  reshape2::melt() %>%
  arrange(-value) %>% 
  .[-1,] %>% 
  .[1:300,]

# Final data set
vi_data <- data.table::fread(input = paste(getwd(),'/data.csv',sep=''), sep = '\t',stringsAsFactors = F) %>% 
  mutate(MAJOR = toupper(major)) %>% 
  dplyr::select(., -major) %>% 
  filter(MAJOR %in% as.character(majors$.[1:x])) %>% 
  filter(!is.na(MAJOR)) %>%
  filter(education %in% c(3,4)) %>% 
  dplyr::select(R1:C8, MAJOR) %>% 
  mutate(R = rowSums(dplyr::select(., grep("R[0-9]", names(.)))),
         I = rowSums(dplyr::select(., grep("I[0-9]", names(.)))),
         A = rowSums(dplyr::select(., grep("A[0-9]", names(.)))),
         S = rowSums(dplyr::select(., grep("S[0-9]", names(.)))),
         E = rowSums(dplyr::select(., grep("E[0-9]", names(.)))),
         C = rowSums(dplyr::select(., grep("C[0-9]", names(.))))) %>% 
  mutate(MAJOR = as.factor(MAJOR)) %>% 
  dplyr::select(R,I,A,S,E,C,MAJOR)

# test_rows  <- sample(1:nrow(vi_data), size = nrow(vi_data)*.2, replace = F)
# train_rows <- outersect(x = 1:nrow(vi_data), y = test_rows)
# 
# mnl <- nnet::multinom(MAJOR ~ R+I+A+S+E+C, data=vi_data[train_rows,])
# 
# rf <- randomForest::randomForest(MAJOR ~ ., data=vi_data[train_rows,])
# 
# list(
# `Multi-Nomial Logit` = data.frame(vi_data[test_rows,], 
#            prediction = predict(mnl, vi_data[test_rows,-ncol(vi_data)])) %>% 
#   dplyr::select(MAJOR, prediction) %>% 
#   table(.)
# ,
# `Random Forest` = data.frame(vi_data[test_rows,], 
#            prediction = predict(rf, vi_data[test_rows,-ncol(vi_data)])) %>% 
#   dplyr::select(MAJOR, prediction) %>% 
#   table(.)
# 
# )

data_ctrl <- trainControl(method = "cv", number = 5, classProbs = T)

model_caret <- train(MAJOR ~ R+I+A+S+E+C,
                     data = vi_data,                        
                     trControl = data_ctrl,              
                     method = "rf")

# To compare each method, we examine the cross-validated log-loss of each model as a function of the number of target majors we wish to predict as a function of an individual's vocational interests.













