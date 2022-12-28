# set wd

library(dplyr)
library(tidyr)
library(caTools)
library(nnet)
library(AER)

# 1. append 4 datasets

df1 <- read.csv("processed.cleveland.data", stringsAsFactors = TRUE)
df2 <- read.csv("processed.hungarian.data", stringsAsFactors = TRUE)
df3 <- read.csv("processed.switzerland.data", stringsAsFactors = TRUE)
df4 <- read.csv("processed.va.data", stringsAsFactors = TRUE)

df <- rbind(df1, df2, df3, df4)

# 2. summary dataset

summary(df)

# 3. cek missing value tiap column a.k.a. '?'

df[df == '?'] <- NA

sum(is.na(df$age))
sum(is.na(df$sex))
sum(is.na(df$cp))
sum(is.na(df$trestbps))
sum(is.na(df$chol))
sum(is.na(df$fbs))
sum(is.na(df$restecg))
sum(is.na(df$thalach))
sum(is.na(df$exang))
sum(is.na(df$oldpeak))
sum(is.na(df$slope))
sum(is.na(df$ca))
sum(is.na(df$thal))
sum(is.na(df$num))

# 4. ganti NA dengan nilai modus

getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df$trestbps[is.na(df$trestbps)] <- getmode(df$trestbps)
df$chol[is.na(df$chol)] <- getmode(df$chol)
df$fbs[is.na(df$fbs)] <- getmode(df$fbs)
df$restecg[is.na(df$restecg)] <- getmode(df$restecg)
df$thalach[is.na(df$thalach)] <-getmode(df$thalach)
df$exang[is.na(df$exang)] <- getmode(df$exang)
df$oldpeak[is.na(df$oldpeak)] <- getmode(df$oldpeak)
df$slope[is.na(df$slope)] <- getmode(df$slope)
df$ca[is.na(df$ca)] <- getmode(df$ca)
df$thal[is.na(df$thal)] <- getmode(df$thal)

sum(is.na(df$age))
sum(is.na(df$sex))
sum(is.na(df$cp))
sum(is.na(df$trestbps))
sum(is.na(df$chol))
sum(is.na(df$fbs))
sum(is.na(df$restecg))
sum(is.na(df$thalach))
sum(is.na(df$exang))
sum(is.na(df$oldpeak))
sum(is.na(df$slope))
sum(is.na(df$ca))
sum(is.na(df$thal))
sum(is.na(df$num))

summary(df)

# 5. keterangan column

#     1. age: age in years
#     2. sex: sex (1 = male; 0 = female)
#     3. cp: chest pain type
#         Value 1 = typical angina
#         Value 2 = atypical angina
#         Value 3 = non-anginal pain
#         Value 4 = asymptomatic
#     4. trestbps: resting blood pressure (in mm Hg on admission to the hospital)
#     5. chol: serum cholestoral in mg/dl
#     6. fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
#     7. restecg: resting electrocardiographic results
#         Value 0 = normal
#         Value 1 = having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
#         Value 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria
#     8. thalach: maximum heart rate achieved
#     9. exang: exercise induced angina (1 = yes; 0 = no)
#     10. oldpeak: ST depression induced by exercise relative to rest
#     11. slope: the slope of the peak exercise ST segment
#         Value 1 = upsloping
#         Value 2 = flat
#         Value 3 = downsloping
#     12. ca: number of major vessels (0-3) colored by flourosopy
#     13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
#     14. num: diagnosis of heart disease (angiographic disease status)
#         Value 0 = <50% diameter narrowing
#         Value 1 = >50% diameter narrowing (in any major vessel: attributes 59 through 68 are vessels)

# 6. cek total data dari dataset

count(df)

# 7. membagi dataset menjadi 2 (sebagian kecil untuk validasi dan sisanya untuk pembuatan model)

split <- sample.split(df, SplitRatio=0.8)
df_model <- df[split,]
df_test <- df[!split,]

# 8. membuat model multiple linear regression

# berdasarkan sex
model_linear1 <- lm(sex ~ ., data = df_model)
summary(model_linear1)

# 9. membuat model logistic regression

model_logistic <- multinom(num ~ ., data = df, MaxNWts=10000)
summary(model_logistic)
exp(coefficients(model_logistic))
coeftest(model_logistic)
