library(tidyverse)
library(caret)

df_metadata <- read_csv("datasets/hdi/gender-metadata.csv")

df_data <- read_csv("datasets/hdi/gender-data.csv")

nanmax = function(v) { ifelse(all(is.na(v)), NA, max(v, na.rm=TRUE)) }
nanmin = function(v) { ifelse(all(is.na(v)), NA, min(v, na.rm=TRUE)) }

df_data <- df_data %>%
    rowwise() %>%
    mutate(
        min=nanmin(c_across(where(is.numeric))) + runif(1, 0, 0.1),
        max=nanmax(c_across(where(is.numeric))) + runif(1, 0, 0.1),
    ) %>%
    select(-c(`2000 [YR2000]`:`2021 [YR2021]`, `Series Name`)) %>% 
    pivot_wider(
        names_from = `Series Code`,
        values_from = c(min, max),
        names_vary = "slowest"
    ) %>%
    drop_na()

df_hdi <- read_csv("datasets/hdi/human-development-index.csv")
df_hdi <- df_hdi %>% filter(Year == 2017) %>% select(Code, `Human Development Index (UNDP)`)


df <- inner_join(df_hdi, df_data, by= c("Code" = "Country Code"))

source("methods/maint.R")
source("methods/maint_fh.R")
source("methods/kknn.R")
source("methods/kpcao.R")
source("methods/polr.R")

levels <- c(3:3)
iterations <- c(1:1)
methods <- c("MAINT", "POLR", "MAINT + FH", "KKNN", "KPCAO")
functions <- c(maint, polr, maint_fh, kknn, kpcao)

res <- array(0, dim = c(length(levels), length(iterations), length(methods)), dimnames = list(levels, iterations, methods))


for (l in levels) {
    print(l)
    data <- df %>% mutate(`Human Development Index (UNDP)` = gtools::quantcut(`Human Development Index (UNDP)`, q=l, ordered = T))
    
    
    sint.IData <- IData(data %>% select(-Code, -`Human Development Index (UNDP)`, -`Country Name`))
    sint.labels <- data %>% pull(`Human Development Index (UNDP)`)
    
    n_levels <- length(levels(sint.labels))
    sint.labels <- factor(as.numeric(sint.labels), levels = 1:n_levels, order = T)
    
    m1_acc <- c()
    m2_acc <- c()
    m3_acc <- c()
    m4_acc <- c()
    m5_acc <- c()
    for (i in iterations) {
        train_id <- sample.int(n = nrow(sint.IData), size = floor(0.8 * nrow(sint.IData)), replace = F)
        
        train.Idata = sint.IData[train_id,]
        train.labels = sint.labels[train_id]
        
        test.Idata = sint.IData[-train_id,]
        test.labels = sint.labels[-train_id]
        
        for (j in 1:length(methods)) {
            model <- functions[[j]](train.Idata, train.labels)
            model.res <- predict(model, test.Idata)
            cm <- confusionMatrix(model.res, test.labels)
            acc <- cm$overall["Accuracy"]  
            res[as.character(l), as.character(i), methods[j]] <- acc
        } 
    }
}

saveRDS(res, "results.rds")
