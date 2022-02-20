library(tidyverse)
library(sparklyr)

# installing and connecting to Spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# getting data
df1 <- iris %>% as_tibble
# copying into Spark
df <- copy_to(sc, df1)

# checking class
class(df1)
class(df)
# df1 is a normal tibble, while df is a Spark table

# checking column names
colnames(df1)
colnames(df)
# the Spark table replaces periods with underscores, probably because periods have some syntactical property in Spark or related software that precludes them from being used in object names

#### RDD examples using tidyverse
# selecting first 6 observations of sepal length and species where sepal length > 5.5
df %>% filter(Sepal_Length>5.5) %>% select(Sepal_Length, Species) %>% head %>% print
# compute average sepal length and number of obserations for each species in the dataset
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length, na.rm = TRUE),
                                              count = n()) %>% head %>% print
# sort the above by species name
df2 %>% arrange(Species) %>% print
