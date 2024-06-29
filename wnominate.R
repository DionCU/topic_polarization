# load packages
library(data.table)
library(wnominate)
library(dplyr)
library(tidyr)

# load dataset of votes with topics
data <- fread("df_member_votes_fv_with_topics_3.tsv")

# split dataset on topics
topics <- split(data, data$topic)

# this part is to split the topics up, in case your laptop can't handle running the entire thing
list_length <- length(topics)

# divide entire size into four parts
part_size <- ceiling(list_length / 4)

# split the list into four equal parts
topic_slices <- lapply(1:4, function(i) {
  start_index <- (i - 1) * part_size + 1
  end_index <- min(i * part_size, list_length)
  topics[start_index:end_index]
})

# access each part
topics1 <- topic_slices[[1]]
topics2 <- topic_slices[[2]]
topics3 <- topic_slices[[3]]
topics4 <- topic_slices[[4]]

# create list for results
results_topic <- list()

# loop through topics (change to topic parts if too large)
for (number in names(topics)) {
  topic <- topics[[number]]
  
  # keep only columns needed for NOMINATE method
  reshaped_data <- topic %>%
    select(nameparty_id, party, bill_id, cast_code)
  
  # reshape data into pivot table, if no cast code fill value with 0
  pivot_data <- reshaped_data %>% 
    pivot_wider(names_from = bill_id, values_from = cast_code, values_fill = 0)
  
  # turn data into matrix
  matrix_data <- as.matrix(pivot_data)
  
  # set nameparty_id to be unique id
  ids <- matrix_data[,1]
  # create dataframe with party types and remove party and nameparty_id from matrix
  legData <- matrix(matrix_data[,2],length(matrix_data[,2]),1) 
  colnames(legData) <- "party" 
  matrix_data <- matrix_data[,-c(1,2)]
  
  # create roll call object using matrix data
  rc <- rollcall(matrix_data, yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9),
                 notInLegis=0, legis.names = ids, legis.data=legData) 
  
  tryCatch({
    # calculate nominate values for legislators based on name party id of conservative politician
    result <- wnominate(rc, dims=1, polarity=c('np_1870'))
    # paste values in list
    results_topic4[[paste0("result_", number)]] <- result
  }, error = function(e) {
    print(paste("Error occurred for", number, ":"))
    print(e)
  })
  
  print(number)
}

# save results
save(results_topic, file = "results_topic.RData")

# for each topic, save values to csv file
df <- results_topic4$`result_social services and public welfare`$legislators
write.table(df, file = "df_soc.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_government budget and administration`$legislators
write.table(df, file = "df_con.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_environmental and natural resources`$legislators
write.table(df, file = "df_env.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_infrastructure and development`$legislators
write.table(df, file = "df_inf.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_defense and military`$legislators
write.table(df, file = "df_def.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_legislation and policy`$legislators
write.table(df, file = "df_leg.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

df <- results_topic4$`result_international relations and government`$legislators
write.table(df, file = "df_for.csv", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

