# Define function to assemble dataframe of ranked statistics
ranked_columns <- function(df,col_list,max_rank,seper){
  base = df %>% select(-col_list)
  base_names = names(base)
  ranks = seq(1,max_rank,by=1)

  for(rank in ranks){

    base = base %>%
      mutate(AVG_RANK_RANK = rank) %>%
      left_join(df,by=base_names)
    colnames(base)[(length(base_names)+length(col_list)*(rank-1)+1):length(colnames(base))] <-
      paste(col_list,rank,sep=seper)

  }
  return(base)
}
