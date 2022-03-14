load_all_packages <- function(vector){
  for (i in vector) {
    if (!requireNamespace(i, quietly = TRUE)){
      install.packages(i)
    }
    library(i,character.only=T)
  }

}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

add_lagged_rolls <- function(df, cols, weight_vec, k) {
  for(col in cols) {
    df$temp = df[[col]]
    df = df %>%
      mutate(
        temp_rolling = lag(round(
          (weight_vec[1]*ifelse(is.na(rollmean(temp,k,fill=NA,align="right")),
                     cummean(temp),
                     rollmean(temp,k,fill=NA,align="right")) +
             weight_vec[2]*cummean(temp)),2)
          ,1)
      ) %>%
      select(-temp)
    var_name = paste0(col,"_rolling_lag")
    df[[var_name]] = df$temp_rolling
    df = df %>%
      select(-temp_rolling)
  }
  return(df)
}

calc_pace_vars <- function(df, cols, rm_cols = F){

  for(col in cols){
    df$temp = df[[col]]
    roll_col = paste0(col,'_rolling_lag')
    df$temp1 = df[[roll_col]]
    df = df %>%
      mutate(
        temp_merged = ifelse(
          is.nan(temp/temp1) | (temp/temp1 == Inf),1,
          ifelse(temp/temp1>3,3,temp/temp1))
        ) %>%
      select(-c(temp,temp1))
    var_name = paste0(col,"_pace")
    df[[var_name]] = df$temp_merged
    df = df %>%
      select(-temp_merged)
  }
  return(df)

}
