## function: Input MxN matrix, where rows are indv cases, and columns are time;
## returns data frame with addition cols for growth, weather

mx2df <- function(x) {
  y_df <- data.frame(y = x[which(!is.na(x))] %>% round(2),
                     which(!is.na(x), arr.ind = TRUE)) %>%
    select(y, r = row, c = col) %>%
    tbl_df()
  
  ## link weather data to cols / time
  lvls = c(0:14)
  lbls = c(201.422,	242.57,	314.706,	436.626,	427.99,	140.208,	288.544, 
           305.562,	458.978,	586.232,	279.146,	74.168,	207.01, 173.228)
  y_df <- mutate(y_df, wx = cut(y_df$c, breaks = lvls, labels = lbls))
  y_df$wx <- as.numeric(levels(y_df$wx))[y_df$wx]
  
  y_df <- y_df %>% arrange(r) %>% group_by(r) %>%
    mutate(z = lag(y), x = lag(c), yz = y - z, cx = c - x) %>%
    select(y, r, c, prev_sz = z, grw = yz, timelag = cx, wx = wx) %>%
    filter(grw > 0) %>%
    na.omit() %>%
    ungroup()
  
  return(y_df)
  
}