## utils function
explore <- function(df, var=colnames(df)){lapply(df %>% select(any_of(var)),
                                                 function(x) list(table(x, useNA = "always"), 
                                                                  paste0("% of non NA values: ", paste0(round(sum(!is.na(x))/length(x)*100, 2), "%. ", class(x), " format.")), 
                                                                  paste0("number of observations: ", sum(!is.na(x)))))}

return.cols.with <- function(df, pattern){colnames(df)[unlist(lapply(df, function(x) sum(pattern %in% unique(x))>0))]}
return.cols.contains <- function(df, pattern){colnames(df)[unlist(lapply(df, function(x) sum(str_detect(na.omit(unique(x)), pattern))>0))]}

