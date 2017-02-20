fisher_method_rev <- function(
  x,
  id = NULL,
  threshold = .5,
  statcheck = FALSE
)
{
  if (statcheck == TRUE)
  {
    res <- fisher_method_rev_statcheck(x)
  }
  else
  {
    if(is.null(id)) id <- rep(1, length(x))
    if(length(x) != length(id)) stop('Vectors x and id need to be equal length') # Create a test here
    if(sum(x > 1 | x < 0, na.rm = TRUE) > 0) stop('Vector x contains values outside 0-1 range') # Create test
    if(threshold > 1 | threshold < 0) stop('Incorrectly specified threshold level.
                                         Please specify between 0-1.') # Create test
    
    df <- data.frame(x, id)
    
    res <- plyr::ddply(df, .(id), function(x)
    {
      sel <- x$x[x$x >= threshold]
      
      fisher <- -2 * sum(log(1 - ((sel - threshold) / (1 - threshold))))
      df_fisher <- length(sel) * 2
      p_fisher <- pchisq(q = fisher, df = df_fisher, lower.tail = FALSE)
      
      res <- data.frame(
        fisher,
        df_fisher,
        p_fisher)
      return(res)
    }, .progress = "text")
  }
  
  return(res)
}

fisher_method_rev_statcheck <- function(x)
{
  res <- plyr::ddply(x, .(Source), function(x)
  {
    sel <- x$Computed[x$Computed >= threshold]
    
    fisher <- -2 * sum(log(1 - ((sel - threshold) / (1 - threshold))))
    df_fisher <- length(sel) * 2
    p_fisher <- pchisq(q = fisher, df = df_fisher, lower.tail = FALSE)
    
    res <- data.frame(
      fisher,
      df_fisher,
      p_fisher)
    return(res)
  }, .progress = "text")
  
  return(res)
}
