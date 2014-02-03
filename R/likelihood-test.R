likelihood.test = function(x) {
  nrows = dim(x)[1]                      # no. of rows in contingency table
  ncols = dim(x)[2]                      # no. of cols in contingency table
  chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
  table = chi.out[[6]]                   # get the OFs
  ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
  sum = 0                                # storage for the test statistic
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sum = sum + table[i,j]*log(ratios[i,j])
    }
  }
  sum = 2 * sum                          # the likelihood ratio chi square
  df = chi.out[[2]]                      # degrees of freedom
  p = 1 - pchisq(sum,df)                 # p-value
  out = c(sum, df, p, chi.out[[1]])      # the output vector
  names(out) = c("LRchi2","df","p-value","Pearschi2")
  return(as.list(out))                           # done!
}
