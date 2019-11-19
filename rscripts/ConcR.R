#' @title Concordancing Loaded Files
#'
#' @description This function creates keyword-in-context (KWIC) displays from a vector of cahracter strings.
#' @param strings A vector of character strings.
#' @param pattern A charater string representing the pattern that the files are searched for which can contain regular expressions.
#' @param context A numeric value which determines how man characters are displayed before and after the search results.
#' @export
#' @keywords Concordancing, Character Srtings, Function.
#' @seealso
#' @return NULL
#' @aliases
#' @examples \dontrun{
#' Example code will come later!
#' }
ConcR <- function(strings, pattern, context) {
  # activate packages
  require(stringr)
  require(plyr)
  # list files
  conc <- sapply(strings, function(x) {
    txt <- str_replace_all(x, " {2,}" , " ")
    txt <- str_trim(txt, side = "both")
    lngth <- as.vector(unlist(nchar(txt)))
    idx <- str_locate_all(txt, pattern)
    idx <- idx[[1]]
    idx1 <- as.vector(unlist(idx[,1]))
    idx2 <- as.vector(unlist(idx[,2]))
    token.idx.start <- idx1
    token.idx.end <- idx2
    pre.idx.start <- token.idx.start-context
    pre.idx.end <- token.idx.start-1
    post.idx.start <- idx2+1
    post.idx.end <- idx2+context
    enddf <- cbind(post.idx.end, c(rep(lngth, length(idx2))))
    end <- apply(enddf, 1, function(x){
      ifelse(x[1] > x[2], x <- x[2], x <- x[1]) } )
    post.idx.end <- as.vector(unlist(end))
    startdf <- cbind(post.idx.start, c(rep(1, length(idx1))))
    start <- apply(startdf, 1, function(x){
      ifelse(x[1] > x[2], x <- x[1], x <- x[2]) } )
    post.idx.start <- as.vector(unlist(start))
    conc.df <- cbind(pre.idx.start, pre.idx.end, token.idx.start, token.idx.end,
                     post.idx.start, post.idx.end)
    concr <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
        pre <- substr(txt, x[1], x[2])
        token <- substr(txt, x[3], x[4])
        post <- substr(txt, x[5], x[6])
        tbc <- as.vector(unlist(c(rep(length(pre)), pre, token, post)))
        return (tbc)
      } )
      conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
      return(conc)
    }
    conc <- concr(conc.df, txt)
    conc <- t(conc)
    return(conc)
  })
  df <- ldply(conc, data.frame)
  ifelse(nrow(df) >= 1, df <- df[,c(1, 3:ncol(df))], df <- df)
  colnames(df) <- c("File", "PreContext", "Token", "PostContext")
  df <- df[is.na(df$Token) == F,]
  rownames(df) <- 1:nrow(df)
  return(df)
}
