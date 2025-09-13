logging <- function(text,file="logging.txt",reset=FALSE) {
  if (reset) file.remove(file)
  if (!missing(text)) {
    write(text, file=file,append=TRUE)
  }
}
