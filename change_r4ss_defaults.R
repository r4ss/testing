dir <- 'c:/GitHub/r4ss/R/'
files <- dir(dir)
for(ifile in 1:length(files)){
  cat("changing file",ifile,"of",length(files),"\n")
  lines <- readLines(file.path(dir,files[ifile]))
  lines <- gsub(pattern="pwidth=7",  replacement="pwidth=6.5", x=lines)
  lines <- gsub(pattern="pheight=7", replacement="pheight=5.0",x=lines)
  lines <- gsub(pattern="ptsize=12", replacement="ptsize=10",  x=lines)
  lines <- gsub(pattern="width of plot written to PNG file",
                replacement="width of plot", x=lines)
  lines <- gsub(pattern="Width of plot written to PNG file",
                replacement="Width of plot", x=lines)
  lines <- gsub(pattern="height of plot written to PNG file",
                replacement="height of plot", x=lines)
  lines <- gsub(pattern="Height of plot written to PNG file",
                replacement="Height of plot", x=lines)
  lines <- gsub(pattern="ptsize ptsize",
                replacement="ptsize point size", x=lines)

  writeLines(text=lines, file.path(dir,files[ifile]))
}


