#!/usr/bin/Rscript --vanilla

################################################################################
#                                                                              #
# Script for tokenizing, lemmatizing, POS-tagging and dependency parsing of    #
# Frisian text.                                                                #
#                                                                              #
# Before running the script install the libraries libcurl, libxml-2.0 and      #
# poppler-cpp. When using Ubuntu 20.04 or later, enter the following commands  #
# in a terminal:                                                               #
#                                                                              #
# $ sudo apt install libssl-dev                                                #
# $ sudo apt install libcurl4-openssl-dev                                      #
# $ sudo apt install libxml2-dev                                               #
# $ sudo apt install libpoppler-cpp-dev                                        #
#                                                                              #
# When running the script for the first time, missing R packages are installed #
# which can take a while!                                                      #
#                                                                              #
# The file 'fy_frysk-ud-1.0-20250407.udpipe' should reside in the same         #
# directory as where this script is stored.                                    #
#                                                                              #
# The file 'Frysk.txt' is added as an example text. It can be processed by     #
# entering the following command in a terminal:                                #
#                                                                              #
# $ ./udpipefrysk.R -i Frysk.txt -f -o Frysk.xlsx -x                           #
#                                                                              #
# where the result is stored as an Excel spreadsheet in Frysk.xlsx             #
#                                                                              #
# or read from pipe:                                                           #
#                                                                              #
# $ cat Frysk.txt|./udpipefrysk.R -i - -e -t > Frysk.tsv                       #
#                                                                              #
# or read from user input (close with Ctrl-d):                                 #
#                                                                              #
# $ ./udpipefrysk.R -i - -e -t > Frysk.tsv                                     #
#                                                                              #
# where the result is stored as a tab-separated file in Frysk.tsv.             #
#                                                                              #
# For information about usage and options enter:                               #
#                                                                              #
# $ ./udpipefrysk.R -h                                                         #
#                                                                              #
# Copyright: Fryske Akademy, Leeuwarden, The Netherlands, 24 March 2022.       #
# Contact  : wheeringa@fryske-akademy.nl                                       #
#                                                                              #
################################################################################

# install and load packages

packages = c("optparse", "readr", "readtext", "xml2", "rvest", "openxlsx", "stringr", "udpipe")

for (p in packages)
{
  if (suppressWarnings((!library(p, character.only=T, logical.return = T, quietly = T))))
  {
    cat("\nInstalling package", p, "...\n", file = stderr())
    suppressWarnings(install.packages(p, quiet = T, repos = "https://cloud.r-project.org/"))
  }
  
  if (suppressWarnings((!library(p, character.only=T, logical.return = T, quietly = T))))
  {
    cat("\nPackage", p, "not installed!\n\n", file = stderr())
    quit(status=1)
  }
}

usage = "usage: %prog -i INPUT -e|-f|-w -o OUTPUT -t|-x|-c"

option_list = list(
  make_option(c("-i", "--input" ), action="store"     , help="text or file name or url of website"),

  make_option(c("-e", "--text"  ), action="store_true", help="input  is some text between ' and '"),
  make_option(c("-f", "--file"  ), action="store_true", help="input  is file (.txt, .docx, .html)"),
  make_option(c("-w", "--web"   ), action="store_true", help="input  is URL of website"),

  make_option(c("-o", "--output"), action="store"     , help="name of output file"),
  
  make_option(c("-t", "--tsv"   ), action="store_true", help="output is tab-separated file" ),
  make_option(c("-x", "--xlsx"  ), action="store_true", help="output is Microsoft Excel file"),
  make_option(c("-c", "--connlu"), action="store_true", help="output is CoNLL-U file")
); 

option_parser <- OptionParser(usage=usage, option_list=option_list)
opt <- parse_args(option_parser)

if (length(opt) == 1)
{  
  print_help(option_parser)
  quit(status=1)
}

if (is.null(opt$input))
{
  con <- file("stdin")
  string <- scan(con, what=character(), quote="")
  close(con)
}                    else
if (unlist(opt$input) == "-")
{
  con <- file("stdin")
  string <- scan(con, what=character(), quote="")
  close(con)
}                    else
{
  string <- unlist(opt$input)
}

if ((!is.null(opt$text) && (opt$text==T)) & ( is.null(opt$file)) & ( is.null(opt$web)))
  input <- "text"    else

if (( is.null(opt$text)) & (!is.null(opt$file) && (opt$file==T)) & ( is.null(opt$web)))
  input <- "file"    else

if (( is.null(opt$text)) & ( is.null(opt$file)) & (!is.null(opt$web) && (opt$web==T)))
  input <- "web"     else
{  
  print_help(option_parser)
  quit(status=1)
}

if (is.null(opt$output))
{
  result <- stdout()
}                    else
if (unlist(opt$output) == "-")
{
  result <- stdout()
}                    else
{
  result <- unlist(opt$output)
}

if ((!is.null(opt$tsv) && (opt$tsv==T)) & (is.null(opt$xlsx)) & (is.null(opt$connlu)))
  output <- "tsv"    else

if ((is.null(opt$tsv)) & (!is.null(opt$xlsx) && (opt$xlsx==T)) & (is.null(opt$connlu)))
  output <- "xlsx"   else

if ((is.null(opt$tsv)) & (is.null(opt$xlsx)) & (!is.null(opt$connlu) && (opt$connlu==T)))
  output <- "connlu" else
{  
  print_help(option_parser)
  quit(status=1)
}  

# read data

if (input=="text")
{
  s <- string
}
  
if (input=="file")
{
  if (file.exists(string))
  {
    s <- readtext(file = string, encoding = "UTF-8")$text
  }
  else
  {
    cat("File ", string, "not found.\n", file = stderr())
    quit(status=1)
  }
}

if (input=="web")
{
  site <- NULL
  
  tryCatch(
    site <- read_html(string),
    error   = function(something) {},
    warning = function(something) {}
  )
  
  if (length(site)>0)
  {
    text <- html_text(html_nodes(site, 'p'))
    text <- gsub("\n", "", text)
    text <- gsub("([)[0-9]+(]))", "", text)
    
    text <- data.frame(text)
    text <- subset(text, str_count(text, "\\w+") > 1)
    text <- subset(text, grepl("[A-Z|a-z]", text))
    
    if (nrow(text) > 0)
      s <- paste(text$text, sep = "", collapse = "\n\n")
    else
      s <- ""
  }
  else
  {
    cat("\nNo website found at given URL!\n\n", file = stderr())
    quit(status=1)
  }
}

s <- paste(s, collapse = " ")

# load model, tokenize, lemmatize, tag, annotate

resultUD <- as.data.frame(udpipe(x = s, object = "fy_frysk-ud-1.0-20250407.udpipe"))
resultUD$term_id <- NULL

# write result

if  (output=="tsv")
  write.table(resultUD, result, sep = "\t", na = "", dec = ".", quote = TRUE, qmethod = "double", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

if ((output=="xlsx") & (result!=stdout()))
  write.xlsx (resultUD, result, sheetName = "table", headerStyle = createStyle(textDecoration = "BOLD"), rowNames=FALSE, colNames=TRUE, na.string = "", firstRow = TRUE)

if ((output=="xlsx") & (result==stdout()))
  cat("\nMicrosoft Excel file cannot be printed to stdout!\n", file = stderr())

if  (output=="connlu")
  writeLines(as_conllu(resultUD), result)

cat("\nDONE\n\n", file = stderr())
