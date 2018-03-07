## import data from the excel, covert month from number to Date

#read the first sheet in the excel
#read_excel ("2017 Updated Weekly Metrics Compilation.xlsx")

#list the sheet names in the excel 
excel_sheets("2017 Updated Weekly Metrics Compilation.xlsx")

#read one sheet data
rdata <- function (x){
  #x is the sheet number or sheet name
  dat <- read_excel ("2017 Updated Weekly Metrics Compilation.xlsx", sheet = x)
  return (dat )
}

#read the entire csv file
redata <- function (x) {
  sdat <- read.csv(x)
  return (sdat)
}

#after read the csv file into r, want to subset the special range
sub_range <- function (d, star_r,end_r,star_c,end_c) {
  return (d[star_r:end_r,star_c:end_c])
}

#delete the row doesn't useful in the subset
delete_row <- function (dat, r){
  return (dat[-c(r),])
}

# read specific range in the excel 
read_range <- function(x,s,y) {  #x: dataset s:sheetname y:range you want to read
  r <- paste0(s,"!", y)
  spedat <- read_excel (x,range= r)
  return (spedat)
}

#make date February 18 to 2018-02-01
clean_date <- function (datafromcsv){
  hh <- as.yearmon(paste(year_format(datafromcsv), month_format(datafromcsv)), "%Y%m", sep="-")
  dates1 <- as.Date(hh)
}


## covert month from February to 2
slm <- vector()
slom <- vector()
sslom <- vector()
month_format  <- function (datacolumn) {
  for (i in 1:length(datacolumn)) 
  {
    slm[i] <- as.character(datacolumn[i])
    slom[i] <- strsplit(slm[i], " ")
    sslom[i] <- slom[i][[1]][1]    #month
    i <- i+ 1
  }
  
  fslm <- factor(sslom) 
  fslm
  convert <- sapply(fslm, function (x) { grep(x, month.name); })   #int 
  return (convert)
}

## covert year 15 to 2015
m <- vector()
year_format <- function (somedata) {
  #r <- datacolumn
  s<- vector()
  p <- vector()
  q<- vector()
  for (i in 1:length(somedata))
  {
    m[i] <- as.character(somedata[i])
    p[i]<- strsplit(m[i], " ")
    q[i] <- p[i][[1]][2] 
    if (q[i] == "18")
    {
      s [i] <- '2018'
    }
    if (q[i] == "17")
    {
      s [i] <- '2017'
    }
    if (q[i] == "16")
    {
      s [i] <- '2016'
    }
    if (q[i] == "15")
    {
      s [i] <- '2015'
    }
    i <- i + 1
  }
  return (s)
}

# make $money to numeric number
make_int <- function (z) {
  return (parse_number(z))
}


