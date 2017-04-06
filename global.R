################################################################################
# global.R - functions and global data for shiny App

##LIBRARIES
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)  
{  
  for(package_name in package_names)  
  {  
    if(!is_installed(package_name))  
    {  
      install.packages(package_name,repos="http://cran.csiro.au/")  
    }  
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
  }  
}  

load_or_install(c("shiny", "ggplot2", "broom", "dplyr", "lubridate"))
                 

## GENERAL FUNCTIONS
# CSV import helper
csvImport <- function(x){
  df <- read.csv(x, header = TRUE, stringsAsFactors = FALSE)
  df$DATE <- as.Date(parse_date_time(df$DATE, orders = c("dmY", "Ymd")))
  df
}
# Function to create named list for selectInput choices
choices <- function(x){
  hnames <- names(x)[-1]
  choices <- as.list(hnames)
  names(choices) <- hnames
}

# Function to create index for closest dates and return a df
df2model <- function(x, y, z){
  
  #specific data to wetland
  id5 <- match(x, bnames)#find b5 data
  b5 <- b5[,c(1,id5)]
  b5 <- b5[complete.cases(b5),]
  idh <- match(x, dnames)#find hdepth data
  hist <- hDepth[,c(1,idh)]
  hist <- hist[complete.cases(hist),]
  
  #trim hist data set
  mb5 <- min(b5$DATE)
  hist <- hist[hist$DATE >= mb5, ]
  names(hist) <- c("DATE_d", "depth")
  
  #b5 date only
  datelist <- b5$DATE
  
  #depth date only
  tomatch <- hist$DATE_d
  
  #index to closest match between hDepth and b5
  ind <- sapply(tomatch, function(x) which.min(abs(datelist-x)))
  
  #subset band 5 values according to closest match up
  b5M <- b5[ind,]
  names(b5M) <- c("DATE_b5", "b5")
  
  #combine matched band 5 values to depth values
  all <- cbind(b5M, hist)
  
  #reorder columns and calculate difference in dates
  all <- all%>%
    select(DATE_b5, DATE_d, b5, depth)%>%
    mutate(diff = abs(DATE_b5 - DATE_d))
  
  # data set with <= selected day differential
  df <- all%>%
    filter(diff <= y)%>%
    filter(depth >= z)
  # NA's removed
  df <- df[complete.cases(df), ]
  
  #add small amount (using gamma model)
  df$depth.i <- df$depth + 0.0001
  return(df)
}

# Function to create wetland specific depth df (used in pred plot)
dfpredhist <- function(x){
  idh <- match(x, dnames)#find hdepth data
  hist <- hDepth[,c(1,idh)]
  hist <- hist[complete.cases(hist),]
  names(hist) <- c("DATE", "depth")
  return(hist)
  
}

# Function to create wetland specific band 5 df (used in pred plot)
dfpredb5 <- function(x){
  id5 <- match(x, bnames)#find b5 data
  b5 <- b5[,c(1,id5)]
  b5 <- b5[complete.cases(b5),]
  names(b5) <- c("DATE", "b5")
  return(b5)
}

# Function to fit model
mod <- function(df, modtype){
  if(modtype == 1){
    fitexp <- lm(log(depth.i) ~ b5, data = df)
    return(fitexp)
  } else {
    fitlin <- lm(depth.i ~ b5, data = df)
    return(fitlin)
  }
  
}

# Function to return p.value from model (not used now)
lmp <- function (model) {
  if (class(model) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Function to create df of model (used in mod plot)
mData <- function(df, model, modtype){
  if(modtype == 1){
    fitexp <- model
    MyData      <- data.frame(X1 = seq(range(df$b5)[1], range(df$b5)[2], 
                                       length = 40))# restrict to modelled data range
    X           <- model.matrix(~ X1, data = MyData) # obtain matrix
    MyData$eta  <- X %*% coef(fitexp)# matrix multiplication!!
    MyData$pred <- exp(MyData$eta)# account for log link
    MyData$SE   <- sqrt(diag(X %*% vcov(fitexp) %*% t(X)))
    MyData$ub   <- exp(MyData$eta + 1.96 * MyData$SE)
    MyData$lb   <- exp(MyData$eta - 1.96 * MyData$SE)
    return(MyData)
  } else {
    fitlin <- model
    MyData      <- data.frame(X1 = seq(range(df$b5)[1], range(df$b5)[2], 
                                       length = 40))# restrict to modelled data range
    X           <- model.matrix(~ X1, data = MyData) # obtain matrix
    MyData$eta  <- X %*% coef(fitlin)# matrix multiplication!!
    MyData$pred <- MyData$eta
    MyData$SE   <- sqrt(diag(X %*% vcov(fitlin) %*% t(X)))
    MyData$ub   <- MyData$eta + 1.96 * MyData$SE
    MyData$lb   <- MyData$eta - 1.96 * MyData$SE
    return(MyData)
  }
}

# Function to create df of prediction values (used in pred plot)
pData <- function(df, model, modtype){
  if(modtype == 1){
    model <- model
    predexp <- exp(predict(model, data.frame(b5 = df$b5)))
    b5modelled <- data.frame(DATE = df$DATE, b5 = df$b5, prediction = predexp)
    return(b5modelled)
  } else {
    model <- model
    pred <- predict(model, data.frame(b5 = df$b5))
    b5modelled <- data.frame(DATE = df$DATE, b5 = df$b5, prediction = pred)
    b5modelled$prediction[b5modelled$prediction < 0] <- 0
    return(b5modelled)
  }
  
}

# Function to create csv export header
csvHead <- function(mod, dd, et){
  title <- "This data has been generated by the wetlandAPP"
  p0 <- "Parameters"
  p1 <- paste0("Days difference: ", dd)
  p2 <- paste0("Error threshold: ", et)
  # eqn <-
  #   paste0("y ~ ", round(coefficients(mod)[1],2), "",
  #          paste(sprintf(" %+.2f*%s ", coefficients(mod)[-1],
  #                        names(coefficients(mod)[-1])), collapse=""))
  header <- c(title, p0, p1, p2)
  return(header)
}


## GLOBAL DATA
# Create global data sets
b5 <- csvImport(list.files(path = "./data", pattern = "^dfb5*", 
                             full.names = TRUE))
hDepth <- csvImport(list.files(path = "./data", pattern = "^dfhist*", 
                                full.names = TRUE))
mychoices <- choices(b5)
bnames <- names(b5)
dnames <- names(hDepth)



