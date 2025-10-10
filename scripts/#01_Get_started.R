# 1. DATA Types

flow <- 12L # Integer
Flow <- 15.5 # Double

station <- "niamey" # Character

DATE <- as.Date("2025-10-08") # Date

is_flooding <- FALSE # Logical

# Check the variables type
typeof(flow)
typeof(Flow)
typeof(station)
typeof(DATE)
class(DATE)
typeof(is_flooding)


# 2. DATA STRUCTURES
## 2.1 Vector
flows <- c(250.2, 198.3, 168.4, 202, 406, 425, 507 )
length(flows)
prcp <- c(102,0,98,62,78,106,0)
evap <- c(4.3, 3.5, 5.4, 3.2,5.2,4.01,2.58)
days_of_week <- c("Monday", "Tuesday", "Wenesday", "Thursday",
                  "Friday", "Saturday", "Sunday")


## 2.2 Matrix
myVariable <- matrix(data = c(flows,prcp,evap),
                     nrow = 7,ncol = 3)

View(myVariable) # display

myVariable2 <- matrix(data = c(flows,prcp,evap),
                     nrow = 3,ncol = 7)


View(myVariable2) # display

## Rename columns or Rows

colnames(myVariable) <- c("flows","precipitation","evaporation")
rownames(myVariable) <- days_of_week

## 2.3 Data frames

my_df <- data.frame(days = days_of_week,
                    discharge = flows,
                    precipitation = prcp,
                    evaporation = evap)

View(my_df)

## 2.4 List
myList <- list(station1 =myVariable,
               station2 = myVariable2,
               station3 = my_df)
View(myList)

# Basic Calculation

4 + 2
10 - 6

8 * 9

78 / 2


27 %% 5 # Modulo

18 %/% 5


4^2
10^4

4**2
10**4

16^(1/2)
16**(1/2)
exp(10)
sqrt(16)
?exp

## Indexation
flows[3]

i <- 2
flows[i]

flows[flows<230]


myVariable[3,2]

myVariable[,2]

myVariable[2,]

myVariable[4:6,2]
myVariable[c(4,5,6),2]

myVariable[c(1,3),2]

colnames(myVariable)
rownames(myVariable)
?colnames

length(myList)
myList[2]

myList[[2]]

myList[["station2"]]

names(myList)

myList[["station2"]][1,3]

myList[[2]][1,3]



# Logical Operators
# <  Inferiority
4 < 10
6<6
# > Superiority
10 > 4
# <=  Less or egal
6<= 4
6<=6

# >= Greather or egal
6>=4
# == Egality
4==5
## Logical connector
### OR |
6>=4 | 4==5

### AND &
6>=4 & 4==5

## Control structure
thr <- 230
if(thr <= 230){
  print("The code is executed")
  flows[flows<thr]
}

q_obs <- 328
if(q_obs>= 215 & q_obs<=300){
  print("Severity 1")
}


q_obs <- 328
if(q_obs>= 215 & q_obs<=300){
  print("Severity 1")
}else{
  print("Normal Situtaion")
}

# Loops
## For
j <- 52
o <- 42
x <- j * o

print(paste(j, o, x,sep = "/"))

for( i in 1:3){
  print(paste0("i value is : ", i))
  print(flows[i])

}


for( i in 1:length(flows)){
  print(paste0("i value is : ", i))
  print(flows[i])

}


for (j in flows) {
  if(j >= 215){
    print(paste0("The value is : ", j," Then is flooding"))
  }else{
    cat(paste0("The value is : ", j," Then is not flooding \n"))
  }
}

cat(paste0("The value is : ", j," Then is flooding"))
print(paste0("The value is : ", j," Then is flooding"))
