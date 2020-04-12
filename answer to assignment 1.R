
# week 1 assignment requirement -------------------------------------------

# Assignment 1.1
# 1) Create a list with length 10: for the first component list[[1]], the dimension is 1, the second is 1*2, the third is 3*3, the fourth is 4*4, and so on. 
# The values should be selected randomly from 1:100.
# 2) For each component in the list, select the values > 50
# 3) and write a function to calculate a value = sd (values)/mean(values) when length(values)>1, otherwise return 0.
# 4) Loop for each component, you will get 10 values, and then calculate the sum of the 10 values
# 5) Repeat the process for many times (could you find any patterns? Plot a histogram)

# Assignment 1.2
# 1) Read the csv file “authorlist.csv”
# 2) Select columns “Author Name” and “Discipline”. The variable discipline contains one or a set of words.
# 3) Output: a co-occurrence matrix M, e.g., M[1,1] = communication, health, 5 authors



# Assignment 1.1 ----------------------------------------------------------

# 1) Create a list with length 10: for the first component list[[1]], the dimension is 1 ------------------- 
# the second is 1*2, the third is 3*3, the fourth is 4*4, and so on. 
# The values should be selected randomly from 1:100.

# A list is a collection of all data structure.
# The assignment asks us to create a list with certain requirements to meet, the basic idea is to 
# [1] create an empty list and then 
# [2] fill the empty list with data 

# [1] create an empty list
list_1 = list()

# [2] fill in data 
# about the list: containing 10 components; list[[1]] 1*1; list[[2]] 1*2; list[[3]] 3*3; list[[4]] 4*4..... list[[n]] = n*n
# we can see: the fist two components have different rules in terms of the dimension of the data set, and the rest components remain the same rule. 
# each of the components can be viewed as a matrix

# to create a matrix:
matrix(1:10,nrow = 2, ncol = 5)

# to generate randomly selected values from 1:100
sample(1:100,size = 2, replace = T)

# for the first two components:
random_1 = sample(100, size = 1*1, replace = T)
component_1 = matrix(random_1,nrow = 1, ncol = 1)

random_2 = sample(100, size = 1*2, replace = T)
component_2 = matrix(random_2,nrow = 1, ncol = 2)

# full the list_1 with component_1 & component_2
list_1[[1]] = component_1
list_1[[2]] = component_2


# for the rest of components:

random_3 = sample(100, size = 3*3, replace = T)
component_3 = matrix(random_3,nrow = 3, ncol = 3)

random_4 = sample(100, size = 4*4, replace = T)
component_4 = matrix(random_4,nrow = 4, ncol = 4)

# find the way how other components are constructed
# random_i = sample(100, size = i*i, replace = T)
# component_i = matrix(random_i,nrow = i, ncol = i)
# and then fill them in list_1 

# write a loop 
for (i in 3:10) {
  random_i = sample(100, size = i*i, replace = T)
  component_i = matrix(random_i,nrow = i, ncol = i)
  list_1[[i]] = component_i
}


# see how list_1 looks like 
list_1

# 2) For each component in the list, select the values > 50 -----------------------

component_1 = list_1[[5]]
aim_value_1 = component_1[component_1>50]

component_2 = list_1[[7]]
aim_value_2 = component_2[component_2>50]

# component_i = list_1[[i]] ; aim_value_i = component_i[component_i>50]

# 3) and write a function to calculate a value = sd (values)/mean(values) when length(values)>1, otherwise return 0.---------
length(aim_value_1)
# sd (values)/mean(values) : Coefficient of variation (CV).
cv_2 = sd(aim_value_2)/mean(aim_value_2)


# 4) Loop for each component, you will get 10 values, and then calculate the sum of the 10 values--------
# first, create an empty vector to store the cv 

cv_all = c()
for (i in 1:10) {
  component_i = list_1[[i]]
  aim_value_i = component_i[component_i>50]
  if (length(aim_value_i >1)) {
    cv_i = sd(aim_value_i)/mean(aim_value_i)
  }
  else {
    cv_i = 0
  }
  cv_all = append(cv_all,cv_i)
}

cv_all
sum(cv_all)


# create a function so that every time we input a list (which meet the requirement of (1)), we will get the sum of cvs 
# so for the function, the input value should a list 
function_cv_sum = function(j) {
  cv_all = c()
  for (i in 1:10) {
    component_i = j[[i]]
    aim_value_i = component_i[component_i>50]
    if (length(aim_value_i)>1) {
      cv_i = sd(aim_value_i)/mean(aim_value_i)
    }
    else {
      cv_i = 0
    }
    cv_all = append(cv_all,cv_i)
  }
  sum_cv = sum(cv_all)
  return(sum_cv)
} 



# 5) Repeat the process for many times (could you find any patterns? Plot a histogram)
# first, generate 1000 lists which meet the requirements
# below is the way to generate 1 list 
gen_list = function(list_length) {
  aim_list = list()
  for (i in 1:list_length) {
    if (i>2) {
      random_i = sample(100, size = i*i, replace = T)
      component_i = matrix(random_i,nrow = i, ncol = i)
      aim_list[[i]] = component_i
    }
    else {
      random_i = sample(100, size = 1*i, replace = T)
      component_i = matrix(random_i,nrow = 1, ncol = i)
      aim_list[[i]] = component_i
    }
  }
  return(aim_list)
}

# repeat the list generation process for 1000 times and then calculate the sum of cv each time and store the result 

multiple_list = list()
multiple_list_cv_sum = c()
for (n in 1:1000) {
  multiple_list[[n]] = gen_list(10)
  cv_sum = function_cv_sum(multiple_list[[n]])
  multiple_list_cv_sum = append(multiple_list_cv_sum,cv_sum)
}

multiple_list[[1]]
multiple_list_cv_sum 
hist(multiple_list_cv_sum)

# Normal distribution with μ = around 1.5 (a little positively skewed)




# assignment 1.2 ----------------------------------------------------------

# read in data
author_data = read.csv("authorlist.csv",stringsAsFactors = FALSE)
head(author_data)
# select columns “Author Name” and “Discipline”. The variable discipline contains one or a set of words.
author_data_new = author_data[,c("Author.Name","Discipline")]
View(author_data_new)

install.packages("dplyr")
library(dplyr)

is.na(author_data_new$Discipline) %>% table()

# we notice that the variable discipline contains one or a set of words: 
  author_data_new$Discipline[12] # "Medicine,Health"
  author_data_new$Discipline[73] # "Disease;Agriculture"
  
# first split the string so that author_data_new$Discipline[12] ("Medicine,Health") can become "Medicine" "Health";
  # and author_data_new$Discipline[73] can become "Disease", "Agriculture"
  
# first change ";" into ","
gsub(";",",",author_data_new$Discipline[73])
# then split the string 
strsplit(author_data_new$Discipline[12],split=',')

author_data_new$Discipline = gsub(";",",",author_data_new$Discipline)
author_data_new$Discipline_sep = strsplit(author_data_new$Discipline,split=',')

# display how many elements in each Discipline_sep
author_data_new$Discipline_sep[[73]] %>% length()

data.frame(author_data_new$Author.Name[[73]],author_data_new$Discipline_sep[[73]])

# reform the dataset into a dataframe with each roll containing one author and one discipline  
author_discipline = data.frame() 
for (i in 1:nrow(author_data_new)) {
  if (length(author_data_new$Discipline_sep[[i]]) >= 2) {
    df = data.frame(author=author_data_new$Author.Name[[i]],
                    disciplines=author_data_new$Discipline_sep[[i]],stringsAsFactors = F)
    author_discipline = rbind(author_discipline,df)
  }
}

# we can ignore the authors who only have 1 discipline since our focus is the cooccurance of disciplines 

# see how many unique disciplines we have 

author_discipline$disciplines %>% unique() %>% sort()
# sort(): alphabetical order 

# notice: " Epidemiology": white space ; 
# "Computer" "Computer science" "Computer Science": "computer science" to "computer" & lowercase 
# "informatics" "Informatics": lowercase 
# "Sciences" into "Science" : 
# "Social Sciences" into "Social Sciences" 
# merge "Public Health" with "Health"

# remove white space
trimws(" Epidemiology")
author_discipline$disciplines = trimws(author_discipline$disciplines)
# to lower case 
tolower("Computer science")
author_discipline$disciplines = tolower(author_discipline$disciplines)
# reform others
author_discipline$disciplines[author_discipline$disciplines == "computer science"] = "computer"
author_discipline$disciplines[author_discipline$disciplines == "sciences"] = "science"
author_discipline$disciplines[author_discipline$disciplines == "social sciences"] = "social science"
author_discipline$disciplines[author_discipline$disciplines == "public health"] = "health"

# see unique disciplines again 
author_discipline$disciplines %>% unique() %>% sort()
head(author_discipline)

# now we want to change into a author*discipline matrix 
# author    discipline_1    discipline_2     discipline_3   .....    discipline_n 
#   1            1              0              1            .....      0
#   2            0              0              1            .....      1
#   .            .              .              .            .....      .
#   n            0              1              1            .....      0 

# packge "reshape" provide a convenient way to cast from long format (the one we have now) into wide format (the matrix we want)
install.packages("reshape")
library("reshape")
?cast
names(author_discipline)
author_discipline$n = 1 
# we need to add a variable "n" to the dataframe because when we cast from long dataframe
# to wide matrix, we need to specify the exact value in the matrix. 
# if an author has discipline x, we should assign the value "1" to him/her.  
author_disc_matrix = cast(author_discipline,author~disciplines,value="n")
names_author = author_disc_matrix[,1]


author_disc_matrix = author_disc_matrix[,2:ncol(author_disc_matrix)]
rownames(author_disc_matrix) = names_author

max(author_disc_matrix) #4  this number should be 1 because for each discipline, each author should only appear onece. 
# the reason why there are values > 1 is because some authors' data has repeated, for example:
author_discipline[author_discipline$author=="Paul, Michael J",]
author_data_new[author_data_new$Author.Name=="Paul, Michael J",]
# so change value > 1 into 1 
author_disc_matrix[author_disc_matrix>1] = 1


# if you don't know the cast function in reshape package, we can also do it in an old school way just using basic functions in R

author_list =  author_discipline$author %>% unique() %>% sort()
discipline_list = author_discipline$disciplines %>% unique() %>% sort()

author_disc_matrix_basic_way = matrix(0,nrow = length(author_list),ncol = length(discipline_list),
                                      dimnames = list(author_list,discipline_list))
for (auth in author_list) {
  for (disci in discipline_list) {
    if (disci %in% author_discipline[author_discipline$author == auth,]$disciplines) {
      author_disc_matrix_basic_way[auth,disci] = 1
    }
  }
}

# see if the two matrixs are the same 
author_disc_matrix==author_disc_matrix_basic_way 
data_matrix = as.matrix(author_disc_matrix)
colnames(data_matrix) = colnames(author_disc_matrix)

# calculate the co-occurrence matrix of disciplines 
coocur_matrix =  t(data_matrix) %*% data_matrix

# to understand the calculation of co-occurrence matrix by t(matrix) %*% (matrix)

# we have original author-discipline matrix: m1
# author    discipline_1    discipline_2     discipline_3   .....    discipline_n 
# auth_1            1              1              1            .....      0
# auth_2            0              0              1            .....      1
#   .            .              .              .            .....      .
# auth_m            0              1              1            .....      0 

# after transformation: discipline-author matrix: m2
# disc        author_1        author_2  .....     author_m
# disc_1          1             0       .....         0
# disc_2          1             0       .....         1
# disc_3          1             1       .....         1
#   .             .             .       .....         .
# disc_n          0             1       .....         0

# matrix multiplication
# M1        M2
# 1,2,3      7,  8
# 4,5,6      9, 10
#           11, 12

# M1*M2[1,1]: 1*7+2*9+3*11 
# M1*M2[1,2]: 1*8+2*10+3*12 

# in our case: pick disc_1 & disc_2 for analysis: the value of t1*t2 is the value of 
# t1
  # author    discipline_1      discipline_2
  # auth_1            1              1
  # auth_2            0              0
  #   .               .
  # auth_m            0              1       

# t2 
# disc        author_1        author_2  .....     author_m
# disc_1          1             0       .....         0
# disc_2          1             0       .....         1

# let calculate co-occurrence bwteen disc_1 & disc_2 [it should be 1 because only auth_1 both appreaed in disc_1 & disc_2]

# t2*t1[1,2] =  1*1 + 0*0 + 0*1 =  1 YES! what does it mean by matrix multiplication?
# for author_1, disc_1 =1 & disc_2 = 1 for author_2: disc_1 = 0 & disc_2 = 0 ...
# therefore coocur_matrix =  t(data_matrix) %*% data_matrix



# if not use matrix matiplication -----------------------------------------
# author_disc_matrix
coocur_matrix_old_school<-matrix(0,nrow = ncol(author_disc_matrix),
                                 ncol = ncol(author_disc_matrix),
                                 dimnames = list(colnames(author_disc_matrix),
                                                 colnames(author_disc_matrix)))

for (auth in rownames(author_disc_matrix)) {
  for (disc_i in colnames(author_disc_matrix)) {
    for (disc_j in colnames(author_disc_matrix)) {
      if (author_disc_matrix[auth,disc_i] == 1 
          & author_disc_matrix[auth,disc_j]) {
        coocur_matrix_old_school[disc_i,disc_j] = coocur_matrix_old_school[disc_i,disc_j]+1
      }
    }
  }
}

# see if two ways work out the same 
coocur_matrix == coocur_matrix_old_school
