library(dplyr)
# Creates a summary table
sum_func <- function(dataset){
  # Finds the most numerous class standing
  standing <- dataset %>% select(What.is.your.current.class.standing.) %>% group_by(What.is.your.current.class.standing.) %>% 
    summarise(majority_standing=sum(What.is.your.current.class.standing.=="Freshman", What.is.your.current.class.standing.=="Sophomore", 
                                    What.is.your.current.class.standing.=="Junior", What.is.your.current.class.standing.=="Senior"))
  # Finds the class standing that is most interested in applying to the major
  applying <- dataset %>% filter(Are.you.interested.in.applying.to.the.Informatics.major.== "Yes") %>% group_by(What.is.your.current.class.standing.) %>% 
    summarise(number_applying=sum(What.is.your.current.class.standing.=="Freshman", What.is.your.current.class.standing.=="Sophomore", 
                                  What.is.your.current.class.standing.=="Junior", What.is.your.current.class.standing.=="Senior"))
  # Finds the most popular os in the class
  os <- dataset %>% select(What.is.your.current.class.standing., What.operating.system.do.you.typically.use.) %>% 
    group_by(What.operating.system.do.you.typically.use.) %>% 
    summarise(popular_os=sum(What.operating.system.do.you.typically.use.=="Linux", What.operating.system.do.you.typically.use.=="Mac",
                             What.operating.system.do.you.typically.use.=="Windows", 
                             What.operating.system.do.you.typically.use.=="Other: Windows at home, Linux (Ubuntu) at work"))
  # Finds the most popular type of pet
  pet <- dataset %>% select(Do.you.consider.yourself.) %>% group_by(Do.you.consider.yourself.) %>% 
    summarise(cat_dog=sum(Do.you.consider.yourself.=="Neither", Do.you.consider.yourself.=="Both!", 
                          Do.you.consider.yourself.=="A cat person....", Do.you.consider.yourself.=="A dog person..."))
  table <- left_join(standing, applying)
  final <- data.frame(table, os, pet)
  names(final)[1] <- "Class Standing"
  names(final)[2] <- "Students in Class"
  names(final)[3] <- "Number Applying to Info"
  names(final)[4] <- "Type of OS"
  names(final)[5] <- "Number of Users"
  names(final)[6] <- "Are You A pet person?"
  names(final)[7] <- "Number of Pet People"
  return(final)
}
