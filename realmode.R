# #################################
#    ---      Real Mode      ---
#
# Author     : Kyle West
# Last Build : 15 Dec 2015
# ---------------------------------
# LIST OF ELEMENTS:
#    * realMode(list)
#    * nodup(value, list)
# #################################


#-----------------------------
# NODUP - check to find a 
#   duplicate value in a list
#  ex: nodup(value, list)
#-----------------------------
nodup <- function(x,l)
{
  state <- TRUE
  for ( i in 1:length(l) )
  {
    if ( x == l[i])
      state <- FALSE
  }
  
  return(state)
}


#-----------------------------
# REALMODE - finds the mode of
#    a list of values.
#-----------------------------
realMode <- function(x)
{
  
  # counters and lists
  count <- 0         # a counter of where we are at in our list
  count_list <- 0    # a correlated list of instances per value
  mode_list <- 0     # a list of mode values
  mode_size <- 1     # a counter for the mode_list
  print_list <- 0    # a list of mode(s) to print to user
  print_size <- 1    # a counter for the print_list
  
  # determine how many instances of each value in the list
  # and create a correlated instance list. E.g. If list[1]
  # has a value of "3", and "3" appears 6 times in the list
  # then count_list[1] has a value of "6".
  for ( i in 1:length(x) )
  {
    for ( j in 1:length(x) )
    {
      if ( x[i] == x[j] )
        count <- 1 + count
    }
    count_list[i] <- count
    count <- 0
  }
  
  # if the count_list value is a max then it is a mode
  # put it on the mode_list
  for ( i in 1:length(count_list) )
  {
    if (count_list[i] == max(count_list) )
    {
      mode_list[mode_size] <- x[i]
      mode_size <- 1 + mode_size
    }
  }
  
  # fliter out any duplicate mode values on the list
  # and put the remainder on another list to print to 
  # the user (by return)
  for ( i in 1:length(mode_list))
  {
    for ( j in 1:length(print_list))
    {
      if (nodup(mode_list[i], print_list))
      {
        print_list[print_size] <- mode_list[i]
        print_size <- print_size + 1
      }
    }
  }

  # print to user our mode(s)
  return(print_list)
}
