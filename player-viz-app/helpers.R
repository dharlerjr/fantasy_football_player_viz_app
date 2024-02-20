
# A function factory for getting integer x-axis values.
# Courtesy of Joshua Cook
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Revised integer breaks function
my_integer_breaks <- function(min = 1, max = 18, n = 5, ...) {
  breaks <- floor(pretty(min:max, n, ...))
  breaks <- append(breaks, min)
  return(breaks)
}


# Simplify Full Player Name String
fullName <- function(playerName = "") {
  if(!is.na(playerName)){  
    return(
      (playerName %>% str_to_lower() %>% 
              str_replace_all("[[:punct:]]", "") %>%
              str_split_1(" "))[1:2] %>%
             str_flatten(collapse = " ")
      )
  }
}

# Single Name - For First & Last Name Individually
singleName <- function(playerName = "", index = 1) {
  if(!is.na(playerName)){  
    return(
      (playerName %>% str_to_lower() %>% 
         str_replace_all("[[:punct:]]", "") %>%
         str_split_1(" "))[index]
    )
  }
}


# Vectorize Simplify Player Names
vFullName  <- Vectorize(fullName)

# Vectorize Single Name Func
vSingleName <- Vectorize(singleName)

# Helper Function to Return Player Name, Given Simple Name
returnPlayerSimple <- function(simpleName = "") {
  return((playerFinishes %>% filter(SimpleName == simpleName))[1, 1])
}

# Helper Function to Return Player Name, Given First Name
returnPlayerFirst <- function(firstName = "") {
  return((playerFinishes %>% filter(FirstName == firstName))[1, 1])
}

# Helper Function to Return Player Name, Given First Name
returnPlayerLast <- function(lastName = "") {
  return((playerFinishes %>% filter(LastName == lastName))[1, 1])
}

# Player Search Function Version 1
playerSearchv1 <- function(playerName = "") {
  if(playerName != "") {
    nameLength <- length(playerName %>% str_to_lower() %>%
                           str_replace_all("[[:punct:]]", "") %>%
                           str_split_1(" "))
    if(nameLength >= 2) {
      playerName <- fullName(playerName)
      if(playerName %in% playerFinishes$SimpleName) {
        return(returnPlayerSimple(playerName))
      }
      firstName <- (playerName %>% str_split_1(" "))[1]
      if(firstName %in% playerFinishes$FirstName) {
        return(returnPlayerFirst(firstName))
      }
      lastName <- (playerName %>% str_split_1(" "))[2]
      if(lastName %in% playerFinishes$LastName) {
        return(returnPlayerLast(lastName))
      }
    }
    else {
      playerName <- playerName %>% str_to_lower() %>% 
        str_replace_all("[[:punct:]]", "")
      if(playerName %in% playerFinishes$FirstName) {
        return(returnPlayerFirst(playerName))
      }
      if(playerName %in% playerFinishes$LastName) {
        return(returnPlayerLast(playerName))
      }
    }
    return("Error: No Player Found")
    
  }
}

# Player List Function
currPlayersList <- function(selectedTeam = "", selectedPos = ""){
  if (selectedTeam != "" & selectedPos != ""){
    return((playerFinishes %>% 
              filter(Team == selectedTeam, Pos == selectedPos) %>%
              arrange(desc(Total)))$Player)
  }
  else {return(c())}

}

