CommonFields <- function (vars){
vars$newvars <- vars$oldvars
## Team variable names
  
  vars$newvars[22] <- 'Team.Name'
  vars$newvars[23] <- 'Team.Section'
  vars$newvars[24] <- 'Team.Full'
  vars$newvars[25] <- 'Email1'
  vars$newvars[26] <- 'Email2'
  vars$newvars[27] <- 'Email3'
  vars$newvars[28] <- 'Email4'
  
## Members variable names
  if (nrow(vars) > 60){
    n <- 4
  } else {
    n <- 1
  }
  
  inc <- 23
  
  for (i in 1:n){

    if (nrow(vars) > 60) {
      m <- paste("Member", i, '.', sep = '')
    } else {
      m <- ''
    }    
    vars$newvars[29+inc*(i-1)]<- paste(m, 'Surname', sep = '')
    vars$newvars[30+inc*(i-1)]<- paste(m, 'Name', sep = '')
    vars$newvars[31+inc*(i-1)]<- paste(m, 'Email', sep = '')
    vars$newvars[32+inc*(i-1)]<- paste(m, 'Phone', sep = '')
    vars$newvars[33+inc*(i-1)]<- paste(m, 'Homecity', sep = '')
    vars$newvars[34+inc*(i-1)]<- paste(m, 'Uni', sep = '')
    vars$newvars[35+inc*(i-1)]<- paste(m, 'Uni.Custom', sep = '')
    vars$newvars[36+inc*(i-1)]<- paste(m, 'Edu', sep = '')
    vars$newvars[37+inc*(i-1)]<- paste(m, 'School', sep = '')
    vars$newvars[38+inc*(i-1)]<- paste(m, 'Class', sep = '')
    vars$newvars[39+inc*(i-1)]<- paste(m, 'Course', sep = '')
    vars$newvars[40+inc*(i-1)]<- paste(m, 'Speciality', sep = '')
    vars$newvars[41+inc*(i-1)]<- paste(m, 'Faculty', sep = '')
    vars$newvars[42+inc*(i-1)]<- paste(m, 'Cathedra', sep = '')
    vars$newvars[43+inc*(i-1)]<- paste(m, 'Group', sep = '')
    vars$newvars[44+inc*(i-1)]<- paste(m, 'Form', sep = '')
    vars$newvars[45+inc*(i-1)]<- paste(m, 'English', sep = '')
    vars$newvars[46+inc*(i-1)]<- paste(m, 'Experience.Total', sep = '')
    vars$newvars[47+inc*(i-1)]<- paste(m, 'Experience.Description', sep = '')
    vars$newvars[48+inc*(i-1)]<- paste(m, 'Cases.Contests', sep = '')
    vars$newvars[49+inc*(i-1)]<- paste(m, 'Cases.Bestresult', sep = '')
    vars$newvars[50+inc*(i-1)]<- paste(m, 'CV', sep = '')
    vars$newvars[51+inc*(i-1)]<- paste(m, 'Permission.PersonalInfo', sep = '')
  }

  vars
}
