CommonFields <- function (vars){
vars$newvars <- vars$oldvars
## Team variable names
  
  vars$newvars[22] <- 'Team.Name'
  vars$newvars[23] <- 'Team.Email'
  vars$newvars[24] <- 'Team.Section'
  vars$newvars[25] <- 'Team.Full'
  vars$newvars[26] <- 'Email4'
  vars$newvars[27] <- 'Email3'
  vars$newvars[28] <- 'Email2'
  vars$newvars[29] <- 'Email1'
  
## Members variable names
  if (nrow(vars) > 70){
    n <- 4
  } else {
    n <- 1
  }
  
  inc <- 32
  
  for (i in 1:n){

    if (nrow(vars) > 70) {
      m <- paste("Member", i, '.', sep = '')
    } else {
      m <- ''
    }    
    vars$newvars[30+inc*(i-1)]<- paste(m, 'Surname', sep = '')
    vars$newvars[31+inc*(i-1)]<- paste(m, 'Name', sep = '')
    vars$newvars[32+inc*(i-1)]<- paste(m, 'Email', sep = '')
    vars$newvars[33+inc*(i-1)]<- paste(m, 'Phone', sep = '')
    vars$newvars[34+inc*(i-1)]<- paste(m, 'Homecity', sep = '')
    vars$newvars[35+inc*(i-1)]<- paste(m, 'Uni', sep = '')
    vars$newvars[36+inc*(i-1)]<- paste(m, 'Uni.Custom', sep = '')
    vars$newvars[37+inc*(i-1)]<- paste(m, 'Course', sep = '')
    vars$newvars[38+inc*(i-1)]<- paste(m, 'Speciality', sep = '')
    vars$newvars[39+inc*(i-1)]<- paste(m, 'English', sep = '')
    vars$newvars[40+inc*(i-1)]<- paste(m, 'Experience.Total', sep = '')
    vars$newvars[41+inc*(i-1)]<- paste(m, 'Experience.Description', sep = '')
    vars$newvars[42+inc*(i-1)]<- paste(m, 'Cases.Contests', sep = '')
    vars$newvars[43+inc*(i-1)]<- paste(m, 'Cases.Bestresult', sep = '')
    vars$newvars[44+inc*(i-1)]<- paste(m, 'Foreign.Masters.Plan', sep = '')
    vars$newvars[45+inc*(i-1)]<- paste(m, 'Master.of.Science.in.International.Business', sep = '')
    vars$newvars[46+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Fashion.Management', sep = '')
    vars$newvars[47+inc*(i-1)]<- paste(m, 'Master.of.Science.in.International.Business.Negotiation', sep = '')
    vars$newvars[48+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Digital.Marketing', sep = '')
    vars$newvars[49+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Business.Analysis.and.Consulting', sep = '')
    vars$newvars[50+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Finance', sep = '')
    vars$newvars[51+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Investment.Banking.and.Capital', sep = '')
    vars$newvars[52+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Accounting.Audit.and.Control', sep = '')
    vars$newvars[53+inc*(i-1)]<- paste(m, 'Master.of.Science.in.Big.Data.Analytics.for.Business', sep = '')
    vars$newvars[54+inc*(i-1)]<- paste(m, 'IELTS.Test.Experience', sep = '')
    vars$newvars[55+inc*(i-1)]<- paste(m, 'GRE.Test.Experience', sep = '')
    vars$newvars[56+inc*(i-1)]<- paste(m, 'TOEFL.Test.Experience', sep = '')
    vars$newvars[57+inc*(i-1)]<- paste(m, 'GMAT.Test.Experience', sep = '')
    vars$newvars[58+inc*(i-1)]<- paste(m, 'CAE.CPE.Test.Experience', sep = '')
    vars$newvars[59+inc*(i-1)]<- paste(m, 'No.Test.Experience', sep = '')
    vars$newvars[60+inc*(i-1)]<- paste(m, 'CV', sep = '')
    vars$newvars[61+inc*(i-1)]<- paste(m, 'Personal.Info.Permition', sep = '')
  }

  vars
}
