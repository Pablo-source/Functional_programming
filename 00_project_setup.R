project_setup <-function(){
  
  if(!dir.exists("data")){dir.create("data")}
  if(!dir.exists("Output")){dir.create("Output")}
  if(!dir.exists("Checks")){dir.create("Checks")}
  if(!dir.exists("Plots")){dir.create("Plots")}
  if(!dir.exists("Archive")){dir.create("Archive")}
  #Create sub-folders within folders. This sub folder is nested under Archive folder
  if(dir.exists("Archive")){dir.create(("./Archive/subfolder_new"))}
  
} 

# Run function to create folder structure
project_setup()