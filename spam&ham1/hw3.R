
#############################################
#function find_body_start is to  find the first blank line in list 'line'
find_body_start = function(line) {
  index = grep("^\\s*$",line)[1]            #find the first blank line
  index               #return the value of indext

}

#########################################
#header() function deal with the header in "line", parameter index_body_start is the location 
#of the first line
header = function(index_body_start , line){
#process the first line of "line", to get away the "From..." type line
  start = 1                                 
  index1 = grepl("\\bFrom\\b\\s" , line[1])            #find weather the first line is needed to delete
  if (index1 == TRUE) start = 2             

#read as .dcf
  tt = textConnection(line[start:(index_body_start-1)])   
  head = read.dcf(tt, all = TRUE)
  close(tt)

  ## get the header
  header = as.vector(head)

  #result
  header
}

#########################################
#function find_index_boundary() is to  find the location of separate lines in line, and parameter
#index is the location of "boundary" in the header.
find_index_boundary = function(index , line){
  #separate the line[index[1]] based on ;". 
  boundry_line = unlist( strsplit(line[index[1]], '[;"]') ) 
  
  #the location of "boundary" in boundary _line, save them as a vector
  index_boundry = grep("\\bboundary[=]" , boundry_line ) 
  
  #use "" to replace the blank, let the "boundary=" be without blank 
  boundry_line[index_boundry] =gsub("\\s+" , "" , boundry_line[index_boundry])
  
  #the seperate line may have two states:
  #1. boundary = "separate line"
  #2. boundary = separate line
  #beacuse the line is seperate base on ;" , so the two situation is treated seperately.
  if(nchar(boundry_line[index_boundry]) > 9){
    separate_line = substr(boundry_line[index_boundry] , 10 , nchar(boundry_line[index_boundry]))
  }
    
 if (nchar(boundry_line[index_boundry]) <= 9) 
    separate_line = boundry_line[index_boundry+1]           #extract the separate line
  
  #there is one situation such as, the seperate line showing in the head 
  #is"=Multipart Boundary 0925021429", however in attachment it is"--= Multipart Boundary 0925021429"
  #so the blank is thorwn away in order to find the string.
  index_sep_line = grep(gsub(" ","",separate_line) , gsub(" ","",line) ,fixed = TRUE ,useBytes =TRUE)         #分割符号出现的地方
  
  index_sep_line
}
#this function is almost the same as above. It is used to find the end separate line, which is added 
#"--" at the end of the separate line.
find_index_boundary_end = function(index , line){
  
  boundry_line = unlist( strsplit(line[index[1]], '[;"]') ) 
  
  index_boundry = grep("\\bboundary[=]" , boundry_line ) #the location of "boundary" in boundary _line, save them as a vector
  
  boundry_line[index_boundry] =gsub("\\s+" , "" , boundry_line[index_boundry])
  
  if(nchar(boundry_line[index_boundry]) > 9){
    separate_line = substr(boundry_line[index_boundry] , 11 , nchar(boundry_line[index_boundry]))
  }
  
  if (nchar(boundry_line[index_boundry]) <= 9) 
    separate_line = boundry_line[index_boundry+1]           
  
  separate_line_end = paste0(separate_line , "--")
  
  index_sep_line_end = grep(gsub(" ","",separate_line_end) , gsub(" ","",line) ,fixed = TRUE , useBytes = TRUE)         #分割符号出现的地方
  
  index_sep_line_end
}

##########################################
#body() function is to extract the body of the email. It is considerated the situation that at the end 
#of the attachments, there is still a part of the body.
#the parameter index_body_start is the location of the first blank line. And the 'line' is the email.
#use paste(...,...,collapse = '\n'). Using "collapse = '\n'" is to want to use cat() to get the original
#body in the next work.
body = function(index_body_start,line){

  #the location of the boundary, and find it in the header.
  index = grep("\\bboundary[=]" , line[1:index_body_start])           
  
  if(length(index) != 0 ){      
    #the location of the separate line(type with more“--”)
    index_end = find_index_boundary_end(index , line)                    
   
    #use index[1] in case there is "boundary=" in body
    index_body_end = find_index_boundary(index[1] , line)[2] - 1               

    #the location of the last on separate line(maybe type with "--" or not)
    index_body_start2 = find_index_boundary(index[1] , line)[length(find_index_boundary(index , line))]+1
    
    #there is the possibility that after the attachments, there is still a part of body
    
    if(index_body_start2 == length(line))       #the separate line is the last line of email
      body_text = paste(line[index_body_start : index_body_end] , collapse = '\n')
      
    else {
      #if the last separate line isn't the end type(with"--"), then the following lines belong to attachment
      if(length(index_end) == 0 ) {            
      body_text = paste(line[index_body_start : index_body_end] , collapse = '\n')
    }    
    
      #if the last separate line is the end type, then the following lines belong to body
      else if(length(index_end) == 1 ){
      body_text = c(line[index_body_start : index_body_end] , line[index_body_start2:length(line)])
      body_text = paste(body_text , collapse = '\n')
        }
      }
    }

  else{
  body_text=NULL
   
  body_text = paste(line[index_body_start : length(line)] , collapse = '\n')
  
}

#this is the body and it is string
body_text
}

##########################################
#attachemnt() is to extract attachments , 
#index is the location of the boundary in header
attachment = function(index , line){ 
  index = find_index_boundary(index , line)          #find the locations of separate lines in line.
  index_end = find_index_boundary_end(index , line)  #find the location of end separate line in line.
  result = c()
    
  if(length(index == 2)){                            #if there is just 1 separate line.
    newline = line[(index[2]+1) : length(line)]       
    index_start = find_body_start(newline)
      
    if(index_start != 1)                              #in case there is no head
      head = header(index_start,newline)
      
      body_text=NULL
      body_text = paste(newline[index_start:length(newline)] , collapse = "\n")
      
      att = list(head , body_text)
      result = c(result , att )
      }
    
  else if(length(index )>2){
    
    for( i in 2 : (length(index)-1)){
    
    newline = line[(index[i]+1):(index[i+1]-1)]       #the ith attachment, define the new line to process.
    index_start = find_body_start(newline)
    
    if(index_start != 1)                              
     head = header(index_start,newline)
      
    body_text=NULL
    body_text = paste(newline[index_start:length(newline)] , collapse = "\n")
    
    att = list(head , body_text)
    result = c(result , att )
      }
    }
    result
}

##############################################
#main() function
main = function(){
  result = list()
  rr = list()
  k=1
  
  dir = list.files()                             #read the files' names, and save them in dir
  
  for(i in 1:length(dir)){
    line = readLines(dir[i])              
    index_body_start = find_body_start(line)
    
    if(is.na(index_body_start))  {
      result[[i]] = "This mail is invalid";
      next
      }           #in case there is no header in header. This mail is thrown away.
    
    hh= header(index_body_start , line)
    bb= body(index_body_start , line)
    
    index = grep("\\bboundary[=]" , line[1:index_body_start])
    
    #save the result in a list
    if(length(index) != 0 ){
      aa = attachment(index , line)
      rr[[1]] = hh
      rr[[2]] = bb
      rr[[3]] = aa
      result[[i]] = rr
    }
    
    else{
      rr[[1]] = hh
      rr[[2]] = bb
      result[[i]] = rr
    }
  }
  
  names(result) = paste(getwd(),dir,sep='/')
  result
}

##############################################
setwd('E:/STA141/ASSIGNMENT/3/SpamAssassinTraining/')
trainMessages = c()
dir1 = list.files()

for(k in 1:length(dir1)){
  setwd('E:/STA141/ASSIGNMENT/3/SpamAssassinTraining/')
  setwd(dir1[k])
  yy=main()
  trainMessages = c(trainMessages , yy)
}

setwd('E:/STA141/ASSIGNMENT/3/')
save(trainMessages , file = "TrainingMessages.rda")
#####################################


