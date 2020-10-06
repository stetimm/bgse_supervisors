### Packages etc ### 
#install.packages("rvest")
#install.packages("pdftools")
#install.packages("openxlsx")
require("rvest")
require("openxlsx")

## scrape links to personal pages 
url = "https://www.bgse.uni-bonn.de/en/people/student-directory"
doc = read_html(url)

column = as.character(html_nodes(doc,"td:nth-child(1)"))
links = c()
first_names = c()
last_names =c()
for(i in 1:length(column)){
  if (substr(column[i],1,6) == "<td><a"){
    url = paste("https://www.bgse.uni-bonn.de", substring(column[i], regexpr("f=\"", column[i]) + 3, regexpr("\">", column[i])-1),sep = "")
    temp = substring(column[i],regexpr("\">", column[i]) + 2, nchar(column[i])) # doing this in two steps because they are irregular
    name_full = substr(temp, 0, regexpr("<", temp)-1) # this is the name in Lastname, Firstname Format
    first_name = substr(name_full, regexpr(",", temp)+2, nchar(name_full))
    last_name = substr(name_full, 0, regexpr(",", temp)-1)
    links = append(links, url, after = length(links))
    first_names = append(first_names, first_name)
    last_names = append(last_names, last_name)
  }
}
# fix one error: 
links[which(links == "https://www.bgse.uni-bonn.desimon-rother/")] = "https://www.bgse.uni-bonn.de/en/people/student-directory/simon-rother"

## now need to find supervisor(s) 
# first define data structure
supervisor_table = data.frame(first_names, last_names, links, stringsAsFactors = FALSE)

## Working Supervisor Function - switch from <p> as identifier to <td> as identifier - identifies more than that with p
find_supervisor = function(student_url){
  url = student_url
  doc = read_html(url)
  content = as.character(html_nodes(doc,"#parent-fieldname-text td"))
  supervisors = c()
  for(i in 1:length(content)){
    if (regexpr("supervisor", content[i])[1] != -1){ #-1 if not contained, if contained supervisor name 2 elements further
      supervisor = substr(content[i+2], regexpr("\">", content[i+2]) + 2, regexpr("</a>", content[i+2])-1) #i+2 to capter 2 elements further, +2 after regex to remove ">, -1 after regex to remove ">"
      supervisors = append(supervisors,supervisor,after = length(supervisors))
    }
  }
  return(supervisors)
}

raw_supervisors = lapply(supervisor_table$links,find_supervisor)

# TODO: this is ugly as shit...

s1 = c()

for(i in 1:length(raw_supervisors)){
  inspect = unlist(raw_supervisors[i])
  if(is.null(inspect)){
    s1 = append(s1,"NA")
  } else{
    s1 = append(s1,inspect[1])
  }
}

s2 = c()
for(i in 1:length(raw_supervisors)){
  inspect = unlist(raw_supervisors[i])
  if(is.null(inspect) || is.na(inspect[2])){
    s2 = append(s2,"NA")
  } else{
    s2 = append(s2,inspect[2])
  }
}

s3 = c()
for(i in 1:length(raw_supervisors)){
  inspect = unlist(raw_supervisors[i])
  if(is.null(inspect) || is.na(inspect[3])){
    s3 = append(s3,"NA")
  } else{
    s3 = append(s3,inspect[3])
  }
}

supervisor_table$s1 = s1
supervisor_table$s2 = s2
supervisor_table$s3 = s3
colnames(supervisor_table) = c("First Name", "Last Name", "URL", "Supervisor 1", "Supervisor 2", "Supervisor 3")

# issue, some supervisors are just "" due to different HTML, not sure how to catch these exceptions. Maybe just flag for now?
issues = supervisor_table[c(which(supervisor_table$'Supervisor 1' ==""), which(supervisor_table$'Supervisor 2' ==""), which(supervisor_table$'Supervisor 3' =="")),]
# need to fix these manually

# export
# write.csv(supervisor_table, file = "bgse_supervisors.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.xlsx(supervisor_table,file = "bgse_supervisors.xlsx", row.names = FALSE, fileEncoding = "UTF-8")
