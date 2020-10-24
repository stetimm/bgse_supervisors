# Packages etc ----
#install.packages("rvest")
#install.packages("pdftools")
#install.packages("openxlsx")
require("rvest")
require("openxlsx")


# scrape links to personal pages  ----
url = "https://www.bgse.uni-bonn.de/en/people/student-directory"
doc = read_html(url)

column = as.character(html_nodes(doc,"td:nth-child(1)")) # just extracts the first column of the directory
links = c()
first_names = c()
last_names = c()
starting_years = c()

# in this loop we assemble the URL of the personal page of each BGSE student listed in the directory
# this is done by extracting the link shown for each name on https://www.bgse.uni-bonn.de/en/people/student-directory

for(i in 1:length(column)){
  # identify a row in the column that contains a link
  if (substr(column[i],1,6) == "<td><a"){ 
    # assemble URL
    student_specific_url_part = substring(column[i], regexpr("f=\"", column[i]) + 3, regexpr("\">", column[i])-1)
    url = paste("https://www.bgse.uni-bonn.de", student_specific_url_part,sep = "")
    # doing this in two steps because they are irregular
    temp = substring(column[i],regexpr("\">", column[i]) + 2, nchar(column[i])) 
    
    name_full = substr(temp, 0, regexpr("<", temp)-1) # this is the name in Lastname, Firstname Format
    # identify first and last name 
    first_name = substr(name_full, regexpr(",", temp)+2, nchar(name_full))
    last_name = substr(name_full, 0, regexpr(",", temp)-1)
    # identify year 
    year = substring(student_specific_url_part, regexpr("directory/", student_specific_url_part)+10, regexpr("directory/", student_specific_url_part)+13)
    # save URL, first, and last name plus year
    links = append(links, url, after = length(links))
    first_names = append(first_names, first_name)
    last_names = append(last_names, last_name)
    starting_years = append(starting_years, year)
  }
}
# fix one error: 
links[which(links == "https://www.bgse.uni-bonn.desimon-rother/")] = "https://www.bgse.uni-bonn.de/en/people/student-directory/simon-rother"
starting_years[which(starting_years == "ther")] = 2016

# now need to find supervisor(s) and fellow information ----
# first define data structure
supervisor_table = data.frame(first_names, last_names, starting_years, links, stringsAsFactors = FALSE)
supervisor_table[c("first_supervisor","second_supervisor","third_supervisor","first_fellow","second_fellow","third_fellow")] = NA

# then go through each row in data frame, scrape info on supervisor and fellow and populate data frame accordingly
for(j in 1:length(supervisor_table$links)){
  doc = read_html(supervisor_table$links[j])
  content = as.character(html_nodes(doc,"#parent-fieldname-text td"))
  numsup = 0 # tracks number of supervisors per student
  numfel = 0 # tracks number of fellowships per student
  for(i in 1:length(content)){
    # read supervisors - works
    if (regexpr("supervisor", content[i])[1] != -1){ #-1 if not contained, if contained supervisor name 2 elements further
      # catching exceptions 
      # first if is for screwed up links (if table content is excessively long)
      # second if is for supervisors without link structure (only name but no link)
      if (nchar(content[i+2])>250){ 
        end_of_string = substr(content[i+2], nchar(content[i+2])-50, nchar(content[i+2]))
        supervisor = substr(end_of_string, regexpr("\">", end_of_string) + 2, regexpr("</a>", end_of_string)-1) #i+2 to capter 2 elements further, +2 after regex to remove ">, -1 after regex to remove ">"
      
      } else if(regexpr("</a>", content[i+2]) == -1){ 
        # need to address cases in which they have "<td><p>Name</p></td>" and in which they just do "<td>Name</td>"
        if (regexpr("<p>", content[i+2]) == -1){
          supervisor = substr(content[i+2], regexpr("<td>", content[i+2]) + 4, regexpr("</td>", content[i+2])-1)
        } else{
          supervisor = substr(content[i+2], regexpr("<p>", content[i+2]) + 3, regexpr("</p>", content[i+2])-1)
        }
        
      } else{
        supervisor = substr(content[i+2], regexpr("\">", content[i+2]) + 2, regexpr("</a>", content[i+2])-1) #i+2 to capter 2 elements further, +2 after regex to remove ">, -1 after regex to remove ">"
      }
      # now save result
      supervisor_table[j, 5+numsup] = supervisor # directly populate first, second or third supervisor column
      numsup = numsup + 1
    }
    # read fellow - works
    if (regexpr("fellow", content[i]) != -1){#-1 if not contained, if contained fellow information in this element
      if (regexpr("<p>", content[i]) != -1){
        fellow = substr(content[i],regexpr("<p>",content[i])+3, regexpr("fellow", content[i])+5)
      } else{
        fellow = substr(content[i],regexpr("\">",content[i])+2, regexpr("fellow", content[i])+5)
      }
      supervisor_table[j, 8+numfel] = fellow # directly populate first, second or third fellow info column
      numfel = numfel + 1
    }
  }
}



# issue, some supervisors are just "" due to different HTML, not sure how to catch these exceptions. Maybe just flag for now?
# issues = supervisor_table[c(which(supervisor_table$first_supervisor ==""), which(supervisor_table$second_supervisor ==""), which(supervisor_table$third_supervisor =="")),]
# need to fix these manually
# Idea to fix this: strings for these links are super long and contain multiple href (or whatever identifier I used) - just add an additional if clause or something that catches this and then treat accordingly
# TODO: implement idea from above
# DONE

test_url = "https://www.bgse.uni-bonn.de/en/people/student-directory/2017/cavit-goerkem-destan"
doc = read_html(test_url)
content = as.character(html_nodes(doc,"#parent-fieldname-text td"))
for(i in 1:length(content)){
  # read supervisors - works
  if (regexpr("supervisor", content[i])[1] != -1){ #-1 if not contained, if contained supervisor name 2 elements further
    print(i)
    # try to fix one issue
    if (nchar(content[i+2])>250){
      print("caught!")
      end_of_string = substr(content[i+2], nchar(content[i+2])-50, nchar(content[i+2]))
      supervisor = substr(end_of_string, regexpr("\">", end_of_string) + 2, regexpr("</a>", end_of_string)-1) #i+2 to capter 2 elements further, +2 after regex to remove ">, -1 after regex to remove ">"
    }else if(regexpr("</a>", content[i+2]) == -1){
      supervisor = substr(content[i+2], regexpr("<p>", content[i+2]) + 3, regexpr("</p>", content[i+2])-1)
    }else{
      supervisor = substr(content[i+2], regexpr("\">", content[i+2]) + 2, regexpr("</a>", content[i+2])-1) #i+2 to capter 2 elements further, +2 after regex to remove ">, -1 after regex to remove ">"
    }
  }
}


# Destan male has issue with fellow info. 


# export
# write.csv(supervisor_table, file = "bgse_supervisors.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.xlsx(supervisor_table,file = "bgse_supervisors.xlsx", row.names = FALSE, fileEncoding = "UTF-8")
