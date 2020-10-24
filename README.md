# bgse_supervisors
Quick and Dirty R Code to scrape a list of all bgse students, their supervisors and funding sources from the BGSE website. 

## Coding Idea
This is just based on HTML nodes and quite simple. Each BGSE student has a personal page on the BGSE website.  
1. We first scrape the URL of these personal pages for each student from the student directory page (https://www.bgse.uni-bonn.de/en/people/student-directory). While doing so, we get first and last name as well as starting year of each student. 
2. On the student pages, information is structured in a table. We scrape the content of the table searching for supervisors and fellowship information using regular expressions. There are a few irregularities in the HTML code used which necessitates some exception handling via if-else statements. 

## Status Quo
As of Oct 24, 2020, info on all BGSE students can be scraped without any obvious errors or missing data. Next step is to implement this as a shiny app and do some fancy data visualization. 