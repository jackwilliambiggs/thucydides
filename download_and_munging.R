

# Loading libraries.

if (!require("pacman")) install.packages("pacman")

pacman::p_load(gutenbergr, tidyverse,rvest, stringi )

# Check if the packages are loaded
if(any(str_detect(as.character(sessionInfo()), "gutenbergr"))){
  print("gutenberg loaded successfully")
}

#retrieving the gutenberg id
thuc_id <- gutenberg_metadata %>%
  filter(title == "The History of the Peloponnesian War", 
         has_text == T) %>% pull(gutenberg_id) %>%  as.numeric

# read in the webpage that will make it so that an adequate mirror can be found.
mirror <-  read_html("https://www.gutenberg.org/MIRRORS.ALL")

# Extract the text from the web page
mirrors <- mirror %>% html_nodes( "body") %>% 
  html_text() %>%
  stri_split_lines()

#  Remove the second line. That does not contain any information
mirrors <- mirrors[[1]][str_detect(mirrors[[1]], "\\|")]

# Separate the lines by the "|"
mirrors <- strsplit(mirrors, "\\|")

# Apply triming of whitepace to all of the lines
mirrors <- lapply(mirrors, str_trim)

# Create the first 'row' that we want to bind proceeding rows onto.
start <- mirrors[[1]]

#For loop to bind rows onto.
for(i in 1:(length(mirrors) -1)){
   start <- rbind(start, mirrors[[i+1]])
   }

#  change matrix into a df
df <- as.data.frame(start)

#get the right names for the dataframe
names(df) <- df[1, ]

# remove first row
df <- df[-1, ]

rownames(df) <- NULL

# Grab the mirrors from GB... 

gb_mirrors <- df %>% filter(nation == "Great Britain") %>%  pull(url)


#downloading the book using one of the mirrors
thuc <- gutenberg_download(thuc_id, 
                            mirror = gb_mirrors[1])


# remove everyting in the environment that is not the thuc object
rm(list = ls()[!str_detect(ls(), "^thuc$")])


# Speeches in Thucydides. 
# Pericles' Funeral Oration: Book two

# Retrieve locations of the BOOK ## 
book_loc <- str_which(thuc$text, "^BOOK")

#retrieve books
books = thuc$text[book_loc]

# initialise feature to track book numbers in a column
thuc$book = ""

# Identify book pairs
for(i in 1:(length(book_loc)-1)){
  
  start_loc = book_loc[i]
  end_loc = book_loc[i+1]
  
  thuc[start_loc:end_loc, "book"] <- books[i]

}

# Last book
thuc[end_loc:length(thuc$text),"book" ] <- books[length(books)]


# Identify Chapter locations

chap_locs <- str_which(thuc$text, "^CHAPTER")

# Identifying chapter
chapters <- thuc$text[chap_locs]


# Applying chapters 
thuc$chapter <- ""

# Identify book pairs
for(i in 1:(length(chap_locs)-1)){
  
  start_loc = chap_locs[i] 
  end_loc = chap_locs[i+1]
  
  thuc[start_loc:end_loc, "chapter"] <- chapters[i]
  
}

# Last book
thuc[end_loc:length(thuc$text),"chapter" ] <- chapters[length(chapters)]

# Adding titles to the book.

thuc$title <- ""

for(row in 1:length(thuc$text)){
  
  if(str_detect(thuc$text[row], "\\_")){
    thuc$title[row] <- thuc$text[row]
    
  }

}

# Making it so that the titles are moved to a single line. This is a mess at the moment.

thuc$title_refined <- ""

#For those that have a title
for(i in 1:length(thuc$title[str_which(thuc$title, ".")])){
  # If the title does not end in _ paste together the title after it so that it is all on one line.
  if(!str_detect(thuc$title[str_which(thuc$title, ".")][i], "_$")){
    thuc$title_refined[str_which(thuc$title, ".")][i] <- paste(thuc$title[str_which(thuc$title, ".")][i],
                 thuc$title[str_which(thuc$title, ".")][i+1])
    
  } else if(str_detect(thuc$title[str_which(thuc$title, ".")][i], "\\_.*?\\_")) {
    thuc$title_refined[str_which(thuc$title, ".")][i] <- thuc$title[str_which(thuc$title, ".")][i]
    
  }
}


# There should be a check here to see if all of the letters have been pulled through into the new
# refined text column.


title_nchar <- thuc %>% filter(str_detect(title, ".")) %>% pull(title) 
title_refined_nchar <- thuc %>% filter(str_detect(title_refined, ".")) %>%  pull(title_refined) 

sum(str_count(title_nchar,  "[a-zA-Z]")) == sum(str_count(title_refined_nchar,  "[a-zA-Z]"))

# As this is true can rename 

thuc$title <- thuc$title_refined



# remove everyting in the environment that is not the thuc object
rm(list = ls()[!str_detect(ls(), "^thuc$")])



