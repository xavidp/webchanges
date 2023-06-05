# title: "NotifyBCNCATopd" # Ajuntament de Barcelona - Oferta Pública d'Ocupació
# author: "Xavier de Pedro"
# date: "14/05/2016"

# system.packages: sudo apt-get install libv8-dev sendemail
install.packages(c("rvest", "dplyr", "sendmailR", "stringr", "magrittr", "R2HTML", "daff"))
setwd(".")
#setwd("/home/xavi/Dropbox/00-ueb-xavi-comu/2016_Notify_VHIR_JOBS")
# See also http://www.r-bloggers.com/identifying-records-in-data-frame-a-that-are-not-contained-in-data-frame-b-%E2%80%93-a-comparison/
require(methods)
require(rvest)
require(dplyr)
require(R2HTML)
require(daff)
# To pick out an element at specified position, use magrittr::extract2
# which is an alias for [[
require(magrittr)
#my.rda.file <- "last.biocat.jobs.Rda"
my.rda.file <- "last.bcncat.jobs.Rda"
if (file.exists(my.rda.file)) {
  load(file=my.rda.file)
  jobs.list.all.previous <- jobs.list.all
} else {
  jobs.list.all.previous <- NULL
}
#table(job.list.all.previous[,Perfil] == job.list.all[,Perfil])
#url_base <- "http://www.biocat.cat/ca/que-fem/borsa-de-treball-i-practiques?page="
#url_base <- "http://www.vhir.org/portal1/search-ofertes_treball.asp?s=institut&contentid=1247&t=Ofertas%20de%20empleo&page_no="
url_base <- c("https://seuelectronica.ajuntament.barcelona.cat/ca/convocatoria/tecnica-superior-en-gestio",
              "https://seuelectronica.ajuntament.barcelona.cat/ca/convocatoria/tecnica-superior-en-organitzacio")
webpage <- list()
if (exists("jobs.list")) rm(jobs.list); jobs.list <- list()
if (exists("jobs.links")) rm(jobs.links); jobs.links <- list()
if (exists("jobs.status")) rm(jobs.status); jobs.status <- list()

# There use to be 6 pages of job links, but just in case, I set this loop until 10, 
# in case we have many more offers in the future
for (ii in 1:length(url_base)) {
  # download html files
  # ii <- 1
#  webpage[[ii]] <- read_html(paste0(url_base, ii))
  webpage[[ii]] <- read_html(paste0(url_base[ii]))
  
  # Check if there are more jobs there. Only fetch jobd list when 
  # the string "No open positions available" is not found in the html fetched
#  if (length(grep("No open positions available", html_text(webpage[[ii]]), fixed = TRUE)) == 0 ) {
  if (length(webpage[[ii]] %>% html_nodes("tbody") %>% html_nodes("a") ) > 0 ) {
    
    # the data we want is in the first table on this page
    # the html_table() command coerces the data into a data frame
    
    # Fetch job names list
    jobs.list[[ii]] <- webpage[[ii]] %>%
#      html_nodes("section") %>%
#      .[[3]] %>%
#      html_nodes(".list-group")%>%
      html_nodes("tbody")%>%
      html_nodes("a") 
    
    jobs.links[[ii]] <- jobs.list[[ii]] %>%
      html_attr("href")
#    jobs.links[[ii]] <- paste0("http://www.vhir.org/portal1/", jobs.links[[ii]])
    # html_table() 


# jobs.list.all.previous2 <- cbind(data.frame(jobs.list.all.previous), unlist(jobs.links.pdf))
# colnames(jobs.list.all.previous2) <- c("Status", "DateOpen", "DateClosed", "JobName", "link.html", "link.pdf")
# jobs.list.all.previous <- jobs.list.all.previous2
# write.table(jobs.list.all.previous, "2016-01-11_jobs.VHIR_list.all.txt", quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
# jobs.list.all <- jobs.list.all.previous

    jobs.list[[ii]] <- jobs.list[[ii]] %>% html_text() 

        
  }
  
}

#---------
# Prepare folders and file names
#---------
# Create folder if missing
folder.txts <- "TXT.BCNCAT"
if (!dir.exists(folder.txts)) {
  dir.create(folder.txts)
}

# Compose the filenames
outFileName.new.noext <- paste0( Sys.Date(), "_jobs.BCNCAT_list.new")
outFileName.changed.noext <- paste0( Sys.Date(), "_jobs.BCNCAT_list.changed")
outFileName.all.noext <- paste0( Sys.Date(), "_jobs.BCNCAT_list.all")
outFileNames <- c(paste0(outFileName.new.noext, ".txt"),
                  paste0(outFileName.new.noext, ".html"),
                  paste0(outFileName.changed.noext, ".txt"),
                  paste0(outFileName.changed.noext, ".html"),
                  paste0(outFileName.all.noext, ".txt"),
                  paste0(outFileName.all.noext, ".html"))
# Remove files of the same day if present
for (filename in outFileNames) {
  if (file.exists(file.path(folder.txts, filename))) {
    file.remove(file.path(folder.txts, filename))
  }
}
#---------


#---------
# Get the differences with the previous job list
#---------
require(data.table)
last.date <- format(Sys.time(), "%Y-%M-%d %X"); 
jobs.list.all <- rbindlist(jobs.list)
#head(jobs.list[[1]])
df1 <- data.frame(jobs.list.all.previous)
df1$link.pdf <- as.character(df1$link.pdf)
df2 <- data.frame(jobs.list.all)
df2$link.pdf <- as.character(df2$link.pdf)
head(df1); str(df1)
head(df2); str(df2)
#any(duplicated(df1))
#any(duplicated(df2))
if (length(all.equal(df1, df2))>0 && length(df1) != 1) {
  # do something, like merging the A and B into AB, and removing B from AB, or similar
  # See http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
  #jobs.new <- dplyr::anti_join(df2, df1, by="links.pdf")
  jobs.changed <- diff_data(df1,df2) 
  write_diff(jobs.changed, file = file.path(folder.txts, paste0(outFileName.changed.noext, ".csv")))
  render_diff(jobs.changed, file = file.path(folder.txts, paste0(outFileName.changed.noext, ".html")))
  
  # Try again with dplyr::setdiff
  #df1 <- apply(df1, 2, as.character)
  #df2 <- apply(df2, 2, as.character)
  #jobs.new <- dplyr::setdiff(df2, df1)
  #jobs.new <- data.table(jobs.new)
  
  #Read the full list of changes into a df, and get a subset of the additions
  jobs.changed.df <- fread( file.path(folder.txts, paste0(outFileName.changed.noext, ".csv")),
                            data.table = FALSE)
  colnames(jobs.changed.df)[1] <- "Type"
  jobs.new <- subset(jobs.changed.df, Type == "+++")[-1]
  
} else {
  jobs.changed.df <- NULL
}

#------------------------
# Store also that job list on disk
#------------------------
# Write results to disk
write.table(jobs.changed.df, file.path(folder.txts, paste0(outFileName.changed.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
write.table(jobs.list.all, file.path(folder.txts, paste0(outFileName.all.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
#HTML(jobs.list.all, paste0(outFileName.all.noext, ".html"), encoding = "utf-8")

#------------------------

# Last, download pdf files and compose the message and send it if there are new jobs found
if (dim(jobs.changed.df)[1] > 0) {
  # Fetch pdf from changed files
  # ------------------------
  # Clean the data frame of changed jobs so that there are no rows with "..."
  jobs.changed.df.clean <- base::subset(jobs.changed.df, Type != "")
  jobs.changed.df.clean <- base::subset(jobs.changed.df.clean, Type != "..." )
  #jobs.new <- data.table(jobs.list.all[1:4,])
  pdf.links <- as.character(jobs.changed.df.clean$link.pdf)
  folder.pdfs <- "PDF.VHIR"
  if (!dir.exists(folder.pdfs)) {
    dir.create(folder.pdfs)
  }
  
  jobs.new <- data.frame(jobs.new)
  jobs.new[] <- lapply(jobs.new, as.character)
  jobs.changed.df.clean <- data.frame(jobs.changed.df.clean)
  jobs.changed.df.clean[] <- lapply(jobs.changed.df.clean, as.character)
  
  for (n.pdf in 1:length(pdf.links)) {
    pdf.filename <- unlist(str_split(pdf.links[n.pdf], "\\\\", n=5))[5]
    download.file(pdf.links[n.pdf], file.path(folder.pdfs, pdf.filename), method="wget", quiet = FALSE, mode = "w",
                 cacheOK = TRUE, extra = getOption("download.file.extra"))
    # Once pdf's are downloaded, get rid of everything which is not the filename that was stored on disk  
    jobs.changed.df.clean[n.pdf, "link.pdf"] <- pdf.filename
  }
  
  # Write results to disk
  write.table(jobs.new, file.path(folder.txts, paste0(outFileName.new.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE)
#  HTML(jobs.new, paste0(outFileName.new.noext, ".html"))
  
  # compose the email
  # -----------------
  #from <- sprintf("<sendmailR@%s>", Sys.info()[4])
  from <- "xavier.depedro@vhir.org"
  to <- "ce.ofertes.treball@vhir.org"
  #to <- "xdpedro@ir.vhebron.net another@example.com athird@example.com"
  subject <- sprintf("[JOBS] VHIR: %s", Sys.Date()) 
  body <- "See the list of new jobs (since the last email) in the first attachment, the list of changes in the colored html table in the second, and the full list of jobs in this website in plain text in the last attachment below."
  cc <- NULL #"xavier.depedro@vhir.org" #NULL 
  bcc <- "xavier.depedro@vhir.org" 
  headers <- NULL 
  smtp <- "smtp.ir.vhebron.net"

  #control <- list(smtpServer="172.18.50.10", verboseShow=TRUE)
  #  control <- list(smtpServer="smtp.ir.vhebron.net", verboseShow=TRUE) # List of SMTP server settings. Valid values are the possible options for sendmail_options
  #sendmail(from, to, subject, body, control)
  
  
  # Send email to notify everything is done
  cat("\nSending the email confirming the job has been done... ")
  
  #key part for attachments, put the body and the mime_part in a list for msg
  attachmentPath.new <- file.path(getwd(), folder.txts, paste0(outFileName.new.noext, ".txt"))
  attachmentPath.changed <- file.path(getwd(), folder.txts, paste0(outFileName.changed.noext, ".html"))
  attachmentPath.all <- file.path(getwd(), folder.txts, paste0(outFileName.all.noext, ".txt"))
  #attachmentName <- outFileName
  #attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
  #bodyWithAttachment <- list(body,attachmentObject)
  #body <- bodyWithAttachment
  
  ## If more than one attachment, use this syntax
  #attachmentObject <- mime_part(x="subfolder/log.txt",name="log.txt")
  #attachmentObject2 <- mime_leName, quote = FALSE, sep=" | ", row.names=TRUE)
  
  #bodyWithAttachment <- list(body,attachmentObject,attachmentObject2)
  
  command <- paste("sendEmail -f ", from, " -t ", to, " -cc ", cc, " -bcc ", bcc, " -u \"", subject,
                   "\" -m \"", body, "\" -s ", smtp,
                   " -a \"", attachmentPath.new, "\" -a \"", attachmentPath.changed, "\" -a \"", attachmentPath.all,
                   "\" >> \"", attachmentPath.all, "\" ", " -o tls=no -o message-charset=utf-8 ", sep="");
  system(command);
  
  cat("\nEmail sent.\n ")  
}


# Save Rda to disk
save(last.date,
     jobs.changed.df,
     jobs.new,
     jobs.list.all,
     file=my.rda.file)

# Call through the command line with:
#
# Rscript "/home/xavi/code/webchanges/NotifyVHIRJobs.R"
#
# or 
#
# R CMD BATCH "/home/xavi/code/webchanges/NotifyVHIRJobs.R"
# cat NotifyVHIRJobs.Rout
#
## For cron jobs, add it for your user on a gnu/linux machine with something like:
## Add to your user's crontab with 
#
# crontab -e
#
## Content to add (something like this for days from Mon to Friday at 10:05 a.m.):
#
## m h  dom mon dow   command
#5 10 * * 1,2,3,4,5  cd /home/xavi/code/webchanges/;R CMD BATCH NotifyVHIRJobs.R
