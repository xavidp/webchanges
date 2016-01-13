# title: "NotifyVHIRJobs"
# author: "Xavier de Pedro"
# date: "09/01/2016"

#install.packages(c("rvest", "dplyr", "sendmailR", "stringr", "magrittr", "R2HTML"))
setwd("/home/xavi/code/NotifyWebChanges")
#setwd("/home/xavi/Dropbox/00-ueb-xavi-comu/2016_Notify_VHIR_JOBS")
require(methods)
require(rvest)
require(dplyr)
require(R2HTML)
# To pick out an element at specified position, use magrittr::extract2
# which is an alias for [[
require(magrittr)
#my.rda.file <- "last.biocat.jobs.Rda"
my.rda.file <- "last.vhir.jobs.Rda"
if (file.exists(my.rda.file)) {
  load(file=my.rda.file)
  jobs.list.all.previous <- jobs.list.all
} else {
  jobs.list.all.previous <- NULL
}
#table(job.list.all.previous[,Perfil] == job.list.all[,Perfil])
#url_base <- "http://www.biocat.cat/ca/que-fem/borsa-de-treball-i-practiques?page="
url_base <- "http://www.vhir.org/portal1/search-ofertes_treball.asp?s=institut&contentid=1247&t=Ofertas%20de%20empleo&page_no="
webpage <- list()
if (exists("jobs.list")) rm(jobs.list); jobs.list <- list()
if (exists("jobs.links")) rm(jobs.links); jobs.links <- list()
if (exists("jobs.links.pdf")) rm(jobs.links.pdf); jobs.links.pdf <- list()
if (exists("jobs.status")) rm(jobs.status); jobs.status <- list()
# There use to be 6 pages of job links, but just in case, I set this loop until 10, 
# in case we have many more offers in the future
for (ii in 1:10) {
  # download html files
  webpage[[ii]] <- read_html(paste0(url_base, ii))
  
  # Check if there are more jobs there. Only fetch jobd list when 
  # the string "No open positions available" is not found in the html fetched
#  if (length(grep("No open positions available", html_text(webpage[[ii]]), fixed = TRUE)) == 0 ) {
  if (length(webpage[[ii]] %>% html_nodes("section") %>% .[[3]] %>% 
      html_nodes(".list-group") %>% html_nodes("a") ) > 0 ) {
    
    # the data we want is in the first table on this page
    # the html_table() command coerces the data into a data frame
    
    # Fetch job names list
    jobs.list[[ii]] <- webpage[[ii]] %>%
      html_nodes("section") %>%
      .[[3]] %>%
      html_nodes(".list-group")%>%
      html_nodes("a") 
    
    jobs.links[[ii]] <- jobs.list[[ii]] %>%
      html_attr("href")
    jobs.links[[ii]] <- paste0("http://www.vhir.org/portal1/", jobs.links[[ii]])
    # html_table() 
    
    # Fetch also the url of the pdf itself, for furhter downloading of that pdf file to disk
    for (nn in 1:length(jobs.links[[ii]])) {
#for (nn in 1:dim(jobs.list.all.previous)[1]) {
#  jobs.links.pdf[[nn]] <- read_html(as.character(jobs.list.all.previous$link[nn]))
  
      jobs.links.pdf[[nn]] <- read_html(jobs.links[[ii]][nn])
      tmp.links <- jobs.links.pdf[[nn]] %>%
        html_nodes("section") %>%
        .[[1]] %>%
        html_nodes("a") %>% 
        html_attr("href") 
      
      tmp.pdf.link <- tmp.links %>% extract2(length(tmp.links))
      jobs.links.pdf[[nn]]  <-  paste0("http://www.vhir.org", tmp.pdf.link)   
    }
# jobs.list.all.previous2 <- cbind(data.frame(jobs.list.all.previous), unlist(jobs.links.pdf))
# colnames(jobs.list.all.previous2) <- c("Status", "DateOpen", "DateClosed", "JobName", "link.html", "link.pdf")
# jobs.list.all.previous <- jobs.list.all.previous2
# write.table(jobs.list.all.previous, "2016-01-11_jobs.VHIR_list.all.txt", quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
# jobs.list.all <- jobs.list.all.previous
    jobs.links[[ii]] <- cbind(unlist(jobs.links.pdf), data.frame(jobs.links[[ii]]))
    jobs.status[[ii]] <- jobs.list[[ii]] %>% html_nodes("img") %>% html_attr("src")
    jobs.status[[ii]] <- gsub("images/admin/check.png", "open", jobs.status[[ii]], fixed=TRUE)
    jobs.status[[ii]] <- gsub("images/admin/closed.png", "closed", jobs.status[[ii]], fixed=TRUE)
    
    jobs.list[[ii]] <- jobs.list[[ii]] %>% html_text() 
    require(stringr)
    jobs.list[[ii]] <- str_split_fixed(jobs.list[[ii]], "\t", 20)[,c(7,13,18)]
    jobs.list[[ii]] <- gsub("\r\n", "", x=jobs.list[[ii]], fixed=TRUE)
    jobs.list[[ii]] <- data.frame(jobs.list[[ii]])
    # %>% repair_encoding() 
    # %>% guess_encoding()
    
    #     # tsting
    #     html_nodes(
    #       html_nodes(
    #         html_nodes(webpage[[ii]], ".views-field-field-documento")[-1],
    #         ".file"),
    #       "a")
    
    #     # Fetch links to pdf of positions also
    #     jobs.links[[ii]] <- webpage[[ii]] %>%
    #       html_nodes(".views-field-field-documento") %>%
    #       html_nodes(".file") %>%
    #       html_nodes("a") %>%  html_attr("href")
    
    # Merge the two data tables
    jobs.list[[ii]] <- cbind(jobs.status[[ii]], jobs.list[[ii]], jobs.links[[ii]])
    jobs.list[[ii]] <- data.frame(jobs.list[[ii]])
    colnames(jobs.list[[ii]])<- c("Status", "DateOpen", "DateClosed", "JobName", "link.pdf", "link.html")
    
  }
  
}

# Get the differences with the previous job list
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
if (length(all.equal(df1, df2))>0 && length(df1) != 1) {
  # do something, like merging the A and B into AB, and removing B from AB, or similar
  jobs.new <- dplyr::setdiff(df2, df1)
  jobs.new <- data.table(jobs.new)
}

#------------------------
# Store also that job list on disk
#------------------------
# Create folder if missing
folder.txts <- "TXT.VHIR"
if (!dir.exists(folder.txts)) {
  dir.create(folder.txts)
}

# Compose the filenames
outFileName.new.noext <- paste0( Sys.Date(), "_jobs.VHIR_list.new")
outFileName.all.noext <- paste0( Sys.Date(), "_jobs.VHIR_list.all")
outFileNames <- c(paste0(outFileName.new.noext, ".txt"),
                  paste0(outFileName.new.noext, ".html"),
                  paste0(outFileName.all.noext, ".txt"),
                  paste0(outFileName.all.noext, ".html"))
# Remove files of the same day if present
for (filename in outFileNames) {
  if (file.exists(file.path(folder.txts, filename))) {
    file.remove(file.path(folder.txts, filename))
  }
}

# Write results to disk
write.table(jobs.list.all, file.path(folder.txts, paste0(outFileName.all.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
#HTML(jobs.list.all, paste0(outFileName.all.noext, ".html"), encoding = "utf-8")

#------------------------

# Last, download pdf files and compose the message and send it if there are new jobs found
if (dim(jobs.new)[1] > 0) {
  # Fetch pdf from new files
  # ------------------------
  #jobs.new <- data.table(jobs.list.all[1:4,])
  pdf.links <- as.character(jobs.new$link.pdf)
  folder.pdfs <- "PDF.VHIR"
  if (!dir.exists(folder.pdfs)) {
    dir.create(folder.pdfs)
  }
  
  jobs.new <- data.frame(jobs.new)
  jobs.new[] <- lapply(jobs.new, as.character)
  
  for (n.pdf in 1:length(pdf.links)) {
    pdf.filename <- unlist(str_split(pdf.links[n.pdf], "\\\\", n=5))[5]
    download.file(pdf.links[n.pdf], file.path(folder.pdfs, pdf.filename), method="wget", quiet = FALSE, mode = "w",
                 cacheOK = TRUE, extra = getOption("download.file.extra"))
    # Once pdf's are downloaded, get rid of everything which is not the filename that was stored on disk  
    jobs.new[n.pdf, "link.pdf"] <- pdf.filename
  }
  
  # Write results to disk
  write.table(jobs.new[,1:4], file.path(folder.txts, paste0(outFileName.new.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE)
  write.table("\n", file.path(folder.txts, paste0(outFileName.new.noext, ".txt")), quote = FALSE, sep="", row.names=FALSE, col.names=FALSE, append=TRUE)
  write.table(jobs.new[,5:6], file.path(folder.txts, paste0(outFileName.new.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
#  HTML(jobs.new, paste0(outFileName.new.noext, ".html"))
  
  # compose the email
  # -----------------
  #from <- sprintf("<sendmailR@%s>", Sys.info()[4])
  from <- "xavier.depedro@vhir.org"
  to <- "xavier.depedro@vhir.org"
  #to <- "xdpedro@ir.vhebron.net"
  subject <- sprintf("[JOBS] VHIR: %s", Sys.Date()) 
  body <- "See the list of new jobs (since the last email) in the first attachment, and the full list of jobs in this website in the second attachment below."
  cc <- NULL 
  bcc <- NULL 
  headers <- NULL 
  smtp <- "smtp.ir.vhebron.net"

  #control <- list(smtpServer="172.18.50.10", verboseShow=TRUE)
  #  control <- list(smtpServer="smtp.ir.vhebron.net", verboseShow=TRUE) # List of SMTP server settings. Valid values are the possible options for sendmail_options
  #sendmail(from, to, subject, body, control)
  
  
  # Send email to notify everything is done
  cat("\nSending the email confirming the job has been done... ")
  
  #key part for attachments, put the body and the mime_part in a list for msg
  attachmentPath.new <- file.path(getwd(), folder.txts, paste0(outFileName.new.noext, ".txt"))
  attachmentPath.all <- file.path(getwd(), folder.txts, paste0(outFileName.all.noext, ".txt"))
  #attachmentName <- outFileName
  #attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
  #bodyWithAttachment <- list(body,attachmentObject)
  #body <- bodyWithAttachment
  
  ## If more than one attachment, use this syntax
  #attachmentObject <- mime_part(x="subfolder/log.txt",name="log.txt")
  #attachmentObject2 <- mime_leName, quote = FALSE, sep=" | ", row.names=TRUE)
  
  #bodyWithAttachment <- list(body,attachmentObject,attachmentObject2)
  
  command <- paste("sendEmail -f ", from, " -t ", to, " -u \"", subject,
                   "\" -m \"", body, "\" -s ", smtp,
                   " -a \"", attachmentPath.new, "\" -a \"", attachmentPath.all,
                   "\" >> \"", attachmentPath.all, "\" ", " -o tls=no -o message-charset=utf-8 ", sep="");
  system(command);
  
  cat("\nEmail sent.\n ")  
}


# Save Rda to disk
save(last.date,
     jobs.new,
     jobs.list.all,
     file=my.rda.file)

# Call through the command line with:
#
# Rscript "/home/xavi/code/NotifyWebChanges/NotifyWebChanges.R"
#
# or 
#
# R CMD BATCH "/home/xavi/code/NotifyWebChanges/NotifyWebChanges.R"
# cat NotifyWebChanges.Rout