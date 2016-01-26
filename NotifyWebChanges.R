# title: "NotifyWebchanges"
# author: "Xavier de Pedro"
# date: "04/01/2016"

# system.packages: sudo apt-get install sendemail
#install.packages(c("rvest", "dplyr", "sendmailR")
setwd(".")
require(methods)
require(rvest)
my.rda.file <- "last.biocat.jobs.Rda"
if (file.exists(my.rda.file)) {
  load(file=my.rda.file)
  jobs.list.all.previous <- jobs.list.all
} else {
  jobs.list.all.previous <- NULL
}
#table(job.list.all.previous[,Perfil] == job.list.all[,Perfil])
url_base <- "http://www.biocat.cat/ca/que-fem/borsa-de-treball-i-practiques?page="
webpage <- list()
if (exists("jobs.list")) rm(jobs.list); jobs.list <- list()
if (exists("jobs.links")) rm(jobs.links); jobs.links <- list()
for (ii in 1:4) {
  # download html files
  webpage[[ii]] <- read_html(paste0(url_base, ii-1))

  # Check if there are more jobs there. Only fetch jobd list when 
  # the string "No open positions available" is not found in the html fetched
  if (length(grep("No open positions available", html_text(webpage[[ii]]), fixed = TRUE)) == 0 ) {
    # the data we want is in the first table on this page
    # the html_table() command coerces the data into a data frame
    
    # Fetch job names list
    jobs.list[[ii]] <- webpage[[ii]] %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table() 
    
    # tsting
    html_nodes(
      html_nodes(
        html_nodes(webpage[[ii]], ".views-field-field-documento")[-1],
        ".file"),
      "a")
    
    # Fetch links to pdf of positions also
    jobs.links[[ii]] <- webpage[[ii]] %>%
      html_nodes(".views-field-field-documento") %>%
      html_nodes(".file") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Merge the two data tables
    jobs.list[[ii]] <- cbind(jobs.list[[ii]], jobs.links[[ii]])
    colnames(jobs.list[[ii]])[4] <- "link"
  }

}
require(data.table)
#n <- n+1
last.date <- format(Sys.time(), "%Y-%M-%d %X"); 
jobs.list.all <- rbindlist(jobs.list)
#head(jobs.list.all)
# Test some differences
#jobs.list.all.previous[2,] <- rep("foo", 4)

df1 <- data.frame(jobs.list.all.previous)
df1$link <- as.character(df1$link)
df2 <- data.frame(jobs.list.all)
df2$link <- as.character(df2$link)

if (length(all.equal(df1, df2))>0) {
  # do something, like merging the A and B into AB, and removing B from AB, or similar
  jobs.new <- dplyr::setdiff(df2, df1)
  jobs.new <- data.table(jobs.new)
}

# Save Rda to disk
save(last.date,
     jobs.list.all,
     jobs.new,
     file=my.rda.file)

# Create folder if missing
folder.txts <- "TXT.BIOCAT"
if (!dir.exists(folder.txts)) {
  dir.create(folder.txts)
}

# Compose the filename
outFileName.new.noext <- paste0( Sys.Date(), "_jobs.BIOCAT_list.new")
outFileName.all.noext <- paste0( Sys.Date(), "_jobs.BIOCAT_list.all")
outFileNames <- c(paste0(outFileName.new.noext, ".txt"),
                  paste0(outFileName.all.noext, ".txt"))
# Remove files of the same day if present
for (filename in outFileNames) {
  if (file.exists(file.path(folder.txts, filename))) {
    file.remove(file.path(folder.txts, filename))
  }
}

# Write results to disk
write.table(jobs.list.all, file.path(folder.txts, paste0(outFileName.all.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)
write.table(jobs.new, file.path(folder.txts, paste0(outFileName.new.noext, ".txt")), quote = FALSE, sep=" | ", row.names=TRUE, append=TRUE)

# Send email with list of jobs and their urls
#from <- sprintf("<sendmailR@%s>", Sys.info()[4])
from <- "xavier.depedro@vhir.org"
to <- "xavier.depedro@vhir.org"
#to <- "xdpedro@ir.vhebron.net"
  subject <- sprintf("[JOBS] BIOCAT: %s", Sys.Date()) 
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
#attachmentObject2 <- mime_part(x="subfolder/log2.txt",name="log2.txt")
#bodyWithAttachment <- list(body,attachmentObject,attachmentObject2)

command <- paste("sendEmail -f ", from, " -t ", to, " -u \"", subject,
                 "\" -m \"", body, "\" -s ", smtp,
                 " -a \"", attachmentPath.new, "\" -a \"", attachmentPath.all,
                 "\" >> \"", attachmentPath.all, "\" ", " -o tls=no -o message-charset=utf-8 ", sep="");
system(command);

cat("\nEmail sent.\n ")   

# Call through the command line with:
#
# Rscript "/home/xavi/code/NotifyWebChanges/NotifyWebChanges.R"
#
# or 
#
# R CMD BATCH "/home/xavi/code/NotifyWebChanges/NotifyWebChanges.R"
# cat NotifyWebChanges.Rout

## For cron jobs, add it for your user on a gnu/linux machine with something like:
## Add to your user's crontab with 
#
# crontab -e
#
## Content to add (something like this for days from Mon to Friday at 10 a.m.):
#
## m h  dom mon dow   command
#0 10 * * 1,2,3,4,5  cd /home/xavi/code/webchanges/;R CMD BATCH NotifyWebChanges.R
