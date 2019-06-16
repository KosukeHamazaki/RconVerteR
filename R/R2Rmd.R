#' Function to convert R file into Rmd file with chunks
#'
#' @param fileName The R file name which you want to refer.
#' @param outFormat  The output format for the subtitle (chapter title). The default is "unknown", and
#' this will result in the simplest format (the number of "#" will always be 3 in Rmd file).
#' The other is outFormat = 1, which returns the number of "#" in Rmd file as below.
#'
#' (the number of "#" in Rmd file) = 8 - (the number of "#" in R file (if > 2))
#'
#' (the number of "#" in Rmd file) = 0 (if (the number of "#" in R file) <= 2)
#'
#' @param rmessage Show message for chunks in the Rmd file or not.
#' @param rwarning  Show warning for chunks in the Rmd file or not.
#' @param textTitle  The title of the Rmd file.
#'
#' If textTitle = NULL, the file name will be used.
#' @param Author  The author of the Rmd file.
#' @param Date  The date of the Rmd file.
#' @param saveFileName The name of the Rmd file to be saved.
#'
#' If saveFileName = NULL,  the file name will be used. (only file extension will change)
#' @param plotEcho  Show the code for plotting in chunks or not.
#'
#'
#' @return Nothing. Coverted ".Rmd" file will be created.
#'
#' @export
#'
R2Rmd <- function(fileName, outFormat = "unknown", rmessage = FALSE,
                  rwarning = FALSE, textTitle = NULL, Author = NULL,
                  Date = NULL, saveFileName = NULL, plotEcho = TRUE) {

  sc0 <- read_file(file = fileName)

  sc0.vec <- str_split(string = sc0, pattern = "\n")[[1]]
  sc0.len <- length(sc0.vec)

  sc0.vec.rm.tab <- str_remove_all(sc0.vec, pattern = "\t")

  sc.kind <- rep(NA, sc0.len)
  sc.com <- sapply(sc0.vec, function(x){
    str_starts(x, pattern = "#")
  })
  sc.kind[sc.com] <- "comment"

  sc.blank <- sapply(sc0.vec, str_count) == 0
  sc.kind[sc.blank] <- "blank"

  sc.kind[is.na(sc.kind)] <- "code"

  plt.st <- c("plot", "ggpl", "curv", "manh")
  pltAdd.st <- c("point", "legen", "segme", "polyg",
                 "ablin", "lines", "text(", "title")

  sc.kind.2 <- rep(NA, sc0.len)

  sc.kind.2[1] <- sc.kind[1]
  for (i in 2:sc0.len) {
    if(sc.kind[i] == "comment") {
      if((sc.kind[i - 1] == "code") |
         (sc.kind.2[i - 1] == "comment_code")) {
        sc.kind.2[i] <- "comment_code"
      } else {
        sc.kind.2[i] <- "comment"
      }
    } else {
      if(sc.kind[i] == "code") {
        sc.st.now.4 <- str_sub(string = sc0.vec[i],
                                      start = 1, end = 4)
        sc.st.now.5 <- str_sub(string = sc0.vec[i],
                                        start = 1, end = 5)

        if(sc.st.now.4 %in% plt.st) {
          sc.kind.2[i] <- "code_plot"
        } else {
          if(sc.st.now.5 %in% pltAdd.st) {
            sc.kind.2[i] <- "code_plot_add"
          } else {
            sc.kind.2[i] <- "code"
          }
        }
      } else {
        sc.kind.2[i] <- "blank"
      }
    }
  }

  code.names <- c("code", "comment_code",
                  "code_plot", "code_plot_add")

  sc.vec <- rep(NA, sc0.len)
  for(i in 1:sc0.len){
    if(sc.kind.2[i] == "comment"){
      sc.rm.now <- str_remove_all(string = sc0.vec[i],
                                  pattern = "#")

      if(outFormat == "unknown"){
        sc.new.now <- paste0("####", sc.rm.now, collapse = "")
      } else {
        if(outFormat == 1){
          sharp.count <- str_count(str_sub(sc0.vec[i], 1, 8), "#")

          if(sharp.count == 8) {
            sc.new.now <- ""
          } else {
            if(sharp.count > 2){
              sc.new.now <- paste0(str_c(rep("#", 8 - sharp.count), collapse = ""),
                                   sc.rm.now, collapse = "")
            } else {
              sc.new.now <- sc.rm.now
            }
          }
        } else {
          stop("Other formats of output script are not available now !!!")
        }
      }
    } else {
      if(sc.kind.2[i] == "blank") {
        sc.new.now <- sc0.vec[i]
      } else {
        sc.new.now <- sc0.vec[i]

        if(plotEcho) {
          if(i == 1){
            sc.new.now <- paste0("```{r message = ", rmessage,
                                 ", warning = ", rwarning,
                                 ", echo = ", TRUE, "}\n",
                                 sc.new.now)
          }else{
            if(!(sc.kind.2[i - 1] %in% code.names)){
              sc.new.now <- paste0("```{r message = ", rmessage,
                                   ", warning = ", rwarning,
                                   ", echo = ", TRUE, "}\n",
                                   sc.new.now)
            }
          }

          if(i == sc0.len){
            sc.new.now <- paste0(sc.new.now, "\n```")
          } else {
            if(sc.kind.2[i + 1] %in% c("comment", "blank")){
              sc.new.now <- paste0(sc.new.now, "\n```")
            }
          }

        } else {
          if (sc.kind.2[i] %in% c("comment_code", "code")) {
            echo.now <- TRUE

            if (i == 1) {
              sc.new.now <- paste0("```{r message = ", rmessage,
                                   ", warning = ", rwarning,
                                   ", echo = ", echo.now, "}\n",
                                   sc.new.now)
            } else {
              if (!(sc.kind.2[i - 1] %in% c("comment_code", "code"))) {
                sc.new.now <- paste0("```{r message = ", rmessage,
                                     ", warning = ", rwarning,
                                     ", echo = ", echo.now, "}\n",
                                     sc.new.now)
              }
            }


            if(i == sc0.len){
              sc.new.now <- paste0(sc.new.now, "\n```")
            } else {
              if(!(sc.kind.2[i + 1] %in% c("comment_code", "code"))){
                sc.new.now <- paste0(sc.new.now, "\n```")
              }
            }

          } else {
            if (sc.kind.2[i] == "code_plot") {
              echo.now <- FALSE

              sc.new.now <- paste0("\n```{r message = ", rmessage,
                                   ", warning = ", rwarning,
                                   ", echo = ", echo.now, "}\n",
                                   sc.new.now)

              if(i == sc0.len){
                sc.new.now <- paste0(sc.new.now, "\n```")
              } else {
                if(sc.kind.2[i + 1] != "code_plot_add"){
                  sc.new.now <- paste0(sc.new.now, "\n```")
                }
              }

            } else {
              if (sc.kind.2[i] == "code_plot_add") {
                if(i == sc0.len){
                  sc.new.now <- paste0(sc.new.now, "\n```")
                } else {
                  if(sc.kind.2[i + 1] != "code_plot_add"){
                    sc.new.now <- paste0(sc.new.now, "\n```")
                  }
                }
              }
            }
          }
        }
      }

    }
    sc.vec[i] <- sc.new.now
  }


  if(is.null(textTitle)){
    fileName.split <- str_split(fileName, "/")[[1]]
    textTitle <- str_remove(fileName.split[length(fileName.split)],
                                      pattern = fixed(".R"))
  }
  aboutTitle <- paste0("title: ", textTitle)

  if(is.null(Author)){
    aboutAuthor <- ""
  }else{
    aboutAuthor <- paste0("\nauthor: ", Author)
  }

  if(is.null(Date)){
    aboutDate <- ""
  }else{
    aboutDate <- paste0("\ndate: ", Date)
  }

  sc.head <- paste0("---\n", aboutTitle,
                    aboutAuthor,
                    aboutDate,
                    "\noutput:",
                    "\n  html_document:",
                    "\n    df_print: paged",
                    "\n  word_document: default",
                    "\n---\n\n-----\n")


  sc.Rmd <- paste0(c(sc.head, sc.vec, "\n-----"),
                   collapse = "\n")



  if(is.null(saveFileName)){
    saveFileName <- paste0(fileName, "md")
  }

  write_file(sc.Rmd, path = saveFileName)
}
