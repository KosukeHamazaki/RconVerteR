#' Function to convert Rmd file into R file
#'
#' @param fileName The Rmd file name which you want to refer.
#' @param outFormat  The output format for the subtitle (chapter title). The default is "unknown", and
#' this will result in the simplest format (the number of "#" will always be 4 in R file).
#' The other is outFormat = 1, which returns the number of "#" in Rmd file as below.
#'
#' (the number of "#" in R file) = 8 - (the number of "#" in Rmd file)
#'
#'
#' @param Numbering  Add chapter numbers for each subtitle (chapter title) or not.
#' @param remainExplanation  Remain explanations in the original Rmd file or remove those.
#' @param textTitle  The title of the R file.
#'
#' If textTitle = NULL, the file name will be used.
#' @param addTitle  Add title part in the R file or not.
#' @param saveFileName The name of the R file to be saved.
#'
#' If saveFileName = NULL,  the file name will be used. (only file extension will change)
#'
#'
#' @return Nothing. Coverted ".R" file will be created.
#'
#' @export
#'
Rmd2R <- function(fileName, outFormat = "unknown", Numbering = TRUE,
                  remainExplanation = TRUE, textTitle = NULL,
                  addTitle = TRUE, saveFileName = NULL) {

  sc0 <- read_file(file = fileName)

  sc0.vec <- str_split(string = sc0, pattern = "\n")[[1]]
  sc0.len <- length(sc0.vec)


  menu_count <- 0
  chunk_count <- 0
  sc.kind <- rep(NA, sc0.len)
  for (i in 1:sc0.len) {

    if(sc0.vec[i] == "---") {

      menu_count <- menu_count + 1

      if(menu_count %% 2 == 1) {
        sc.kind[i] <- "menu_start"
      } else {
        sc.kind[i] <- "menu_end"
      }

    } else {

      if(str_sub(sc0.vec[i], 1, 3) == "```") {

        chunk_count <- chunk_count + 1

        if(chunk_count %% 2 == 1) {
          sc.kind[i] <- "chunk_start"
        } else {
          sc.kind[i] <- "chunk_end"
        }

      } else {

        if(menu_count %% 2 == 1){
          sc.kind[i] <- "menu"
        } else {
          if(chunk_count %% 2 == 1) {
            sc.kind[i] <- "code"
          } else {
            if(str_starts(sc0.vec[i], pattern = "#")) {
              sc.kind[i] <- "subtitle"
            } else {
              if(sc0.vec[i] == "") {
                sc.kind[i] <- "blank"
              } else {
                sc.kind[i] <- "explanation"
              }
            }
          }
        }

      }

    }

  }

  if ("subtitle" %in% sc.kind) {
    subtitle.sharp.count <- str_count(sc0.vec[sc.kind == "subtitle"],
                                      pattern = "#")
    subtitle.sharp.count.unique <- sort(unique(subtitle.sharp.count))
    subtitle.sharp.count.len <- length(subtitle.sharp.count)

    subtitle.number <- rep(NA, subtitle.sharp.count.len)
    subtitle.subnumber <- rep(NA, subtitle.sharp.count.len)
    subtitle.level <- rep(NA, subtitle.sharp.count.len)
    for (i in 1:subtitle.sharp.count.len) {
      subtitle.level[i] <-
        match(subtitle.sharp.count[i], subtitle.sharp.count.unique)


      if (subtitle.level[i] == 1) {
        subtitle.count.start <- 1
      } else {
        subtitle.count.start <-
          max(which((subtitle.sharp.count <
                       subtitle.sharp.count.unique[subtitle.level[i]])[1:i]))
      }
      subtitle.subnumber[i] <- sum((subtitle.sharp.count ==
                                      subtitle.sharp.count.unique[subtitle.level[i]])
                                   [subtitle.count.start:i])

      if (subtitle.level[i] == 1){
        subtitle.number[i] <- paste0(subtitle.subnumber[i], ".")
      } else {
        subtitle.number[i] <- paste0(subtitle.number[subtitle.count.start],
                                     subtitle.subnumber[i], ".")
      }
    }
  }

  sc.vec <- rep(NA, sc0.len)

  for(i in 1:sc0.len) {

    if (str_sub(sc.kind[i], 1, 4) == "menu") {
      sc.vec[i] <- NA
    }

    if (sc.kind[i] %in% c("chunk_start", "chunk_end")) {
      sc.vec[i] <- NA
    }

    if (sc.kind[i] %in% c("code", "blank")) {
      sc.vec[i] <- sc0.vec[i]
    }

    if (sc.kind[i] == "explanation") {
      if(remainExplanation){
        sc.vec[i] <- paste0("# ", sc0.vec[i])
      } else {
        sc.vec[i] <- NA
      }
    }

    if (sc.kind[i] == "subtitle") {
      sc.rm.sharp.now <- str_remove_all(sc0.vec[i], "#")
      if (outFormat == "unknown") {
        if (Numbering) {
          subtitle.order.now <- sum((sc.kind == "subtitle")[1:i])
          subtitle.number.now <- subtitle.number[subtitle.order.now]

          sc.vec[i] <- paste0("#### ", subtitle.number.now, " ",
                              sc.rm.sharp.now, " ####")
        } else {
          sc.vec[i] <- paste0("#### ", sc.rm.sharp.now, " ####")
        }
      } else {
        if (outFormat == 1) {
          if (Numbering) {
            subtitle.order.now <- sum((sc.kind == "subtitle")[1:i])
            subtitle.number.now <- subtitle.number[subtitle.order.now]

            sharp.count.now <- subtitle.sharp.count[subtitle.order.now]
            sharp.now <- str_c(rep("#", 8 - sharp.count.now), collapse = "")
            sc.vec[i] <- paste0(sharp.now, " ",
                                subtitle.number.now, " ",
                                sc.rm.sharp.now, " ",
                                sharp.now)
          } else {
            subtitle.order.now <- sum((sc.kind == "subtitle")[1:i])

            sharp.count.now <- subtitle.sharp.count[subtitle.order.now]
            sharp.now <- str_c(rep("#", 8 - sharp.count.now), collapse = "")
            sc.vec[i] <- paste0(sharp.now, " ",
                                sc.rm.sharp.now, " ",
                                sharp.now)
          }
        } else {
          stop("Other formats of output script are not available now !!!")
        }
      }
    }
  }

  if (is.null(textTitle)) {
    fileName.split <- str_split(fileName, "/")[[1]]
    textTitle <- str_remove(fileName.split[length(fileName.split)],
                            pattern = fixed(".Rmd"))
  }

  if(addTitle) {
    sharpTitle <- str_c(rep("#", str_count(textTitle) + 16),
                        collapse = "")

    scTitle <- paste0(sharpTitle, "\n",
                      "####### ", textTitle,
                      " #######\n", sharpTitle)

    sc.R <- paste0(c(scTitle,
                     sc.vec[!is.na(sc.vec)]),
                   collapse = "\n")

  } else {
    sc.R <- paste0(sc.vec[!is.na(sc.vec)],
                   collapse = "\n")
  }

  if(is.null(saveFileName)){
    fileTitle <- str_remove(fileName,
                            pattern = fixed(".Rmd"))

    saveFileName <- paste0(fileTitle, ".R")
  }

  write_file(sc.R, path = saveFileName)
}
