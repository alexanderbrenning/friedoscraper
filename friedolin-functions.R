# url <- "https://friedolin.uni-jena.de/qisserver/rds?state=modulBeschrGast&moduleParameter=modDescr&struct=auswahlBaum&navigation=Y&next=tree.vm&nextdir=qispos/modulBeschr/gast&nodeID=auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018&expand=0&lastState=modulBeschrGast&asi=#auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018"
# read_module_catalog(url, filename = "BSc_Geographie_2018.xlsx")

read_module_catalog <- function(url, filename, verbose = 1) {
  x <- read_html(url)
  module_urls <- retrieve_module_links(x)
  # module_urls <- sample(module_urls, size = 5)
  modules_intermediate_pages <- module_urls %>% map(alexmisc::try_read_html)
  module_final_urls <- modules_intermediate_pages %>% map_chr(retrieve_module_link)
  modules <- module_final_urls %>% map(alexmisc::try_read_html)
  dats <- modules %>% map(read_module)
  dat <- as.data.frame(dplyr::bind_rows(dats))
  str(dat)
  if (verbose >= 1) {
    cat("\nRead the following modules:\n")
    print(dat$Modulcode)
  }
  openxlsx::write.xlsx(dat, file = filename, overwrite = TRUE)
  if (verbose >= 1) {
    cat("\nSaved data to file", filename, "\n")
    cat("\nDone!\n")
  }
  invisible(dat)
}


# url <- "https://friedolin.uni-jena.de/qisserver/rds?state=modulBeschrGast&moduleParameter=modDescr&struct=auswahlBaum&navigation=Y&next=tree.vm&nextdir=qispos/modulBeschr/gast&nodeID=auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018&expand=0&lastState=modulBeschrGast&asi=#auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018"
# x <- read_html(url)
# retrieve_module_links(x)

retrieve_module_links <- function(x, verbose = 1) {
  links <- x %>% html_nodes("a")
  link_class <- links %>% html_attr("class")
  regular <- !is.na(link_class) & (link_class == "regular")
  links <- links[regular]
  texts <- html_text2(links)
  unsel <- texts %in% c("Oberste Ebene",
                        "Erläuterungen zum Modulkatalog")
  links <- links[!unsel]
  for (stri in c("Module für Abschluss", "Studiengang")) {
    texts <- html_text2(links)
    unsel <- str_starts(texts, stri)
    links <- links[!unsel]
  }
  urls <- links %>% html_attr("href")
  if (verbose >= 1) {
    cat("Found links to", length(urls), "modules.\n")
    if (verbose >= 2) {
      texts <- html_text2(links)
      print(texts)
    }
  }
  urls
}



# Retrieve module link from this kind of web page:

# url <- "https://friedolin.uni-jena.de/qisserver/rds?state=modulBeschrGast&moduleParameter=modDescr&struct=auswahlBaum&navigation=Y&next=tree.vm&nextdir=qispos/modulBeschr/gast&nodeID=auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33804&expand=0&lastState=modulBeschrGast&asi=#auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33804"
# x <- rvest::read_html(url)
# retrieve_module_link(x)

retrieve_module_link <- function(x, verbose = 1) {
  if (verbose >= 1) {
    title <- x %>% html_nodes("span") %>% html_nodes("b") %>% html_text()
    cat("Retrieving link to module", title, "\n")
  }
  link_titles <- x %>% rvest::html_nodes("a") %>% rvest::html_attr("title")
  links <- x %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  wh <- which(link_titles == "Modulbeschreibung ansehen")
  res <- NULL
  if (length(wh) >= 1)
    res <- links[wh[1]]
  res
}


# Read module:

# url <- "https://friedolin.uni-jena.de/qisserver/rds?state=modulBeschrGast&moduleParameter=modDescr&struct=auswahlBaum&nextdir=qispos/modulBeschr/gast&next=redTree.vm&createInfoTree=Y&create=blobs&nodeID=auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33809&expand=1&lastState=modulBeschrGast&asi=#auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33809"
# url <- "https://friedolin.uni-jena.de/qisserver/rds?state=modulBeschrGast&moduleParameter=modDescr&struct=auswahlBaum&nextdir=qispos/modulBeschr/gast&next=redTree.vm&createInfoTree=Y&create=blobs&nodeID=auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33804&expand=1&lastState=modulBeschrGast&asi=#auswahlBaum%7Cabschluss%3Aabschl%3D82%7Cstudiengang%3Astg%3D050%7CstgSpecials%3Avert%3D%2Cschwp%3D%2Ckzfa%3DH%2Cpversion%3D2018%7Ckonto%3Apordnr%3D33804"
# x <- rvest::read_html(url)
# dat <- read_module(x)
# str(dat)

read_module <- function(x, verbose = 1) {
  tbl <- rvest::html_table(x)
  
  keys <- tbl[[1]]$X1
  values <- tbl[[1]]$X2
  
  keys <- c(keys, tbl[[1]]$X3)
  values <- c(values, tbl[[1]]$X4)
  
  for (i in 2:length(tbl)) {
    if (!is.na(tbl[[i]]$X1)) {
      keys <- c(keys, tbl[[i]]$X1)
      values <- c(values, tbl[[i]]$X2)
    }
  }
  
  sel <- keys != ""
  keys <- keys[sel]
  values <- values[sel]
  
  names(values) <- keys
  dat <- as.data.frame(t(values))
  
  if (verbose >= 1)
    cat("Read module", alexmisc::replace_null(dat$Modulcode), "\n")
  
  dat
}
