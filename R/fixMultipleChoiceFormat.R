#' @name fixMultipleChoiceFormat
#' @title Convert Legacy Multiple Choice Formats to The Modern Strict Format
#' 
#' @description Earlier versions of REDCap allowed users to define multiple
#' choices options with a format "1" which would then be intepreted by 
#' REDCap as "1, 1" (choice, label). More recent versions of REDCap disallow
#' this shortcut.  `redcapAPI` currently recognizes and facilitates data 
#' preparation for both formats, provided that only one format is used 
#' within a field. Fields that utilize both formats simultaneously throw 
#' errors. (for example, "0, zero | 1" is a valid legacy string, but 
#' `redcapAPI` will reject it).
#' 
#' Relaxing the contraints on what `redcapAPI` recognizes as acceptable 
#' formatting has the potential to cause poorly formatted choice strings to
#' quietly cast valid data to missing values, with no indication of why. 
#' Rather than try to navigate that minefield, the user is encouraged to 
#' update their Data Dictionary using `fixMultipleChoiceFormat`. This function
#' will convert any legacy style formats to the more strict modern format.
#' The metadata can then be imported or uploaded to the project to make it
#' work with `redcapAPI`.
#' 
#' @param metadata `data.frame`, such as `rcon$metadata()`.
#' @param filename `character(1)`. A file location to which the corrected 
#'   data should be written using `write.csv`.
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' fixMultipleChoiceFormat(rcon$metadata())              
#' }
#'    
#'   
#' @export

fixMultipleChoiceFormat <- function(metadata, 
                                    filename = NULL){
  w <- which(!grepl(REGEX_MULT_CHOICE, 
                   metadata$select_choices_or_calculations) & 
               metadata$field_type %in% c("checkbox", "dropdown", "radio"))
  
  for (i in w){
    ch <- unlist(strsplit(metadata$select_choices_or_calculations[i], "\\|"))
    ch_bad <- which(!grepl(",", ch))
    ch[ch_bad] <- sprintf("%s, %s", ch[ch_bad], ch[ch_bad])
    ch <- paste0(ch, collapse = " | ")
    
    metadata$select_choices_or_calculations[i] <- ch
  }
  
  if (is.null(filename)){
    return(metadata)
  }

  write.csv(metadata, 
            file = filename, 
            row.names = FALSE)
}
