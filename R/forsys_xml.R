parse_output_xml <- function() {
  setwd("C:/Users/Houtmanr/Desktop/ForSysR_2.0")

  # Load the packages required to read XML files.
  library("XML")
  library("methods")

  # Give the input file name to the function.
  result <- xmlParse(file = "Temp4test_newSPMPCP.xml")

  # Print the result.
  print(result)
  xmltop = xmlRoot(result) #gives content of root
  class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
  xmlName(xmltop) #give name of node, PubmedArticleSet
  xmlSize(xmltop) #how many children in node
  xmltop[["Objectives"]] 
}
