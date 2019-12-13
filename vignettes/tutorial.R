<<<<<<< HEAD
=======
# first load the package
load_all()
## 2 - Data entry  (FUNCTIONS NOT CREATED YET)

>>>>>>> eec2364b1743e5f1ebee4338d8a724947f25cd43
### 3 - Data processing ###

## 3.1 Names, numbers and dates

#We first create the new columns that will receive the edited fields
occs.clean$recordedBy.new = occs.clean$recordedBy
occs.clean$recordNumber.new = occs.clean$recordNumber
occs.clean$identifiedBy.new = occs.clean$identifiedBy
occs.clean$year.new = occs.clean$year
occs.clean$dateIdentified.new = occs.clean$dateIdentified

#For the year of collection, sometimes the information is stored on the field 'eventDate' and not on the field 'year'
occs.clean$year.new[is.na(occs.clean$year.new)] = occs.clean$eventDate[is.na(occs.clean$year.new)]

#### CONFIRM HERE: in the original codes fixName() was executed after tdwgNames() ####
#We then prepare the new fields for further processing (functions `fixName()`, `colNumber()` and `getYear()`)
occs.clean$recordedBy.new = unlist(lapply(occs.clean$recordedBy.new, fixName, special.char = FALSE))
occs.clean$identifiedBy.new = unlist(lapply(occs.clean$identifiedBy.new, fixName, special.char = FALSE))
occs.clean$recordNumber.new = unlist(lapply(occs.clean$recordNumber.new, colNumber, noNumb = "s.n."))
occs.clean$year.new = unlist(lapply(occs.clean$year.new, getYear, noYear = "n.d."))
occs.clean$dateIdentified.new = unlist(lapply(occs.clean$dateIdentified.new, getYear, noYear = "n.d."))

#Next, we format the names (function `formatName()`)
occs.clean$recordedBy.new = unlist(lapply(occs.clean$recordedBy.new, formatName))
occs.clean$identifiedBy.new = unlist(lapply(occs.clean$identifiedBy.new, formatName))

#We then separate first and auxiliary names for multiple names and then convert them to the TDWG format (function `tdwgNames()`)
#To do so, we set the argument 'out' of function `tdwgNames()` to "aux" and "first" (by default `tdwgNames()` edits all names from multiple name strings).
#We also define that the symbol that will separate different names will be a semi-colon followed by a space.
occs.clean$recordedBy.aux = unlist(lapply(occs.clean$recordedBy.new, tdwgNames, out = "aux", sep.out = "; "))
occs.clean$identifiedBy.aux = unlist(lapply(occs.clean$identifiedBy.new, tdwgNames, out = "aux", sep.out = "; "))
occs.clean$recordedBy.new = unlist(lapply(occs.clean$recordedBy.new, tdwgNames, out = "first"))
occs.clean$identifiedBy.new = unlist(lapply(occs.clean$identifiedBy.new, tdwgNames, out = "first"))

#We can inspect what is the result of this separation between first and auxiliary names:
head(occs.clean[,c("recordedBy","recordedBy.new","recordedBy.aux")], 3)

#It is also useful for the validation process to standardize the notation for missing collector and identificator name
occs.clean$recordedBy.new = missName(occs.clean$recordedBy.new, type = "collector", noName = "s.n.")
occs.clean$identifiedBy.new = missName(occs.clean$identifiedBy.new, type = "identificator", noName = "s.n.")

#And to extract the last name of the first collector, stored in our edited and formated field 'recordedBy.new'
occs.clean$last.name = unlist(lapply(occs.clean$recordedBy.new, lastName))

#We can inspect what these functions are doing by comparing the original and edit columns (only the first 15 records)
head(cbind(occs$recordedBy, occs.clean$recordedBy.new, occs.clean$recordedBy.aux), n = 15)
head(cbind(occs$recordedBy, occs.clean$last.name), n = 15)
head(cbind(occs$identifiedBy, occs.clean$identifiedBy.new), n = 15)
head(cbind(occs$year, occs$eventDate, occs.clean$year.new), n = 15)
head(cbind(occs$dateIdentified, occs.clean$dateIdentified.new), n = 15)

#Note that the function handles well most formats but not all of them  (e.g. name format 'A. Custódio, Filho' or date format "Jul-02").
#Anyways, this standardization will be very useful for the search of duplicated specimens across herbaria (see below).

#Finally, note that plantR can execute all these steps at once using the wrapper function `formatOcc()`:
occs.clean1 = fixField(occs, origin = "jabot")
occs.clean1 = formatOcc(occs.clean1)

#And we can compare if the output of the two approaches are the same
table(occs.clean$recordedBy.new == occs.clean1$recordedBy.new)
table(occs.clean$identifiedBy.new == occs.clean1$identifiedBy.new)
table(occs.clean$year.new == occs.clean1$year.new)
table(occs.clean$dateIdentified.new == occs.clean1$dateIdentified.new)
table(occs.clean$last.name == occs.clean1$last.name)

## 3.2 Editing localities and match with gazetteer information: function `formatLoc()`


#3.3 Editing coordinates and defining working coordinates: function `formatCoord()` (TO BE CHECKED)
