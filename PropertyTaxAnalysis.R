# Copyright 2014 Ryan Brideau

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

fredericton.property.tax <- read.csv("Fredericton.csv", header=TRUE, stringsAsFactors=FALSE)

# Keep only the columns we need
fredericton.property.tax <- subset(fredericton.property.tax, select=c("pid", "civic_number", "street_name", "street_type", "year", "levy", "X", "Y", "property_description"))

# Combine the street names
fredericton.property.tax["Street"] <- NA
fredericton.property.tax$street <- paste(fredericton.property.tax$street_name, fredericton.property.tax$street_type, sep=" ")

# Reorder the columns
fredericton.property.tax <- fredericton.property.tax[,c("pid", "civic_number", "street", "year", "X", "Y", "property_description", "levy")]

# Load property size data and convert to hectares
fredericton.property.sizes <- read.csv("PropertyPIDSize.csv", header=TRUE, stringsAsFactors=FALSE)
fredericton.property.sizes["Area_ha"] <- fredericton.property.sizes["Area_m2"] / 10000
fredericton.property.sizes["Area_m2"] <- NULL

# Merge the two tables together by PID so we have the property size and tax
size.tax.merged <- merge(fredericton.property.tax, fredericton.property.sizes, by.x = "pid", by.y = "PID")

remove(fredericton.property.sizes)

# Calculate tax/hectare
size.tax.merged["tax_per_ha"] <- size.tax.merged["levy"] / size.tax.merged["Area_ha"]

# Separate the data by year
fred.tax <- list()
for (year in c(2011,2014)) {
  fred.tax[[toString(year)]] <- size.tax.merged[size.tax.merged$year == year,]
  fred.tax[[toString(year)]]$year <- NULL
}

remove(year)
remove(size.tax.merged)

# Combine the data back into a single dataframe
fred.tax.year.cols <- fred.tax[["2011"]]
names(fred.tax.year.cols)[names(fred.tax.year.cols) == "levy"] <- "levy2011"
names(fred.tax.year.cols)[names(fred.tax.year.cols) == "tax_per_ha"] <- "levy2011_ha"
fred.tax[["2014"]] <- fred.tax[["2014"]][,c("pid", "levy", "tax_per_ha")]
fred.tax.year.cols <- merge(fred.tax.year.cols, fred.tax[["2014"]], by="pid")
names(fred.tax.year.cols)[names(fred.tax.year.cols) == "levy"] <- "levy2014"
names(fred.tax.year.cols)[names(fred.tax.year.cols) == "tax_per_ha"] <- "levy2014_ha"

# Create new columns based on changes in tax data
fred.tax.year.cols["Change"] <- fred.tax.year.cols$levy2014 - fred.tax.year.cols$levy2011
fred.tax.year.cols["Change_per_ha"] <- fred.tax.year.cols$levy2014_ha - fred.tax.year.cols$levy2011_ha
fred.tax.year.cols["PercentDiff"] <- (fred.tax.year.cols$levy2014 - fred.tax.year.cols$levy2011) / fred.tax.year.cols$levy2011 * 100

# Create new colums based on % above/below average
levy_median = median(fred.tax.year.cols$levy2014_ha)
fred.tax.year.cols["LevyDiffAvg"] <- (fred.tax.year.cols$levy2014_ha - levy_median) / levy_median * 100

levy_change_median = median(fred.tax.year.cols$Change_per_ha)
fred.tax.year.cols["ChangeDiffAvg"] <- (fred.tax.year.cols$Change_per_ha - levy_change_median) / levy_change_median * 100

# Remove any that didn't exist in 2011
#fred.tax.year.cols <- fred.tax.year.cols[fred.tax.year.cols$levy2011 > 0,]

# Clean before export
cleaned <- fred.tax.year.cols[fred.tax.year.cols$Area_ha > 0.001,]

# Further remove any columns that aren't absolutely necessary
cleaned$X <- NULL
cleaned$Y <- NULL
cleaned$Area_ha <- NULL
cleaned$Change <- NULL
cleaned$PercentDiff <- NULL

write.csv(cleaned, file = "FrederictonPropertyTaxDiffCleanedv2.csv", row.names = FALSE)
