library(EML)

unitlist <- get_unitList()
unitlist$units$description <- unitlist$units$description %>% stringr::str_replace_all("[\r\n][ ]+", "")


# ATTRIBUTES:
# possibly what we need?
# why int as string? numeric needs a unit?
attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                                               ~formatString, ~definition,      ~unit,  ~numberType,
    "run.num",    "which run number (=block). Range: 1 - 6. (integer)",                 NA,            "which run number", NA,       NA,
    "year",       "year, 2012",                                                         "YYYY",        NA,                 NA,       NA,
    "day",        "Julian day. Range: 170 - 209.",                                      "DDD",         NA,                 NA,       NA,
    "hour.min",   "hour and minute of observation. Range 1 - 2400 (integer)",           "hhmm",        NA,                 NA,       NA,
    "i.flag",     "is variable Real, Interpolated or Bad (character/factor)",           NA,            NA,                 NA,       NA,
    "variable",   "what variable being measured in what treatment (character/factor).", NA,            NA,                 NA,       NA,
    "value.i",    "value of measured variable for run.num on year/day/hour.min.",       NA,            NA,                 NA,       NA,
    "length",     "length of the species in meters (dummy example of numeric data)",    NA,            NA,                 "meter",  "real")

i.flag <- c(R = "real",
            I = "interpolated",
            B = "bad")

variable <- c(
  control  = "no prey added",
  low      = "0.125 mg prey added ml-1 d-1",
  med.low  = "0,25 mg prey added ml-1 d-1",
  med.high = "0.5 mg prey added ml-1 d-1",
  high     = "1.0 mg prey added ml-1 d-1",
  air.temp = "air temperature measured just above all plants (1 thermocouple)",
  water.temp = "water temperature measured within each pitcher",
  par       = "photosynthetic active radiation (PAR) measured just above all plants (1 sensor)"
)

value.i <- c(
  control  = "% dissolved oxygen",
  low      = "% dissolved oxygen",
  med.low  = "% dissolved oxygen",
  med.high = "% dissolved oxygen",
  high     = "% dissolved oxygen",
  air.temp = "degrees C",
  water.temp = "degrees C",
  par      = "micromoles m-1 s-1"
)

## Write these into the data.frame format
factors <- rbind(
  data.frame(
    attributeName = "i.flag",
    code = names(i.flag),
    definition = unname(i.flag)
  ),
  data.frame(
    attributeName = "variable",
    code = names(variable),
    definition = unname(variable)
  ),
  data.frame(
    attributeName = "value.i",
    code = names(value.i),
    definition = unname(value.i)
  )
)

attributeList <- set_attributes(attributes,
                                factors,
                                col_classes = c("character", "Date", "Date", "Date", "factor", "factor", "factor", "numeric"))

# PHYSICAL
# can add descriptions of the physical data file, e.g. is it tab delimited, nr of header rows, ...
physical <- set_physical("hf205-01-TPexp1.csv")

dataTable <- list(
  entityName = "hf205-01-TPexp1.csv",
  entityDescription = "tipping point experiment 1",
  physical = physical,
  attributeList = attributeList)



# COVERAGE: site?
geographicDescription <- "Harvard Forest Greenhouse, Tom Swamp Tract (Harvard Forest)"

coverage <-
  set_coverage(begin = '2012-06-01', end = '2013-12-31',
               sci_names = "Sarracenia purpurea",
               geographicDescription = geographicDescription,
               west = -122.44, east = -117.15,
               north = 37.38, south = 30.00,
               altitudeMin = 160, altitudeMaximum = 330,
               altitudeUnits = "meter")

# methods
methods_file <- system.file("examples/hf205-methods.docx", package = "EML")
methods <- set_methods(methods_file)

R_person <- person("Aaron", "Ellison", ,"fakeaddress@email.com", "cre",
                   c(ORCID = "0000-0003-4151-6081"))
aaron <- as_emld(R_person)

others <- c(as.person("Benjamin Baiser"), as.person("Jennifer Sirota"))
associatedParty <- as_emld(others)
associatedParty[[1]]$role <- "Researcher"
associatedParty[[2]]$role <- "Researcher"

HF_address <- list(
  deliveryPoint = "324 North Main Street",
  city = "Petersham",
  administrativeArea = "MA",
  postalCode = "01366",
  country = "USA")
publisher <- list(
  organizationName = "Harvard Forest",
  address = HF_address)
contact <-
  list(
    individualName = aaron$individualName,
    electronicMailAddress = aaron$electronicMailAddress,
    address = HF_address,
    organizationName = "Harvard Forest",
    phone = "000-000-0000")


keywordSet <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list("bacteria",
                   "carnivorous plants",
                   "genetics",
                   "thresholds")
  ),
  list(
    keywordThesaurus = "LTER core area",
    keyword =  list("populations", "inorganic nutrients", "disturbance")
  ),
  list(
    keywordThesaurus = "HFR default",
    keyword = list("Harvard Forest", "HFR", "LTER", "USA")
  ))




pubDate <- "2012"

title <- "Thresholds and Tipping Points in a Sarracenia
Microecosystem at Harvard Forest since 2012"

abstract <- "The primary goal of this project is to determine
  experimentally the amount of lead time required to prevent a state
change. To achieve this goal, we will (1) experimentally induce state
changes in a natural aquatic ecosystem - the Sarracenia microecosystem;
(2) use prote..."

intellectualRights = "blabla"

dataset <- list(
  title = title,
  creator = aaron,
  pubDate = pubDate,
  intellectualRights = intellectualRights,
  abstract = abstract,
  associatedParty = associatedParty,
  keywordSet = keywordSet,
  coverage = coverage,
  contact = contact,
  methods = methods,
  dataTable = dataTable)

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

write_eml(eml, "eml.xml")


# for us:
# attributes list
# maybe we have attributes list for multiple datatables (one for each data table, sample, tree, site, ...)
# maybe we provide an eml.xml with the csv if someone downloads a dataset?

# to define our own units: (if not standardUnit)
custom_units <-
  data.frame(id = "speciesPerSquareMeter",
             unitType = "arealDensity",
             parentSI = "numberPerSquareMeter",
             multiplierToSI = 1,
             description = "number of species per square meter")


unitList <- set_unitList(custom_units)

me <- list(individualName = list(givenName = "Carl", surName = "Boettiger"))
my_eml <- list(dataset = list(
  title = "A Minimal Valid EML Dataset",
  creator = me,
  contact = me),
  additionalMetadata = list(metadata = list(
    unitList = unitList
  ))
)
