# Eurostat utan nätverk: en liten innehållsförteckning och två tabeller.
mock_toc <- function(lang = "en") {
  rad <- function(title, code, type, h, start = " ", end = " ", values = NA) {
    data.frame(title = title, code = code, type = type, last.update.of.data = " ",
               last.table.structure.change = " ", data.start = start, data.end = end,
               values = values, hierarchy = h, stringsAsFactors = FALSE)
  }
  rbind(
    rad(paste("Themes", lang), "data", "folder", 0),
    rad("Population", "demo", "folder", 1),
    rad("Demographic balance", "demo_gind", "dataset", 2, "2022", "2024", 12),
    rad("Population on 1 January", "tps00001", "table", 3, "2023", "2024", 4),
    rad("Empty folder", "tom", "folder", 2),
    rad("Economy", "econ", "folder", 1),
    rad("GDP & main components", "nama_gdp", "dataset", 2, "2023-Q1", "2023-Q2", 8),
    rad("Demographic balance", "demo_gind", "dataset", 2, "2022", "2024", 12)
  )
}

mock_structure <- function(id, lang = "en", source = "eurostat") {
  v <- function(code, label, role = NA_character_, n) {
    data.frame(code = code, label = label, role = role, size = n, elimination = n == 0,
               stringsAsFactors = FALSE)
  }
  geo <- data.frame(code = c("EU27_2020", "SE", "NO", "SE1", "SE11"),
                    label = c("EU", paste0("Sweden ", lang), "Norway", "East", "Stockholm"),
                    stringsAsFactors = FALSE)
  freq <- data.frame(code = "A", label = "Annual", stringsAsFactors = FALSE)
  tid <- function(k) data.frame(code = k, label = k, stringsAsFactors = FALSE)
  a <- data.frame(type = c("UPDATE_DATA", "OBS_COUNT", "ESMS_HTML"),
                  title = c("2026-10-27T23:00:00+0100", "12", "Explanatory texts"),
                  text = NA_character_,
                  url = c(NA, NA, "https://ec.europa.eu/eurostat/cache/metadata/en/x.htm"),
                  stringsAsFactors = FALSE)
  if (id == "nama_gdp") {
    unit <- data.frame(code = c("MEUR", "PC"), label = c("Million euro", "Percent"), stringsAsFactors = FALSE)
    return(list(
      id = id, title = "GDP", lang = lang, annotations = a,
      variables = rbind(v("freq", "Frequency", n = 1), v("unit", "Unit", n = 2),
                        v("geo", "Geo", "geo", 2), v("time", "Time", "time", 2)),
      values = list(freq = freq, unit = unit, geo = geo[2:3, ], time = tid(c("2023-Q1", "2023-Q2")))
    ))
  }
  ind <- data.frame(code = c("JAN", "GBIRTH"), label = c("Population", "Births"), stringsAsFactors = FALSE)
  list(
    id = id, title = paste("Tabell", id), lang = lang, annotations = a,
    variables = rbind(v("freq", "Frequency", n = 1), v("indic_de", "Indicator", n = 2),
                      v("geo", "Geo", "geo", 5), v("time", "Time", "time", 3)),
    values = list(freq = freq, indic_de = ind, geo = geo, time = tid(c("2022", "2023", "2024")))
  )
}

mock_get_data <- function(id, filters = NULL, code_variables = NULL, lang = "en",
                          time_format = "raw", quiet = FALSE) {
  mock_calls$data <- c(mock_calls$data, list(list(id = id, filters = filters, lang = lang)))
  d <- data.frame(geo_code = "SE", geo = "Sweden", freq_code = "A", freq = "Annual",
                  time = "2024", values = 1, stringsAsFactors = FALSE)
  d
}

mock_calls <- new.env()

mock_backend <- function(...) {
  b <- list(
    toc = mock_toc,
    structure = mock_structure,
    get_data = mock_get_data,
    comments = function(id, lang = "en") es_structure_comments(mock_structure(id, lang))
  )
  utils::modifyList(b, list(...))
}

# Sätter upp en testServer-körning med mock-backend och tillfällig cache
local_mock_app <- function(env = parent.frame()) {
  mock_calls$data <- NULL
  old <- options(intstatgui.backend = mock_backend(),
                 intstatgui.cache_file = tempfile(fileext = ".rds"))
  withr_defer <- function() options(old)
  do.call(on.exit, list(substitute(f(), list(f = withr_defer)), add = TRUE), envir = env)
  invisible()
}

# Liten SDMX-struktur som Eurostat svarar med
mock_flow_xml <- function() {
  xml2::read_xml('<?xml version="1.0" encoding="UTF-8"?>
<m:Structure xmlns:m="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message" xmlns:s="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure" xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"><m:Structures>
<s:Dataflows><s:Dataflow id="X"><c:Annotations>
<c:Annotation><c:AnnotationTitle>2026-10-22T23:00:00+0200</c:AnnotationTitle><c:AnnotationType>UPDATE_DATA</c:AnnotationType></c:Annotation>
<c:Annotation><c:AnnotationTitle>Texts</c:AnnotationTitle><c:AnnotationType>ESMS_HTML</c:AnnotationType><c:AnnotationURL>https://ec.europa.eu/m.htm</c:AnnotationURL></c:Annotation>
<c:Annotation><c:AnnotationText xml:lang="en">Eurostat</c:AnnotationText><c:AnnotationType>SOURCE_INSTITUTIONS</c:AnnotationType></c:Annotation>
<c:Annotation><c:AnnotationTitle>&lt;a rdf:about="https://doi.org/10.2908/X"&gt;&lt;/a&gt;</c:AnnotationTitle><c:AnnotationType>DISSEMINATION_DOI_XML</c:AnnotationType></c:Annotation>
</c:Annotations><c:Name xml:lang="de">Titel</c:Name><c:Name xml:lang="en">Title</c:Name></s:Dataflow></s:Dataflows>
<s:Codelists><s:Codelist id="GEO"><s:Code id="SE"><c:Name xml:lang="en">Sweden</c:Name><c:Name xml:lang="de">Schweden</c:Name></s:Code><s:Code id="NO"><c:Name xml:lang="en">Norway</c:Name></s:Code><s:Code id="DK"><c:Name xml:lang="en">Denmark</c:Name></s:Code></s:Codelist></s:Codelists>
<s:Concepts><s:ConceptScheme id="C"><s:Concept id="geo"><c:Name xml:lang="en">Geo entity</c:Name><c:Name xml:lang="de">Geo Einheit</c:Name></s:Concept></s:ConceptScheme></s:Concepts>
<s:DataStructures><s:DataStructure id="X"><s:DataStructureComponents><s:DimensionList>
<s:TimeDimension id="TIME_PERIOD" position="2"/>
<s:Dimension id="geo" position="1"><s:LocalRepresentation><s:Enumeration><Ref id="GEO"/></s:Enumeration></s:LocalRepresentation></s:Dimension>
</s:DimensionList></s:DataStructureComponents></s:DataStructure></s:DataStructures>
</m:Structures></m:Structure>')
}

mock_constraint_xml <- function() {
  xml2::read_xml('<m:Structure xmlns:m="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message" xmlns:s="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure" xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"><s:CubeRegion include="true"><c:KeyValue id="geo"><c:Value>NO</c:Value><c:Value>SE</c:Value></c:KeyValue><c:KeyValue id="TIME_PERIOD"><c:Value>2024</c:Value><c:Value>2023</c:Value></c:KeyValue></s:CubeRegion></m:Structure>')
}
