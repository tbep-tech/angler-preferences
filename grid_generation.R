library(sf)
library(dplyr)
library(leaflet)

# Reproduce the numbered grid squares in 'Fig X Grid.png' (panel b) ----------
#
# The figure is a Google basemap, which uses Web Mercator (EPSG:3857), so image
# pixels are linear in EPSG:3857 coordinates. The image is georeferenced by
# matching the pixel location of place-name labels (panel a, displayed at
# 2464 x 1497 px) to their known coordinates. A single scale is fit to both
# axes because Web Mercator pixels are square.

# place-name label centers (pixels, panel a) and known coordinates
lbls <- tibble::tribble(
  ~name,                 ~px,  ~py,    ~lon,     ~lat,
  'Clearwater',          160,  266, -82.8001, 27.9659,
  'Tampa',               870,  298, -82.4572, 27.9506,
  'St. Petersburg',      490,  740, -82.6403, 27.7676,
  'Dunedin',             184,  155, -82.7718, 28.0197,
  'Largo',               204,  379, -82.7873, 27.9095,
  'Pinellas Park',       362,  558, -82.6995, 27.8428,
  'Ruskin',              922,  848, -82.4332, 27.7209,
  'Palmetto',            630, 1323, -82.5723, 27.5214,
  'Temple Terrace',     1015,  100, -82.3893, 28.0353,
  'Seminole',            170,  564, -82.7912, 27.8398,
  'Gibsonton',          1030,  532, -82.3826, 27.8536,
  'Sun City Center',    1095,  853, -82.3518, 27.7181,
  'Parrish',             940, 1165, -82.4251, 27.5873,
  'Oldsmar',             435,  102, -82.6651, 28.0342,
  "Town 'N' Country",    617,  158, -82.5773, 28.0106,
  'Holmes Beach',        338, 1385, -82.7101, 27.4953,
  'Anna Maria',          290, 1298, -82.7334, 27.5306,
  'Safety Harbor',       375,  205, -82.6931, 27.9908,
  'Bradenton',           625, 1376, -82.5748, 27.4989,
  'Greater Carrollwood', 753,   54, -82.5148, 28.0617,
  'Belleair',            138,  335, -82.8062, 27.9359,
  'Madeira Beach',       157,  664, -82.7976, 27.7981,
  'Treasure Island',     208,  732, -82.7690, 27.7692,
  'St Pete Beach',       255,  795, -82.7412, 27.7253,
  'Bradenton Beach',     362, 1448, -82.7037, 27.4670
)

merc <- st_as_sf(lbls, coords = c('lon', 'lat'), crs = 4326) |>
  st_transform(3857) |>
  st_coordinates()
lbls$X <- merc[, 1]
lbls$Y <- merc[, 2]

# joint fit: X = x0 + s * px, Y = y0 - s * py (one scale s, in m/px)
stk <- data.frame(
  val = c(lbls$X, lbls$Y),
  pix = c(lbls$px, -lbls$py),
  isx = rep(c(1, 0), each = nrow(lbls)),
  isy = rep(c(0, 1), each = nrow(lbls))
)
mod <- lm(val ~ 0 + isx + isy + pix, data = stk)
x0 <- coef(mod)[['isx']]
y0 <- coef(mod)[['isy']]
s <- coef(mod)[['pix']]

# check residuals (m), should be within a few hundred meters
lbls$resx <- lbls$X - (x0 + s * lbls$px)
lbls$resy <- lbls$Y - (y0 - s * lbls$py)
print(lbls[, c('name', 'resx', 'resy')], n = Inf)

# grid layout from panel b ---------------------------------------------------

# panel b is offset horizontally from panel a by ~1231 px
offx <- 1231

# cell size and top-left corner of the grid (pixels, panel b)
cellpx <- 100.5
gx0 <- 1278
gy0 <- 105

# cell numbers by row, with the starting column (0-indexed) of each run
runs <- list(
  list(row = 0,  col = 3, ids = 1:2),
  list(row = 1,  col = 2, ids = 3:7),
  list(row = 2,  col = 2, ids = 8:15),
  list(row = 3,  col = 4, ids = 16:21),
  list(row = 4,  col = 0, ids = 73),
  list(row = 4,  col = 4, ids = 22:28),
  list(row = 5,  col = 1, ids = 74),
  list(row = 5,  col = 4, ids = 29:34),
  list(row = 6,  col = 1, ids = 75:76),
  list(row = 6,  col = 4, ids = 35:39),
  list(row = 7,  col = 2, ids = 40:46),
  list(row = 8,  col = 2, ids = 47:53),
  list(row = 9,  col = 2, ids = 54:58),
  list(row = 10, col = 2, ids = 59:63),
  list(row = 11, col = 2, ids = 64:67),
  list(row = 12, col = 3, ids = 68:72)
)

cells <- lapply(runs, function(r) {
  data.frame(square = r$ids, row = r$row, col = r$col + seq_along(r$ids) - 1)
}) |>
  bind_rows() |>
  arrange(square)

# cell size in projected units
cellm <- cellpx * s

# top-left of grid in EPSG:3857
X0 <- x0 + s * (gx0 - offx)
Y0 <- y0 - s * gy0

polys <- lapply(seq_len(nrow(cells)), function(i) {
  xmn <- X0 + cells$col[i] * cellm
  ymx <- Y0 - cells$row[i] * cellm
  xmx <- xmn + cellm
  ymn <- ymx - cellm
  st_polygon(list(matrix(
    c(xmn, ymn, xmx, ymn, xmx, ymx, xmn, ymx, xmn, ymn),
    ncol = 2, byrow = TRUE
  )))
})

grd <- st_sf(square = cells$square, geometry = st_sfc(polys, crs = 3857)) |>
  st_transform(4326)

# approximate ground size of each square (km), Web Mercator is scaled by 1/cos(lat)
cat('Cell side (km):', round(cellm * cos(27.8 * pi / 180) / 1000, 2), '\n')

dir.create('Data/shapefile', showWarnings = FALSE)
st_write(grd, 'Data/shapefile/grid.shp', delete_dsn = file.exists('Data/shapefile/grid.shp'), quiet = TRUE)

# leaflet map ------------------------------------------------------------------

grd <- st_read('Data/shapefile/grid.shp')

cnt <- suppressWarnings(st_centroid(grd))

leaflet() |>
  addProviderTiles('Esri.WorldTopoMap') |>
  addPolygons(data = grd, color = 'black', weight = 2, fillColor = 'steelblue', fillOpacity = 0.2) |>
  addLabelOnlyMarkers(
    data = cnt,
    label = ~as.character(square),
    labelOptions = labelOptions(
      noHide = TRUE, direction = 'center', textOnly = TRUE,
      style = list('font-weight' = 'bold', 'font-size' = '14px')
    )
  )

# function to map a continuous value to the grid squares ---------------------

# dat: data frame with the square number in the first column and a continuous
#   value in the second column, column names can be anything
# grd: grid squares as an sf object with a 'square' column
# pal: color ramp passed to leaflet::colorNumeric, either a palette name
#   (e.g., 'viridis', 'magma', 'Blues') or a vector of colors
gridmap <- function(dat, grd, pal = 'viridis') {

  valnm <- names(dat)[2]
  dat <- data.frame(square = as.numeric(dat[[1]]), val = dat[[2]])

  if (anyDuplicated(dat$square))
    stop('Square numbers in the first column must be unique')

  # squares not in dat are kept and shown as NA
  tomap <- grd |>
    st_transform(4326) |>
    mutate(square = as.numeric(square)) |>
    left_join(dat, by = 'square')

  cols <- colorNumeric(pal, domain = tomap$val, na.color = 'grey')
  cnt <- suppressWarnings(st_centroid(tomap))

  leaflet(tomap) |>
    addProviderTiles('Esri.WorldTopoMap') |>
    addPolygons(
      color = 'black', weight = 1, fillColor = ~cols(val), fillOpacity = 0.8,
      label = ~paste0('Square ', square, ': ', signif(val, 3))
    ) |>
    addLabelOnlyMarkers(
      data = cnt,
      label = ~as.character(square),
      labelOptions = labelOptions(
        noHide = TRUE, direction = 'center', textOnly = TRUE,
        style = list('font-weight' = 'bold', 'font-size' = '12px')
      )
    ) |>
    addLegend(pal = cols, values = ~val, title = valnm, opacity = 0.8)

}

# example with random values
grd <- st_read('Data/shapefile/grid.shp')
exdat <- data.frame(grid = 1:76, value = runif(76))
gridmap(exdat, grd)
gridmap(exdat, grd, pal = 'magma')
