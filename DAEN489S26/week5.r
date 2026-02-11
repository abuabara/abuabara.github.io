### SPATIAL OPERATIONS VECTOR ###
setwd("/Users/abuabara/Downloads/")

# SLIDE 8

library(sf)
library(dplyr)
library(spData)

seine_simp = st_simplify(seine,
                         dTolerance = 2000) # 2000 m

object.size(seine)
# 18096 bytes
object.size(seine_simp)
# 9112 bytes

# SLIDE 9-10-11

us_states_simp1 = st_simplify(us_states,
                              dTolerance = 100000) # 100 km

head(us_states)

head(us_states_simp1)

# proportion of points to retain (0-1; default 0.05)
us_states_simp2 = rmapshaper::ms_simplify(us_states,
                                          keep = 0.01,
                                          keep_shapes = TRUE)

us_states_simp3 = smoothr::smooth(us_states,
                                  method = "ksmooth",
                                  smoothness = 6)

plot(us_states["total_pop_15"])
plot(us_states_simp1["total_pop_15"])
plot(us_states_simp2["total_pop_15"])
plot(us_states_simp3["total_pop_15"])

# SLIDE 19

nz_centroid = st_centroid(nz)
nz_pos = st_point_on_surface(nz)

# Warning message: st_centroid assumes attributes are constant over geometries
# Warning message: st_point_on_surface assumes attributes are constant over geometries
# the warning is reminding you that centroid/point on surface calculation is purely geometric; any associated attributes may not meaningfully reflect the point location.

head(nz)
head(nz_centroid)
class(nz)
class(nz_centroid)
st_geometry_type(nz)
unique(st_geometry_type(nz))
table(unique(st_geometry_type(nz)))
table(unique(st_geometry_type(nz_centroid)))

# Quick summary of data
# list(
#   head_nz = head(nz),
#   head_nz_centroid = head(nz_centroid),
#   class_nz = class(nz),
#   class_nz_centroid = class(nz_centroid),
#   geometry_nz = st_geometry_type(nz),
#   geometry_nz_centroid = st_geometry_type(nz_centroid),
#   unique_geometry_counts = list(
#     nz = table(unique(st_geometry_type(nz))),
#     nz_centroid = table(unique(st_geometry_type(nz_centroid)))
#   )
# )

plot(st_geometry(nz), col = "lightgray")
plot(st_geometry(nz_centroid), add = TRUE, pch = 19, col = "red")
plot(st_geometry(nz_pos), add = TRUE, pch = 18, col = "blue")

seine_centroid = st_centroid(seine)
seine_pos = st_point_on_surface(seine)

plot(st_geometry(seine), col = "black")
plot(st_geometry(seine_centroid), add = TRUE, pch = 19, col = "red")
plot(st_geometry(seine_pos), add = TRUE, pch = 18, col = "blue")

# SLIDE 21

nz_centroid = st_centroid(nz)

plot(st_geometry(nz), col = "lightgray")

plot(st_geometry(nz_centroid), add = TRUE, pch = 19, col = "red")

seine_centroid = st_centroid(seine)

plot(st_geometry(seine), col = "black")

plot(st_geometry(seine_centroid), add = TRUE, pch = 19, col = "blue")

# SLIDE 24

nz_sfc = st_geometry(nz)

# shift 100 km north (assuming projected CRS in meters)
nz_shift = nz_sfc + c(0, 100000)

plot(st_geometry(nz_sfc),
     col = "lightgray")

plot(st_geometry(nz_shift),
     col = "red", add = TRUE)

# SLIDE 25

nz_centroid_sfc = st_centroid(nz_sfc)

nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

plot(st_geometry(nz_sfc),
     col = "lightgray")

plot(st_geometry(nz_scale),
     col = "red", add = TRUE)

# SLIDE 26

rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)),
         nrow = 2,
         ncol = 2)
}

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

plot(st_geometry(nz_sfc),
     col = "lightgray")
plot(st_geometry(nz_rotate),
     col = "red", add = TRUE)

# SLIDE 28

b = st_sfc(st_point(c(0, 1)),
           st_point(c(1, 1))) # create 2 points

b = st_buffer(b, dist = 1) # convert points to circles

plot(b, border = "gray")
text(x = c(-0.5, 1.5),
     y = 1,
     labels = c("x", "y"),
     cex = 3)

# SLIDE 29

(x = b[1])
(y = b[2])

(x_and_y = st_intersection(x, y))

plot(b, border = "gray")
plot(x_and_y, col = "lightgray", border = "gray", add = TRUE) # intersecting area

# SLIDE 32

(bb = st_bbox(st_union(x, y)))
(box = st_as_sfc(bb))

set.seed(2024)

(p = st_sample(x = box, size = 10))
(p_xy1 = p[x_and_y])

plot(box, border = "gray", lty = 2)
plot(x, add = TRUE, border = "gray")
plot(y, add = TRUE, border = "gray")
plot(p, add = TRUE, cex = 3.5)
plot(p_xy1, cex = 5, col = "red", add = TRUE)
text(x = c(-0.5, 1.5),
     y = 1,
     labels = c("x", "y"),
     cex = 3)

# SLIDE 33

(bb = st_bbox(st_union(x, y)))
(box = st_as_sfc(bb))

set.seed(2024)

(p = st_sample(x = box, size = 10))

(x_and_y = st_intersection(x, y))

(p_xy2 = st_intersection(p, x_and_y))

plot(box, border = "gray", lty = 2)
plot(x, add = TRUE, border = "gray")
plot(y, add = TRUE, border = "gray")
plot(p, add = TRUE, cex = 3.5)
plot(p_xy2, cex = 5, col = "red", add = TRUE)
text(x = c(-0.5, 1.5),
     y = 1,
     labels = c("x", "y"),
     cex = 3)

# SLIDE 34

# way #1
p_xy1 = p[x_and_y]

# way #2
p_xy2 = st_intersection(p, x_and_y)

# way #3
sel_p_xy = st_intersects(p, x,
                         sparse = FALSE)[, 1] & st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]

# SLIDE 35

plot(us_states["total_pop_15"])

regions = aggregate(
  x = us_states[, "total_pop_15"],
  by = list(us_states$REGION), 
  FUN = sum,
  na.rm = TRUE)

plot(regions["Group.1"])
plot(regions["total_pop_15"])

regions2 = us_states |> 
  group_by(REGION) |>
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

plot(regions2["REGION"])
plot(regions2["pop"])

(us_west = us_states[us_states$REGION == "West", ])
(us_west_union = st_union(us_west))

plot(us_west["NAME"])
plot(st_geometry(us_west_union))

(texas = us_states[us_states$NAME == "Texas", ])
(texas_union = st_union(us_west_union, texas))

plot(st_geometry(texas), col = "maroon")
plot(st_geometry(texas_union))

# SLIDE 37

(regions = aggregate(x = us_states[, "total_pop_15"],
                     by = list(us_states$REGION), 
                     FUN = sum, na.rm = TRUE))

(regions2 = us_states |> 
    group_by(REGION) |>
    summarize(pop = sum(total_pop_15, na.rm = TRUE)))

us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)

# SLIDE 39

(multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1),
                                   ncol = 2)))

(linestring = st_cast(multipoint, "LINESTRING"))
(polyg = st_cast(multipoint, "POLYGON"))

# SLIDE 40

(multipoint_2 = st_cast(linestring, "MULTIPOINT"))
(multipoint_3 = st_cast(polyg, "MULTIPOINT"))

all.equal(multipoint, multipoint_2)
# [1] TRUE

all.equal(multipoint, multipoint_3)
# [1] TRUE
