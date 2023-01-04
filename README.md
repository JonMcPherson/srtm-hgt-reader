# SRTM Heightmap File Reader - Scala Implementation

This is a simple but robust `.hgt` file reader/decoder implemented in Scala and compatible with Java.


### Decoding an HGT file into a single tile
```scala
case class Tile(key: (Int, Int), heights: Array[Short])

object TileFactory extends HeightFile.TileFactory[Tile] {
  override val hgtFileResolution: Resolution = Resolution.SRTM1

  override def createTile(tileCol: Int, tileRow: Int, heights: Array[Short]): Tile =
    Tile((tileCol, tileRow), heights)

  override def createTileNoData(tileCol: Int, tileRow: Int): Tile =
    createTile(tileCol, tileRow, Array.empty[Short])
}

val fileName = "N15W024.hgt"
val hgtFileData = Files.readAllBytes(Paths.get(fileName))

val tile = HeightFile.decodeTile(fileName, hgtFileData, TileFactory)
```

### Decoding an HGT file into many sub-tiles
```scala
case class SubTile(key: (Int, Int), heights: Array[Short])

object SubTileFactory extends HeightFile.SubTileFactory[SubTile] {
  override val hgtFileResolution: Resolution = Resolution.SRTM1
  override val subTileGridDimension: Int = 400 // = 20^3 = 3600 / 9

  override def createTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): SubTile =
    SubTile((subTileCol, subTileRow), heights)

  override def createTileNoData(subTileCol: Int, subTileRow: Int): SubTile =
    createTile(subTileCol, subTileRow, Array.empty[Short])
}

val fileName = "N15W024.hgt"
val hgtFileData = Files.readAllBytes(Paths.get(fileName))

val subTiles: Array[SubTile] =
  HeightFile.decodeSubTiles(fileName, hgtFileData, SubTileFactory)
```

### Supports Zipped HGT files

The first 4 bytes of the file will be checked for the zip file header and will handle decompressing it in memory.
Nothing changes with the call for zip or unzipped files. 
```scala
val fileName = "N15W024.hgt.zip"
val hgtFileData = Files.readAllBytes(Paths.get(fileName))

val subTiles: Array[SubTile] =
  HeightFile.decodeSubTiles(fileName, hgtFileData, SubTileFactory)
```

### Parse and format the HGT tile coordinates
```scala
val fileName = "N15W024.hgt.zip"
for {
  tileKey <- TileKey.parseCoordinates(fileName) // returns Try[TileKey]

  (longitude, latitude) = tileKey.coordinates // (tileKey.lng, tileKey.lat)
  (col, row) = tileKey.spatialKey // (tileKey.col, tileKey.row)
} yield {
  println(s"$longitude° lng, $latitude° lat")
  println(s"column $col, row $row")
  assert(fileName startsWith tileKey.coordinatesString)
  tileKey
}
```