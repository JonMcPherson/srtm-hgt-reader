# SRTM HGT Heightmap File Reader

This is a simple but robust `.hgt` file reader/decoder implemented in Scala, compatible with Java and JVM languages, and works well with [GeoTrellis](https://github.com/locationtech/geotrellis).

## What are SRTM HGT Files?

HGT files are the global topology heightmap tiles that NASA released to the public from their Shuttle Radar Topology Mission (SRTM) in 2015. The dataset is available in 2 resolutions; 1 arc-second (~ 30 meters) and 3 arc-second (~90 meters) which are both supported by this decoder. The tiles are rasterized with signed 16-bit short integers representing the elevation in feet above or below sea level (`0` represents sea level). The 1 arc-second tiles (SRTM1) contain 1201² values, and the 3 arc-second tiles (SRTM3) contain 3601² values where the extra 1 value is extra padding as a buffer from the adjacent east and south tiles.

You can find more details on the SRTM dataset and how to download it here:  
https://www2.jpl.nasa.gov/srtm/


## Quick Start


The `HeightFile` companion object has static functions for decoding an HGT file into the raster tile of signed short 16-bit integers.
* `HeightFile.decodeTile`
  - Decodes an entire HGT file into a single tile
* `HeightFile.decodeSubTiles`
  - Decodes an entire HGT file into many sub-tiles evenly divided by the specified factor

```scala
// Get specific HGT file name format
val fileName = "N15W024.hgt"

// Read the HGT file into memory in an Array[Byte]
// SRTM1 Resolution: about 26MB of memory (25,920,000 bytes)
// SRTM3 Resolution: about 3MB of memory (2,880,000 bytes)
val fileData = Files.readAllBytes(Paths.get(fileName))

// SRTM1 for 1-arc-second data, SRTM3 for 3-arc-second data
val resolution = HeightFile.Resolution.SRTM3

// Decode the file into a a single tile returning Try[HeightFile[Array[Short]]]
HeightFile.decodeTile(fileName, fileData, resolution).map { tile =>
  // Do something with the decoded heights
  tile.copy(
    tileData = bicubicInterpolation(tile.tileData, HeightFile.Resolution.SRTM1.tileDimension)
  )
}

// Or decode the file into 100x100 sub-tiles returning Try[HeightFile[Array[SubTile]]]
HeightFile.decodeSubTiles(fileName, fileData, resolution, 100)
```


## How to decode an HGT file into a single tile

First you will read the raw `.hgt` (or `.hgt.zip`) file into memory in an `Array[Byte]`. and provide that with the file name to the `HeightFile.decodeTile` function. The file name must be the correct format from the original dataset that specifies the tile coordinates as `[NS]<##latitude>[EW]<###longitude>` like `N15W024.hgt` where the file extension is optional. Additionally, you need to specify the `HeightFile.Resolution` as either `SRTM1` for the 1-arc-second data, or `SRTM3` as the 3-arc-second data.

The simplest `decodeTile` overload returns a `Try[HeightFile[Array[Short]]]` as a HeightFile with the decoded Short height values in a flattened Array with square length equal to squared resolution.

**Note:** This library uses mutable `Array` instead of `Seq` for Java interoperability and performance reasons, so consider implicitly converting/wrapping to `Seq` if using this from Scala.

```scala
val fileName = "N15W024.hgt"
val fileData = Files.readAllBytes(Paths.get(fileName))
val resolution = HeightFile.Resolution.SRTM1

// returns Try[HeightFile[Array[Short]]]
val tile = HeightFile.decodeTile(fileName, fileData, resolution)
```

Alternatively, you can provide your own `TileFactory` implementation to create your own Tile model in cases where you want to inject more information or modify the decoded data such as converting to a 2D array. In this case, the resolution is passed to the TileFactory.
```scala
case class Tile(heights: Seq[Seq[Short]])

object TileFactory extends HeightFile.TileFactory[Tile](Resolution.SRTM1) {
  override def createTile(heights: Array[Short]): Tile =
    Tile(heights.grouped(hgtFileResolution.tileDimension).toSeq)

  override val createTileNoData: Tile = Tile(Seq.empty)
}

val fileName = "N15W024.hgt"
val fileData = Files.readAllBytes(Paths.get(fileName))

// returns Try[HeightFile[Tile]]
val tile = HeightFile.decodeTile(fileName, fileData, TileFactory)
```

You can also use the overload providing a `TileKey` instance instead of a file name `String`. The `TileKey` represents the location of the HGT tile which is encoded by the file name. This is useful when you already parsed the file name using `TileKey.parseCoordinates(fileName)`.  
The `TileKey` case class can be constructed using the constructor `apply(longitude: Int, latitude: Int): TileKey` or from these static factory functions:
- `parseCoordinates(coordinates: String): Try[TileKey]`
- `fromPosition(column: Int, row: Int): TileKey`
  - The position is the grid coorinates ranging from `x[0, 360)`  and `y[0, 180)` rather than cartesian coordinates ranging from `lng[-180, 180)` and `lat[-90, 90)`

```scala
val fileName = "N15W024.hgt.zip"
// parseCoordinates returns Try[TileKey]
val heightFile = TileKey.parseCoordinates(fileName).flatMap { tileKey =>
  assert(tileKey == TileKey(24, 15))
  assert(tileKey == TileKey.fromPosition(204, 76)) // (lng + 180, 90 - (lat + 1))
  assert(tileKey == TileKey(tileKey.lng, tileKey.lat))
  assert(tileKey.position == (tileKey.col, tileKey.row))

  assert(fileName startsWith tileKey.coordinatesString)

  HeightFile.decodeTile(tileKey, fileData, Resolution.SRTM1)
}
```

## How to decode an HGT file into many sub-tiles

In this case, you will need to implement the `SubTileFactory` (or `JavaSubTileFactory`) trait to create your sub-tile like with a simple case class. 
```scala
case class SubTile(col: Int, row: Int, heights: Seq[Seq[Short]])
```
Your `SubTileFactory` will need to specify the file resolution (like in `TileFactory`), and additionally the sub-tile grid dimension which must be a factor of the file resolution to be evenly subdivided.  
This example is subdividing the 1 arc-second tiles of 3600² values into 10² tiles containing 36² values.
```scala
val subTileGridDimension: Int = 100 // == 10^2 == 3600 / 36

object SubTileFactory
  extends HeightFile.SubTileFactory[SubTile](Resolution.SRTM1, subTileGridDimension) {

  override def createSubTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): SubTile =
    SubTile(subTileCol, subTileRow, heights.grouped(hgtFileResolution.subTileDimension).toSeq)

  override def createSubTileNoData(subTileCol: Int, subTileRow: Int): SubTile =
    SubTile(subTileCol, subTileRow, Array.empty)
}
```
**Note:** If you are using this library from Java, you will want to implement `JavaSubTileFactory` instead of `SubTileFactory` because the latter requires an implicit `ClassTag[T]` to construct the array, wheras the former requires the Java `Class<T>` type token.

Just like with decoding into a single-tile, the raw `.hgt` (or `.hgt.zip`) file is read into memory in an `Array[Byte]` and then provided with the file name (or TileKey) and your SubTileFactory to the `HeightFile.decodeSubTiles` functions
```scala
val fileName = "N15W024.hgt"
val fileData = Files.readAllBytes(Paths.get(fileName))

// returns Try[HeightFile[Array[SubTile]]]
val subTiles = HeightFile.decodeSubTiles(fileName, fileData, SubTileFactory)
```

## Zipped HGT files

The HGT files downloaded from `nasa.gov` will be zipped, so this checks the first 4 bytes of the file data for the zip file header and will handle decompressing it in memory.
Nothing changes with the call for zip or unzipped files. 
```scala
val fileName = "N15W024.hgt.zip"
val hgtFileData = Files.readAllBytes(Paths.get(fileName))

val subTiles: Array[SubTile] =
  HeightFile.decodeSubTiles(fileName, hgtFileData, SubTileFactory)
```

## Parallelising with Apache Spark and GeoTrellis

Distributed computing is a good choice for processing this large dataset, and Apache Spark with the GeoTrellis raster data Spark library is the perfect tool for the job.

Here is a complete example paralellising the ingestion of the HGT files from an S3 bucket and then using the GeoTrellis Spark library to create a `TileLayerRDD` to do complex operations on or transformations of this large raster data.

```scala
val hgtDirectoryUri = "s3://some-bucket/path/to/hgt/files/"
val subTileGridDimension = 100 // Subdivide into 100x100 sub-tiles
val subTileFactory = new HeightFile.SubTileFactory[(SpatialKey, Tile)](HeightFile.Resolution.SRTM1, subTileGridDimension) {

  override def createSubTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): (SpatialKey, Tile) = (
    SpatialKey(subTileCol, subTileRow),
    ShortConstantNoDataArrayTile(heights, subTileDimension, subTileDimension)
  )

  override def createSubTileNoData(subTileCol: Int, subTileRow: Int): (SpatialKey, Tile) = (
    SpatialKey(subTileCol, subTileRow),
    ShortConstantTile(ShortConstantNoDataCellType.noDataValue, subTileDimension, subTileDimension)
  )
}

// Paralellize over the files and read/decode them into sub-tiles
val hgtTiles: RDD[HeightFile[Array[(SpatialKey, Tile)]]] =
  spark.sparkContext.binaryFiles(hgtDirectoryUri).flatMap {
    case (hgtFilePath, hgtFileBytes) =>
      val hgtFile = hgtFilePath.substring(hgtFilePath.lastIndexOf('/') + 1)

      TileKey.parseCoordinates(hgtFile).map { tileKey =>
        HeightFile.decodeSubTiles(
          tileKey,
          hgtFileBytes.toArray,
          subTileFactory
        ).get // terminate on decode errors
      }.toOption // skip files with invalid name or unbounded coordinates
  }.cache()

// Flatten the RDD of Arrays of keyed sub-tiles
val subTiles: RDD[(SpatialKey, Tile)] = hgtTiles.flatMap(_.tileData)

// Calculate the Extent and KeyBounds of the data
val (dataExtent, dataKeyBounds) = hgtTiles map { case HeightFile(tileKey, _) =>
  val minSubTileCol = tileKey.col * subTileGridDimension
  val minSubTileRow = tileKey.row * subTileGridDimension
  val tileExtent = Extent(tileKey.lng, tileKey.lat, tileKey.lng + 1, tileKey.lat + 1)
  val tileBounds = KeyBounds(
    SpatialKey(minSubTileCol, minSubTileRow),
    SpatialKey(minSubTileCol + subTileGridDimension - 1, minSubTileRow + subTileGridDimension - 1)
  )

  (tileExtent, tileBounds)
} reduce { (a, b) => (a._1 combine b._1, a._2 combine b._2) }

// Construct the TileLayerMetadata of this raster data
val tileLayerMetadata = TileLayerMetadata(
  cellType = ShortCellType,
  layout = LayoutDefinition(
    LatLng.worldExtent,
    TileLayout(
      layoutCols = subTileGridDimension * 360,
      layoutRows = subTileGridDimension * 180,
      tileCols = subTileFactory.subTileDimension,
      tileRows = subTileFactory.subTileDimension
    )
  ),
  extent = dataExtent,
  crs = LatLng,
  bounds = dataKeyBounds
)

// Construct the TileLayerRDD to do some cool stuff like reprojecting a higher resolution
val heightTiles = TileLayerRDD(subTiles, tileLayerMetadata)

val baseTileLayout = tileLayerMetadata.layout.tileLayout
val (targetZoom, reprojectedHeightTiles) = heightTiles.reproject(
  tileLayerMetadata.crs,
  tileLayerMetadata.layout.copy(
    tileLayout = baseTileLayout.copy(
      tileCols = baseTileLayout.tileCols * 2,
      tileRows = baseTileLayout.tileCols * 2
    )
  ),
  bufferSize = 1,
  Reproject.Options(ResampleMethods.CubcSpline)
)
```