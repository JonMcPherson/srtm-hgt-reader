package com.soulfiresoft.file.hgt

import java.io.{ByteArrayInputStream, IOException}
import java.util.zip.ZipInputStream

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
 * A decoded SRTM heightmap `.hgt` file
 *
 * @param tileKey The TileKey (tile location and name) of the height file
 * @param tileData The decoded tile data of type T
 * @tparam T the type of the decoded height tile created by a `TileFactory[T]`
 */
final case class HeightFile[T](tileKey: HeightFile.TileKey, tileData: T)

/**
 * A utility for reading `.hgt` (and `.hgt.zip`) files from the NASA SRTM DEM dataset.
 *
 * @note The NASA Shuttle Radar Topoligy Mission (SRTM) obtained global elevation data using radar interferometry,
 *       and publicly released a Digital Elevation Model (DEM) serialized in a simple custom "HGT" file format.
 */
object HeightFile {

  private val ZipFileHeader = Seq(0x50, 0x4B, 0x03, 0x04).map(_.toByte)

  sealed class Resolution private(val tileLength: Int) extends Serializable {
    @transient val tileDimension: Int = tileLength - 1
    @transient val fileSize: Int = Math.pow(tileLength, 2).toInt * 2
  }

  object Resolution {

    // SRTM HGT files are divided into 1°² data tiles which the following resolutions:
    case object SRTM3 extends Resolution(1201) // 3-arc-second: 1200 (+ 1 overlapping col/row)
    case object SRTM1 extends Resolution(3601) // 1-arc-second: 3600 (+ 1 overlapping col/row)
  }

  sealed trait BaseTileFactory[T, C[_]] extends Serializable {

    /**
     * Create a non-empty tile
     *
     * @param tileCol The column of the tile to create
     * @param tileRow The row of the tile to create
     * @param heights A flattened array with a square length containing the decoded Short height values
     * @return A non-empty tile of type T at the specified tile location
     */
    def createTile(tileCol: Int, tileRow: Int, heights: Array[Short]): T

    /**
     * Create an empty tile (when all heights values were 0 such as in the ocean)
     *
     * @param tileCol The column of the tile to create
     * @param tileRow The row of the tile to create
     * @return An empty tile of type T at the specified tile location
     */
    def createTileNoData(tileCol: Int, tileRow: Int): T

    /**
     * @return the resolution of the HGT file/tile (SRTM1 or SRTM3)
     */
    def hgtFileResolution: Resolution

    private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte])(implicit ct: ClassTag[T]): C[T]

  }

  type ID[T] = T

  /**
   * The trait for constructing a tile of type T provided the TileKey and decoded flat array of heights for an entire HGT file.
   *
   * @tparam T the tile type that will be constructed (ideally a simple case class)
   */
  trait TileFactory[T] extends BaseTileFactory[T, ID] {

    override private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte])(implicit ct: ClassTag[T]): T =
      decodeTile(tileKey.col, tileKey.row, fileData, this)

  }

  /**
   * The trait for constructing a sub-tile of type T provided the decoded flat array of heights for a sub-tile of an HGT file.
   *
   * @tparam T the tile type that will be constructed (ideally a simple case class)
   */
  trait SubTileFactory[T] extends BaseTileFactory[T, Array] {

    /**
     * The dimension of sub tiles (along one axis).
     * Calculated as `hgtFileResolution.tileDimension / subTileGridDimension`
     */
    @transient lazy val subTileDimension: Int = {
      require(hgtFileResolution.tileDimension % subTileGridDimension == 0,
        "subTileGridDimension must be an integer factor of the tileDimension")
      hgtFileResolution.tileDimension / subTileGridDimension
    }

    /**
     * @return the dimension of the sub-tile grid to subdivide the HGT tile dimension by
     *         (must be a factor of hgtFileResolution.tileDimension)
     */
    def subTileGridDimension: Int

    override private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte])(implicit ct: ClassTag[T]): Array[T] =
      decodeSubTiles(tileKey.col, tileKey.row, fileData, this)

  }

  /**
   * A class representing the coordinates of a full HGT tile in both lng/lat and col/row formats.
   *
   * Construct an instance of this class using the factory functions in the
   * [[com.soulfiresoft.file.hgt.HeightFile.TileKey$ TileKey]] companion object
   *
   * @param lng the longitudinal (horizonal axis) integer coordinate of the HGT tile
   * @param lat the latitudinal (vertical axis) integer coordinate of the HGT tile
   * @param col the column of the HGT tile
   * @param row the row of the HGT tile
   */
  final class TileKey private(val lng: Int, val lat: Int, val col: Int, val row: Int) extends Serializable {

    /**
     * @return a tuple of the (lng, lat) integer coordinates
     */
    def coordinates: (Int, Int) = (lng, lat)

    /**
     * @return a tuple of the (col, row)
     */
    def spatialKey: (Int, Int) = (col, row)

    /**
     * Example: `N15W024`
     *
     * @see [[com.soulfiresoft.file.hgt.HeightFile.TileKey#parseCoordinates TileKey.parseCoordinates(coordinates)]]
     * @return a string representing these coordinates in the HGT filename format
     */
    def coordinatesString: String = {
      (if (lat < 0) "S" else "N") + "%02d".format(Math.abs(lat)) +
        (if (lng < 0) "W" else "E") + "%03d".format(Math.abs(lng))
    }

    override def toString: String = coordinatesString

    override def hashCode(): Int = lng * 31 + lat

    override def equals(obj: Any): Boolean = obj match {
      case other: TileKey => lng == other.lng && lat == other.lat
      case _ => false
    }
  }

  /**
   * The TileKey companion containing the following factory functions:
   * <ul>
   * <li>[[com.soulfiresoft.file.hgt.HeightFile.TileKey$#parseCoordinates parseCoordinates]]
   * <li>[[com.soulfiresoft.file.hgt.HeightFile.TileKey$#fromCoordinates fromCoordinates]]
   * <li>[[com.soulfiresoft.file.hgt.HeightFile.TileKey$#fromSpatialKey fromSpatialKey]]
   */
  object TileKey {

    /**
     * Parse the HGT file coordinate string into a TileKey.<br>
     * `coordinates` string must start with the coordinates but may end with anything such as the file extension.
     *
     * @param coordinates the integer coordinates string in the HGT filename format
     * @return a Try containing the parsed TileKey or an IllegalArgumentException if the string is invalid
     */
    def parseCoordinates(coordinates: String): Try[TileKey] = Try {
      require(coordinates.length >= 7, "coordinates must be 7 characters") // allow trailing text (ex: N19W156.hgt)
      fromCoordinates(
        (coordinates(3), coordinates.substring(4, 7).toInt) match {
          case ('E', eastLongitude) => eastLongitude
          case ('W', westLongitude) if westLongitude > 0 => -westLongitude
          case _ => throw new MatchError(coordinates.substring(3, 7))
        },
        (coordinates(0), coordinates.substring(1, 3).toInt) match {
          case ('N', northLatitude) => northLatitude
          case ('S', southLatitude) if southLatitude > 0 => -southLatitude
          case _ => throw new MatchError(coordinates.substring(0, 3))
        }
      )
    }.recover {
      case NonFatal(e) => throw new IllegalArgumentException(s"Invalid HGT tile coordinates; coordinates=$coordinates", e)
    }

    /**
     * Construct a new TileKey from a tuple of the integer coordinates
     *
     * @param key a tuple of the tile (longitude, latitude) to represent as a TileKey
     * @return the TileKey for the given tile longitude and latitude
     * @throws IllegalArgumentException if not in range: -180 &lt;= longitude &lt;= 180, or if not in range -90 &lt;= latitude &lt;= 90
     */
    def fromCoordinates(key: (Int, Int)): TileKey = fromCoordinates(key._1, key._2)

    /**
     * Construct a new TileKey from the integer coordinates
     *
     * @param lng the tile longitude
     * @param lat the tile latitude
     * @return the TileKey for the given tile longitude and latitude
     * @throws IllegalArgumentException if not in range: -180 &lt;= longitude &lt;= 180, or if not in range -90 &lt;= latitude &lt;= 90
     */
    def fromCoordinates(lng: Int, lat: Int): TileKey = {
      require(lng >= -180 && lng < 180, s"lng not within [-180, 180); lng=$lng")
      require(lat >= -90 && lat < 90, s"lat not within [-90, 90); lat=$lat")
      new TileKey(lng, lat, lng + 180, 90 - (lat + 1))
    }

    /**
     * Construct a new TileKey from a tuple of the spatial column and row
     *
     * @param key a tuple of the tile (col, row) to represent as a TileKey
     * @return the TileKey for the given tile column and row
     * @throws IllegalArgumentException if not in range: 0 &lt;= col &lt;= 360, or if not in range 0 &lt;= row &lt;= 180
     */
    def fromSpatialKey(key: (Int, Int)): TileKey = fromSpatialKey(key._1, key._2)

    /**
     * Construct a new TileKey from the spatial column and row
     *
     * @param col the tile column
     * @param row the tile row
     * @return the TileKey for the given tile column and row
     * @throws IllegalArgumentException if not in range: 0 &lt;= col &lt;= 360, or if not in range 0 &lt;= row &lt;= 180
     */
    def fromSpatialKey(col: Int, row: Int): TileKey = {
      require(col >= 0 && col < 360, s"col not within [0, 360); col=$col")
      require(row >= 0 && row < 180, s"row not within [0, 180); row=$row")
      new TileKey(col - 180, 90 - row - 1, col, row)
    }
  }
val fileName = "N15W024.hgt.zip"
for {
  tileKey <- TileKey.parseCoordinates(fileName) // returns Try[TileKey]

  (longitude, latitude) = tileKey.coordinates
  (col, row) = tileKey.spatialKey
} yield {
  println(s"$longitude° lon, $latitude° lat")
  println(s"column $col, row $row")
  assert(fileName startsWith tileKey.coordinatesString)
  tileKey
}
  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile
   *
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param tileFactory the SubTileFactory instance used to create the sub-tiles of type T in the defined dimension
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the (tileCol, tileRow) and the decoded tile
   */
  def decodeTile[T: ClassTag](hgtTileKey: TileKey, hgtFileData: Array[Byte], tileFactory: TileFactory[T]): Try[HeightFile[T]] =
    decodeHeightFile[T, ID](hgtTileKey, hgtFileData, tileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile
   *
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param tileFactory the SubTileFactory instance used to create the sub-tiles of type T in the defined dimension
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the parsed (tileCol, tileRow) and the decoded tile
   */
  def decodeTile[T: ClassTag](hgtFileName: String, hgtFileData: Array[Byte], tileFactory: TileFactory[T]): Try[HeightFile[T]] =
    decodeHeightFile[T, ID](hgtFileName, hgtFileData, tileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into sub tiles
   *
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param subTileFactory the SubTileFactory instance used to create the sub-tiles of type T in the defined dimension
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the parsed (tileCol, tileRow) and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles[T: ClassTag](hgtTileKey: TileKey, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Try[HeightFile[Array[T]]] =
    decodeHeightFile(hgtTileKey, hgtFileData, subTileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into sub tiles
   *
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param subTileFactory the SubTileFactory instance used to create the sub-tiles of type T in the defined dimension
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the parsed (tileCol, tileRow) and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles[T: ClassTag](hgtFileName: String, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Try[HeightFile[Array[T]]] =
    decodeHeightFile(hgtFileName, hgtFileData, subTileFactory)


  private def decodeHeightFile[T: ClassTag, C[_]](hgtTileKey: TileKey, hgtFileData: Array[Byte], tileFactory: BaseTileFactory[T, C]): Try[HeightFile[C[T]]] =
    decodeHeightFile(s"tileKey=$hgtTileKey", Success(hgtTileKey), hgtFileData, tileFactory)

  private def decodeHeightFile[T: ClassTag, C[_]](hgtFileName: String, hgtFileData: Array[Byte], tileFactory: BaseTileFactory[T, C]): Try[HeightFile[C[T]]] =
    decodeHeightFile(s"name=$hgtFileName", TileKey.parseCoordinates(hgtFileName), hgtFileData, tileFactory)

  private def decodeHeightFile[T: ClassTag, C[_]](
    hgtFileIdentifier: => String,
    hgtTileKey: Try[TileKey],
    hgtFileData: Array[Byte],
    tileFactory: BaseTileFactory[T, C],
  ): Try[HeightFile[C[T]]] = {

    val hgtFileSize = hgtFileData.length
    val hgtTileLength = Math.sqrt(hgtFileSize / 2).toInt

    val resolution = tileFactory.hgtFileResolution

    val maybeHgtFile = hgtTileLength == resolution.tileLength
    val maybeZipFile = hgtFileData startsWith ZipFileHeader

    def decodeHgtFile(tryTileKey: Try[TileKey], fileData: Array[Byte] = hgtFileData) =
      tryTileKey.map(k => HeightFile(k, tileFactory.decode(k, fileData)))

    def decodeZippedHgtFile() = unzipHGTFile(hgtFileData, resolution.fileSize).flatMap { case (fileName, fileData) =>
      decodeHgtFile(hgtTileKey.orElse(TileKey.parseCoordinates(fileName)), fileData)
    }

    if (maybeHgtFile && !maybeZipFile) decodeHgtFile(hgtTileKey)
    else if (maybeHgtFile && maybeZipFile) decodeZippedHgtFile() // Very unlikely edge case (but maybe possible)
      .recoverWith { case NonFatal(_) => decodeHgtFile(hgtTileKey) }
    else if (maybeZipFile) decodeZippedHgtFile()
      .recover { case NonFatal(e) => throw new IllegalArgumentException(s"Invalid zipped HGT file; $hgtFileIdentifier", e) }
    else throw new IllegalArgumentException(s"Invalid HGT file; $hgtFileIdentifier, size=$hgtFileSize")
  }

  private def decodeTile[T: ClassTag](tileCol: Int, tileRow: Int, hgtFileData: Array[Byte], tileFactory: TileFactory[T]): T = {
    val hgtTileResolution = tileFactory.hgtFileResolution
    val tileDimension = hgtTileResolution.tileDimension
    // 2 bytes per height value
    val rowBytes = hgtTileResolution.tileLength * 2 // ex: 3601x1 height values
    val tileHeights = new Array[Short](tileDimension * tileDimension)

    var y, j = 0
    var empty = true
    while (y < tileDimension) {
      var i = y * rowBytes

      var x = 0
      while (x < tileDimension) {
        val hi = hgtFileData(i)
        val lo = hgtFileData(i + 1)
        val height = (((hi & 0xFF) << 8) | (lo & 0xFF)).toShort

        tileHeights(j) = height
        if (empty && height != 0) empty = false

        i += 2
        j += 1
        x += 1
      }
      y += 1
    }

    if (empty) {
      tileFactory.createTileNoData(tileCol, tileRow)
    } else {
      tileFactory.createTile(tileCol, tileRow, tileHeights)
    }
  }

  // Uses imperative while loops for the significant performance benefits
  private def decodeSubTiles[T: ClassTag](tileCol: Int, tileRow: Int, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Array[T] = {
    val hgtTileResolution = subTileFactory.hgtFileResolution
    val subTileDimension = subTileFactory.subTileDimension

    // 2 bytes per height value
    val rowBytes = hgtTileResolution.tileLength * 2 // ex: 3601x1 height values
    val subRowBytes = subTileDimension * 2 // ex: 9x1 height values
    val subAreaRowBytes = hgtTileResolution.tileLength * subTileDimension * 2 // ex: 3601x36 height values

    val subTileSize = subTileDimension * subTileDimension
    var subTileHeights: Array[Short] = null

    val subTileGridDimension = subTileFactory.subTileGridDimension
    val subTiles = new Array[T](subTileGridDimension * subTileGridDimension)

    var y, j = 0
    while (y < subTileGridDimension) {
      var i = y * subAreaRowBytes

      var x = 0
      while (x < subTileGridDimension) {
        if (subTileHeights == null) subTileHeights = new Array[Short](subTileSize)

        var y2, j2 = 0
        var empty = true
        while (y2 < subTileDimension) {
          var i2 = i + (y2 * rowBytes)

          var x2 = 0
          while (x2 < subTileDimension) {
            val hi = hgtFileData(i2)
            val lo = hgtFileData(i2 + 1)
            val height = (((hi & 0xFF) << 8) | (lo & 0xFF)).toShort

            subTileHeights(j2) = height
            if (empty && height != 0) empty = false

            i2 += 2
            j2 += 1
            x2 += 1
          }
          y2 += 1
        }

        val subTileCol = (tileCol * subTileGridDimension) + x
        val subTileRow = (tileRow * subTileGridDimension) + y

        if (empty) {
          subTiles(j) = subTileFactory.createTileNoData(subTileCol, subTileRow)
          // reuse subTileHeights array
        } else {
          subTiles(j) = subTileFactory.createTile(subTileCol, subTileRow, subTileHeights)
          subTileHeights = null // reset with null to avoid unnecessary alloc when finished
        }

        i += subRowBytes
        j += 1
        x += 1
      }
      y += 1
    }

    subTiles
  }

  private def unzipHGTFile(zipFileData: Array[Byte], expectedFileSize: Int): Try[(String, Array[Byte])] = Try {
    val zipFile = new ZipInputStream(new ByteArrayInputStream(zipFileData))
    val hgtZipEntry = zipFile.getNextEntry
    if (hgtZipEntry == null) return Failure(new IllegalArgumentException("Empty zip file"))

    val hgtFileData = new Array[Byte](expectedFileSize)

    @tailrec // avoiding var
    def readZipEntry(offset: Int = 0): Int = {
      if (offset >= expectedFileSize) offset else {
        val bytesRead = zipFile.read(hgtFileData, offset, expectedFileSize - offset)
        if (bytesRead == -1) offset else readZipEntry(offset + bytesRead)
      }
    }

    // Cannot rely on optional ZipEntry.getSize(), so read up to expectedFileSize or EOF, and then fail if unexpected size
    val fileSize = readZipEntry()

    if (fileSize != expectedFileSize) return Failure(new IOException(s"HGT file size too small; actual=$fileSize, expected=$expectedFileSize"))
    if (zipFile.read() != -1) return Failure(new IllegalArgumentException(s"HGT File size too large; actual=${hgtZipEntry.getSize}, expected=$expectedFileSize"))
    if (zipFile.getNextEntry != null) return Failure(new IllegalArgumentException("Multiple zip entries"))

    Success((hgtZipEntry.getName, hgtFileData))
  }.recover {
    case e: Exception => Failure(new IllegalArgumentException("Invalid zip file", e))
  }.flatten

}
