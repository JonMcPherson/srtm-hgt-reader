package com.soulfiresoft.file.hgt

import java.io.{ByteArrayInputStream, IOException}
import java.util.zip.ZipInputStream

import scala.annotation.tailrec
import scala.language.higherKinds
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
     * @return the resolution of the HGT file/tile (SRTM1 or SRTM3)
     */
    def hgtFileResolution: Resolution

    private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte]): C[T]

  }

  type ID[T] = T

  /**
   * The trait for constructing a tile of type T provided the TileKey and decoded flat Array of heights for an entire HGT file.
   *
   * @tparam T the tile type that will be constructed (like a simple case class or even just `Array[Short]`)
   */
  trait TileFactory[T] extends BaseTileFactory[T, ID] {

    /**
     * Create a non-empty tile
     *
     * @param heights A flattened `Array[Short]` with square length containing the decoded height values
     * @return A non-empty tile of type T at the specified tile location
     */
    def createTile(heights: Array[Short]): T

    /**
     * Create an empty tile (when all heights values were 0 such as in the ocean)
     *
     * @return An empty tile of type T
     */
    def createTileNoData(): T

    override private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte]): T =
      decodeTile(fileData, this)

  }

  /**
   * The trait for constructing a sub-tile of type T provided the decoded flat array of heights for a sub-tile of an HGT file.
   *
   * @tparam T the tile type that will be constructed (like a simple case class)
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
     * This exists for interoperability with Java to avoid implicit ClassTag for new Array[T]
     *
     * @param size the size of the array to initialize
     * @return a new array of type T with the specified size
     */
    def initSubTilesArray(size: Int): Array[T]

    /**
     * @return the dimension of the sub-tile grid to subdivide the HGT tile dimension by
     *         (must be a factor of hgtFileResolution.tileDimension)
     */
    def subTileGridDimension: Int

    /**
     * Create a non-empty sub-tile
     *
     * @param subTileCol The global column of the sub-tile calculated as: `(hgtTile.col * subTileGridDimension) + x`
     * @param subTileRow The global row of the sub-tile calculated as: `(hgtTile.row * subTileGridDimension) + y`
     * @param heights A flattened `Array[Short]` with square length containing the decoded height values
     * @return A non-empty tile of type T at the specified tile location
     */
    def createSubTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): T

    /**
     * Create an empty sub-tile (when all heights values were 0 such as in the ocean)
     *
     * @param subTileCol The global column of the sub-tile calculated as: `(hgtTileCol * subTileGridDimension) + x`
     * @param subTileRow The global row of the sub-tile calculated as: `(hgtTileRow * subTileGridDimension) + y`
     * @return An empty tile of type T at the specified sub-tile location
     */
    def createSubTileNoData(subTileCol: Int, subTileRow: Int): T

    override private[HeightFile] def decode(tileKey: TileKey, fileData: Array[Byte]): Array[T] =
      decodeSubTiles(tileKey.column, tileKey.row, fileData, this)

  }


  /**
   * A simple case class representing the integer coordinates (longitude, latitude) of an HGT tile
   *
   * @param longitude the integer longitude (x-axis) coordinate of the HGT tile
   * @param latitude the integer latitude (y-axis) coordinate of the HGT tile
   */
  final case class TileCoords(longitude: Int, latitude: Int)

  /**
   * A simple case class representing the position (col, row) a tile (or sub-tile)
   * relative to the north west corner
   *
   * @param column the tile column
   * @param row the tile row
   */
  final case class TilePos(column: Int, row: Int)

  /**
   * A simple case class for a decoded sub-tile of of an enclosing HGT tile
   *
   * @note This uses a mutable Array for Java interoperability, but this breaks the immutability contract of the case class.
   *       Be careful not to mutate the enclosed heights Array
   * @param position the position of the sub-tile within the enclosing HGT tile
   * relative to the north west corner where (0,0) represents (-180,89)
   * @param heights the decoded height values in a flattened `Array[Short]` with square length
   */
  final case class SimpleSubTile(position: TilePos, heights: Array[Short])


  case class SimpleTileFactory(hgtFileResolution: Resolution) extends HeightFile.TileFactory[Array[Short]] {
    override def createTile(heights: Array[Short]): Array[Short] = heights
    override val createTileNoData: Array[Short] = Array.empty[Short]
  }

  case class SimpleSubTileFactory(hgtFileResolution: Resolution, override val subTileGridDimension: Int)
    extends HeightFile.SubTileFactory[SimpleSubTile] {

    override def initSubTilesArray(size: Int): Array[SimpleSubTile] = new Array(size)
    override def createSubTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): SimpleSubTile =
      SimpleSubTile(TilePos(subTileCol, subTileRow), heights)
    override def createSubTileNoData(subTileCol: Int, subTileRow: Int): SimpleSubTile =
      SimpleSubTile(TilePos(subTileCol, subTileRow), Array.empty)
  }

  /**
   * A class representing the coordinates of a full HGT tile in both longitude/latitude and column/row formats.
   *
   * Construct an instance of this class using the constructor providing a valid longitude/latitude,
   * or form the factory functions in the [[com.soulfiresoft.file.hgt.HeightFile.TileKey$ TileKey]] companion object
   *
   * @param longitude the longitudinal (horizonal axis) integer coordinate of the HGT tile
   * @param latitude the latitudinal (vertical axis) integer coordinate of the HGT tile
   * @throws java.lang.IllegalArgumentException if not in range: -180 &lt;= longitude &lt; 180, or if not in range -90 &lt;= latitude &lt; 90
   */
  final case class TileKey(longitude: Int, latitude: Int) {

    require(longitude >= -180 && longitude < 180, s"longitude not within [-180, 180); longitude=$longitude")
    require(latitude >= -90 && latitude < 90, s"latitude not within [-90, 90); latitude=$latitude")

    lazy val position: TilePos = TilePos(longitude + 180, 90 - (latitude + 1))

    /**
     * @return the column of the HGT tile (shorthand for `position.column`)
     */
    def column: Int = position.column

    /**
     * @return the row of the HGT tile (shorthand for `position.row`)
     */
    def row: Int = position.row

  }

  /**
   * The TileKey companion containing the following factory functions:
   * <ul>
   * <li>[[com.soulfiresoft.file.hgt.HeightFile.TileKey$#parseCoordinates parseCoordinates]]
   * <li>[[com.soulfiresoft.file.hgt.HeightFile.TileKey$#fromPosition fromPosition]]
   */
  object TileKey {

    def apply(coordinates: (Int, Int)): TileKey = TileKey(coordinates._1, coordinates._2)

    /**
     * Parse the HGT file coordinate string into a TileKey.<br>
     * `coordinates` string must start with the coordinates but may end with anything such as the file extension.
     *
     * @param coordinates the integer coordinates string in the HGT filename format
     * @return a Try containing the parsed TileKey or an IllegalArgumentException if the string is invalid
     */
    def parseCoordinates(coordinates: String): Try[TileKey] = Try {
      require(coordinates.length >= 7, "coordinates must be 7 characters") // allow trailing text (ex: N19W156.hgt)
      TileKey(
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
     * Construct a new TileKey from a tile position (column, row)
     * relative to the north west corner where (0,0) represents (-180,89)
     *
     * @param position of the tile (col, row) to represent as a TileKey
     * @return the TileKey for the given tile column and row
     * @throws java.lang.IllegalArgumentException if not in range: 0 &lt;= col &lt;= 360, or if not in range 0 &lt;= row &lt;= 180
     */
    def fromPosition(position: TilePos): TileKey = fromPosition(position.column, position.row)

    /**
     * Construct a new TileKey from a tile position (column, row)
     * * relative to the north west corner where (0,0) represents (-180,89)
     *
     * @param column the tile column
     * @param row the tile row
     * @return the TileKey for the given tile column and row
     * @throws java.lang.IllegalArgumentException if not in range: 0 &lt;= col &lt;= 360, or if not in range 0 &lt;= row &lt;= 180
     */
    def fromPosition(column: Int, row: Int): TileKey = {
      require(column >= 0 && column < 360, s"col not within [0, 360); column=$column")
      require(row >= 0 && row < 180, s"row not within [0, 180); row=$row")
      new TileKey(column - 180, 90 - row - 1)
    }
  }


  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile
   * as a flattened `Array[Short]` of the decoded height values with square length `resolution^2`.
   *
   * @note This returns a mutable Array for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the returned heights Array.
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param resolution the resolution of the HGT file/tile (SRTM1 or SRTM3)
   * @return a HeightTile containing the provided TileKey and the decoded tile heights
   */
  def decodeTile(hgtFileName: String, hgtFileData: Array[Byte], resolution: Resolution): Try[HeightFile[Array[Short]]] =
    decodeHeightFile[Array[Short], ID](hgtFileName, hgtFileData, SimpleTileFactory(resolution))

  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile
   * as a flattened `Array[Short]` of the decoded height values with square length `resolution^2`.
   *
   * @note This returns a mutable Array for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the returned heights Array.
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param resolution the resolution of the HGT file/tile (SRTM1 or SRTM3)
   * @return a HeightTile containing the provided TileKey and the decoded tile heights
   */
  def decodeTile(hgtTileKey: TileKey, hgtFileData: Array[Byte], resolution: Resolution): Try[HeightFile[Array[Short]]] =
    decodeHeightFile[Array[Short], ID](hgtTileKey, hgtFileData, SimpleTileFactory(resolution))

  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile of type T
   *
   * @note This provides a mutable Array for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the provided heights Array.
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param tileFactory the TileFactory instance used to create the tiles of type T in the resolution it specifies
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the provided TileKey and the decoded tile
   */
  def decodeTile[T](hgtTileKey: TileKey, hgtFileData: Array[Byte], tileFactory: TileFactory[T]): Try[HeightFile[T]] =
    decodeHeightFile[T, ID](hgtTileKey, hgtFileData, tileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) into a single 1°² tile of type T
   *
   * @note This provides a mutable Array for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the provided heights Array.
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param tileFactory the TileFactory instance used to create the tiles of type T in the resolution it specifies
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the parsed TileKey and the decoded tile
   */
  def decodeTile[T](hgtFileName: String, hgtFileData: Array[Byte], tileFactory: TileFactory[T]): Try[HeightFile[T]] =
    decodeHeightFile[T, ID](hgtFileName, hgtFileData, tileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into a flattened array of sub tiles with squared length `subTileGridDimension²`.<br>
   * Each sub-tile contains the tile position and decoded height values in a flattened `Array[Short]` with square length.
   *
   * @note This returns mutable Arrays for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile and SimpleSubTile case classes.
   *       Be careful not to mutate the enclosed sub-tiles and heights Arrays.
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param resolution the resolution of the HGT file/tile (SRTM1 or SRTM3)
   * @return a HeightTile containing the provided TileKey and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles(hgtTileKey: TileKey, hgtFileData: Array[Byte], resolution: Resolution, subTileGridDimension: Int): Try[HeightFile[Array[SimpleSubTile]]] =
    decodeHeightFile(hgtTileKey, hgtFileData, SimpleSubTileFactory(resolution, subTileGridDimension))

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into a flattened array of sub tiles with squared length `subTileGridDimension²`.<br>
   * * Each sub-tile contains the tile position and decoded height values in a flattened `Array[Short]` with square length.
   *
   * @note This returns mutable Arrays for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile and SimpleSubTile case classes.
   *       Be careful not to mutate the enclosed sub-tiles and heights Arrays.
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param resolution the resolution of the HGT file/tile (SRTM1 or SRTM3)
   * @return a HeightTile containing the provided TileKey and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles(hgtFileName: String, hgtFileData: Array[Byte], resolution: Resolution, subTileGridDimension: Int): Try[HeightFile[Array[SimpleSubTile]]] =
    decodeHeightFile(hgtFileName, hgtFileData, SimpleSubTileFactory(resolution, subTileGridDimension))

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into a flattened array of sub tiles with squared length `subTileGridDimension²`.
   *
   * @note This returns mutable Arrays for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the returned sub-tiles Array and provided heights Array.
   * @param hgtTileKey the TileKey of the HGT file defining the tile coordinates (ex: `TileKey.parseCoordinates("N47W123.hgt")`)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param subTileFactory the SubTileFactory instance used to create the sub-tiles of type T in the dimension and resolution it specifies
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the provided TileKey and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles[T](hgtTileKey: TileKey, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Try[HeightFile[Array[T]]] =
    decodeHeightFile(hgtTileKey, hgtFileData, subTileFactory)

  /**
   * Decode an HGT file (or zipped HGT file) from the 1°² tile into a flattened array of sub tiles with squared length `subTileGridDimension²`.
   *
   * @note This returns mutable Arrays for Java interoperability,
   *       but this breaks the immutability contract of the HeightFile case class.
   *       Be careful not to mutate the returned sub-tiles Array and provided heights Array.
   * @param hgtFileName the name of the HGT file defining the tile coordinates (ex: N47W123.hgt)
   * @param hgtFileData the bytes of the HGT file (or zipped HGT file) to decode
   * @param subTileFactory the SubTileFactory instance used to create the sub-tiles of type T in the dimension and resolution it specifies
   * @tparam T the type of the sub-tiles to decode into
   * @return a HeightTile containing the parsed TileKey and a square Array of the decoded sub-tiles
   */
  def decodeSubTiles[T](hgtFileName: String, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Try[HeightFile[Array[T]]] =
    decodeHeightFile(hgtFileName, hgtFileData, subTileFactory)


  private def decodeHeightFile[T, C[_]](hgtTileKey: TileKey, hgtFileData: Array[Byte], tileFactory: BaseTileFactory[T, C]): Try[HeightFile[C[T]]] =
    decodeHeightFile(s"tileKey=$hgtTileKey", Success(hgtTileKey), hgtFileData, tileFactory)

  private def decodeHeightFile[T, C[_]](hgtFileName: String, hgtFileData: Array[Byte], tileFactory: BaseTileFactory[T, C]): Try[HeightFile[C[T]]] =
    decodeHeightFile(s"name=$hgtFileName", TileKey.parseCoordinates(hgtFileName), hgtFileData, tileFactory)

  private def decodeHeightFile[T, C[_]](
    hgtFileIdentifier: => String,
    hgtTileKey: Try[TileKey],
    hgtFileData: Array[Byte],
    tileFactory: BaseTileFactory[T, C]
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

  private def decodeTile[T](hgtFileData: Array[Byte], tileFactory: TileFactory[T]): T = {
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
        val height = decodeShort(hgtFileData, i)

        tileHeights(j) = height
        if (empty && height != 0) empty = false

        i += 2
        j += 1
        x += 1
      }
      y += 1
    }

    if (empty) {
      tileFactory.createTileNoData()
    } else {
      tileFactory.createTile(tileHeights)
    }
  }

  // Uses imperative while loops for the significant performance benefits
  private def decodeSubTiles[T](tileCol: Int, tileRow: Int, hgtFileData: Array[Byte], subTileFactory: SubTileFactory[T]): Array[T] = {
    val hgtTileResolution = subTileFactory.hgtFileResolution
    val subTileDimension = subTileFactory.subTileDimension

    // 2 bytes per height value
    val rowBytes = hgtTileResolution.tileLength * 2 // ex: 3601x1 height values
    val subRowBytes = subTileDimension * 2 // ex: 9x1 height values
    val subAreaRowBytes = hgtTileResolution.tileLength * subTileDimension * 2 // ex: 3601x36 height values

    val subTileSize = subTileDimension * subTileDimension
    var subTileHeights: Array[Short] = null

    val subTileGridDimension = subTileFactory.subTileGridDimension
    val subTiles = subTileFactory.initSubTilesArray(subTileGridDimension * subTileGridDimension)

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
            val height = decodeShort(hgtFileData, i2)

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
          subTiles(j) = subTileFactory.createSubTileNoData(subTileCol, subTileRow)
          // reuse subTileHeights array
        } else {
          subTiles(j) = subTileFactory.createSubTile(subTileCol, subTileRow, subTileHeights)
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

  private def decodeShort(hgtFileData: Array[Byte], index: Int): Short =
    (((hgtFileData(index) & 0xFF) << 8) | (hgtFileData(index + 1) & 0xFF)).toShort

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
