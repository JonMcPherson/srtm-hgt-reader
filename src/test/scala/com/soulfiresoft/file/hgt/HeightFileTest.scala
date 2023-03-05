package com.soulfiresoft.file.hgt

import java.nio.file.{Files, Paths}

import com.soulfiresoft.file.hgt.HeightFile._
import org.scalatest._

class HeightFileTest extends FlatSpec with Matchers with OptionValues with TryValues with AppendedClues {

  import HeightFileTest._

  "decodeSubTiles" should "read and decode HGT files" in {
    val subTiles = decodeSubTiles().tileData

    val subTileResolution = TestSubTileFactory.subTileDimension * TestSubTileFactory.subTileDimension

    subTiles should have length TestSubTileGridDimension * TestSubTileGridDimension
    for (subTile <- subTiles) subTile.heights should (have length subTileResolution or have length 0) withClue s"in ${subTile.key}"
  }

  it should "read and decode zipped HGT files" in {
    val subTiles = decodeSubTiles().tileData
    val zippedSubTiles = decodeSubTiles(s"$TestHeightFileName.zip").tileData

    zippedSubTiles.length shouldBe subTiles.length
    for ((zippedSubTile, i) <- zippedSubTiles.zipWithIndex) {
      val subTile = subTiles(i)

      zippedSubTile.key shouldBe subTile.key withClue s"at index $i"
      zippedSubTile.heights shouldBe subTile.heights withClue s"for subTile ${subTile.key}"
    }
  }

  it should "decode sub tiles with the correct location code" in {
    val HeightFile(tileKey, subTiles) = decodeSubTiles()

    val dim = TestSubTileGridDimension
    val subTileCol = tileKey.column * dim
    val subTileRow = tileKey.row * dim

    subTiles should not be empty
    subTiles.head.key shouldBe(subTileCol, subTileRow) // top left
    subTiles(dim - 1).key shouldBe(subTileCol + dim - 1, subTileRow) // top right
    subTiles(dim * 10 + 10).key shouldBe(subTileCol + 10, subTileRow + 10) // middle of top left
    subTiles(dim * dim - dim).key shouldBe(subTileCol, subTileRow + dim - 1) // bottom left
    subTiles.last.key shouldBe(subTileCol + dim - 1, subTileRow + dim - 1) // bottom right 797R2X2X+
  }

  it should "decode flat sea level sub tiles into empty heights sequence" in {
    val subTileKey = (62540, 29899) // https://plus.codes/797R7922+

    val subTile = decodeSubTiles().tileData.find(_.key == subTileKey)

    subTile.value.heights shouldBe empty
  }

  it should "decode sub tile [797R5CQ6+] as expected" in {
    val subTileKey = (62564, 29924) // https://plus.codes/797R5CQ6+

    val subTile = decodeSubTiles().tileData.find(_.key == subTileKey)

    subTile.value.heights shouldBe Seq[Short](
      0, 1, 1, 0, 0, 0, 0, 0, 0,
      1, 1, 1, 0, 0, 0, 0, 0, 0,
      1, 1, 2, 0, 0, 0, 0, 0, 0,
      3, 2, 2, 2, 0, 0, 0, 0, 0,
      5, 3, 2, 2, 0, 0, 0, 0, 0,
      6, 4, 4, 3, 2, 2, 4, 3, 0,
      9, 6, 5, 4, 3, 2, 3, 3, 1,
      9, 7, 6, 4, 3, 2, 2, 2, 3,
      10, 8, 7, 5, 3, 2, 2, 2, 3
    )
  }

  it should "exclude the extraneous last row and last column" in {
    val heightData = new Array[Byte](FileSize)

    // Set tile height values to -1 (extraneous height values remain 0)
    var i = 0
    while (i < FileSize) { // while loop is 1200ms faster than indices.filter().foreach()
      if (!ExtraneousIndices.contains(i)) heightData(i) = -1 // (-1:Byte, -1:Byte) is -1: Short
      i += 1
    }

    val subTiles = decodeSubTiles("N00E000", heightData).tileData

    subTiles should not be empty
    for (subTile <- subTiles) {
      subTile.heights should contain only -1 withClue s"in ${subTile.key}"
    }
  }

  // unlikely but handled edge case
  it should "decode non-zipped HGT files that start with zip header bytes" in {
    val heightData = new Array[Byte](FileSize)
    for ((b, i) <- Seq(0x50, 0x4B, 0x03, 0x04).zipWithIndex) heightData(i) = b.toByte

    val subTiles = decodeSubTiles("N00E000", heightData).tileData

    subTiles should not be empty
    subTiles.head.heights.slice(0, 2) shouldBe Seq(20555, 772)
    subTiles.head.heights.slice(2, 81) should contain only 0
    for (subTile <- subTiles.tail) subTile.heights shouldBe empty withClue s"in ${subTile.key}"
  }

  "decodeTile" should "read and decode HGT files" in {
    val heightFile = decodeTile()

    val heightsLength = TestTileFactory.resolution.tileDimension * TestTileFactory.resolution.tileDimension

    heightFile.tileData.heights should (have length heightsLength or have length 0) withClue s"in ${heightFile.tileKey}"
  }

  it should "read and decode zipped HGT files" in {
    val heightFile = decodeTile()
    val zippedHeightFile = decodeTile(s"$TestHeightFileName.zip")

    zippedHeightFile.tileKey shouldBe heightFile.tileKey
    zippedHeightFile.tileData shouldBe heightFile.tileData withClue s"for subTile ${heightFile.tileKey}"
  }


  private val TestTileKeyMappings = Map(
    // Coordinates -> Position
    (-24, 15) -> TilePos(156, 74),
    (0, 0) -> TilePos(180, 89),
    (-1, -1) -> TilePos(179, 90),
    // these SRTM tiles don't exist, but the TileKey is still valid
    (-180, 89) -> TilePos(0, 0),
    (179, -90) -> TilePos(359, 179)
  )

  "TileKey.apply" should "construct a TileKey with equivalent position" in {
    for ((coordinates, position) <- TestTileKeyMappings)
      TileKey(coordinates).position shouldBe position withClue s"from coordinates $coordinates"
  }

  it should "throw IllegalArgumentException for invalid tile coordinates" in {
    an[IllegalArgumentException] should be thrownBy TileKey(-181, 0)
    an[IllegalArgumentException] should be thrownBy TileKey(180, 0)
    an[IllegalArgumentException] should be thrownBy TileKey(0, -91)
    an[IllegalArgumentException] should be thrownBy TileKey(0, 90)
  }

  "TileKey.fromPosition" should "construct a TileKey with equivalent coordinates" in {
    for (((longitude, latitude), position) <- TestTileKeyMappings) {
      val tileKey = TileKey.fromPosition(position)
      tileKey.longitude shouldBe longitude withClue s"from position $position"
      tileKey.latitude shouldBe latitude withClue s"from position $position"
    }
  }

  it should "throw IllegalArgumentException for invalid position" in {
    an[IllegalArgumentException] should be thrownBy TileKey.fromPosition(-1, 0)
    an[IllegalArgumentException] should be thrownBy TileKey.fromPosition(360, 0)
    an[IllegalArgumentException] should be thrownBy TileKey.fromPosition(0, -1)
    an[IllegalArgumentException] should be thrownBy TileKey.fromPosition(0, 180)
  }

  "TileKey.parseTileCoordinates" should "parse the latitude and longitude from tile coordinate strings" in {
    TileKey.parseCoordinates("N15W024").success.value shouldBe TileKey(-24, 15)
    TileKey.parseCoordinates("N15W024.hgt").success.value shouldBe TileKey(-24, 15)

    TileKey.parseCoordinates("N00E000").success.value shouldBe TileKey(0, 0)
    TileKey.parseCoordinates("N00W001").success.value shouldBe TileKey(-1, 0)
    TileKey.parseCoordinates("S01E000").success.value shouldBe TileKey(0, -1)
    TileKey.parseCoordinates("S01W001").success.value shouldBe TileKey(-1, -1)

    TileKey.parseCoordinates("S90W180").success.value shouldBe TileKey(-180, -90)
    TileKey.parseCoordinates("N89E179").success.value shouldBe TileKey(179, 89)
  }

  it should "return Failure with IllegalArgumentException for invalid tile coordinates" in {
    TileKey.parseCoordinates("N15W24").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("INVALID").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("NXXEXXX").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("X11X111").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("S00W000").failure.exception shouldBe an[IllegalArgumentException]
    // max lat/lng is exclusive because min is inclusive S90E180
    TileKey.parseCoordinates("N90E180").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("N15 W024").failure.exception shouldBe an[IllegalArgumentException]
    TileKey.parseCoordinates("N40.7W74").failure.exception shouldBe an[IllegalArgumentException]
  }

}

object HeightFileTest extends TryValues {

  private val FileResolution = 3601 // 3601 or 1201
  private val BytesPerFileRow = FileResolution * 2
  // tile resolution excludes extraneous overlapping edges
  private val AreaResolution = FileResolution - 1
  private val FileSize = FileResolution * BytesPerFileRow

  private val ExtraneousIndices = (AreaResolution * BytesPerFileRow until FileSize).toSet ++ // last row
    (1 until FileResolution).map(_ * BytesPerFileRow).flatMap(x => Seq(x - 2, x - 1)) // last column
  assert(ExtraneousIndices.size == BytesPerFileRow * 2 - 2)


  private val TestSubTileGridDimension = 400 // = 20^3 = 3600 / 9
  private val TestHeightFileName = "N15W024.hgt"


  def loadHeightFile(hgtFile: String): Array[Byte] =
    Files.readAllBytes(Paths.get(s"src/test/resources/$hgtFile"))

  def decodeTile(fileName: String, fileData: Array[Byte]): HeightFile[Tile] =
    HeightFile.decodeTile(fileName, fileData, TestTileFactory).success.value

  def decodeTile(loadFile: String = TestHeightFileName): HeightFile[Tile] =
    decodeTile(TestHeightFileName, loadHeightFile(loadFile))

  def decodeSubTiles(fileName: String, fileData: Array[Byte]): HeightFile[Array[SubTile]] =
    HeightFile.decodeSubTiles(fileName, fileData, TestSubTileFactory).success.value

  def decodeSubTiles(loadFile: String = TestHeightFileName): HeightFile[Array[SubTile]] =
    decodeSubTiles(TestHeightFileName, loadHeightFile(loadFile))


  case class Tile(heights: Seq[Short])

  case class SubTile(key: (Int, Int), heights: Seq[Short])

  private object TestTileFactory extends TileFactory[Tile](Resolution.SRTM1) {
    override def createTile(heights: Array[Short]): Tile = Tile(heights)

    override val createTileNoData: Tile = Tile(Seq.empty)
  }

  private object TestSubTileFactory extends SubTileFactory[SubTile](Resolution.SRTM1, TestSubTileGridDimension) {
    override def createSubTile(subTileCol: Int, subTileRow: Int, heights: Array[Short]): SubTile =
      SubTile((subTileCol, subTileRow), heights)

    override def createSubTileNoData(subTileCol: Int, subTileRow: Int): SubTile =
      SubTile((subTileCol, subTileRow), Seq.empty)
  }

}
