package circe.intro

import io.circe.Decoder
import io.circe.HCursor
import io.circe.parser
import io.circe.DecodingFailure
import io.circe.CursorOp
import io.circe.ParsingFailure
import DecoderWithPrettyErrorMessage._

trait DecoderWithPrettyErrorMessage[A] {
  def getWithPrettyErrorMessage(c: HCursor, field: String)(
      implicit decoder: Decoder[A]
  ): Decoder.Result[A]
}

object DecoderWithPrettyErrorMessage {
  implicit val stringDecoderWithPrettyErrorMessage
      : DecoderWithPrettyErrorMessage[String] =
    new DecoderWithPrettyErrorMessage[String] {
      override def getWithPrettyErrorMessage(c: HCursor, field: String)(
          implicit decoder: Decoder[String]
      ): Decoder.Result[String] =
        c.get(field)(
          decoder
            .withErrorMessage(s"$field should be a String")
        )
    }
  implicit val intDecoderWithPrettyErrorMessage
      : DecoderWithPrettyErrorMessage[Int] =
    new DecoderWithPrettyErrorMessage[Int] {
      override def getWithPrettyErrorMessage(c: HCursor, field: String)(
          implicit decoder: Decoder[Int]
      ): Decoder.Result[Int] =
        c.get(field)(
          decoder
            .withErrorMessage(s"$field should be a Int")
        )
    }
  implicit class HCursorOps(c: HCursor) {
    def getWithPrettyErrorMessage[A: DecoderWithPrettyErrorMessage: Decoder](
        field: String
    ) =
      implicitly[DecoderWithPrettyErrorMessage[A]]
        .getWithPrettyErrorMessage(c, field)
  }
}
object Main extends App {
  case class Person(name: String, age: Int)
  case class IdCard(id: Int, person: Person)
  case class ValidateError(path: String, message: String)

  def printPrettyDecodeFailure(failure: DecodingFailure): ValidateError = {
    ValidateError(CursorOp.opsToPath(failure.history), failure.message)
  }

  implicit val personDecoder: Decoder[Person] = new Decoder[Person] {
    def apply(c: HCursor): Decoder.Result[Person] =
      for {
        name <- c.getWithPrettyErrorMessage[String]("name")
        age <- c.getWithPrettyErrorMessage[Int]("age")
      } yield Person(name, age)
  }.validate(c => {
    List(("name", "name is required"), ("age", "age is required"))
      .filter(x => !c.downField(x._1).succeeded)
      .map(_._2)
  })

  implicit val idCardDecoder: Decoder[IdCard] = new Decoder[IdCard] {
    def apply(c: HCursor): Decoder.Result[IdCard] =
      for {
        id <- c.getWithPrettyErrorMessage[Int]("id")
        person <- c.get[Person]("person")
      } yield IdCard(id, person)
  }.validate(c => {
    List(("id", "id is required"), ("person", "person is required"))
      .filter(x => !c.downField(x._1).succeeded)
      .map(_._2)
  })

  val personJson = """
   {
     "name": "Job",
     "age": 18
   }
  """

  val idCardJson1 = """
  {
    "id": 18,
    "person": {
       "name": "Job",
       "age": 22
    }
  }
  """

  val idCardJson2 = """
  {
    "id": 19,
    "person": {
       "name": "Tom",
       "age": 21
    }
  }
  """

  val idCards = """
  [
  {
    "id": 19,
    "person": {
       "name": "Tom",
       "age": 21
    }
  },
    {
    "id": 18,
    "person": {
       "name": 1,
       "age": 11
    }
  }
  ]
  """

  val idCard = parser.decodeAccumulating[List[IdCard]](idCards)
  println(
    idCard.leftMap(e =>
      e.map {
        case e1: ParsingFailure  => e1
        case e1: DecodingFailure => printPrettyDecodeFailure(e1)
      }
    )
  )
}
