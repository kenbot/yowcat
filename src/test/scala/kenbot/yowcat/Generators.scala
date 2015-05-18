package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._

object Generators {

  val streamSampleSize = 1000

  def fromStream[A](stream: Stream[A]): Gen[A] = 
    Gen.oneOf(stream.take(streamSampleSize).to[Vector])
}

