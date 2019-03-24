package btm

import java.util.NoSuchElementException

import utest._

import scala.util.Random
import scala.collection.immutable.TreeMap

object BTreeMapJ_tests extends TestSuite
{
  override def tests = Tests{

    'random {
      val rng = new Random(1337)
      val SIZE = 32*16
      val RANGE = SIZE

      for( run <- 1 to 8 )
      {
        println( f"RUN$run%4d" )

        var map = BTreeMapJ.empty[Int,String]( implicitly[Ordering[Int]] )
        var ref =  TreeMap .empty[Int,String]

        for( _ <- 1 to SIZE )
        {
          val key   =  rng.nextInt(2*RANGE) - RANGE
          val value = (rng.nextInt(2*RANGE) - RANGE).toString
          //          println((key,value))

          map = map.updated(key, value)
          ref = ref.updated(key, value)

          for( k <- -RANGE until +RANGE )
          {
            try {
              val     v =  ref(k)
              assert( v == map(k) )
            }
            catch {
              case _: NoSuchElementException =>
                intercept[NoSuchElementException]{ map(k) }
            }
          }
        }
      }
    }

  }
}
