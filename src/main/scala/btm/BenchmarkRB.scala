package btm

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.immutable._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 16)
@Measurement(iterations = 16)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BenchmarkRB
{
//  @Param(Array("1", "10", "100", "1000", "10000"))
//  var SIZE: Int = _

  val rng = new Random(1337)

  var mapRB = TreeMap.empty[Int,String]
  val keys = ArrayBuffer.empty[Int]
  val vals = ArrayBuffer.empty[String]

  @Setup(Level.Trial) def init: Unit = {
    val SIZE = 1337
    val RANGE = SIZE*4

    for( _ <- 1 to SIZE )
    {
      val k =  rng.nextInt(2*RANGE) - RANGE
      val v = (rng.nextInt(2*RANGE) - RANGE).toString
      mapRB = mapRB updated (k,v)
      keys += k
      vals += v
    }
    // shuffle
    val shuffled = rng shuffle keys
    keys.clear()
    keys ++= shuffled
  }

  @Benchmark
  def update( bh: Blackhole ): Unit =
  {
    var map = TreeMap.empty[Int,String]

    var    i = keys.length
    while( i > 0 ) {
      i-= 1
      map = map updated ( keys(i), vals(i) )
    }

    bh consume map
  }

  @Benchmark
  def apply( bh: Blackhole ): Unit =
  {
    var    i = keys.length
    while( i > 0 ) {
           i-= 1
      bh consume mapRB( keys(i) )
    }
  }
}
