package btm

import BTreeMapS._
import java.util.Arrays.binarySearch
import java.util.Comparator
import System.arraycopy
import java.util.Arrays

import scala.annotation.tailrec

/* Implementation Details
 * ----------------------
 * BTreeMap is a tree-based SortedMap implementation that uses B-Trees
 * to keep a balanced search tree of its entries.
 *
 *   - https://en.wikipedia.org/wiki/B-tree
 *   - https://www.youtube.com/watch?v=JZhdUb5F7oY
 *   - https://www.youtube.com/watch?v=TOb1tuEZ2X4
 *
 * Data Structure
 * --------------
 * For performance reasons, the B-Tree nodes are implemented as Array[Any].
 * The nodes do not contain any information about their own height, which
 *
 * A branch node with `n` key-value-pairs has the following memory layout:
 *   - [ `n`    * K         ] the keys in sorted order
 *   - [ `n`    * V         ] the values
 *   - [     1  * Array[Int]] the cumulative sizes
 *   - [(`n`+1) * Array[Any]] the child nodes
 *
 * A leaf node with `n` key-value-pairs has the following memory layout:
 *   - [ `n`    * K         ] the keys in sorted order
 *   - [ `n`    * V         ] the values
 *
 */
class BTreeMapS[K,+V] private(height: Int, root: Array[Any] )(implicit ordering: Ordering[K] )
{
  assert( height >= 0 )

  // TODO: SANITY CHECK
  {
//    println( root mkString ("[",", ","]") )
  }

  def apply( key: K ): V =
  {
    var h = height
    var node = root
    while(true)
    {
      val n = if( 0 < h ) (node.length-2) / 3
              else        (node.length  ) / 2
      var from =   0
      var   to = n-1
      // binary search
      while( from <= to )
      {
        val mid = (from+to) >>> 1
        val c = ordering compare (node(mid).asInstanceOf[K], key)
             if( c < 0 ) from= mid + 1
        else if( c > 0 ) to  = mid - 1
        else return node(n+mid).asInstanceOf[V]
      }
      if( h == 0 ) throw new NoSuchElementException()
          h -= 1
      node = node(2*n+1 + from).asInstanceOf[Array[Any]]
    }
    throw new AssertionError()
//    @tailrec def get( h: Int, node: Array[Any] ): V =
//    {
////      assert( 0 <= h )
//      val n = if( 0 < h ) (node.length-2) / 3
//              else        (node.length  ) / 2
////      assert( n <= nMax(h) )
////      assert( n >= nMin(h) || h == height )
//
//      val i = search(node, 0,n, key)(ordering.asInstanceOf[Ordering[Any]])
//      if( i >= 0 ) // key IN BRANCH/NODE
//        node(n+i).asInstanceOf[V]
//      else { // key NOT IN BRANCH/NODE
//        if( h == 0 ) throw new NoSuchElementException
//        get(h-1, node(2*n-i).asInstanceOf[Array[Any]])
//      }
//    }
//
//    get(height,root)
  }

  def updated[W >: V]( _key: K, _val: W ): BTreeMapS[K,W] =
  {
    object below
    {
      var node0,
          node1 = null: Array[Any]
      var key   = _key
      var value = _val
      var sizeChanged = true

      def wasSplit
        = node1 != null ensuring node0 != null
    }
    import below._

    def update( h: Int, node: Array[Any] ): Unit =
    {
      assert( 0 <= h )
      val n = if( 0 < h ) (node.length-2) / 3
              else        (node.length  ) / 2
      assert( n <= nMax(h) )
      assert( n >= nMin(h) || h == height )

      var i = search(node, 0,n, key)(ordering.asInstanceOf[Ordering[Any]])
      if( i >= 0 ) // KEY IN BRANCH/NODE
      {
        node0 = node.clone
        node0(i  ) = key
        node0(i+n) = value
        sizeChanged = false
      }
      else
      {
        i = ~i
        if( 0 < h ) { // BRANCH NODE
          update(h-1, node(2*n+1+i).asInstanceOf[Array[Any]])
          if( ! wasSplit ) { // CHILD WAS NOT SPLIT -> INSERT
            node1 = node.clone
            node1(2*n+1+i) = node0
                             node0 = node1
                                     node1 = null
          }
          else // CHILD WAS SPLIT
          {
            if( n < nMax(h) ) // BRANCH NOT FULL -> INSERT
            {
              val parent = new Array[Any](5+3*n)
              arraycopy(node,0,       parent,  0    , i  ); parent(i      ) = key
              arraycopy(node,i,       parent,i+1    , n  ); parent(i+1+n  ) = value
              arraycopy(node,i+n,     parent,i+2+n  , n+1); parent(i+3+n*2) = node0
                                                            parent(i+4+n*2) = node1
              arraycopy(node,i+n*2+2, parent,i+5+n*2, n-i)
              node0 = parent
              node1 = null
            }
            else { // BRANCH FULL -> SPLIT
              assert( n%2 == 0 )
              val m = n >>> 1
              val parent0,
                  parent1 = new Array[Any](2 + 3*m)

              if( i < m ) { // key AND value IN LEFT HALF
                arraycopy(node,0      , parent0,0      ,   i  ); parent0(i  ) = key
                arraycopy(node,i      , parent0,i+1    , m-i-1)
                arraycopy(node,0+n    , parent0,    m  ,   i  ); parent0(i+m) = value
                arraycopy(node,i+n    , parent0,i+1+m  , m-i-1)
                arraycopy(node,0+n*2+1, parent0,  1+m*2,   i  ); parent0(i+m*2+1) = node0
                                                                 parent0(i+m*2+2) = node1
                arraycopy(node,i+n*2+2, parent0,i+3+m*2, m-i-1)

                arraycopy(node,m      , parent1,0,     m  )
                arraycopy(node,m+n    , parent1,m,     m  )
                arraycopy(node,m+n*2+1, parent1,m*2+1, m+1)

                key   = node(m-1  ).asInstanceOf[K]
                value = node(m-1+n).asInstanceOf[W]
              }
              else if( i > m ) { // key AND value IN RIGHT HALF
                arraycopy(node,0    , parent0,0,     m  )
                arraycopy(node,0+n  , parent0,m,     m  )
                arraycopy(node,1+n*2, parent0,m*2+1, m+1)

                arraycopy(node,  m+1  , parent1, 0    , i-m-1); parent1(i-1-m) = key
                arraycopy(node,  i    , parent1, i-m  , n-i  )
                arraycopy(node,n+m+1  , parent1,   m  , i-m-1); parent1(i-1  ) = value
                arraycopy(node,n+i    , parent1, i    , n-i  )
                arraycopy(node,5*m+2  , parent1, 1+m*2, i-m-1); parent1(i+  m) = node0
                                                                parent1(i+1+m) = node1
                arraycopy(node,4*m+2+i, parent1, i+m+2, n-i  )

                key   = node(m  ).asInstanceOf[K]
                value = node(m+n).asInstanceOf[W]
              }
              else { // key AND value IN LEFT MID
                arraycopy(node,  0,     parent0,0,     m)
                arraycopy(node,  n,     parent0,m,     m)
                arraycopy(node,  n*2+1, parent0,m*2+1, m); parent0(m*3+1) = node0
                arraycopy(node,m+0,     parent1,0,     m)
                arraycopy(node,m+n,     parent1,m,     m); parent1(m*2+1) = node1
                arraycopy(node,m+n*2+2, parent1,m*2+2, m)
              }
              node0 = parent0
              node1 = parent1
            }
          }
        }
        else { // LEAF NODE
          if( n < nMax(h) ) // LEAF NOT FULL -> INSERT
          {
            node0 = new Array[Any](2+2*n)
            arraycopy(node, 0,   node0, 0    ,   i); node0(i    ) = key
            arraycopy(node, i,   node0, i+1  , n  ); node0(i+1+n) = value
            arraycopy(node, i+n, node0, i+2+n, n-i)
          }
          else { // LEAF FULL -> SPLIT
            assert( n%2 == 0 )
            node0 = new Array[Any](n)
            node1 = new Array[Any](n)
            val m = n >>> 1
            if( i < m ) { // key AND value IN LEFT HALF
              arraycopy(node,0,   node0,  0  ,   i  ); node0(i  ) = key
              arraycopy(node,i,   node0,i+1  , m-i-1)
              arraycopy(node,0+n, node0,    m,   i  ); node0(i+m) = value
              arraycopy(node,i+n, node0,i+1+m, m-i-1)

              arraycopy(node,m,   node1, 0, m)
              arraycopy(node,m+n, node1, m, m)

              key   = node(m-1  ).asInstanceOf[K]
              value = node(m-1+n).asInstanceOf[W]
            }
            else if( i > m ) { // key AND value IN RIGHT HALF
              arraycopy(node,0, node0,0, m)
              arraycopy(node,n, node0,m, m)

              arraycopy(node,  m+1,   node1, 0  , i-m-1); node1(i-1-m) = key
              arraycopy(node,  i,     node1, i-m, n-i  )
              arraycopy(node,n+m+1,   node1,   m, i-m-1); node1(i-1  ) = value
              arraycopy(node,n+i  ,   node1, i  , n-i  )

              key   = node(m  ).asInstanceOf[K]
              value = node(m+n).asInstanceOf[W]
            }
            else { // key AND value IN MID (i == m)
              arraycopy(node, 0  , node0, 0, m)
              arraycopy(node,   n, node0, m, m)
              arraycopy(node, m  , node1, 0, m)
              arraycopy(node, m+n, node1, m, m)
            }
          }
        }
      }
    }

    update(height,root)

    if( ! wasSplit )
      new BTreeMapS[K,W](height, node0)
    else
      new BTreeMapS[K,W](height+1, Array(key, value, null, node0, node1) ) // <- TODO compute cumulative sizes
  }
}
object BTreeMapS
{
  def empty[K: Ordering,V] = new BTreeMapS[K,V](0, Array.empty[Any])

  @inline private[BTreeMapS] def nMin( h: Int ): Int = nMax(h) >>> 1
  @inline private[BTreeMapS] def nMax( h: Int ): Int = 14 // ensuring { _%2 == 0 && h >= 0 }

  @inline private[BTreeMapS] def search[@specialized E](arr: Array[E], from: Int, until: Int, key: E )(implicit order: Ordering[E] ): Int =
  {
    @tailrec def binSearch( from: Int, to: Int ): Int
      = if( from > to )
           ~from
        else {
          val                             mid = (from + to) / 2
          val      c = order.compare( arr(mid), key )
               if( c < 0 ) binSearch(     mid+1,to)
          else if( c > 0 ) binSearch(from,mid-1   )
          else mid
        }
    binSearch(from,until-1)
  }
}
