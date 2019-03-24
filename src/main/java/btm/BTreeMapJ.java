package btm;

import java.util.Comparator;
import java.util.NoSuchElementException;

import static java.util.Arrays.binarySearch;
import static java.lang.System.arraycopy;

public final class BTreeMapJ<K,V>
{
// STATIC FIELDS
  private static final Object[] EMPTY_ARRAY = {};

  private static final class Updated
  {
  // FIELDS
    private final Comparator<Object> order;
    public Object[] node0 = null,
                    node1 = null;

    public Object key = null,
                  val = null;

    private boolean sizeChanged = true;

  // CONSTRUCTORS
    public Updated( Object _key, Object _val, Comparator<Object> _order )
    {
      key =_key;
      val =_val;
      order = _order;
    }

  // METHODS
    public boolean wasSplit()
    {
      return null != node1;
    }

    public void update( int h, Object[] node )
    {
//      assert 0 <= h;
      final int n = 0 < h ? (node.length-2) / 3
                          : (node.length  ) / 2;
//      assert n <= nMax(h);
//      assert n >= nMin(h) || h == height;

      int i = binarySearch(node, 0,n, key, order);
      if( i >= 0 ) // KEY IN BRANCH/NODE
      {
        node0 = node.clone();
        node0[i  ] = key;
        node0[i+n] = val;
        sizeChanged = false;
      }
      else
      {
        i = ~i;
        if( 0 < h ) { // BRANCH NODE
          update(h-1, (Object[]) node[2*n+1+i]);
          if( ! wasSplit() ) { // CHILD WAS NOT SPLIT -> INSERT
            node1 = node.clone();
            node1[2*n+1+i] = node0;
            node0 = node1;
            node1 = null;
          }
          else // CHILD WAS SPLIT
          {
            if( n < nMax(h) ) // BRANCH NOT FULL -> INSERT
            {
              Object[] parent = new Object[5+3*n];
              arraycopy(node,0,       parent,  0    , i  ); parent[i      ] = key;
              arraycopy(node,i,       parent,i+1    , n  ); parent[i+1+n  ] = val;
              arraycopy(node,i+n,     parent,i+2+n  , n+1); parent[i+3+n*2] = node0;
                                                            parent[i+4+n*2] = node1;
              arraycopy(node,i+n*2+2, parent,i+5+n*2, n-i);
              node0 = parent;
              node1 = null;
            }
            else { // BRANCH FULL -> SPLIT
              assert n%2 == 0;
              final int m = n >>> 1;
              Object[] parent0 = new Object[2 + 3*m],
                       parent1 = new Object[2 + 3*m];

              if( i < m ) { // key AND value IN LEFT HALF
                arraycopy(node,0      , parent0,0      ,   i  ); parent0[i  ] = key;
                arraycopy(node,i      , parent0,i+1    , m-i-1);
                arraycopy(node,0+n    , parent0,    m  ,   i  ); parent0[i+m] = val;
                arraycopy(node,i+n    , parent0,i+1+m  , m-i-1);
                arraycopy(node,0+n*2+1, parent0,  1+m*2,   i  ); parent0[i+m*2+1] = node0;
                                                                 parent0[i+m*2+2] = node1;
                arraycopy(node,i+n*2+2, parent0,i+3+m*2, m-i-1);

                arraycopy(node,m      , parent1,0,     m  );
                arraycopy(node,m+n    , parent1,m,     m  );
                arraycopy(node,m+n*2+1, parent1,m*2+1, m+1);

                key = node[m-1  ];
                val = node[m-1+n];
              }
              else if( i > m ) { // key AND value IN RIGHT HALF
                arraycopy(node,0    , parent0,0,     m  );
                arraycopy(node,0+n  , parent0,m,     m  );
                arraycopy(node,1+n*2, parent0,m*2+1, m+1);

                arraycopy(node,  m+1  , parent1, 0    , i-m-1); parent1[i-1-m] = key;
                arraycopy(node,  i    , parent1, i-m  , n-i  );
                arraycopy(node,n+m+1  , parent1,   m  , i-m-1); parent1[i-1  ] = val;
                arraycopy(node,n+i    , parent1, i    , n-i  );
                arraycopy(node,5*m+2  , parent1, 1+m*2, i-m-1); parent1[i+  m] = node0;
                                                                parent1[i+1+m] = node1;
                arraycopy(node,4*m+2+i, parent1, i+m+2, n-i  );

                key = node[m  ];
                val = node[m+n];
              }
              else { // key AND value IN LEFT MID
                arraycopy(node,  0,     parent0,0,     m);
                arraycopy(node,  n,     parent0,m,     m);
                arraycopy(node,  n*2+1, parent0,m*2+1, m); parent0[m*3+1] = node0;
                arraycopy(node,m+0,     parent1,0,     m);
                arraycopy(node,m+n,     parent1,m,     m); parent1[m*2+1] = node1;
                arraycopy(node,m+n*2+2, parent1,m*2+2, m);
              }
              node0 = parent0;
              node1 = parent1;
            }
          }
        }
        else { // LEAF NODE
          if( n < nMax(h) ) // LEAF NOT FULL -> INSERT
          {
            node0 = new Object[2+2*n];
            arraycopy(node, 0,   node0, 0    ,   i); node0[i    ] = key;
            arraycopy(node, i,   node0, i+1  , n  ); node0[i+1+n] = val;
            arraycopy(node, i+n, node0, i+2+n, n-i);
          }
          else { // LEAF FULL -> SPLIT
            assert n%2 == 0;
            node0 = new Object[n];
            node1 = new Object[n];
            final int m = n >>> 1;
            if( i < m ) { // key AND value IN LEFT HALF
              arraycopy(node,0,   node0,  0  ,   i  ); node0[i  ] = key;
              arraycopy(node,i,   node0,i+1  , m-i-1);
              arraycopy(node,0+n, node0,    m,   i  ); node0[i+m] = val;
              arraycopy(node,i+n, node0,i+1+m, m-i-1);

              arraycopy(node,m,   node1, 0, m);
              arraycopy(node,m+n, node1, m, m);

              key = node[m-1  ];
              val = node[m-1+n];
            }
            else if( i > m ) { // key AND value IN RIGHT HALF
              arraycopy(node,0, node0,0, m);
              arraycopy(node,n, node0,m, m);

              arraycopy(node,  m+1,   node1, 0  , i-m-1); node1[i-1-m] = key;
              arraycopy(node,  i,     node1, i-m, n-i  );
              arraycopy(node,n+m+1,   node1,   m, i-m-1); node1[i-1  ] = val;
              arraycopy(node,n+i  ,   node1, i  , n-i  );

              key = node[m  ];
              val = node[m+n];
            }
            else { // key AND value IN MID (i == m)
              arraycopy(node, 0  , node0, 0, m);
              arraycopy(node,   n, node0, m, m);
              arraycopy(node, m  , node1, 0, m);
              arraycopy(node, m+n, node1, m, m);
            }
          }
        }
      }
    }
  }

// STATIC CONSTRUCTOR

// STATIC METHODS
  public static <K,V> BTreeMapJ<K,V> empty( Comparator<? super K> ordering )
  {
    return new BTreeMapJ<>(0, EMPTY_ARRAY, (Comparator<Object>) ordering);
  }
//  private static int nMin( int h ) { return nMax(h) >>> 1; }
  private static int nMax( int h ) { return 14; }
//  private static int nMax( int h ) { return 1024*1024; }

//  private static int N = 2;

// FIELDS
  private final int height;
  private final Object[] root;
  private final Comparator<Object> order;

// CONSTRUCTORS
  private BTreeMapJ( int _height, Object[] _root, Comparator<Object> _order )
  {
    assert _height >= 0;
    height = _height;
    root = _root;
    order = _order;
  }

// METHODS
  public V apply( K key )
  {
    int h = height;
    Object[] node = root;
    while(true)
    {
      final int n = 0 < h ? (node.length-2) / 3
                          : (node.length  ) / 2;
      int from = 0,
            to = n-1;
      // binary search
      while( from <= to )
      {
        final int mid = (from+to) >>> 1,
                 c = order.compare(node[mid], key);
             if( c < 0 ) from= mid + 1;
        else if( c > 0 ) to  = mid - 1;
        else return (V) node[n+mid];
      }
      if( h-- == 0 ) throw new NoSuchElementException();
      node = (Object[]) node[2*n+1 + from];
    }
  }

  public BTreeMapJ<K,V> updated( K key, V val )
  {
    Updated below = new Updated(key,val,order);
    below.update(height,root);

    return below.wasSplit()
      ? new BTreeMapJ<>(height+1, new Object[]{below.key, below.val, null, below.node0, below.node1}, order) // <- TODO compute cumulative sizes
      : new BTreeMapJ<>(height, below.node0, order);
  }
}
