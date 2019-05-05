package mud

import collection.mutable

class BSTMap[K, V](lt: (K, K) => Boolean) extends mutable.Map[K, V] {
  import BSTMap._

  private var root: Node[K, V] = null

  def -=(key: K): this.type = {
    def removeNode(n: Node[K, V]): Node[K, V] = {
      if (n == null) null else {
        if (lt(key, n.key)) {
          n.left = removeNode(n.left)
          n
        }
        else if (lt(n.key, key)) {
          n.right = removeNode(n.right)
          n
        } else {
          if(n.left == null) n.right
          else if(n.right == null) n.left
          else {
            if (n.left.right == null) {
              n.left.right = n.right
              n.left
            } else {
              def runAndRemoveRight(runner: Node[K, V], trailer: Node[K, V]): Node[K, V] = {
                if(runner.right == null) {
                  trailer.right = runner.left
                  runner
                } else {
                  runAndRemoveRight(runner.right, runner)
                }
              }
              val replacement = runAndRemoveRight(n.left.right, n.left)
              replacement.left = n.left
              replacement.right = n.right
              replacement
            }
          }
        }
      }
    }
    root = removeNode(root)
    this
  }
  def +=(kv: (K, V)): this.type = {
    def addNode(n: Node[K, V]): Node[K, V] = {
      if (n == null) new Node[K, V](kv._1, kv._2, null, null) else {
        if (lt(kv._1, n.key)) n.left = addNode(n.left)
        else if (lt(n.key, kv._1)) n.right = addNode(n.right)
        else n.value = kv._2
        n
      }
    }
    root = addNode(root)
    this
  }

  def get(key: K): Option[V] = {
    def recur(n: Node[K, V]): Option[V] = {
      if (n == null) None else {
        if (lt(key, n.key)) recur(n.left)
        else if (lt(n.key, key)) recur(n.right)
        else Some(n.value)
      }
    }
    recur(root)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = new ListStack[Node[K, V]]()
    private def pushAllLeft(n: Node[K, V]): Unit = {
      if (n != null) {
        stack.push(n)
        pushAllLeft(n.left)
      }
    }
    pushAllLeft(root)
    def hasNext: Boolean = !stack.isEmpty
    def next(): (K, V) = {
      val ret = stack.pop()
      pushAllLeft(ret.right)
      ret.key -> ret.value
    }
  }

  def postorder(n: Node[K, V], visit: (K, V) => Unit): Unit = {
    if (n != null) {
      postorder(n.left, visit)
      postorder(n.right, visit)
      visit(n.key, n.value)
    }
  }
  def preorder(n: Node[K, V], visit: (K, V) => Unit): Unit = {
    if (n != null) {
      visit(n.key, n.value)
      preorder(n.left, visit)
      preorder(n.right, visit)
    }
  }
  def inorder(n: Node[K, V], visit: (K, V) => Unit): Unit = {
    if (n != null) {
      inorder(n.left, visit)
      visit(n.key, n.value)
      inorder(n.right, visit)
    }
  }
}

object BSTMap {
  private class Node[K, V](val key: K, var value: V, var left: Node[K, V], var right: Node[K, V])
}