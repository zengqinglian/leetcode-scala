package addOneRowtoTree

import scala.collection.mutable.ListBuffer
/*
Runtime
634 ms
Beats
100%
Memory
58.4 MB
Beats
100%
 */
object Solution {
  def addOneRow(root: TreeNode, `val`: Int, depth: Int): TreeNode = {
    if (depth == 1) {
      val newRoot = new TreeNode(`val`)
      newRoot.left = root
      return newRoot
    }

    val q = new ListBuffer[TreeNode]()
    q.addOne(root)
    var level = 1;
    while(level<depth) {
      val size = q.size;
      if(level+1<depth) {
        for (i <- 0 until size) {
          val cur = q.remove(0)
          if (cur.left != null) {
            q.addOne(cur.left)
          }
          if (cur.right != null) {
            q.addOne(cur.right)
          }
        }
      }else{
        for (i <- 0 until size) {
          val cur = q.remove(0)
          val insertLeft=new TreeNode(`val`)
          insertLeft.left = cur.left
          cur.left = insertLeft


          val insertRight = new TreeNode(`val`)
          insertRight.right = cur.right
          cur.right = insertRight

        }
      }
      level += 1
    }
    root
  }


  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }
}
