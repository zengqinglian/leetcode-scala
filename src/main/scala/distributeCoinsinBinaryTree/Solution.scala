package distributeCoinsinBinaryTree

object Solution {
  var count = 0
  def distributeCoins(root: TreeNode): Int = {
    count = 0

    def helper(node: TreeNode): Int = {
      if (node.left == null && node.right == null) {
        return node.value - 1
      }
      var left = 0
      if (node.left != null) {
        left = helper(node.left)
      }
      var right = 0
      if (node.right != null) {
        right = helper(node.right)
      }
      count = count + Math.abs(left) + Math.abs(right)
      left + right + node.value - 1
    }
    helper(root)
    count;
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(0)
    val left = new TreeNode(3)
    val right = new TreeNode(0)
    root.left = left
    root.right = right
    Solution.distributeCoins(root)
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
