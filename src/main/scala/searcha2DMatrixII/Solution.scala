package searcha2DMatrixII
/*
Runtime
588 ms
Beats
50%
Memory
65.2 MB
Beats
33.33%
 */
object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    val row_start_index = 0
    val row_end_index = matrix.length-1
    val col_start_index = 0
    val col_end_index = matrix(0).length-1

    def helper(row_start_index:Int, row_end_index:Int, col_start_index:Int, col_end_index:Int) : Boolean = {
      if (row_start_index == row_end_index && col_start_index == col_end_index) {
        return target == matrix(row_start_index)(col_end_index)
      }
      if (row_start_index == row_end_index && col_start_index+1 == col_end_index) {
        return target == matrix(row_start_index)(col_start_index)  || target == matrix(row_start_index)(col_end_index)
      }
      if (row_start_index+1 == row_end_index && col_start_index == col_end_index) {
        return target == matrix(row_start_index)(col_end_index) || target == matrix(row_start_index+1)(col_end_index)
      }
      if (row_start_index+1==row_end_index && col_start_index+1==col_end_index) {
        return (matrix(row_start_index)(col_start_index) == target ) ||
          (matrix(row_end_index)(col_start_index) == target) ||
          (matrix(row_start_index)(col_end_index) == target ) ||
          (matrix(row_end_index)(col_end_index) == target)
      }
      val row_mid_index = (row_end_index + row_start_index) /2
      val col_mid_index = (col_end_index + col_start_index) /2
      if (matrix(row_mid_index)(col_mid_index) == target) {
        true
      }else{
        if(matrix(row_mid_index)(col_mid_index) > target) {
          if (helper(row_start_index, row_mid_index, col_start_index, col_mid_index)) {
            return true
          }
          var res2 = false;
          if (row_mid_index + 1 <= row_end_index && col_mid_index - 1 >= col_start_index) {
            res2 = helper(row_mid_index + 1, row_end_index, col_start_index, col_mid_index - 1)
          }
          if (res2) {
            return true;
          }
          var res3 = false;
          if (row_mid_index - 1 >= row_start_index && col_mid_index + 1 <= col_end_index) {
            res3 = helper(row_start_index, row_mid_index - 1, col_mid_index + 1, col_end_index)
          }
          res3
        }else{
          val res1 = helper(row_mid_index, row_end_index, col_mid_index, col_end_index)
          if (res1) {
            return true;
          }
          var res2 = false;
          if (row_mid_index+1<=row_end_index && col_mid_index-1>=col_start_index) {
            res2 = helper(row_mid_index+1, row_end_index, col_start_index,  col_mid_index-1)
          }
          if (res2) {
            return true;
          }
          var res3 = false;
          if (row_mid_index - 1 >= row_start_index && col_mid_index + 1 <=col_end_index) {
              res3 = helper(row_start_index, row_mid_index - 1, col_mid_index + 1, col_end_index)
          }
          res3
        }
      }
    }
    helper(row_start_index, row_end_index, col_start_index, col_end_index)
  }

  def main(args: Array[String]): Unit = {
    val array = Array(Array(3,8,8,10),Array(3,8,8,14),Array(3,9,13,14))
    //var array = Array(Array(-1,3))
    val res = Solution.searchMatrix(array, 12)
    println(res)
  }
}
