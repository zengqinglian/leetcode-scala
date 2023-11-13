package sumofEvenNumbersAfterQueries
/*
Runtime
728 ms
Beats
100%
Memory
67.1 MB
Beats
100%
 */
object Solution {
  def sumEvenAfterQueries(nums: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
    var sum = 0;
    for(i <- nums.indices) {
      if (nums(i) % 2 ==0) {
        sum+=nums(i)
      }
    }
    val res = new Array[Int](nums.length)
    for(i <- queries.indices) {
      val query = queries(i)
      if (Math.abs(query(0) %2)==Math.abs(nums(query(1)) %2)) {
        if (Math.abs(query(0) %2)==1) {
          nums(query(1)) += query(0)
          sum = sum + nums(query(1))
          res(i) = sum
        }else{
          nums(query(1)) += query(0)
          sum  = sum + query(0)
          res(i) = sum
        }
      }else{
        if (Math.abs(query(0) % 2) == 1) {
          sum = sum - nums(query(1))
          nums(query(1)) += query(0)
          res(i) = sum
        } else {
          nums(query(1)) += query(0)
          res(i) = sum
        }
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1,2,3,4)
    val queries: Array[Array[Int]] = Array(Array(1,0),Array(-3,1),Array(-4,0),Array(2,3))
    Solution.sumEvenAfterQueries(nums, queries)
  }
}
