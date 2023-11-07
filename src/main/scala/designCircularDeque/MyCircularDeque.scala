package designCircularDeque
/*
Runtime
566 ms
Beats
100%
Memory
59.5 MB
Beats
100%
 */
class MyCircularDeque(_k: Int) {
  var currentSize=0
  var startIdx = -1
  var endIdx = -1
  val array:Array[Int]=Array.fill(_k)(0)
  def insertFront(value: Int): Boolean = {
    if (isFull()) {
      false
    } else {
      if (isEmpty()) {
        array(0) = value
        startIdx=0
        endIdx=0
      }else{
        if(startIdx==0){
          startIdx=_k-1
        } else{
          startIdx-=1
        }
        array(startIdx) = value
      }
      currentSize += 1
      true
    }
  }

  def insertLast(value: Int): Boolean = {
    if(isFull()) {
      false
    }else{
      if (isEmpty()) {
        array(0) = value
        startIdx = 0
        endIdx = 0
      } else {
        if (endIdx == _k-1) {
          endIdx =0
        }else{
          endIdx+=1
        }
        array(endIdx) = value
      }
      currentSize += 1

      true
    }
  }

  def deleteFront(): Boolean = {
    if (isEmpty()) {
      false
    } else {
      currentSize -= 1
      if (startIdx < _k-1) {
        startIdx += 1
      } else {
        startIdx = 0
      }
      true
    }
  }

  def deleteLast(): Boolean = {
    if (isEmpty()) {
      false
    } else {
      currentSize-=1
      if(endIdx>0){
        endIdx-=1
      }else{
        endIdx=_k-1
      }
      true
    }
  }

  def getFront(): Int = {
    if (isEmpty()) {
      -1
    } else {
      array(startIdx)
    }
  }

  def getRear(): Int = {
    if (isEmpty()){
      -1
    }else{
      array(endIdx)
    }
  }

  def isEmpty(): Boolean = {
    currentSize==0
  }

  def isFull(): Boolean = {
    currentSize == _k
  }
}

object Main{
  def main(args: Array[String]): Unit = {
    val s = new MyCircularDeque(5)
    s.insertFront(7)
    s.insertLast(0)
    s.getFront()
  }
}

/**
 * Your MyCircularDeque object will be instantiated and called as such:
 * var obj = new MyCircularDeque(k)
 * var param_1 = obj.insertFront(value)
 * var param_2 = obj.insertLast(value)
 * var param_3 = obj.deleteFront()
 * var param_4 = obj.deleteLast()
 * var param_5 = obj.getFront()
 * var param_6 = obj.getRear()
 * var param_7 = obj.isEmpty()
 * var param_8 = obj.isFull()
 */
