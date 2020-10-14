package com.atguigu.realtime.mock.util

/**
 * @author zrr
 * @Create 2020-10-2020/10/14 11:36
 */
import scala.collection.mutable.ListBuffer

/**
 * 根据提供的值和比重, 来创建RandomOptions对象.
 * 然后可以通过getRandomOption来获取一个随机的预定义的值
 *
 * (李四, 40) (张三, 60)
 *
 * RandomOptions((李四, 40) (张三, 60))
 */
object RandomOptions {  //伴生对象
  def apply[T](opts: (T, Int)*) = {
    val randomOptions = new RandomOptions[T]()  //new 一个伴生类对象
    //        randomOptions.totalWeight = (0 /: opts) (_ + _._2) // 计算出来总的比重
    randomOptions.totalWeight = opts.foldLeft(0)((total,curr)=>{total + curr._2})         // 计算出来总的比重
   // randomOptions.totalWeight = opts.foldLeft(0)(_ + _._2)
    opts.foreach {  //opt里有两个参数遍历 :(李四, 40) (张三, 60)
      case (value, weight) => randomOptions.options ++= (1 to weight).map(x => value)
      // 每一个opts是一个元组,把每一个value取出来,加到 ListBuffer 里 比如把张三放到List 里
        //(张三 ,60) 先把60个张三放进去,("李四", 40) 再放40个李四
    }
    randomOptions//返回一个对象,对象中包含属性totalWeight和options   totalWeight=100, options 中有40 个李四,60个张三
  }

  def main(args: Array[String]): Unit = {
    val r: RandomOptions[String] = RandomOptions(("李四", 40), ("张三", 60), ("王五", 20))//得到一个对象,对象中包含属性totalWeight和options   totalWeight=100, options 中有40 个李四,60个张三
    println(r.getRandomOption())
    println(r.getRandomOption())
    println(r.getRandomOption())
    println(r.getRandomOption())
    println(r.getRandomOption())
    println(r.options(1)+"---")

  }
}

class RandomOptions[T] {   //伴生类
  var totalWeight: Int = _
  var options = ListBuffer[T]()  //存40个李四 60 个张三

  /**
   * 获取随机的 Option 的值
   *
   * @return
   */
  def getRandomOption() = {
    //options(1)相当于取出角标为1 的ListBuffer 中的值,ListBuffer里有40个李四,60个张三, 得到随机的张三 或 李四
    options(RandomNumUtil.randomInt(0, totalWeight - 1)) //RandomNumUtil.randomInt(0, totalWeight - 1) 生成随机的下标
  }
}