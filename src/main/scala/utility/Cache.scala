package utility

import scala.collection.mutable

class Cache[K, V] {
  private val store = mutable.Map.empty[K, V]

  def getOrCompute(key: K, value: => V): V = {
    store.getOrElseUpdate(key, value)
  }
}
