object Factory {
  def waitAndPrint(str: String): Unit = {
    Thread.sleep(6000)
    println(str)
  }
}

object Testing extends App {
    val t1 = new Thread {
      override def run() = Factory.waitAndPrint("cam")
    }
    val t2 = new Thread {
      override def run() = Factory.waitAndPrint("eron")
    }

    t1.start()
    t2.start()

    t1.join()
    t2.join()
}