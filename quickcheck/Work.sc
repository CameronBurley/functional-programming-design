object Testing extends App {
object Factory {
    def waitAndPrint(str: String) = {
    Thread.sleep(6000)
    println(str)
}

val t1 = new Thread {
    override run(): Unit = Factory.waitAndPrint("cam")
}
val t2 = new Thread {
    override run(): Unit = Factory.waitAndPrint("cam")
}

t1.start()
t2.start()

t1.join()
t2.join()
}
