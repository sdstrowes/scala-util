package com.sdstrowes.util

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._

object BasicSinkApp {
    def main(args: Array[String]) : Unit = {
        if (args.length == 1) {
            val port = args(0).toInt
            val sink = new RemoteSink(port)
            sink.start()
        }
        else {
            println("usage: scala BasicSinkApp [LocalPort]")
        }
    }
}

class RemoteSink(port: Int) extends Actor {
    RemoteActor.classLoader = getClass().getClassLoader()
    def act() {
        alive(port)
        register('SinkApp, self)

        while (true) {
            receive {
                case msg =>
                    println(msg)
            }
        }
    }
}
