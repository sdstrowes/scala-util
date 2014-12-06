package com.sdstrowes.util

import scala.actors.Actor
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._
import scala.actors.remote.Node

object BasicSourceApp {
    def main(args: Array[String]) : Unit = {
        if (args.length == 2) {
            val remoteport = args(1).toInt
            val peer = Node(args(0), remoteport)
            val source = new RemoteSource(peer)
            source.start()
        }
        else {
            println("usage: scala BasicSourceApp [RemoteHostName] [RemotePort]")
        }
    }
}

class RemoteSource(peer: Node) extends Actor {
    def act() {
        RemoteActor.classLoader = getClass().getClassLoader()
        val sink = select(peer, 'SinkApp)
        link(sink)

        while (true) {
            sink ! "Hello, world!"
            Thread sleep 5000
        }
    }
}
