// Copyright (c) 2010, Stephen D. Strowes
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without 
// modification, are permitted provided that the following conditions are met:
// 
//   * Redistributions of source code must retain the above copyright notice, 
//     this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright 
//     notice, this list of conditions and the following disclaimer in the 
//     documentation and/or other materials provided with the distribution.
//   * The author named in the above copyright notice may not be used to
//     endorse or promote products derived from this software without
//     specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
// POSSIBILITY OF SUCH DAMAGE.

package com.sdstrowes.util

import scala.io.Source
import scala.collection.mutable.{Set, Map}
import scala.actors.Actor

case class DoneDijkstra(origin: Int, dists: Map[Int, Byte])
case class Dijkstra(origin: Int)
case object Continue
case object Terminate

/* ------------------------------------------------------------------------ */
class Spinner(g: Graph[Int]) extends Actor
{
    def act() {
        var alive = true
        while(alive) {
            receive {
                case Dijkstra(n) => {
                    try {
                        val result = g.dijkstra(n)
                         val distances = result._1
                        sender ! DoneDijkstra(n, distances)
                    } catch {
                        case e:java.lang.AssertionError => {
                            System.err.println("Source "+n+" not in graph!")
                            sender ! Continue
                        }
                    }
                }

                case Terminate =>
                    alive = false

                case _ =>
                    System.err.println("Error in Spinner.")
            }
        }
    }
}

/* ------------------------------------------------------------------------ */
class Master(args: Array[String], graph: Graph[Int]) extends Actor
{
    var spinners = List[Spinner]()

    def act() {
        /* Kick start the spinners. */
        for (i <- 1 to java.lang.Runtime.getRuntime().availableProcessors()) {
            spinners = (new Spinner(graph)) :: spinners
        }
        spinners.foreach(s => s.start())

        /* Determine the set of origins to run Dijkstra's algorithm from. If
            * there is no input file describing a subset of the graph, then assume
        * all-pairs. */
        var nodes = if (args.size == 1) {
            var temp = Set[Int]()
            for (line <- Source.fromFile(new java.io.File(args(0))).getLines()) {
                val pair = line.trim.split(" ")
                val source = pair(0).toInt
                val dest = pair(1).toInt
                //temp = temp + scala.math.min(source, dest)
                temp = temp + source
                temp = temp + dest
            }
            temp.toList
        }
                    else {
                        graph.vertices.toList
                    }

        /* Retain a copy; I'm going to walk through the full list. */
        val allNodes = nodes

        val distances = new DistanceTable(graph.vertices.toList)
        var total = nodes.size
        var completed = 0

        val dStartTime = System.currentTimeMillis()

        /* Generate the distances table by calculating Dijkstra from all points */
        for (i <- 0 until scala.math.min(total, spinners.size)) {
            spinners(i) ! Dijkstra(nodes.head)
            nodes = nodes.tail
        }

        /* -- Split ------------------------------------------------------------ */
        while(completed < total) {
            receive {
                case DoneDijkstra(origin, d) => {
                    d.keys.foreach(destination => {
                        val distance = d(destination)
                        distances.set(origin, destination, distance)
                    })

                    if (!nodes.isEmpty) {
                        sender ! Dijkstra(nodes.head)
                        nodes = nodes.tail
                    }

                    completed += 1
                }

                case Continue => {
                    if (!nodes.isEmpty) {
                        sender ! Dijkstra(nodes.head)
                        nodes = nodes.tail
                    }

                    completed += 1
                }

                case _ => {
                    System.err.println("2. Error in Master")
                }
            }
        }
        /* -- Join ------------------------------------------------------------- */

        val dEndTime = System.currentTimeMillis()

        /* Output the distances */
        if (args.size == 1) {
            for (line <- Source.fromFile(new java.io.File(args(0))).getLines()) {
                val pair = line.trim.split(" ")
                val source = pair(0).toInt
                val dest = pair(1).toInt
                println(source+" "+dest+" "+distances.dist(source, dest))
            }
        }
        else {
            for (nodeA <- allNodes)
                for (nodeB <- allNodes)
                    println(nodeA +" "+ nodeB +" "+distances.dist(nodeA, nodeB))
        }

        spinners.foreach(s => s ! Terminate)

        println("Took "+(dEndTime - dStartTime)+"ms to run Dijkstra from all locations.")
        exit
    }
}

/* ========== Bootstrap =================================================== */
object ParallelDijkstraTest {
    def main(args: Array[String]) : Unit = {

        val dijkstraGraph = new Graph[Int]()

        val gStartTime = System.currentTimeMillis()
        val lines = Source.fromInputStream(System.in).getLines()
        while (lines.hasNext) {
            val line = lines.next

            val temp = line.trim.split(' ')
            val asnums = temp.slice(0,2).map(str => str.toInt)

            dijkstraGraph.addEdge(asnums(0), asnums(1))
        }
        val gEndTime = System.currentTimeMillis()

        /* Run algorithm. */
        val master = new Master(args, dijkstraGraph)
        master.start()

        /* Output stats. */
        println("Processed graph of "+dijkstraGraph.size+" nodes.")
        println("Took "+(gEndTime - gStartTime)+"ms to read the graph.")
    }
}
