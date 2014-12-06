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

import scala.actors.Actor
import scala.collection.mutable.HashMap

/* ======== Message types ================================================= */
case class DoneDijkstra(origin: Int, results: HashMap[Int, Byte])
case class Dijkstra(n: Int)
case object Terminate

/* Class spinner is a worker thread which is spawned to perform a task
 * before returning its output via a message to its creator. Multiple
 * spinners should run to utilise multiple cores. This Spinner will
 * stay resident until it is sent a 'Terminate' message.
 */
class Spinner(g: Graph[Int], id: String) extends Actor
{
    def act() {
        var alive = true
        while(alive) {
            receive {
                case Dijkstra(n) => {
                    val dStartTime = System.currentTimeMillis()
                    val result = g.dijkstra(n)
                    val dEndTime = System.currentTimeMillis()

                     val distances = result._1

                    println("Spinner complete: "+n)

                    sender ! DoneDijkstra(n, distances)
                }

                case Terminate =>
                    alive = false

                case _ =>
                    println("Error in Spinner.")
            }
        }
    }
}

/* Class Master governs the set of Spinners until all work is complete.
*/
class Master(nodes: List[Int], graph: Graph[Int]) extends Actor
{
    /* Create and initiate one spinner per core */
    var spinners = List[Spinner]()
    for (i <- 1 to java.lang.Runtime.getRuntime().availableProcessors()) {
        spinners = (new Spinner(graph, i.toString)) :: spinners
    }
    spinners.foreach(s => s.start())


    val n = graph.size

    val distances = new DistanceTable(nodes)

    def act() {
        val total = nodes.size
        var completed = 0
        var n = nodes

        /* Kick start spinners. */
        if (total < spinners.size) {
            for (i <- 0 until total) {
                spinners(i) ! Dijkstra(n.head)
                n = n.tail
            }
        }
        else {
            spinners.foreach(s => {
                s ! Dijkstra(n.head)
                n = n.tail
            })
        }

        /* Wait for responses to come back; this loop will continue to receive
        * messages until all responses have been received. */
        while(completed < total) {
            receive {
                case DoneDijkstra(origin, results) => {
                    results.foreach(destination => {
                        val dest = destination._1
                        val dist = destination._2
                        distances.set(origin, dest, dist)
                    })

                    if (!n.isEmpty) {
                        sender ! Dijkstra(n.head)
                        n = n.tail
                    }

                    completed += 1
                }
                case _ => {
                    println("Error in Master")
                }
            }
        }

        /* Complete. Blindly output all distances between all nodes... */
        nodes.foreach(i => {    
            print(i+":")
            nodes.foreach(j => {
                print(" ["+j+"->"+distances.dist(i,j)+"]")
            })
            println()
        })

        /* Kill off the spinners! */
        spinners.foreach(s => s ! Terminate)
    }
}

/* ========== Bootstrap ==================================================== */
object AllPairs {
    def main(args: Array[String]) : Unit = {
        import scala.io.Source

        val graph = new Graph[Int]()
        var nodes = List[Int]()

        val lines = Source.fromInputStream(System.in).getLines
        while (lines.hasNext) {
            val line = lines.next

            val temp = line.trim.split(' ')
            val asnums = temp.slice(0,2).map(str => str.toInt)

            if (!nodes.contains(asnums(0)))
                nodes = asnums(0) :: nodes

            graph.addEdge(asnums(0), asnums(1))
        }

        val master = new Master(nodes, graph)
        master.start()
    }
}
