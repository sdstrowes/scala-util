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

import scala.collection.mutable.{Set, Map}

/* Class Graph[V] describes a graph with vertices of type V, with connecting
 * direction, unweighted edges.
 */
class Graph[V]()
{
    val edges = Map[V, Set[V]]()
    val vertices = Set[V]()

    /* addEdge(from, to)
     * Adds a unidirectional, unweighted edge to the graph between nodes 'from'
     * and 'to' */
    def addEdge(from: V, to: V) = {
        if (edges.contains(from)) {
            val adjMap = edges(from)
            adjMap += to
        }
        else {
            val adjMap = Set[V]()
            adjMap += to
            edges += from -> adjMap
        }

        vertices += from
        vertices += to 
    }

    def size = vertices.size
    def neighboursFor(node: V) = {
        edges(node)
    }


    /* distancesAndNextHops(source, e*):
     * Calculates the shortest paths to all nodes from starting point 'source',
     * up to an optional endpont 'e'.
     * Returns a Map of distances of the form (node -> distance from source)
     * and a map of next hops of the form (destination -> next hop from source)
    * node used to derive the shortest path.
    */
    def distancesAndNextHops(source: V, e: V*): (Map[V, Byte], Map[V, V]) = {
        assume(edges.contains(source), "Source is not known!")
        val end = if (e.size != 0) e(0) else -1

        /* Results */
        val distances = Map[V, Byte]()
        val predecessors = Map[V, V]()
        val nextHops     = Map[V, V]()

        val Q = new PriorityMap[V]
        val settled = Set[V]()

        /* Prime to start with 'source' node, by setting priority to zero */
        distances += source -> 0.toByte
        Q.push(source, 0)

        while (!Q.isEmpty) {
            /* Extract nearest vertex to computed set, and declare it as settled.
            * Update the distances to this node's neighbours, and update Q for
                * next iteration. */
            val u = Q.pop
            settled += u

            if (end != -1 && u == end)
                return (distances, nextHops)

            for (v <- edges(u)) {
                if (! settled.contains(v)) {
                    val vNewDist:Byte = (distances(u) + 1).toByte
                    if ( ! distances.isDefinedAt(v) || vNewDist < distances(v)) {
                        distances += v -> vNewDist
                        predecessors += v -> u
                        Q.push(v, vNewDist)

                        /* Determine the next hop used to reach here */
                        var tmp = v
                        while (predecessors(tmp) != source) tmp = predecessors(tmp)
                        nextHops += v -> tmp
                    }
                }
            }
        }

        /* Return a tuple containing distances and nexthops */
        (distances, predecessors)
    }

    /* dijkstra(source):
    * Calculates the shortest paths to all nodes from starting point 'source'.
    * Returns a Map of distances of the form (node -> distance from source)
    * and a map of predecessors of the form (node -> preceding node used to
    * achieve that distance).
    */
    def dijkstra(source: V) = {
        assume(edges.contains(source), "Source is not known!")

        /* Results */
        val distances = Map[V, Byte]()
        val predecessors = Map[V, V]()

        val Q = new PriorityMap[V]
        val settled = Set[V]()

        /* Prime to start with 'source' node, by setting priority to zero */
        distances += source -> 0.toByte
        Q.push(source, 0)

        while (!Q.isEmpty) {
            /* Extract nearest vertex to computed set, and declare it as settled.
            * Update the distances to this node's neighbours, and update Q for
                * next iteration. */
            val u = Q.pop
            settled += u

            for (v <- edges(u)) {
                if (! settled.contains(v)) {
                    val vNewDist:Byte = (distances(u) + 1).toByte
                    if ( ! distances.isDefinedAt(v) || vNewDist < distances(v)) {
                        distances += v -> vNewDist
                        predecessors += v -> u
                        Q.push(v, vNewDist)
                    }
                }
            }
        }

        /* Return a tuple containing distances and predecessors */
        (distances, predecessors)
    }
}

/* ========== Bootstrap ==================================================== */
object DijkstraTest {
    def main(args: Array[String]) : Unit = {
        import scala.io.Source

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
        val dStartTime = System.currentTimeMillis()
        val result = dijkstraGraph.dijkstra(7911)
        val dEndTime = System.currentTimeMillis()
        
        /* Print all distances... Ensures I have a result for every node. Would
        * raise an exception if something was broken. */
        val distances = result._1
        for (node <- dijkstraGraph.vertices) {
            println("Distance to "+node+": "+distances(node))
        }

        /* Output stats. */
        println("Processed graph of "+dijkstraGraph.size+" nodes.")
        println("Took "+(gEndTime - gStartTime)+"ms to read the graph.")
        println("Took "+(dEndTime - dStartTime)+"ms to run Dijkstra from one location.")
    }
}
