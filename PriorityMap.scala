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

/* Implements a simple array-backed binary heap data structure */
class PriorityMap[A]
{
    /* key:priority pair; the ordering is on the priority. */
    class Pair(val key: A, val priority: Int) extends Ordered[Pair] {
        override def toString: String = "("+priority+":"+key+")"
        def compare(that: Pair) = this.priority - that.priority
    }

    /* Abstraction for the backing array, to mask the zero-based array indexing
     * from the 1-based heap indexing. */
    class BackingArray[A] extends java.util.ArrayList[A] {
        private def offset(index: Int): Int = index - 1
        override def set(index: Int, value: A): A = super.set(offset(index), value)
        override def get(index: Int): A = super.get(offset(index))
    }

    var heap = new BackingArray[Pair]()
    /* Maintain a mapping of keys to their current location (index) in the
     * heap. */
    var indexOf = Map[A, Int]()

    /* Check if this is an addressable element in the heap. */
    private def exists(index: Int): Boolean = 
        if (index <= heap.size()) true else false

    /* Tests the key:priority pair at location 'index' in the heap, and bubbles
     * it upward if required to maintain the heap property. */
    private def bubbleUp(index: Int): Unit = {
        if (index > 1 && heap.get(index) < heap.get(index/2)) {
            println("-- Bubbling up: "+index)
            val tempA = heap.get(index)
            val tempB = heap.get(index/2)
            heap.set(index,   tempB)
            heap.set(index/2, tempA)

            indexOf += (tempA.key -> index/2)
            indexOf += (tempB.key -> index)

                bubbleUp(index/2)
        }
    }

    /* Tests the key:priority pair at location 'index' in the heap, and bubbles
     * it downward if required to maintain the heap property. */
    private def bubbleDown(index: Int): Unit = {
        val lChild = index*2
        if (!exists(lChild)) return
        val rChild = lChild+1
        val lVal = heap.get(lChild)

        val s = if (exists(rChild) && lVal < heap.get(rChild))
            lChild
                else if (exists(rChild) && lVal >= heap.get(rChild))
                    rChild
                else
                    lChild

        if (heap.get(s) < heap.get(index)) {
            val tempA = heap.get(s)
            val tempB = heap.get(index)
            heap.set(s,     tempB)
            heap.set(index, tempA)

            indexOf += (tempA.key -> index)
                indexOf += (tempB.key -> s)
        }

        bubbleDown(s)
    }

    /* Pop the lowest priority element off the head of the heap. */
    def pop: A = {
        if (heap.isEmpty()) { //throw Exception
            println("ERROR")
            throw new Exception()
        }
        val min = heap.get(1)

        if (heap.size > 1) {
            val temp = heap.remove(heap.size()-1)
            indexOf += (temp.key -> 1)
                heap.set(1, temp)
            bubbleDown(1)
        }
        else
            heap.remove(0)

        indexOf = indexOf - min.key
        min.key
    }

    /* Push an element into the heap at the given priority. */
    def push(key: A, priority: Int) = {
        if (indexOf contains key) reAssignPriority(key, priority)
        else {
            heap.add(new Pair(key, priority))
            indexOf += (key -> heap.size())
                bubbleUp(heap.size())
        }
    }

    /* Reassign the priority of a node; on modifying the priority, check the
     * priorities of the node's parents and children. Bubble up or down
     * as necessary. */
    private def reAssignPriority(key: A, newPriority: Int) {
        val index = indexOf(key)
        val oldPair = heap.get(index)
        if (newPriority != oldPair.priority) {
            val newPair = new Pair(oldPair.key, newPriority)
            heap.set(index, newPair)
            /* Reassigning a priority may violate the heap property. Correct
             * this, if need be. */
            if (index > 1 && newPair < heap.get(index/2)) {
                bubbleUp(index)
            }
            else {
                bubbleDown(index)
            }
        }
    }

    def checkValidity: Boolean = {
        var correct = true
        for (index <- 1 to heap.size()) {
            val lChild = index * 2
            val rChild = lChild + 1
            if (lChild <= heap.size() && heap.get(lChild) < heap.get(index)) {
                correct = false
            }
            if (rChild <= heap.size() && heap.get(rChild) < heap.get(index)) {
                correct = false
            }
        }
        correct
    }

    def isEmpty: Boolean = heap.isEmpty()
    def size: Int = heap.size()

    override def toString: String = {
        return heap.toString
    }
}

object PriorityMapTest {
    def main(args: Array[String]) = {
        import java.util.Random

        val random = new Random()

        val test = new PriorityMap[Int]()

        //     test.push(15, 3)
        //     test.push(16, 2)
        //     test.push(17, 1)
        //     test.push(18, 0)
        //     test.push(19, 9)

        //     test.push(20, 8)
        //     test.push(21, 7)
        //     test.push(22, 6)
        //     test.push(23, 5)
        //     test.push(24, 4)

        //     println("indexes: "+test.indexOf)
        //     println("Valid? "+test.checkValidity)

        //     // Reassign some priorities
        //     println("REASSIGN")
        //     test.push(20, 3)
        //     println("REASSIGN")
        //     test.push(21, 1)
        //     println("REASSIGN")
        //     test.push(22, 10)

        //     println("Valid? "+test.checkValidity)
        //    Thread.sleep(20000)

        for (i <- 0 to 100) {
             val num = random.nextInt(50)
             val priority = random.nextInt(100)
            test.push(num, priority)

            if (!test.checkValidity) {
                println("Failed on inserting: "+ num + "with priority: "+priority)
            }
        }

        var i = 1000000
        while (i != 0) {
            i -= 1
            println("index of is: "+test.indexOf)
            if (random.nextDouble() < 0.45) {
                try {
                    test.pop
                } catch {
                    case e: Exception => println("Caught exception")
                }
                if (!test.checkValidity) {
                    println("Failed after test.pop()")
                }
            }
                else {
                     val num = random.nextInt(50)
                     val priority = random.nextInt(100)
                     test.push(num, priority)
                     if (!test.checkValidity) {
                         println("Failed on inserting: "+ num + "with priority: "+priority)
                     }
                 }
         }

        println("##########################################################################")
        while (!test.isEmpty) {
            println("Removing: "+test.pop)
         }
    }
}
