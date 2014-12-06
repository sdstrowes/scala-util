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

import scala.collection.immutable._

object BGPPathsToLinks {
    import scala.io.Source

    def deriveASNum(in: String) = {
        val asN = in.split('.')
        if (asN.size > 1) 
            asN(0).toInt * 65536 + asN(1).toInt
        else
            asN(0).toInt
    }

    def main(args: Array[String]) : Unit = {
        val lines = Source.fromInputStream(System.in).getLines()

        var nodes = HashMap[Int, Set[Int]]()

        for (line <- lines) {
            val asnums = line.trim.split(" ")

            for (i <- 0 until asnums.size-1) {
                try {
                    val as1 = deriveASNum(asnums(i))
                    val as2 = deriveASNum(asnums(i+1))

                    if (as1 != as2) {
                        try {
                            nodes = nodes + (as1 -> (nodes(as1) + as2))
                        } catch {
                            case e:NoSuchElementException => {
                                nodes = nodes + (as1 -> Set(as2))
                            }
                        }

                        try {
                            nodes = nodes + (as2 -> (nodes(as2) + as1))
                        } catch {
                            case e:NoSuchElementException => {
                                nodes = nodes + (as2 -> Set(as1))
                            }
                        }
                    }

                } catch {
                    /* deriveASNum can throw an exception if the input is not an
                    * integer. (A strange pre-pending bug noticed via
                    * routeviews.eqix appears to give an AS number 3699236992,
                    * which fails.) Numbers that are not numbers will be
                    * skipped, but the rest of the path will be output. */
                    case e:NumberFormatException => {}
                }
            }
        }

        for (key <- nodes.keys) {
            for (value <- nodes(key)) {
                println(key+" "+value)
            }
        }
    }
}
