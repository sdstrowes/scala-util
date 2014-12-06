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

/* Class DistanceTable maintains a triangular array of distances
 * between two nodes.
 * This data structure is not resizeable; the number of addressable
 * nodes must be known beforehand. Nodes may be numbered and added
 * to the graph as arbitrary Int's, but the number of elements must
 * be known on creation.
 */
class DistanceTable(nodes: List[Int]) {
  /* Internal lookup table, from node names to local array index. */
  var lookup = Map[Int, Int]()
  val n = nodes.size

  {
	var counter = 0
	nodes.foreach(v => {
	  lookup = lookup + (v -> counter)
	  counter += 1
	})
  }

  /* Half 2-dimensional (triangular) array of distances. */
  val distances = new Array[Array[Byte]](n)
  for (i <- 0 until n) {
	distances(i) = new Array[Byte](i)
	for (j <- 0 until i) distances(i)(j) = scala.Byte.MaxValue
  }

  /* set(a, b, x)
   * Sets the path length between two nodes 'a' and 'b' to distance 'x'.
   * Returns the previous distance betweeen 'a' and 'b'.
   */
  def set(a: Int, b: Int, x: Byte): Byte = {
	if (a == b) return 0

	val tmpA = lookup(a)
	val tmpB = lookup(b)

	val from = if (tmpA > tmpB) tmpA else tmpB
	val to   = if (tmpA < tmpB) tmpA else tmpB

	val oldDist = distances(from)(to)
	distances(from)(to) = x
	oldDist
  }

  /* dist(a, b)
   * Returns the distance between 'a' and 'b'.
   */
  def dist(a: Int, b: Int): Byte = {
	if (a == b) return 0

	val tmpA = lookup(a)
	val tmpB = lookup(b)

	val from = if (tmpA > tmpB) tmpA else tmpB
	val to   = if (tmpA < tmpB) tmpA else tmpB

	distances(from)(to)
  }

  def keys = lookup.keys

  def size = n
}
