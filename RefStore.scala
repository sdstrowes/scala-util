package com.sdstrowes.util

class RefStore[T<:AnyRef]
{
    import scala.collection.mutable.HashSet
    var store = new HashSet[T]()

    def intern(ref: T): T = {
        store.findEntry(ref) match {
            case None => {
                store += ref
                // Added new shared ref, ref
                ref
            }
            case Some(r) => {
                // Returning shared ref, r
                r
            }
        }
    }
}
