package questions

object Sets extends App{
  trait IntSet{
    def incl(x: Int): IntSet
    def contains (x: Int): Boolean
    def union(other: IntSet): IntSet
    def intersection(other: IntSet): IntSet
    def isEmpty: Boolean
    def excl(x: Int): IntSet

  }

  class EmptySet extends IntSet{
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
    def union(other: IntSet): IntSet = other
    def intersection(other: IntSet): IntSet = this
    def isEmpty: Boolean= true
    def excl(x: Int): IntSet = this
  }



  class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean = {
      if (x<elem ) left contains x
      else if (x>elem ) right contains x
      else true
    }

    def incl(x:Int): IntSet = {
      if (x<elem) new NonEmptySet (elem, left incl x, right)
      else if (x> elem) new NonEmptySet(elem, left, right incl x)
      else this
    }

    def union(other: IntSet ): IntSet =((left union right) union other) incl elem

    def intersection (other: IntSet): IntSet ={
      val newSet = (right intersection other) union (left intersection other)
      if (other contains elem) newSet incl elem
      else newSet
    }

    def isEmpty = false

    def excl(x: Int): IntSet = {
      if (elem == x) left union right
      else if (elem < x) (left excl x) union right incl elem
      else left union (right excl x) incl elem
    }
  }

  // Arrange
  val one = new EmptySet incl 1
  val two = new EmptySet incl 2

  // Test union
  val oneTwo = one union two
  assert(oneTwo contains 1)
  assert(oneTwo contains 2)

  // Test intersection
  val oneIntersection = one intersection oneTwo
  assert(oneIntersection contains 1)
  assert(!(oneIntersection contains 2))

  val a = new EmptySet incl 1 incl 2 incl 3
  val b = new EmptySet incl 4 incl 5
  assert((a intersection b).isEmpty)
  assert((a intersection b).isEmpty)
  // Test exclusion
  assert(oneTwo excl 2 contains 1)
  assert(!(oneTwo excl 2 contains 2))
  assert((oneTwo excl 2 excl 1).isEmpty)
}
