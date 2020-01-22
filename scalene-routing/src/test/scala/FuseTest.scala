package scalene.corerouting

import org.scalatest._

class FuseTest extends WordSpec with MustMatchers {

  "fuse" must {
    "handle unit" in {
      implicitly[Fuse[Unit, Unit]].fuse((),()) must equal(())
      implicitly[Fuse[Unit, Int]].fuse((), 3) must equal(3)
      implicitly[Fuse[Int, Unit]].fuse(4, ()) must equal(4)
    }

    "tuple with single value" in {
      implicitly[Fuse[(Int, String), Unit]].fuse((3, "hello"), ()) must equal((3, "hello"))
      implicitly[Fuse[(Int, String), Boolean]].fuse((3, "hello"), false) must equal((3, "hello", false))
      implicitly[Fuse[Boolean, (Int, String)]].fuse(false, (3, "hello")) must equal((false, 3, "hello"))
    }

    "two tuples" in {
      implicitly[Fuse[(Boolean, Double), (Int, String)]].fuse((false, 3.14), (3, "hello")) must equal((false, 3.14, 3, "hello"))
    }

  }

}


