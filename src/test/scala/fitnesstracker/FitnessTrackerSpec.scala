package fitnesstracker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrackerSpec extends AnyFlatSpec with Matchers {

  "Tracker" should "process normal day" in {
    val start = State()
    val events = List(
      PassiveBurn(24, 70),
      Eat("Breakfast", 500),
      Train(Run, 600, 45),
      Eat("Dinner", 600)
    )

    val res = Tracker.run(start, events)
    res.isRight shouldBe true

    val st = res.getOrElse(start)
    st.consumed shouldBe 1100
    st.burned shouldBe 2280
    st.balance shouldBe -1180
    st.trainTime shouldBe 45
    st.bestWorkout.map(_.cals) shouldBe Some(600)
    st.checkGoal should include("Done!")
  }

  it should "fail on negative food" in {
    val res = Tracker.applyEvent(State(), Eat("Apple", -50))
    res.isLeft shouldBe true
  }

  it should "fail on zero time training" in {
    val res = Tracker.applyEvent(State(), Train(Swim, 300, 0))
    res.isLeft shouldBe true
  }

  it should "recalculate on weigh in" in {
    val res = Tracker.applyEvent(State(), WeighIn(71.0, 70.0))
    res.toOption.get.consumed shouldBe 7700
  }
}