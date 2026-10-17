package fitnesstracker

sealed trait Sport
case object Run extends Sport
case object Swim extends Sport

sealed trait Event
case class Eat(name: String, cals: Int) extends Event
case class Train(sport: Sport, cals: Int, minutes: Int) extends Event
case class PassiveBurn(hours: Int, calsPerHour: Int) extends Event
case class WeighIn(actual: Double, expected: Double) extends Event

case class State(
  consumed: Int = 0,
  burned: Int = 0,
  trainTime: Int = 0,
  history: List[Train] = Nil,
  targetDeficit: Int = 500
) {
  def balance: Int = consumed - burned
  
  def bestWorkout: Option[Train] = history.maxByOption(_.cals)

  def checkGoal: String = {
    val defc = burned - consumed
    if (defc >= targetDeficit) s"Done! Deficit: $defc"
    else s"Need to burn ${targetDeficit - defc} more"
  }
}

object Tracker {
  def applyEvent(s: State, e: Event): Either[String, State] = e match {
    case Eat(_, cals) =>
      if (cals < 0) Left("Cals can't be negative")
      else Right(s.copy(consumed = s.consumed + cals))

    case t @ Train(_, cals, mins) =>
      if (cals < 0 || mins <= 0) Left("Invalid training data")
      else Right(s.copy(
        burned = s.burned + cals,
        trainTime = s.trainTime + mins,
        history = t :: s.history
      ))

    case PassiveBurn(h, cph) =>
      if (h < 0 || h > 24 || cph < 0) Left("Wrong passive burn data")
      else Right(s.copy(burned = s.burned + (h * cph)))

    case WeighIn(act, exp) =>
      if (act <= 0 || exp <= 0) Left("Weight must be > 0")
      else {
        val diff = act - exp
        val cals = (diff * 7700).toInt
        if (cals > 0) Right(s.copy(consumed = s.consumed + cals))
        else Right(s.copy(burned = s.burned + Math.abs(cals)))
      }
  }

  def run(start: State, events: List[Event]): Either[String, State] = {
    events.foldLeft(Right(start): Either[String, State]) { (acc, ev) =>
      acc.flatMap(st => applyEvent(st, ev))
    }
  }
}