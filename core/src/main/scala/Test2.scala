import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * Task mapping algorithm for arbitrary size NoC.
  *
  * This algorithm takes inspiration from a spiral heuristic mapping algorithm [1].
  * It works as follows:
  * - calculate the value of each task using a heuristic function (explained later)
  * - sort the task set by order of decreasing value
  * - map tasks depending on number of tasks communicating with them:
  * - highly communicating into PEs that have an appropriate number of PEs around them (not boundary)
  * - tasks that communicate with each other should be near each other
  *
  * [1] Mehran, A., Saeidi, S., Khademzadeh, A. and Afzali-Kusha, A. (2007). Spiral: A heuristic mapping algorithm for network on chip. IEICE Electron. Express, 4(15), pp.478-484.
  *
  */
@vista.enable
object Test2 extends App {

  // NoC topology
  // create the PE mesh
  val (meshX, meshY) = (4, 4)

  // weight of terms for the heuristic function
  val BANDWIDTH_TERM = 0.6
  val SLACK_TERM     = 0.4

  // wrapper for a task
  case class Task(id: Int,
                  period: Double,
                  compTime: Double,
                  messageSize: Int,
                  destination: Int,
                  priority: Int,
                  slack: Double)

  // represents a processing core
  case class PE(x: Int, y: Int) {

    /**
      * Finds all connected PEs (in shape of cross from given PE). Handles corners automatically.
      */
    def connected: Seq[PE] = {
      // find all PEs that are in the same Y level
      val sameY: Seq[Option[PE]] = for (x <- this.x - 1 to this.x + 1)
        yield pes.find(p => p.x == x && p.y == this.y)
      // find all PEs that are in the same X level
      val sameX: Seq[Option[PE]] = for (y <- this.y - 1 to this.y + 1)
        yield pes.find(p => p.y == y && p.x == this.x)

      // combine PEs that have been found and remove the PE which is the same one we're looking at.
      // sameY and sameX are actually Seq[Option[PE]] because this method also handles edge PEs.
      // This means that some PEs are not "defined", meaning they're not actually there
      // this allows us the handle corners without much additional code or any exceptions
      (sameX ++ sameY).flatten.filter(_ != this)
    }

    /**
      * Finds neighbours (square grid around) of the given PE. Handles corners automatically.
      *
      * @param size how big of a neighbourhood to check around.
      *             Default is 1, meaning only one PE to the to all directions will be checked
      * @return list of neighbouring PEs
      */
    def neighbors(size: Int = 1): Seq[PE] = {
      // simply check all the surrounding coordinates
      val maybeNeighbors: Seq[Option[PE]] =
        for (x <- this.x - size to this.x + size; y <- this.y - size to this.y + size) yield {
          pes.find(p => p.x == x && p.y == y)
        }
      // just like for "connected", we handle corners by attempting to get them, and
      // when they're not there, simply None is returned. We then only get the neighbours
      // which are defined. Finally remove the PE that we're looking at
      maybeNeighbors.flatten.filter(_ != this)
    }
  }

  // regex to match tasks file
  val regex =
    """(\d+) (\d+\.\d+) (\d+\.\d+) (\d+) (\d+) (\d+)""".r

  // parse tasks into wrapper objects
  val tasks = Source
    .fromFile("tasks.txt")
    .getLines()
    .map {
      case regex(id, period, compTime, messageSize, destination, priority) =>
        val slack = period.toDouble - compTime.toDouble
        Task(id.toInt,
             period.toDouble,
             compTime.toDouble,
             messageSize.toInt,
             destination.toInt,
             priority.toInt,
             slack)
    }
    .toVector

  // we get those values so we can later scale and give a weight without creating bias
  val minMessageSize = tasks.minBy(_.messageSize).messageSize
  val maxMessageSize = tasks.maxBy(_.messageSize).messageSize

  // the second important parameter of tasks – once again calculated
  // so we can scale and give weight without creating bias
  val minSlack = tasks.minBy(_.slack).slack
  val maxSlack = tasks.maxBy(_.slack).slack

  // find the maximum priority in the given task set
  val maxPriority = tasks.maxBy(_.priority).priority

  // list of corner PEs in our grid
  val corners = List(PE(0, 0), PE(0, meshY - 1), PE(meshX - 1, 0), PE(meshX - 1, meshY - 1))

  // create the list of all PEs for quick access
  val pes = for (x <- 0 until meshX; y <- 0 until meshY) yield PE(x, y)

  /**
    * Scale a parameter according to its min and max.
    * Source: http://stackoverflow.com/a/23157705/371737
    *
    * @param value      the actual value to scale
    * @param min        the minimum possible value of this parameter
    * @param max        the maximum possible value of this parameter
    * @param limitLower lower bound, e.g 0
    * @param limitUpper upper bound, e.g 1
    * @return scaled value. Can convert a parameter to a zero-mean
    */
  def scale(value: Double,
            min: Double,
            max: Double,
            limitLower: Double,
            limitUpper: Double): Double = {
    ((limitUpper - limitLower) * (value - min) / (max - min)) + limitLower
  }

  /**
    * Heuristic to determine how important a task is. Detailed comments are in code, but in general:
    * - look at all tasks communicating to given tasks
    * - sum their bandwidth and slack and multiply by their average priority
    * - add this task's bandwidth and slack multiplied by its priority
    *
    * @param task task to analyse
    * @return a number representing the value of the task. Higher number = more important
    */
  def heuristic(task: Task, tasks: Seq[Task]): Double = {
    // find all tasks communicating with this task
    val comms = tasks.filter(_.destination == task.id)

    // add up their bandwidth, their relative deadlines and their priority
    // values are scaled so the message size would create enormous bias (as the numbers are much bigger than the slack's)
    val totalBandwidth = comms.foldLeft(0.0) { (carry, t) =>
      scale(t.messageSize, minMessageSize, maxMessageSize, 0, 1) + carry
    }
    val totalSlack = comms.foldLeft(0.0) { (carry, t) =>
      scale(t.slack, minSlack, maxSlack, 0, 1) + carry
    }

    // priority is flipped because we want that tasks with higher priority will have bigger value
    val totalPriority = comms.foldLeft(0) { (carry, t) =>
      (maxPriority - t.priority) + carry
    }

    // then we look at the task we're analysing and add its bandwidth, relative deadline
    // multiplied by its reversed priority as a term
    val selfBandwidth = scale(task.messageSize, minMessageSize, maxMessageSize, 0, 1)
    val selfSlack     = scale(task.slack, minSlack, maxSlack, 0, 1)

    // since a task may have no tasks communicating with it, we must check it first.
    // if there aren't any – the term is simply 0 –– the task, despite not having anyone
    // communicating with it, may still be important
    val commTerm =
      if (comms.nonEmpty)
        (totalBandwidth * BANDWIDTH_TERM + totalSlack * SLACK_TERM) * (totalPriority / comms.size)
      else 0.0

    // now calculate our own term using a similar fashion as for the communications term
    val selfTerm = (selfBandwidth * BANDWIDTH_TERM + selfSlack * SLACK_TERM) * (maxPriority - task.priority)

    commTerm + selfTerm
  }

  /**
    * To determine if a task would be schedulable in this PE:
    * - take the difference between its period and total computation time in the PE together with the given task
    * This checks if the task will finish before its period, it means it is schedulable.
    *
    * Alternatively, response time analysis can be used, however, as our processors are non-preemptive,
    * hence we can use a simpler variant.
    */
  def isSchedulable(taskToMap: Task, tasks: Seq[Task]): Boolean = {
    if (tasks.isEmpty) true
    else taskToMap.period - (tasks.map(_.compTime).sum + taskToMap.compTime) > 0
  }

  /**
    * Allows finding the least used PE in the given mapping.
    * Least utilised PE is defined by having the biggest total amount of slack (adding up all tasks assigned to it).
    * Furthermore, it should be noted that only PEs where the task would be schedulable are considered (this is
    * a prefilter done before finding the least utilised PE).
    *
    * @param mapping the mapping to analyse (pe -> list of tasks mapped to it)
    * @param filter  an optional filter can remove some PEs if needed. e.g. if we want to limit the set of
    *                PEs we want to use, for examples neighbours only. However, if a filter is given and a potential PE
    *                is failed to be found (one that satisfies the pre-filter) then any schedulable PE is given (i.e filter is ignored)
    * @return least used PEs, in the correct order
    */
  def findLeastUtilisedPe(taskToMap: Task,
                          mapping: Map[PE, Seq[Task]],
                          filter: (PE => Boolean) = pe => true): Seq[PE] = {
    val potentialPEs: Seq[PE] = mapping.toSeq
      .filter {
        case (_, mappedTasks) => isSchedulable(taskToMap, mappedTasks)
      }
      .sortBy {
        case (_, mappedTasks) => mappedTasks.map(_.slack).sum
      }
      .map(_._1)

    // first attempt to find a PE with the provided filter
    val filtered = potentialPEs.filter(filter)

    // if there is no PEs after filtering, simply return the the unfiltered ones
    if (filtered.isEmpty) potentialPEs
    else filtered
  }

  /**
    * Attempts to map the task to a PE closest to the PE given.
    * This function first tries to map to a connected PE, if there is none that can "sustain" the given task
    * (i.e. tasks in PE would become unschedulable) then look at neighbors as well.
    *
    * @param pe      map close to this PE
    * @param task    map this task
    * @param mapping the current mapping
    */
  def attemptNearbyMapping(pe: PE, task: Task, mapping: Mapping): PE = {

    /**
      * Find the PE in an ever-increasing neighborhood size.
      * Start with size 1 (only check nearest neighbors) and gradually expand
      * the search into bigger neighborhoods.
      *
      * @param size the size of the neighborhood
      */
    @tailrec
    def findInIncreasingNeighborhood(size: Int = 1): PE = {
      val potentialPEs = pe.neighbors(size)
      val leastUsedPEs = findLeastUtilisedPe(task, mapping, p => potentialPEs.contains(p))

      if (leastUsedPEs.nonEmpty) leastUsedPEs.head
      else findInIncreasingNeighborhood(size + 1)
    }
    // it is most efficient to assign this task to a connected PE, hence we find any
    // PE connected to the one that has the task we talk to
    val ce = pe.connected
    val le = findLeastUtilisedPe(task, mapping, p => ce.contains(p))

    // if there is a connected PE that fits, just return that
    // else find using an increasingly big neighbourhood
    if (le.nonEmpty) le.head
    else findInIncreasingNeighborhood()
  }

  // convenience type to make function signatures smaller
  type Mapping = Map[PE, Seq[Task]]

  /**
    * A tail recursive implementation of creating a mapping.
    *
    * @param mapping   the existing mapping that has all the tasks mapped to PEs
    * @param tasksLeft tasks that are not mapped yet
    * @return final mapping
    */
  @tailrec
  def mkMap(mapping: Mapping, tasksLeft: IndexedSeq[(Int, Double)]): Mapping = {
    // (recursion base case) once we're done mapping all tasks, simply return the mapping
    if (tasksLeft.isEmpty) mapping
    else {
      val (taskId, _) = tasksLeft.head
      val currentTask = tasks(taskId)

      // get communicating tasks and order them by importance
      val commTasks  = tasks.filter(_.destination == taskId).map(_.id)
      val commSorted = tasksByImportance.filter(t => commTasks.contains(t._1))

      // depending on the number of tasks communicating with the tasks being analysed
      // we will determine which PE to map to
      val leastUsedPe = commSorted.size match {
        case 0 => // if we communicate with no tasks
          // Since we order tasks by their importance, it is very likely that tasks that communicate
          // a lot will be assigned to a PE first. However, in case there isn't,
          // we just find any PE which is least utilised
          taskToPe(tasks(currentTask.destination)) match {
            case Some(pe) => attemptNearbyMapping(pe, currentTask, mapping)
            case None     => findLeastUtilisedPe(currentTask, mapping).head
          }
        case 1 => // if we communicate to only one task, we need to choose between:
          // 1. task communicating with me
          // 2. task I'm communicating with
          // depending on their value.
          val communicatingWith = tasksByImportance.filter(_._1 == currentTask.destination).head
          val communicatingToMe = commSorted.head // highest value task communicating to me

          // choose the task with the biggest value
          val beNearThisTaskId =
            if (communicatingToMe._2 > communicatingWith._2) communicatingToMe._1
            else communicatingWith._1

          // we can now finally find an appropriate PE
          taskToPe(tasks(beNearThisTaskId)) match {
            case Some(pe) => attemptNearbyMapping(pe, currentTask, mapping)
            case None     => findLeastUtilisedPe(currentTask, mapping).head
          }
        case 2 => // if we communicate with 2 tasks
          //  we can put the task in the corner or any PE that has at least 2 connected PEs
          // hence we combine these two options
          val potential = corners ++ pes.filter(_.connected.size >= 2)
          // and find an appropriate PE
          findLeastUtilisedPe(currentTask, mapping, p => potential.contains(p)).head
        case size if size <= 4 => // if we communicate with 4 or 3 tasks
          // we can assign them to adjacent (connected) PEs
          // find pe which has 4 adjacent PEs and is least utilised by our current mapping
          val potential = pes.filter(_.connected.size == 4)
          findLeastUtilisedPe(currentTask, mapping, p => potential.contains(p)).head
        case _ => // in all other cases...
          // we attempt to find a PE that has a least 8 neighbors assign them to neighbours as well
          val potential = pes.filter(_.neighbors().size >= 8)
          findLeastUtilisedPe(currentTask, mapping, p => potential.contains(p)).head
      }

      // map current task to PE for convenience
      taskToPe(currentTask) = Some(leastUsedPe)

      // add task mapping to the existing mapping and continue the recursion
      // with tasks left without the current task (tasksLeft.tail)
      mkMap(mapping + (leastUsedPe -> (mapping(leastUsedPe) :+ currentTask)), tasksLeft.tail)
    }
  }

  // initialise mapping map
  val mapping: Map[PE, Seq[Task]]             = pes.map(pe => pe -> Seq.empty[Task]).toMap
  var taskToPe: mutable.Map[Task, Option[PE]] = mutable.Map.empty ++ tasks.map(_ -> None)

  // We can now calculate the importance of each task. We will then sort the list by importance
  // so we first analyse the task with the highest value.
  val tasksByImportance = tasks
    .map { t =>
      t.id -> heuristic(t, tasks)
    }
    .sortBy(_._2)
    .reverse

  // finally, create the mapping
  val finalMapping = mkMap(mapping, tasksByImportance)

  tasksByImportance.foreach {
    case (id, value) => println(s"$id, $value")
  }

  taskToPe.toSeq.sortBy(_._1.id).foreach {
    case (task, pe) => println(s"${task.id} -- $pe")
  }

  // and print out the mapping in the same "order" the PEs are set
  for (y <- meshY - 1 to 0 by -1) {
    val column = for (x <- 0 until meshX) yield {
      val ts: Seq[Task] = finalMapping(PE(x, y))
      if (ts.isEmpty) "NaN"
      else s"${ts.map(_.id).mkString(",")}"
    }
    print(column.mkString("\t\t\t"))
    println()
  }

  println()
}
