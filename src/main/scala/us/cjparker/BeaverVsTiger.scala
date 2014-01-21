package us.cjparker


import java.io._
import scala.io.Source.fromInputStream
import processing.core._
import java.awt._
import java.awt.event.KeyEvent._
import processing.core.PApplet._
import processing.core.PConstants._
import java.util.Random
import java.applet.AudioClip
import java.applet.Applet
import java.net.URL
import java.lang.Math
import scala.collection.immutable.{Map => ScalaMap}
import scala.collection.immutable.{List => SL}
import scala.collection.mutable.{HashMap => ScalaHM}
import scala.collection.mutable.{ListBuffer => ScalaLB}
import scala.collection.mutable.{HashSet => ScalaHS}
import java.applet.AudioClip
import java.applet.Applet


sealed trait ShapeType

case object Rect extends ShapeType
case object Ellipse extends ShapeType
case object Image extends ShapeType
case object Line extends ShapeType

sealed trait GameItem
case object Apple extends GameItem
case object Beaver extends GameItem
case object Tiger extends GameItem
case object PowerMeter extends GameItem
case object Other extends GameItem

case class AttachedShape(shape: SmartShape, xOffset: Float, yOffset: Float)

case class Waypoint(x: Float, y: Float, speed: Int)

trait FollowsPath {
  self: SmartShape =>
  var points: SL[Waypoint] = Nil
  var currentPointIndex: Int = 0

  pathFollower = () => {
    val currentDestPoint = points(currentPointIndex)
    // cycle through waypoints
    if (Math.abs(xPos - currentDestPoint.x) <= 2.0f && Math.abs(yPos - currentDestPoint.y) <= 2.0f)
      currentPointIndex = (currentPointIndex + 1) % points.size
    val rise = currentDestPoint.y - yPos
    val run = currentDestPoint.x - xPos
    val slope = run match {
      case 0 => 0;
      case _ => rise / run
    }
    val angleToX = Math.toDegrees(Math.atan(slope))
    val xPercent = rise match {
      case 0 => 1.0f;
      case _ => 1.0f - angleToX / 90.0f
    }
    val yPercent = run match {
      case 0 => 1.0f;
      case _ => 1.0f - xPercent
    }
    xSpeed = Math.abs(currentDestPoint.speed.toFloat * xPercent.toFloat)
    ySpeed = Math.abs(currentDestPoint.speed.toFloat * yPercent.toFloat)
    if (rise > 0)
      yDir = 1
    else if (rise == 0)
      yDir = 0
    else
      yDir = -1

    if (run > 0)
      xDir = 1
    else if (run == 0)
      xDir = 0
    else
      xDir = -1

    ()
  }
}

case class SmartShape(
                       val pApp: PApplet, // required
                       val shapeType: ShapeType = Rect,
                       var xPos: Float = 0.0f,
                       var yPos: Float = 0.0f,
                       var xDir: Int = 0,
                       var lastXDir: Int = 0,
                       var yDir: Int = 0,
                       var height: Float = 10.0f,
                       var width: Float = 10.0f,
                       var xSpeed: Float = 0.0f,
                       var ySpeed: Float = 0.0f,
                       var fillColor: Tuple3[Float, Float, Float] = (128.0f, 128.0f, 128.0f),
                       var strokeColor: Tuple3[Float, Float, Float] = (128.0f, 128.0f, 128.0f),
                       var strokeWeight: Float = 1.0f,
                       var stroke: Boolean = true,
                       var fill: Boolean = true,
                       var normalImage: PImage = null,
                       var obeyGravity: Boolean = true,
                       attachedShapes: ScalaHM[String, AttachedShape] = new ScalaHM[String, AttachedShape](),
                       var movesWithParent: Boolean = false,
                       var lifeRemaining: Int = 5,
                       gameItem: GameItem = Other) {

  var tossCounter: Int = 0
  var tossSpeed: Float = 0.0f
  var shapeAlive: Boolean = true
  var maxLife: Int = 5
  var rotPosRadians: Float = 0.0f
  var endXPos = 0.0f
  var endYPos = 0.0f
  var keyPressHandler: (Int) => Unit = (x => ())
  var keyReleaseHandler: (Int) => Unit = (x => ())
  var collisionHandler: (SmartShape) => Unit = (x => ())
  var movementBehaviors: ScalaHM[String, () => Unit] = new ScalaHM[String, () => Unit]()
  movementBehaviors += Tuple2("default", () => Unit)

  var currentBehavior: String = "default"
  var boundaryHandler: () => Unit = () => Unit
  var gameDeath: () => Unit = () => Unit
  var timerCallbacks: ScalaHM[Long, () => Unit] = new ScalaHM[Long, () => Unit]()
  val gravityConstant = 4.0f
  var pathFollower: () => Unit = () => Unit


  def isCollidingWith(otherShape: SmartShape): Boolean = {

    val collisionPaddingPixels = 15

    if (this.eq(otherShape))
      false

    // if the other shape is an 'attached' shape, false
    if (attachedShapes.exists { x => 
      val (_, as) = x 
      as.shape.eq(otherShape) 
    } || 
      otherShape.attachedShapes.exists { x => val (_, as) = x 
      as.shape.eq(this)
    })
      false
    else {
      val otherLeftMost = otherShape.xPos - otherShape.width / 2.0
      val otherRightMost = otherShape.xPos + otherShape.width / 2.0
      val thisLeftMost = this.xPos - otherShape.width / 2.0
      val thisRightMost = this.xPos + otherShape.width / 2.0
      val otherTop = otherShape.yPos - otherShape.height / 2.0
      val otherBot = otherShape.yPos + otherShape.height / 2.0
      val thisTop = this.yPos - this.height / 2.0
      val thisBot = this.yPos + this.height / 2.0

      // the overlaps take a little bit of padding into account, such that the shapes have to overlap more
      // than just touching
      val xOverlap =
        thisRightMost - collisionPaddingPixels >= otherLeftMost &&
        thisLeftMost + collisionPaddingPixels <= otherRightMost ||
        otherRightMost - collisionPaddingPixels >= thisLeftMost &&
        otherLeftMost + collisionPaddingPixels <= thisRightMost

      val yOverlap =
        thisBot - collisionPaddingPixels >= otherTop &&
        thisTop + collisionPaddingPixels <= otherBot ||
        otherBot - collisionPaddingPixels >= thisTop &&
        otherTop + collisionPaddingPixels <= thisBot

      xOverlap && yOverlap
    }
  }


  def move() = {
    // don't move this shape if it should move according to the shape its attached to
    if (!movesWithParent) {
      pathFollower()
      movementBehaviors(currentBehavior)()
      boundaryHandler()

      yPos += ySpeed * yDir
      xPos += xSpeed * xDir

      // move attached shapes
      attachedShapes.foreach {
        case (attachedShapeName, attachedShape) =>
          attachedShape.shape.xPos = xPos + attachedShape.xOffset
          attachedShape.shape.yPos = yPos + attachedShape.yOffset
      }
    }
  }


  private def handleCallbacks()  {
    timerCallbacks.foreach { callback =>
      val (frameCount, cbFunc) = callback
      if (pApp.frameCount >= frameCount) {
        cbFunc()
        timerCallbacks -= frameCount // TODO, THIS IS WEAK!!!!
      }
    }
  }



  def drawMe() {

    handleCallbacks()

    if (stroke) {
      pApp.stroke(strokeColor._1, strokeColor._2, strokeColor._3)
      pApp.strokeWeight(strokeWeight)
    } else {
      pApp.noStroke
    }

    if (fill)
      pApp.fill(fillColor._1, fillColor._2, fillColor._3)
    else
      pApp.noFill

    pApp.pushMatrix

    if (rotPosRadians != 0.0) {
      pApp.translate(xPos, yPos)
      pApp.rotate(PApplet.radians(rotPosRadians))
      pApp.translate(-xPos, -yPos)
    }

    shapeType match {
      case Rect => pApp.rect(xPos, yPos, width, height)
      case Ellipse => pApp.ellipse(xPos, yPos, width, height)
      case Image => pApp.image(normalImage, xPos, yPos, width, height)
      case Line => pApp.bezier(xPos, yPos, xPos, yPos, endXPos, endYPos, endXPos, endYPos)
      case _ => println("cant draw unknown shape")
    }
    pApp.popMatrix
  }


  def toss(yTossSpeed: Float = 10.0f, xTossSpeed: Float = 0.0f, xTossDir: Int = 1) {
    tossSpeed = yTossSpeed
    // negative y means 'up'
    yDir = -1

    // set the initial speed of the 'toss'
    ySpeed = yTossSpeed
    xSpeed = xTossSpeed
    xDir = xTossDir
  }


  def fall() {
    if (ySpeed <= 0) {
      yDir *= -1
      ySpeed = Math.abs(ySpeed)
    }

    ySpeed += 1 * yDir
  }


  def handleKeyPress(keyNum: Int) {
    keyPressHandler(keyNum)
  }


  def handleKeyRelease(keyNum: Int) {
    keyReleaseHandler(keyNum)
  }


  def attachShape(s: SmartShape, name: String)(xPos: Float, yPos: Float) {
    attachedShapes += name -> AttachedShape(s, xPos, yPos)
    s.movesWithParent = true
  }


  def detachShape(name: String): Option[SmartShape] = {
    attachedShapes.get(name) match {
      case Some(as: AttachedShape) =>
        as.shape.movesWithParent = false
        attachedShapes -= name
        Some(as.shape)
      case None =>
        println("couldn't detach shape " + name)
        None
    }
  }


  def collisionEvent(otherShape: SmartShape) {
    collisionHandler(otherShape)
  }
}


class BeaverVsTigerFrame extends Frame("Beaver vs. Tiger") {
  setLayout(new BorderLayout())
  val embed = new BeaverVsTigerApplet()
  add(embed, BorderLayout.CENTER)
  embed.init()
  pack
  setLocation(100, 100)
  setVisible(true)
}


object BeaverVsTigerMain {
  val windowWidth = 850
  val windowHeight = 680

  def main(args: Array[String]) {
    println("starting beaver vs tiger")

    new BeaverVsTigerFrame
  }
}


class BeaverVsTigerApplet extends PApplet {
  private var smartShapes: ScalaLB[SmartShape] = new ScalaLB[SmartShape]()
  private var tempTossDir: Int = -1
  private var keysDown: ScalaHS[Int] = new ScalaHS[Int]()
  private var keyTimers: ScalaHM[Int, Float] = new ScalaHM[Int, Float]()
  private var lastKeyDuration: ScalaHM[Int, Float] = new ScalaHM[Int, Float]()
  private var images: ScalaMap[String, PImage] = ScalaMap[String, PImage]()
  private var sounds: ScalaMap[String, AudioClip] = ScalaMap[String, AudioClip]()
  private val rand = new Random
  private var birdTimer: Int = 30

  def randSign: Int = rand.nextInt(2) match {
    case 0 => 1
    case 1 => -1
  }


  override def init() {
    setSize(BeaverVsTigerMain.windowWidth, BeaverVsTigerMain.windowHeight)
    setPreferredSize(new Dimension(BeaverVsTigerMain.windowWidth, BeaverVsTigerMain.windowHeight))
    super.init()
  }


  override def setup() {
    try {
      size(BeaverVsTigerMain.windowWidth, BeaverVsTigerMain.windowHeight)
      rectMode(CENTER)
      ellipseMode(CENTER)
      imageMode(CENTER)
      noStroke()
      frameRate(30)

      images = loadImages
      sounds = loadSounds

      // APPLE
      def makeApple(x: Float, y: Float) = {
        val app = SmartShape(
          this,
          shapeType = Image,
          xPos = x,
          yPos = y,
          width = 40.0f,
          height = 40.0f,
          normalImage = images("apple"),
          gameItem = Apple)

        app.collisionHandler = {
          case s: SmartShape if (s.gameItem == Tiger) => app.shapeAlive = false
          case _ => ()
        }

        app.gameDeath = () => {
          // add an entry to the map, where the
          // KEY is a long, which is a frameCount
          // VALUE is a function from nothing to unit (like a call by name)
          // handle the 'death' of the apple (when it hits the bottom) by erasing it
          app.timerCallbacks += Tuple2((frameCount + frameRate * 3).toLong, () => {
            app.shapeAlive = false
            ()
          })
        }

        app.boundaryHandler = () => {
          // handle the bottom of the screen edge, kickoff the apple death timer when apples are stuck at the bottom
          if (app.yPos + 10 + app.height / 2 > height) {
            app.yDir = 0; app.yPos = height - app.height / 2 - 10; app.xDir = 0; app.gameDeath()
          }

          // handle the walls
          if (app.xPos - app.width / 2 < 0 || app.xPos + app.height / 2 > width) {
            app.xDir *= -1
          }

          if (app.yPos - app.height / 2 < height - 5 && app.obeyGravity == true) {
            // if it's off the ground and not rising anymore, it should fall
            app.fall
          }
        }

        app
      }

      // BEAVER
      val beavStartX = 50.0f
      val beavStartY = height - 25.0f
      val beaver = SmartShape(
        this,
        shapeType = Image,
        xPos = beavStartX,
        yPos = beavStartY,
        width = 100.0f,
        height = 100.0f,
        obeyGravity = false,
        normalImage = images("beaver-right"),
        xSpeed = 5,
        ySpeed = 5,
        gameItem = Beaver)

      val apple = makeApple(x = beavStartX, y = beavStartY)
      beaver.attachShape(apple, "apple")(0.0f, 0.0f)

      // the beaver moves around with the arrow keys
      beaver.keyPressHandler = {
        case 37 => {
          beaver.xDir = -1; beaver.normalImage = images("beaver-left"); beaver.lastXDir = -1
        }
        case 39 => {
          beaver.xDir = 1; beaver.normalImage = images("beaver-right"); beaver.lastXDir = 1
        }
        case 38 => beaver.yDir = -1
        case 40 => beaver.yDir = 1
        case _ => ()
      }

      beaver.keyReleaseHandler = {
        // handle the movement
        case 38 | 40 => beaver.yDir = 0
        case 37 | 39 => beaver.xDir = 0

        // handle throwing things
        case VK_SPACE =>
          tempTossDir *= -1
          val yTossSpeed = lastKeyDuration.get(VK_SPACE) match {
            case Some(f: Float) => f
            case None => 1.0f
          }

          // detach
          beaver.detachShape("apple") match {
            case Some(s: SmartShape) =>
              s.toss(yTossSpeed = yTossSpeed, xTossSpeed = yTossSpeed, beaver.lastXDir)
              sounds("toss").play

              // reattach, reload
              val newApple = makeApple(beaver.xPos, beaver.yPos)
              smartShapes.insert(0, newApple) // add apple to the front so other things draw on top of it
              beaver.attachShape(newApple, "apple")(0.0f, 0.0f)

            case None =>
              println("could find no shape %s to toss".format("apple"))
          }
        case _ => ()
      }


      // TIGER
      val tiger = SmartShape(
        this,
        shapeType = Image,
        xPos = width - 150.0f / 2,
        yPos = height - 113.0f / 2,
        width = 150.0f,
        height = 113.0f,
        normalImage = images("tiger"),
        gameItem = Tiger,
        lifeRemaining = 5)

      tiger.movementBehaviors += Tuple2("default", () => {
        tiger.xSpeed = rand.nextInt(4) + 1
        tiger.ySpeed = rand.nextInt(4) + 1
        if (frameCount % 60 == 0) {
          tiger.xDir = randSign
          tiger.yDir = randSign
        }

        if (tiger.xDir > 0)
          tiger.normalImage = images("tiger")
        else
          tiger.normalImage = images("tiger-left")
      })

      tiger.boundaryHandler = () => {
        if (tiger.xPos + tiger.width / 2 > this.width || tiger.xPos - tiger.width / 2 < 0) {
          tiger.xDir *= -1
        }

        if (tiger.yPos + tiger.height / 2 > this.height || tiger.yPos - tiger.height / 2 < 0) {
          tiger.yDir *= -1
        }
      }

      tiger.gameDeath = () => {
        tiger.xDir = 0
        tiger.yDir = 0
        sounds("tigerdie").play
        tiger.rotPosRadians = 180
        tiger.movementBehaviors("default") = () => Unit
      }

      // TREE
      val treeImg = images("tree")
      val tree = SmartShape(
        this,
        shapeType = Image,
        xPos = 150,
        yPos = 250,
        width = treeImg.width,
        height = treeImg.width,
        normalImage = treeImg)


      // METER
      val meterMaxHeight = 150
      val meterBottomYPos = 175
      val tigerPowerMeterOutline = SmartShape(
        this,
        shapeType = Rect,
        xPos = width - 30,
        yPos = meterBottomYPos - meterMaxHeight / 2,
        width = 20,
        height = meterMaxHeight,
        normalImage = treeImg,
        fill = false,
        stroke = true,
        strokeWeight = 3,
        strokeColor = (0, 0, 0))

      val tigerPowerMeterFill = SmartShape(
        this,
        shapeType = Rect,
        xPos = width - 30,
        yPos = meterBottomYPos - meterMaxHeight / 2,
        width = 20,
        height = meterMaxHeight,
        normalImage = treeImg,
        fill = true,
        stroke = false,
        fillColor = (0, 255, 0))

      // SUN
      val sunImg = images("sun")
      val sun = SmartShape(
        this,
        shapeType = Image,
        xPos = width - sunImg.width / 2,
        yPos = sunImg.width / 2,
        width = sunImg.width,
        height = sunImg.width,
        normalImage = sunImg)

      tiger.collisionHandler = {
        case ss: SmartShape if (ss.gameItem == Apple && tiger.lifeRemaining > 0) =>
          tiger.lifeRemaining -= 1
          tigerPowerMeterFill.height = (meterMaxHeight / tiger.maxLife) * tiger.lifeRemaining
          tigerPowerMeterFill.yPos = meterBottomYPos - tigerPowerMeterFill.height / 2
          if (tiger.lifeRemaining >= 1)
            sounds("roar").play
          else
            tiger.gameDeath()
        case _ => ()
      }


      // BIRD
      val birdStartXPos = 40
      val birdStartYPos = 40
      val birdImg = images("bird")
      val bird = new SmartShape(
        this,
        shapeType = Image,
        xPos = birdStartXPos,
        yPos = birdStartYPos,
        width = birdImg.width,
        height = birdImg.height,
        normalImage = birdImg) with FollowsPath

      bird.points =
        SL(
          Waypoint(50.0f, 100.0f, 5), // upper left
          Waypoint(BeaverVsTigerMain.windowWidth / 2, 150.0f, 5), // center screen, a bit lower
          Waypoint(BeaverVsTigerMain.windowWidth - 100, 100.0f, 5) // upper right
        )

      bird.movementBehaviors += Tuple2("chaseBeaver", () => {
        bird.points = SL(Waypoint(beaver.xPos, beaver.yPos, 7))
        bird.currentPointIndex = 0
      })

      bird.movementBehaviors += Tuple2("flyAway", () => {
        // send the bird flying away
        bird.points = SL(Waypoint(BeaverVsTigerMain.windowWidth / 2, -200.0f, 8))
        bird.currentPointIndex = 0
      })

      bird.movementBehaviors += Tuple2("default", () => {
        // bump the bird around a little bit as it follows its path
        // for a few frames, screw with the speed a little
        if (frameCount % frameRate >= 6) {
          bird.xSpeed += randSign * (rand.nextInt(3) + 1) * randSign
          bird.ySpeed += randSign * (rand.nextInt(3) + 1) * randSign
        }

        // turn it around
        if (bird.xDir > 0)
          bird.normalImage = images("bird")
        else
          bird.normalImage = images("bird-left")

        // after some time, send the bird after the beaver
        if (birdTimer <= 0) {
          // change the behavior to chase the beaver
          bird.currentBehavior = "chaseBeaver"

          // add a collision handler for the bird / beaver
          bird.collisionHandler = {
            case ss: SmartShape if ss.gameItem == Beaver => {
              // attach the beaver to the bird
              bird.attachShape(beaver, "beaver")(0.0f, 80.0f)

              // send the bird flying away
              bird.currentBehavior = "flyAway"
            }
            case _ => ()
          }
        }
      })

      smartShapes += apple
      smartShapes += beaver
      smartShapes += tiger
      smartShapes += tree
      smartShapes += tigerPowerMeterFill
      smartShapes += tigerPowerMeterOutline
      smartShapes += sun
      smartShapes += bird

    } catch {
      case (e: Exception) =>
        e.printStackTrace
    }
    ()
  }


  private def loadImages = {
    images += "beaver-left" -> loadImage("beaver-left.png")
    images += "beaver-right" -> loadImage("beaver-right.png")
    images += "beaver-right-tail-up" -> loadImage("beaver-right-tail-up.png")
    images += "beaver-left-tail-up" -> loadImage("beaver-left-tail-up.png")
    images += "apple" -> loadImage("apple.png")
    images += "tiger" -> loadImage("tiger.png")
    images += "tiger-left" -> loadImage("tiger-left.png")
    images += "tree" -> loadImage("tree.png")
    images += "back" -> loadImage("back.png")
    images += "sun" -> loadImage("sun.png")
    images += "bird" -> loadImage("bird.png")
    images += "bird-left" -> loadImage("bird-left.png")
    images
  }

  private def loadSounds = {
    val cl = Thread.currentThread.getContextClassLoader
    sounds += "toss" -> Applet.newAudioClip(cl.getResource("toss.aiff"))
    sounds += "roar" -> Applet.newAudioClip(cl.getResource("roar.aiff"))
    sounds += "tigerdie" -> Applet.newAudioClip(cl.getResource("tigerdie.aiff"))
    sounds
  }


  override def draw = {
    try {
      background(images("back"))

      smartShapes = smartShapes.filter(_.shapeAlive == true)

      handleKeyTimings

      handleCollision(smartShapes)

      smartShapes.foreach {
        shape =>
          shape.move
          shape.drawMe
      }

      //fill(50,111,200)
      fill(0, 255, 0) // bright green
      textSize(40)
      text(birdTimer + "", BeaverVsTigerMain.windowWidth - 300, 50)
      if (birdTimer > 0 && Math.floor(frameCount % frameRate) == 0)
        birdTimer -= 1

    } catch {
      case (e: Exception) =>
        e.printStackTrace
    }
  }


  private def handleCollision(shapes: ScalaLB[SmartShape]) = {
    // for each combination of shapes (not permutations)
    shapePairings(Nil, shapes.toList).foreach {
      shapePair =>
        if (shapePair._1.isCollidingWith(shapePair._2)) {
          shapePair._1.collisionEvent(shapePair._2)
          shapePair._2.collisionEvent(shapePair._1)
        }
    }
  }


  private def shapePairings(acc: SL[Tuple2[SmartShape, SmartShape]], list: SL[SmartShape]): SL[Tuple2[SmartShape, SmartShape]] = {
    if (list.size <= 0)
      acc
    else
      shapePairings(acc ::: list.tail.map(item => Tuple2(list.head, item)), list.tail)
  }


  private def handleKeyTimings() = {
    // count how long keys are being held down
    keysDown.foreach {
      keyThatIsDown =>
        keyTimers.get(keyThatIsDown) match {
          case Some(v: Float) =>
            // increment the number of frames that this key has been down
            keyTimers += keyThatIsDown -> (v + 1.0f)
          case None =>
            keyTimers += keyThatIsDown -> 1.0f
        }
    }
  }


  override def keyPressed {
    try {
      keysDown += keyCode
      smartShapes.foreach {
        _.handleKeyPress(keyCode)
      }
    } catch {
      case e: Exception =>
        println("caught exception handling key press")
        e.printStackTrace
    }
  }


  override def keyReleased {
    try {
      keysDown -= keyCode
      lastKeyDuration += keyCode -> keyTimers(keyCode)
      keyTimers += keyCode -> 0.0f
      //println("key %s held for frames %s".format(keyCode, lastKeyDuration(keyCode)))
      smartShapes.foreach {
        _.handleKeyRelease(keyCode)
      }
    } catch {
      case e: Exception =>
        println("caught exception handling key release ")
        e.printStackTrace
    }
  }
}




