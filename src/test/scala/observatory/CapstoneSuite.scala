package observatory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CapstoneSuite
  extends ExtractionTest
    with VisualizationTest
    with InteractionTest
    with Visualization2Test
    with Interaction2Test

