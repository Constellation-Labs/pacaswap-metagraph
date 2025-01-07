package org.amm_metagraph.l0

import eu.timepit.refined.api.Refined
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Interval

package object rewards {
  type MonthRefinement = Interval.Closed[1, 12]
  type Month = Int Refined MonthRefinement
}
