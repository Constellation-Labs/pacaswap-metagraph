package org.amm_metagraph.shared_data

import io.constellationnetwork.security.hash.Hash

import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.Staking.getStakingCalculatedState
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.types.Swap.getSwapCalculatedState

object AllowSpends {

  def getAllAllowSpendsInUseFromState(
    ammCalculatedState: AmmCalculatedState
  ): Set[Hash] = {
    val lpAllowSpends = getLiquidityPoolCalculatedState(ammCalculatedState).getPendingUpdates
      .flatMap(u => List(u.tokenAAllowSpend, u.tokenBAllowSpend))

    val stakingAllowSpends = getStakingCalculatedState(ammCalculatedState).getPendingUpdates
      .flatMap(u => List(u.tokenAAllowSpend, u.tokenBAllowSpend))

    val swapAllowSpends = getSwapCalculatedState(ammCalculatedState).getPendingUpdates
      .map(_.allowSpendReference)

    lpAllowSpends ++ stakingAllowSpends ++ swapAllowSpends
  }
}
